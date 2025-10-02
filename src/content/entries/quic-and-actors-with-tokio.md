---
title: 'QUIC and Actors with Tokio'
description: "Modular server design: QUIC networking with Tokio actors"
pubDate: 'Mar 1 2025'
editDate: 'Sep 25 2025'
tags: ["QUIC", "Tokio", "Systems"]
group: "Software Architecture"
---

After discovering [A. Ryhl, Actors with Tokio](https://ryhl.io/blog/actors-with-tokio/) in my search of architecting servers in a more modular way via common encapsulation patterns, I was delighted to finally find something that helped me understand the bigger picture. This resource made me rethink server architecture and provided valuable insights into the use of actors with Tokio. The explanations were clear, and the examples were practical, making it an excellent starting point for anyone interested in this topic. However, while it was incredibly informative, I found it did not fully satisfy my needs in my endeavors. I was left wanting more detailed guidance and advanced techniques to further enhance my server architecture.

*You can find an example server and client (and necessary configuration for QUIC) for this post [here](https://github.com/meowesque) - for simplicity sake we only go over the server's implementation.*

## Actors with Tokio

The gist of [A. Ryhl, Actors with Tokio](https://ryhl.io/blog/actors-with-tokio/) is how an actor is split into a handle (also referred to as a proxy) and the task. Typically the task is an I/O operation that the handle communicates with, providing a simple interface for the programmer whilst keeping everything decoupled. For example let's first look at a simple (pure) actor and handle that doesn't actually interact with the "outside world," merely producing naturals incrementally:

```rust
use tokio::sync::{oneshot, mpsc};

enum Msg {
  Next(oneshot::Sender<u64>),
}

struct Actor {
  id: u64,
  rx: mpsc::Receiver<Msg>,
}

impl Actor {
  fn new(rx: mpsc::Receiver<Msg>) -> Self {
    Self { 
      id: 0,
      rx,
    }
  }

  async fn run(mut self) {
    while let Some(msg) = self.rx.recv().await {
      self.update(msg);
    }
  }

  fn update(&mut self, msg: Msg) {
    match msg {
      Msg::Next(tx) => {
        // We specify `let _ = ...` to ignore handling an error, intentionally!
        let _ = tx.send(self.id);

        self.id += 1;
      }
    }
  }
}
```

Initially, you may recognize that this follows a reactive pattern - and you'd be right! This is where the implementation of the task lies. 

In the method `Actor::run` we take ownership of the actor, wait for incoming messages indefinitely. If at any point `self.rx.recv()` returns `Option::None` it's presumed that all senders to our receiver have been dropped, we then gracefully shut down.

Within the `Actor::update` method, we intentionally ignore the possibility of an error if we fail to send our response. Again, we presume any receiver has been dropped. So we'll just pretend like nothing happened.

### Handle Implementation

```rust
const CHANNEL_SIZE: usize = 2;

// Note `pub` as this is what the user actually interfaces with.
#[derive(Clone)]
pub struct Handle {
  tx: tokio::sync::mpsc::Sender<Msg>
}

impl Handle {
  pub fn new() -> Self {
    let (rx, tx) = tokio::sync::mpsc::channel(CHANNEL_SIZE);
    let actor = Actor::new(rx);

    tokio::spawn(async move { actor.run().await });

    Self { tx }
  }

  /// Retrieve the next unique id.
  pub async fn next(&self) -> u64 {
    let (tx, rx) = tokio::oneshot::channel();

    // If `self.tx.send` fails, so does `rx.await`.
    // There's no reason to check for errors twice.
    let _ = self.tx.send(Msg::Next(tx)).await;

    rx.await.expect("Actor died")
  }
}
```

This is the actor's handle, responsible for spawning a task of which the actor resides, communicating with the actor and providing an interface for the programmer.

## Integrating Our Actors with [quinn](https://crates.io/crates/quinn)

The example project can be found [here](https://github.com/meowesque/quic-and-actors-with-tokio), where you can find the implementation of the client as well, I haven't included it here given that both the server and the client have nearly identical code when it comes to the actors listed below. 

Now that we have a basic idea of what an actor looks like, let's build a basic server with QUIC! Our server will be broken into several pieces:

  - [Listener](#listener) Accepts incoming clients and sets up our actors. 

  - [Inbound](#inbound) Receives incoming messages from the client.

  - [Outbound](#outbound) Sends messages to the client.

  - [Dispatch](#dispatch) Handles each client message and acts as a switch for each actors.

Splitting our actors up into very basic responsibilities is convenient for multiple reasons:

  1. The architecture of the server becomes far more reasonable to work with when amassing more complex tasks.
  
  2. Legibility is increased given that the effects of an actor is much more apparent in contrast to a monolithic 
  design.

  3. Actors provide a form of state encapsulation, keeping moving parts consolidated.

  4. Decoupling provides easier error recovery without sacrificing simplicity.

### Listener Implementation

```rust
struct Actor {
  endpoint: quinn::Endpoint,
}

impl Actor {
  async fn run(mut self) {
    while let Some(incoming) = self.endpoint.accept().await {
      log::info!("Accepting connection from {}", incoming.remote_address());

      tokio::spawn(async move {
        // Accept bidirectional channels from the incoming connection
        let (send, recv) = incoming
          .await
          .expect("Failed to accept incoming connection")
          .accept_bi()
          .await
          .expect("Failed to accept a bidirectional stream");

        let outbound = outbound::Handle::new(send);
        let dispatch = dispatch::Handle::new(outbound);
        let inbound = inbound::Handle::new(recv, dispatch);

        inbound.join().await;
      });
    }
  }
}

pub struct Handle {
  join_handle: task::JoinHandle<()>,
}

impl Handle {
  pub fn new(endpoint: quinn::Endpoint) -> Self {
    let actor = Actor { endpoint };

    let join_handle = tokio::spawn(async move { actor.run().await });

    Self { join_handle }
  }

  /// Wait for the listener to terminate.
  pub async fn join(self) {
    self.join_handle.await.expect("Listener actor panicked");
  }
}
```

[Source](https://github.com/meowesque/quic-and-actors-with-tokio/blob/main/crates/server/src/actors/listener.rs)

Within `Actor::run` whenever we accept an incoming connection, we'll accept bidirectional channels to separate receiving and sending data into two actors: [Inbound](#inbound) and [Outbound](#outbound). The [inbound actor](#inbound) will be equipped with its own newly created [dispatch](#dispatch) actor handle.

### Inbound

```rust
const CHANNEL_SIZE: usize = 8;
const BUFFER_SIZE: usize = 1024 * 8;

struct Actor {
  stream: quinn::RecvStream,
  dispatch: dispatch::Handle,
}

impl Actor {
  async fn run(mut self) {
    let mut buffer = Box::new([0u8; BUFFER_SIZE]);

    loop {
      match self
        .stream
        .read(buffer.as_mut())
        .await
        .expect("Failed to read stream")
      {
        Some(0) | None => continue,
        Some(read) => {
          let client_message =
            bitcode::decode(&buffer[..read]).expect("Failed to decode ClientMessage");

          self.dispatch.send(client_message).await;
        }
      }
    }
  }
}

pub struct Handle {
  join_handle: task::JoinHandle<()>,
}

impl Handle {
  pub fn new(stream: quinn::RecvStream, dispatch: dispatch::Handle) -> Self {
    let actor = Actor { stream, dispatch };

    let join_handle = tokio::spawn(async move { actor.run().await });

    Self { join_handle }
  }

  /// Wait for the actor to finish processing inbound messages.
  pub async fn join(self) {
    self.join_handle.await.expect("Failed to join actor");
  }
}
```

[Source](https://github.com/meowesque/quic-and-actors-with-tokio/blob/main/crates/server/src/actors/inbound.rs)

This is our first seemingly complex actor, the goal here is to receive incoming data and deserialize it with [bitcode](crates.io/crates/bitcode) and send it off to be dispatched. Any deserialization crate (like [bincode](crates.io/crates/bincode)) will do. For performance and memory efficient applications that need to scale, [bitcode](crates.io/crates/bitcode) may be preferable.  

### Outbound

```rust
const CHANNEL_SIZE: usize = 8;

struct Actor {
  stream: quinn::SendStream,
  rx: mpsc::Receiver<proto::server::Message>,
}

impl Actor {
  async fn run(mut self) {
    while let Some(msg) = self.rx.recv().await {
      self.send(msg).await;
    }
  }

  async fn send(&mut self, message: proto::server::Message) {
    let buffer = bitcode::encode(&message);

    self
      .stream
      .write_all(&buffer)
      .await
      .expect("Failed to write message to stream");

    self.stream.flush().await.expect("Failed to flush stream");
  }
}

#[derive(Clone)]
pub struct Handle {
  tx: mpsc::Sender<proto::server::Message>,
}

impl Handle {
  pub fn new(stream: quinn::SendStream) -> Self {
    let (tx, rx) = mpsc::channel(CHANNEL_SIZE);
    let actor = Actor { stream, rx };

    tokio::spawn(async move { actor.run().await });

    Self { tx }
  }

  pub async fn send(&self, message: proto::server::Message) {
    self
      .tx
      .send(message)
      .await
      .expect("Failed to send message");
  }
}
```

[Source](https://github.com/meowesque/quic-and-actors-with-tokio/blob/main/crates/server/src/actors/outbound.rs)

This actor is trivial, existing only to encode messages and send them to the channel.

### Dispatch

```rust
const CHANNEL_SIZE: usize = 16;

struct Actor {
  rx: mpsc::Receiver<proto::client::Message>,
  outbound: outbound::Handle,
}

impl Actor {
  async fn run(mut self) {
    while let Some(message) = self.rx.recv().await {
      log::info!("Received message: {:?}", message);

      // Match on message, communicate with other actors, etc.
    }
  }
}

#[derive(Clone)]
pub struct Handle {
  tx: mpsc::Sender<proto::client::Message>,
}

impl Handle {
  pub fn new(outbound: outbound::Handle) -> Self {
    let (tx, rx) = mpsc::channel(CHANNEL_SIZE);
    let actor = Actor { rx, outbound };

    tokio::spawn(async move { actor.run().await });

    Self { tx }
  }

  pub async fn send(&self, message: proto::client::Message) {
    self
      .tx
      .send(message)
      .await
      .expect("Failed to send actor a message");
  }
}
```

[Source](https://github.com/meowesque/quic-and-actors-with-tokio/blob/main/crates/server/src/actors/dispatch.rs)

The purpose of this actor is to only communicate with other actors, possibly even keeping track of certain events (like authentication).

## Improvements

Overall, there are a few things I would want to improve in a production scenario:

  1. Buffering the outbound actor, saving flush times. This could be especially critical for high throughput scenarios.

  2. Utilize chunking on the inbound actor for handling potentially large portions of data.

  3. Tracking each client handle within the listener actor.

Regardless, I've been really enjoying my experience implementing actors with [tokio](crates.io/crates/tokio) and [quinn](crates.io/crates/quinn).