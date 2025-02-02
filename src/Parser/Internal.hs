module Parser.Internal
  ( AlexInput (..),
    mkAlexInput,
    alexGetByte,
    alexInputPrevChar,
    PState (..),
    mkPState,
    P (..),
    fail,
    throwError,
    catchError,
    getAlexInput,
    getAlexMode,
    setAlexInput,
    pushToken,
    popToken',
    popToken,
    L (..),
    liftP,
    modeL,
    excerptL,
    tokenL,
  )
where

import Control.Monad (ap, liftM)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.State.Strict (MonadState (..), gets, modify)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Source (Source (..))
import Data.SourcePos (SourcePos (..))
import Data.SourcePos qualified as SourcePos
import Data.SourceSpan (SourceSpan)
import Data.Text qualified as StrictText
import Data.Text.Lazy qualified as LazyText
import Data.Word (Word8)
import Deque.Strict (Deque)
import Deque.Strict qualified as Deque
import Encoding.UTF8 qualified as UTF8
import Parser.Error (PError (..))
import Syntax.Token (Token (..), TokenClass)

type AlexMode = Int

data AlexInput = AlexInput
  { source :: Source,
    pos :: !SourcePos,
    cur :: !Char,
    prev :: !Char,
    offset :: !Int64,
    rest :: !LazyText.Text,
    codepoint :: ![Word8]
  }
  deriving (Show)

mkAlexInput :: Source -> AlexInput
mkAlexInput src = AlexInput src (SourcePos src 0 0) '\0' '\0' 0 src.content []

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
  case List.uncons input.codepoint of
    Just (x, xs) -> Just (x, input {codepoint = xs})
    Nothing -> case LazyText.uncons input.rest of
      Just (c, rest') ->
        let prev' = input.cur
            x :| codepoint' = UTF8.encode c
         in Just
              ( x,
                input
                  { cur = c,
                    pos = SourcePos.next input.pos c,
                    prev = prev',
                    offset = input.offset + 1,
                    rest = rest',
                    codepoint = codepoint'
                  }
              )
      Nothing -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prev

data PState = PState
  { -- | Current alex mode
    alexMode :: !AlexMode,
    alexInput :: AlexInput,
    tokenStack :: !(Deque Token)
  }
  deriving (Show)

mkPState :: Source -> PState
mkPState src = PState 0 (mkAlexInput src) []

-- | Parser Monad
newtype P a = P
  { runP :: PState -> Either PError (a, PState)
  }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure a = P $ Right . (a,)
  (<*>) = ap

instance Monad P where
  P m >>= f = P \s -> case m s of
    Left e -> Left e
    Right (a, s') -> runP (f a) s'

instance MonadFail P where
  fail = P . const . Left . PFailure

instance MonadError PError P where
  throwError = P . const . Left
  catchError (P m) f = P \s -> case m s of
    Left e -> runP (f e) s
    Right (a, s') -> Right (a, s)

instance MonadState PState P where
  state = P . (Right .)

getAlexMode :: P AlexMode
getAlexMode = gets alexMode

getAlexInput :: P AlexInput
getAlexInput = gets alexInput

setAlexInput :: AlexInput -> P ()
setAlexInput input = modify \s -> s {alexInput = input}

pushToken :: Token -> P ()
pushToken token = modify \s -> s {tokenStack = Deque.cons token s.tokenStack}

popToken' :: P (Maybe Token)
popToken' =
  gets tokenStack
    >>= ( \case
            Just (a, xs) -> state \s -> (Just a, s {tokenStack = xs})
            Nothing -> pure Nothing
        )
      . Deque.unsnoc

popToken :: P Token
popToken =
  popToken' >>= \case
    Just token -> pure token
    Nothing -> error "Oopsies!"

-- | Lexlet Monad
newtype L a = L
  { runL :: AlexInput -> SourceSpan -> Int64 -> P a
  }

instance Functor L where
  fmap = liftM

instance Applicative L where
  pure = L . const . const . const . pure
  (<*>) = ap

instance Monad L where
  L m >>= f = L \input srcspan len -> do
    a <- m input srcspan len
    runL (f a) input srcspan len

liftP :: P a -> L a
liftP = L . const . const . const

modeL :: AlexMode -> L ()
modeL code = L \_ _ _ -> modify \s -> s {alexMode = code}

excerptL :: L StrictText.Text
excerptL = L \input _ len -> pure . LazyText.toStrict $ LazyText.take len input.rest

tokenL :: TokenClass -> L Token
tokenL tc = L \input srcspan len -> pure $ Token srcspan tc