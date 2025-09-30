---
title: 'Vanity Repositories for GitHub Pages'
description: 'Automate deploying GitHub pages'
pubDate: 'Mar 07 2025'
tags: ["GitHub"]
group: "DevOps"
---

Earlier today I wanted to be able to automate building my website and putting it on GitHub pages, unlike a sensible person I had split the site code and it's build files in two seperate repositories. In this predicament, I was just going to make the pages repository a subrepository which would be the build output - but that's got issues to it. Instead, I created an action to build my website and deploy it within the pages repository - Heres how I did it:

### Pages Repository Action

```yaml
name: Deploy site to pages

on:
  repository_dispatch:
  # Note here, this is what makes this all tick!
  workflow_dispatch:

permissions:
  contents: read
  pages: write
  id-token: write

concurrency:
  group: "pages"
  cancel-in-progress: false

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4
        with:
          repository: meowesque/site
      - name: Detect package manager
        id: detect-package-manager
        run: |
          echo "manager=npm" >> $GITHUB_OUTPUT
          echo "command=ci" >> $GITHUB_OUTPUT
          echo "runner=npx --no-install" >> $GITHUB_OUTPUT
          exit 0
      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: "20"
          cache: ${{ steps.detect-package-manager.outputs.manager }}
      - name: Setup Pages
        uses: actions/configure-pages@v5
      - name: Install dependencies
        run: ${{ steps.detect-package-manager.outputs.manager }} ${{ steps.detect-package-manager.outputs.command }} --legacy-peer-deps
      - name: Build with npm
        run: ${{ steps.detect-package-manager.outputs.manager }} run build
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: ./dist

  deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    needs: build
    steps:
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

Nothing unfamiliar to any simple build from another repository and deploy action for GitHub, but the trick here is `workspace_dispatch`. In the GitHub UI, you use this to manually start the action - we utilize the endpoint to trigger it instead.

In our site repository where the webapp is stored, we have an action:

### Site Repository Action

```yaml
name: Dispatch pages

on:
  push:
    branches: ["main"]

jobs:
  dispatch:
    runs-on: ubuntu-latest
    steps:
      - name: Dispatch pages
        id: dispatch
        run: |
          curl -XPOST -u "${{secrets.PAT_USERNAME}}:${{secrets.PAT_TOKEN}}" -H "Accept: application/vnd.github.everest-preview+json" -H "Content-Type: application/json" https://api.github.com/repos/meowesque/meowesque.github.io/actions/workflows/deploy.yml/dispatches --data '{"ref": "main"}'
```

Here, I send an API request to the workflow dispatch endpoint with my credentials (make sure to put them in your __repository secrets__!) Triggering the action in the pages repository everytime I push.