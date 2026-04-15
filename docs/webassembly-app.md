---
sidebar_position: 3
title: WebAssembly App
---

## WebAssembly App

The WebAssembly version of cardano-addresses can be run directly in the browser. This provides a client-side address inspection tool that runs entirely in the user's browser without any server-side processing.

### Live Demo

The app is available at: **https://IntersectMBO.github.io/cardano-addresses/browser/**

### Running Locally

First, build the WebAssembly binary:

```bash
cd browser
./serve.sh
```

This will:
1. Build the `inspect-address` WASM binary using nix
2. Copy it to the browser directory
3. Start a local HTTP server on port 8080

Then open http://localhost:8080 in your browser.

### How It Works

The app uses:
- WebAssembly compiled from the Haskell library via GHC's WASM backend
- [@bjorn3/browser_wasi_shim](https://www.npmjs.com/package/@bjorn3/browser_wasi_shim) to provide WASI compatibility in the browser

The browser app performs address inspection entirely client-side, ensuring sensitive data never leaves the user's device.
