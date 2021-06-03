function runCardanoAddressesApi(cardanoAddressesInitComplete) {
  // TODO find a way to avoid passing this using globalThis
  globalThis.cardanoAddressesInitComplete = cardanoAddressesInitComplete;
  // Prevent exiting the process on cleanup.
  h$exitProcess = stopCardanoAddressesApi;
  // Start
  h$main(h$mainZCZCMainzimain);
}

function stopCardanoAddressesApi(code) {
  if(h$currentThread) {
    h$finishThread(h$currentThread);
    h$stack = null;
    throw new h$ThreadAbortedError(code);
  }
}
