/// <reference path="./foreign.d.ts" />

// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Typescript bindings for `Cardano.Address`.
 *
 * @module
 */

import { Address, XPub, InspectAddress } from './types';

/**
 * Get information about an address.
 *
 * If the address can't be parsed, the promise will be rejected with
 * [[ErrInspectAddress]].
 *
 * @returns The fields parsed from the address.
 */
export async function inspectAddress(address: Address, rootXPub?: XPub): Promise<InspectAddress> {
  const api = await init();
  return new Promise((resolve, reject) =>
    api.inspectAddress(rootXPub || null, address, resolve, reject));
}

/**
 * @returns The Cabal package version string and git revision.
 */
export async function version(): Promise<string> {
  const api = await init();
  return new Promise(done => api.version(done));
}

var apiCreate: undefined|Promise<CardanoAddressesApi>;
var apiDestroy: undefined|(() => void) = () => {};

/**
 * Start the cardano-addresses runtime system.
 *
 * There is no need to call this because it's done automatically the
 * first time a library function is used.
 *
 * @internal
 */
export function init(): Promise<CardanoAddressesApi> {
  if (!apiCreate) {
    var run: CardanoAddressesEntrypoint;
    if (typeof process != 'undefined' && process?.stdin) {
      process.stdin.destroy();
    }
    if (typeof require != 'undefined') {
      const path = typeof process != 'undefined'
        ? process.env?.CARDANO_ADDRESSES_JS
        : undefined;
      const filename = './cardano-addresses-jsapi.cjs.js';
      const mod = require((path ? path + '/' : '') + filename);
      run = mod.runCardanoAddressesApi;
    } else if (typeof window != 'undefined') {
      run = window?.runCardanoAddressesApi;
    } else {
      throw "Unable to load runCardanoAddressesApi()";
    }

    apiCreate = new Promise(resolve => run((api, cleanup) => {
      apiDestroy = cleanup;
      resolve(api);
    }));
  }
  return apiCreate;
}

/**
 * De-allocates resources used for the runtime system.
 *
 * @internal
 */
export function cleanup() {
  if (apiDestroy) {
    apiDestroy();
  }
  apiCreate = undefined;
  apiDestroy = undefined;
}
