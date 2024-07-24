/// <reference path="../src/foreign.d.ts" />

// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Typescript bindings for `Cardano.Address`.
 *
 * @module
 */

import { Address, XPub, InspectAddress } from './types';
import { CardanoAddressesJSModule, CardanoAddressesApi } from '../src/foreign';

/**
 * Get information about a Cardano address. Three address formats are
 * supported: Shelley, Icarus, and Byron.
 *
 * If the address can't be parsed, the promise will be rejected with
 * [[ErrInspectAddress]].
 *
 * @param address the address to inspect.
 * @param rootXPub an optional bech32-encoded root extended public
 *   key. This only applies to Byron addresses, and is for decrypting
 *   the {@link InspectAddressByron.payload} field.
 * @returns The fields parsed from the address.
 */
export async function inspectAddress(address: Address, rootXPub?: XPub): Promise<InspectAddress> {
  var api = await init();
  return new Promise((resolve, reject) =>
    api.inspectAddress(rootXPub || null, address, resolve, reject));
}

/**
 * @returns The Cabal package version string and git revision.
 */
export async function version(): Promise<string> {
  var api = await init();
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
    apiCreate = loadJSAPI().then(mod => {
      var run = mod.runCardanoAddressesApi;
      return new Promise(resolve => run((api, cleanup) => {
        apiDestroy = cleanup;
        resolve(api);
      }));
    });
  }
  return apiCreate;
}

async function loadJSAPI(): Promise<CardanoAddressesJSModule> {
  var haveProcess = typeof process != 'undefined';
  var nixPath = haveProcess && process.env?.CARDANO_ADDRESSES_JS;
  var isNode = haveProcess && process.versions?.node !== 'undefined';
  var mod = isNode ? 'cjs' : 'esm';
  var dir = nixPath || '.';
  var file = `${dir}/cardano-addresses-jsapi.${mod}.js`;
  try {
    console.log(`Importing File: ${file}`);
    return await import(file);
  } catch (err) {
    console.warn(`${file}: ES module loading failed!`, err);
    console.info("Using ambient definitions from cardano-addresses-jsapi.js");
    return { runCardanoAddressesApi };
  }
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
