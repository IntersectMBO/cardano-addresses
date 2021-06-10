// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Low-level interface code for the Haskell/ghcjs library functions.
 *
 * @packageDocumentation
 */

import { Address, XPub, InspectAddress, ErrInspectAddress } from './types';

declare global {
  /**
   * Foreign ghcjs functions exported from the Haskell code.
   */
  export interface CardanoAddressesApi {
    version: (result: ((ver: string) => void)) => void;
    inspectAddress: (rootXPub: XPub|null, address: Address, success: ((res: InspectAddress) => void), failure: ((err: ErrInspectAddress) => void)) => void;
  }

  /**
   * Foreign ghcjs function entrypoint.
   */
  export type CardanoAddressesEntrypoint = (ready: (api: CardanoAddressesApi, cleanup: () => void) => void) => void;

  export var runCardanoAddressesApi: CardanoAddressesEntrypoint;
}
