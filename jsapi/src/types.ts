// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Cardano-addresses types
 *
 * @packageDocumentation
 */

/**
 * Hex-encoded bytes.
 *
 * TODO: what's the best way of representing bytestrings in TypeScript
 */
export type Bytes = string;
export type KeyHash = Bytes;
export type ScriptHash = Bytes;
/**
 * A Cardano address, encoded as bech32, base58, or hexadecimal.
 */
export type Address = string;
/**
 * A bech32-encoded extended public key.
 *
 * TODO: Add proper XPub type, which is the result of bech32 parsing a
 * string.
 */
export type XPub = string;

export type AddressStyle = "Shelley" | "Icarus" | "Byron";

export type StakeReference = "none" | "by value" | "by pointer";

export type InspectAddress
    = InspectAddressShelley
    | InspectAddressIcarus
    | InspectAddressByron;

export interface ChainPointer {
  slot_num: number
  transaction_index: number;
  output_index: number;
};

export interface InspectAddressShelley {
  address_style: "Shelley";
  stake_reference?: StakeReference;
  pointer?: ChainPointer;
  spending_key_hash?: KeyHash;
  spending_key_hash_bech32?: string;
  stake_key_hash?: KeyHash;
  stake_key_hash_bech32?: string;
  script_hash?: ScriptHash;
  script_hash_bech32?: string;
  network_tag: number;
};

/**
 * Corresponds to `Cardano.Address.Style.Icarus.AddressInfo`.
 *
 * TODO: Add fields.
 */
export interface InspectAddressIcarus {
  address_style: "Icarus";
};

/**
 * Corresponds to `Cardano.Address.Style.Byron.AddressInfo`.
 *
 * TODO: Add fields.
 */
export interface InspectAddressByron {
  address_style: "Byron";
};

export interface ErrInspectAddress {
  error: {
    code: string;
    details?: unknown;
  };
  message: string;
};
