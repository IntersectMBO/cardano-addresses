// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Cardano-addresses types.
 *
 * @module
 */

/**
 * Hex-encoded bytes.
 *
 * _TODO:_ what's the best way of representing bytestrings in
 * TypeScript?
 */
export type Bytes = string;
/** Verification key hash. */
export type KeyHash = Bytes;
/** Hash of a script. */
export type ScriptHash = Bytes;

/**
 * A Cardano address, encoded as bech32, base58, or hexadecimal.
 */
export type Address = string;

/**
 * A bech32-encoded extended public key.
 *
 * _TODO:_ Add proper XPub type, which is the result of bech32 parsing
 * a string.
 */
export type XPub = string;

/** Supported address formats for the Cardano Shelley era. */
export type AddressStyle = "Shelley" | "Icarus" | "Byron";

/** How the stake at this address will be delegated. */
export type StakeReference
  /** No delegation. */
  = "none"
  /** Stake key is in address. */
  | "by value"
  /** Look up certificate in transaction at slot. */
  | "by pointer";

/**
 * The return value of [[inspectAddress]].
 */
export type InspectAddress
    = InspectAddressShelley
    | InspectAddressIcarus
    | InspectAddressByron;

/**
 * A [[StakeReference | stake reference]] pointer.
 */
export interface ChainPointer {
  slot_num: number;
  transaction_index: number;
  output_index: number;
};

export interface InspectAddressShelley {
  address_style: "Shelley";
  /** An integer denoting which network the address belongs to. */
  network_tag: number;
  spending_key_hash?: KeyHash;
  spending_key_hash_bech32?: string;
  stake_reference?: StakeReference;
  pointer?: ChainPointer;
  stake_key_hash?: KeyHash;
  stake_key_hash_bech32?: string;
  script_hash?: ScriptHash;
  script_hash_bech32?: string;
  /** Numeric address type field. */
  address_type: number;
};

/**
 * Corresponds to `Cardano.Address.Style.Icarus.AddressInfo`.
 */
export interface InspectAddressIcarus {
  address_style: "Icarus";
  /** Numeric address type field. */
  address_type: number;
  /** Which network the address belongs to. Unset for mainnet. */
  network_tag: number;
  /** Hex-encoded address payload */
  address_root: Bytes;
};

/**
 * Corresponds to `Cardano.Address.Style.Byron.AddressInfo`.
 */
export interface InspectAddressByron {
  address_style: "Byron";
  /** Which network the address belongs to. Unset for mainnet. */
  network_tag?: number;
  /** Numeric address type field. */
  address_type: number;
  /** Hex-encoded address payload */
  address_root: Bytes;
  /** Heirarchical derivation payload. If a root XPub is provided,
      the derivation indices are decrypted.
      Otherwise, it will be the encrypted payload. **/
  payload: Bytes  | { account_index: number; address_index: number; };
}

/**
 * Represents a failure to decode the given address.
 */
export interface ErrInspectAddress {
  error: {
    code: string;
    details?: unknown;
  };
  message: string;
};
