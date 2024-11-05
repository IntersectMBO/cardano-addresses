// Copyright © 2021 IOHK
// License: Apache-2.0

import { version, inspectAddress } from '../src';
import * as library from '../src/cardano-addresses';

import { LIB_VERSION } from './version';

beforeAll(() => {
  console.debug("Starting ghcjs RTS...");
  // Workaround: Prevent registration of nodejs event handlers on
  // stdin so that the tests can exit.
  process.stdin.destroy();
  // This is not strictly necessary because the wrapper functions
  // already call init().
  library.init();
  console.debug("... Done.");
});

afterAll(() => {
  console.debug("Stopping ghcjs RTS...");
  // This is also not strictly necessary, if the program or web page
  // is going to exit anyway.
  library.cleanup();
  console.debug("... Done.");
});

describe('version', () => {
  it('matches package.json', async () => {
    const ver = await version();
    const cabalVersion = ver.split(" ")[0];
    const npmVersion = LIB_VERSION.split("-")[0];
    expect(cabalVersion).toEqual(npmVersion)
  });
});

describe('inspectAddress', () => {
  describe('Happy path', () => {
    it('Byron testnet encrypted', async () => {
      const addr = "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL43NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh";
      const res = await inspectAddress(addr);
      expect(res).toEqual({
        "stake_reference": "none",
        "address_style": "Byron",
        "address_type": 8,
        "address_root": "b3e5c07c62cec629b5c56e1a3e5f7ded6d1a7fce9608d60a6f50f912",
        "derivation_path": "581c03b465be70a591156f4562c0ef4ebbad81fddd5be477234ea4fb2c3f",
        "network_tag": 1097911063
      });
    });
    it('Byron mainnet decrypted', async () => {
      const xpub = "root_xvk18amv7cs8kj0mxpk0l3vk2w6g22vyf7y5texr9huevqg9kd3davgv5j52xrfcf90kxx2zdrrl826pzc2kptgwegzzzpfgddwqkrk2gpclvvx76";
      const addr = "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr";
      const res = await inspectAddress(addr, xpub);
      expect(res).toEqual({
        "stake_reference": "none",
        "address_style": "Byron",
        "address_type": 8,
        "address_root": "c5805e1b48ffb6dc1f7ea4d5dbca56c48128fdbb98af1a571116f0e9",
        "derivation_path": {
            "account_index": "0H",
            "address_index": "0H"
        },
        "network_tag": null
      });
    });
    it('Shelley', async () => {
      const addr = "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w";
      const res = await inspectAddress(addr);
      expect(res).toEqual({
        "stake_reference": "none",
        "spending_key_hash_bech32": "addr_vkh109r8c6df43nzsqt56zwky46m492hfzepmmpmfqafg6dx28nj4yj",
        "address_style": "Shelley",
        "address_type": 6,
        "spending_key_hash": "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65",
        "network_tag": 0
      });
    });
  });
  describe('Malformed address', () => {
    it('Truncated Shelley', () => {
      const addr = "addr1vpu5vlrf4xkxv2qpwngf6cj";
      return expect(inspectAddress(addr)).rejects.toEqual({
        code: "decode",
        error: {},
        message: "Bech32 error: Invalid character(s) in string"
      });
    });
    it('Typo in Shelley', () => {
      const addr = "addr1q9rad75p5r9qk9pnczhyqhkqfv5j2hp7lrmwxpusfqmc8pp7y5xc6cr99fsjcw0hqksgwv3enatmz9ulpp6zn2kuskaqmq72h5";
      return expect(inspectAddress(addr)).rejects.toEqual({
        code: "decode",
        error: {},
        message: "Bech32 error: Invalid character(s) in string:\naddr1q9rad75p5r9qk9pnczhyqhkqfv5j2hp7lrmwxpusfqmc8pp7y5xc6cr99fsjcw0hqksgwv3enatmz9ulpp6zn2kuskaqmq72h5"
      });
    });
    it('Worng Byron', () => {
      const addr = "DdzFFzCqrht1JCKJpVrCzJQaQ4KxReo5A3UgXR2aAb1pYpkv82YD75";
      return expect(inspectAddress(addr)).rejects.toEqual({
        error: { code: "ErrShelley",
                 details: {
                   error: {
                     code: "UnknownType",
                     details: 0,
                   },
                   message: "Unknown address type 0"
                 },
               },
        message: "Invalid Shelley address: Unknown address type 0"
      });
    });
    it('Worng Icarus', () => {
      const addr = "Ae2tdPwUPEZHZKDNXMgeh7YqXXXaLYHCiN2dvhcFgiTr8RMjdDQTfAD";
      return expect(inspectAddress(addr)).rejects.toEqual({
        error: { code: "ErrShelley",
                 details: {
                   error: {
                     code: "UnknownType",
                     details: 192,
                   },
                   message: "Unknown address type 192"
                 },
               },
        message: "Invalid Shelley address: Unknown address type 192"
      });
    });
    it('Cannot detect encoding', () => {
      const addr = "%#@?;''";
      return expect(inspectAddress(addr)).rejects.toEqual({
        code: "decode",
        error: {},
        message: "Couldn't detect input encoding? Data on stdin must be encoded as bech16, bech32 or base58."
      });
    });
    it('Bad xpub for Byron', () => {
      const xpub = "hello";
      const addr = "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr";
      return expect(inspectAddress(addr, xpub)).rejects.toEqual({
        code: "xvk",
        error: {},
        message: "Bech32 error: string is too short"
      });
    });
    it('Non-matching xpub for Byron', () => {
      const xpub = "root_xvk1mpy60krjazfnaryn9adu0nsk27djk0zjqcvk790wm36tfl47s6yxma7nm0vyfwlz26hv5h20skgq2hrse0608wrhevl779x0hmy533ss7gvug";
      const addr = "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr";
      return expect(inspectAddress(addr, xpub)).rejects.toEqual({
        error: { code: "ErrByron",
                 details: {
                   error: {
                     code: "FailedToDecryptPath"
                   },
                   message: "Failed to decrypt derivation path"
                 },
               },
        message: "Invalid Byron address: Failed to decrypt derivation path"
      });
    });
  });
  describe('Other', () => {
    it('Icarus Mainnet', () => expect(inspectAddress("Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe")).resolves.toEqual({
      "address_root": "20e9d7a0aeb05aa9d0ea4c4b97a2214cd31e7f855abab30e608afe19",
      "address_style": "Icarus",
      "address_type": 8,
      "network_tag": null,
      "stake_reference": "none",
    }));
    it('Icarus Testnet', () => expect(inspectAddress("2cWKMJemoBamEeRQGMgcFo2UcS8Ue3e7FpStsfcCu2Dv4o2xGy1GRS6Je85EUrQh9WQ2o")).resolves.toEqual({
      "address_root": "f61cc6e2a72997f6fc6800c6977b7afc1685be09f3b8cf817eb2297e",
      "address_style": "Icarus",
      "address_type": 8,
      "network_tag": 1097911063,
      "stake_reference": "none",
    }));
    it('Shelley network tag 3', () => expect(inspectAddress("addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 0,
      "network_tag": 3,
      "spending_key_hash": "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65",
      "spending_key_hash_bech32": "addr_vkh109r8c6df43nzsqt56zwky46m492hfzepmmpmfqafg6dx28nj4yj",
      "stake_key_hash": "cc339a35f9e0fe039cf510c761d4dd29040c48e9657fdac7e9c01d94",
      "stake_key_hash_bech32": "stake_vkh1esee5d0eurlq8884zrrkr4xa9yzqcj8fv4la43lfcqweg22x6sq",
      "stake_reference": "by value",
    }));
    it('Shelley Payment Mainnet', () => expect(inspectAddress("addr1vx3qz578hx8vhjteujmpm8lczzh8hxghnq9y6t4t2x420dg05vms2")).resolves.toEqual({
      "stake_reference": "none",
      "spending_key_hash_bech32": "addr_vkh15gq483ae3m9uj70ykcwel7qs4eaej9ucpfxja26342nm2frv4mv",
      "address_style": "Shelley",
      "address_type": 6,
      "spending_key_hash": "a20153c7b98ecbc979e4b61d9ff810ae7b9917980a4d2eab51aaa7b5",
      "network_tag": 1
    }));
    it('Shelley Pointer network tag 3', () => expect(inspectAddress("addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 4,
      "network_tag": 3,
      "pointer": {
        "output_index": 42,
        "slot_num": 24157,
        "transaction_index": 177,
      },
      "spending_key_hash": "9493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8e",
      "spending_key_hash_bech32": "addr_vkh1jjfnzhxe966a33psfenm0ct2udkkr569qf55v4uprgkgu8zsvmg",
      "stake_reference": "by pointer",
    }));
    it('Shelley Stake network tag 0', () => expect(inspectAddress("stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 14,
      "network_tag": 0,
      "stake_key_hash": "61766572796e69636561646472726573736c6f6c6f6c6f6c6f6c6f6c",
      "stake_key_hash_bech32": "stake_vkh1v9mx2unede5kxetpv3j8yun9wdekcmmvdakx7mr0d3hkcjpqtv8",
      "stake_reference": "by value",
    }));
    it('Shelley Stake Shared network tag 0', () => expect(inspectAddress("stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 15,
      "network_tag": 0,
      "stake_reference": "by value",
      "stake_script_hash": "61766572796e69636561646472726573736c6f6c6f6c6f6c6f6c6f6c",
      "stake_script_hash_bech32": "stake_vkh1v9mx2unede5kxetpv3j8yun9wdekcmmvdakx7mr0d3hkcjpqtv8",
      "stake_shared_hash": "61766572796e69636561646472726573736c6f6c6f6c6f6c6f6c6f6c",
      "stake_shared_hash_bech32": "stake_shared_vkh1v9mx2unede5kxetpv3j8yun9wdekcmmvdakx7mr0d3hkcjta3en",
    }));
    it('Shelley network tag 0', () => expect(inspectAddress("addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5ahmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 0,
      "network_tag": 0,
      "spending_key_hash": "5c33fe9ec641f5449f2047fd01185aa25f98373e3ee3128e38ea0688",
      "spending_key_hash_bech32": "addr_vkh1tsela8kxg865f8eqgl7szxz65f0esde78m339r3cagrgsms0rh6",
      "stake_key_hash": "f85979c5d3b7d981643885ce274c7470644a91c11003cbcf3ddb03ae",
    "stake_key_hash_bech32": "stake_vkh1lpvhn3wnklvczepcsh8zwnr5wpjy4ywpzqpuhneamvp6uwkgwm8",
    "stake_reference": "by value",
    }));
    it('Shelley Stake network tag 0', () => expect(inspectAddress("stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 14,
      "network_tag": 0,
      "stake_key_hash": "f85979c5d3b7d981643885ce274c7470644a91c11003cbcf3ddb03ae",
      "stake_key_hash_bech32": "stake_vkh1lpvhn3wnklvczepcsh8zwnr5wpjy4ywpzqpuhneamvp6uwkgwm8",
      "stake_reference": "by value",
    }));
    it('Shelley Mainnet', () => expect(inspectAddress("addr1q9777p2w2hqa3cl0ah97pdwyavjnpf0ex3muvqgttavjxhku2rp98h9drzkdfva8ea775jszmd799k59aknpvqyn6wwqwll7uw")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 0,
      "network_tag": 1,
      "spending_key_hash": "7def054e55c1d8e3efedcbe0b5c4eb2530a5f93477c6010b5f59235e",
      "spending_key_hash_bech32": "addr_vkh10hhs2nj4c8vw8mlde0stt38ty5c2t7f5wlrqzz6lty34up32gnt",
      "stake_key_hash": "dc50c253dcad18acd4b3a7cf7dea4a02db7c52da85eda6160093d39c",
      "stake_key_hash_bech32": "stake_vkh1m3gvy57u45v2e49n5l8hm6j2qtdhc5k6shk6v9sqj0feccj4rjm",
      "stake_reference": "by value",
    }));
    it('Shelley Stake Mainnet', () => expect(inspectAddress("stake1u8w9psjnmjk33tx5kwnu7l02fgpdklzjm2z7mfskqzfa88qsjpk8l")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 14,
      "network_tag": 1,
      "stake_key_hash": "dc50c253dcad18acd4b3a7cf7dea4a02db7c52da85eda6160093d39c",
      "stake_key_hash_bech32": "stake_vkh1m3gvy57u45v2e49n5l8hm6j2qtdhc5k6shk6v9sqj0feccj4rjm",
      "stake_reference": "by value",
    }));
    it('Shelley Hex', () => expect(inspectAddress("0147d6fa81a0ca0b1433c0ae405ec04b29255c3ef8f6e30790483783843e250d8d60652a6123b9f705a08732399f57b1179f087429aadc85ba")).resolves.toEqual({
      "address_style": "Shelley",
      "address_type": 0,
      "network_tag": 1,
      "spending_key_hash": "47d6fa81a0ca0b1433c0ae405ec04b29255c3ef8f6e3079048378384",
      "spending_key_hash_bech32": "addr_vkh1glt04qdqeg93gv7q4eq9aszt9yj4c0hc7m3s0yzgx7pcgjg7ttx",
      "stake_key_hash": "3e250d8d60652a6123b9f705a08732399f57b1179f087429aadc85ba",
      "stake_key_hash_bech32": "stake_vkh18cjsmrtqv54xzgae7uz6ppej8x040vghnuy8g2d2mjzm5nj6par",
      "stake_reference": "by value",
    }));
    it('Icarus Hex', () => expect(inspectAddress("82d818582183581c20e9d7a0aeb05aa9d0ea4c4b97a2214cd31e7f855abab30e608afe19a0001a96a202b9")).resolves.toEqual({
      "address_root": "20e9d7a0aeb05aa9d0ea4c4b97a2214cd31e7f855abab30e608afe19",
      "address_style": "Icarus",
      "address_type": 8,
      "network_tag": null,
      "stake_reference": "none",
    }));
    it('Byron Hex decrypted', async () => {
      const xpub = "root_xvk18amv7cs8kj0mxpk0l3vk2w6g22vyf7y5texr9huevqg9kd3davgv5j52xrfcf90kxx2zdrrl826pzc2kptgwegzzzpfgddwqkrk2gpclvvx76";
      const addr = "82d818584283581cc5805e1b48ffb6dc1f7ea4d5dbca56c48128fdbb98af1a571116f0e9a101581e581c93b458439e62dbabbaa18dcd1a7db5fa5e63d6782c03f58bd8603bb6001ac464e1ef";
      const res = await inspectAddress(addr, xpub);
      expect(res).toEqual({
        "stake_reference": "none",
        "address_style": "Byron",
        "address_type": 8,
        "address_root": "c5805e1b48ffb6dc1f7ea4d5dbca56c48128fdbb98af1a571116f0e9",
        "derivation_path": {
            "account_index": "0H",
            "address_index": "0H"
        },
        "network_tag": null
      });
    });
  });
});
