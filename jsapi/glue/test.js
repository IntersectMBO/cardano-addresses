function testStart(api, cleanup) {
  console.log("cardano-addresses-jsapi:test:jsapi-tests");
  console.log("- api.version");
  api.version(success, failure);
  console.log("- api.inspectAddress");
  api.inspectAddress( "root_xvk18amv7cs8kj0mxpk0l3vk2w6g22vyf7y5texr9huevqg9kd3davgv5j52xrfcf90kxx2zdrrl826pzc2kptgwegzzzpfgddwqkrk2gpclvvx76", "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr", success, failure);
  console.log("- cleanup");
  cleanup();
}

function success(r) {
  console.log("  - Success", r);
}

function failure(e) {
  console.log("  - Failure", e);
}
