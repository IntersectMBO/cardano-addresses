function testStart(api, cleanup) {
  api.version( function(r){ console.log(r); } );
  api.inspectAddress( null, "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL43NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( "root_xvk18amv7cs8kj0mxpk0l3vk2w6g22vyf7y5texr9huevqg9kd3davgv5j52xrfcf90kxx2zdrrl826pzc2kptgwegzzzpfgddwqkrk2gpclvvx76"
    , "DdzFFzCqrht5csm2GKhnVrjzKpVHHQFNXUDhAFDyLWVY5w8ZsJRP2uhwZq2CEAVzDZXYXa4GvggqYEegQsdKAKikFfrrCoHheLH2Jskr", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mq36s8xc", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5ahmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "addr1q9777p2w2hqa3cl0ah97pdwyavjnpf0ex3muvqgttavjxhku2rp98h9drzkdfva8ea775jszmd799k59aknpvqyn6wwqwll7uw", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  api.inspectAddress( null, "stake1u8w9psjnmjk33tx5kwnu7l02fgpdklzjm2z7mfskqzfa88qsjpk8l", function(r){ console.log(r); }, function(r){ console.log( "Error: " + r ); } );
  cleanup();
}
