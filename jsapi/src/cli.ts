// Copyright © 2021 IOHK
// License: Apache-2.0

/**
 * Toy command-line interface for the `inspectAddress` function.
 *
 * This is only really for testing.
 *
 * See also: the entrypoint script `bin/js-inspect-address`.
 *
 * @packageDocumentation
 */

import Process = NodeJS.Process;

import { inspectAddress } from './cardano-addresses';

function usage(): void {
  console.log('usage: js-inspect-address ADDRESS');
  process.exit(1);
}

/**
 * Main function of the CLI.
 */
export function cli(argv: Process['argv']): void {
  const args = argv;

  args.shift(); // /usr/bin/node
  args.shift(); // js-inspect-address

  if (args.length < 1) {
    usage();
  }

  const address = args.shift() as string;

  const waitForExit = setInterval(() => undefined, 3600000);

  inspectAddress(address)
    .then(res => {
      console.log(res);
      return 0;
    })
    .catch(err => {
      console.error(err);
      return 1;
    })
    .finally(() => {
      clearInterval(waitForExit);
    })
    .then(code => {
      process.exit(code);
    });
}
