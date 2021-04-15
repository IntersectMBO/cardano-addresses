## How to use rspec e2e tests

Install dependencies and run tests:

```terminal
$ bundle install
$ PATH=$bin_path:$PATH bundle exec rspec
```

It needs `cardano-address` in the `$PATH`, so set your `$bin_path`
variable to the relevant build directory from `.stack-work` or
`dist-newstyle`.
