require 'bundler/setup'
require 'open3'
require 'tmpdir'
require_relative '../helpers/matchers'

RSpec.configure do |config|
  # Enable flags like --only-failures and --next-failure
  config.example_status_persistence_file_path = ".rspec_status"

  # Disable RSpec exposing methods globally on `Module` and `main`
  config.disable_monkey_patching!

  config.expect_with :rspec do |c|
    c.syntax = :expect
  end
end

# Run all examples in a temporary directory, expect them all to
# succeed.
def run_examples(examples)
  Dir.mktmpdir do |dir|
    examples.each do |example|
      expect(run_example(example, dir)).to succeed
    end
  end
end

def run_example(example, dir)
  # running commands against bash because not all are compatible with
  # posix shell
  cmd = ["bash", "-c", example]
  out, err, status = Open3.capture3(*cmd, :chdir=>dir)
  return [out, err, status, cmd]
end

# cli command
def ca_cmd(*args)
  return Open3.capture3(*args.unshift("cardano-address"))
end

def get_examples(*cmd)
  o, e, status = ca_cmd(*cmd)
  expect(status.exitstatus).to be_between(0, 1).inclusive
  examples = parse_examples(e)

  # sanity check
  expect(examples).to have_attributes(size: (be > 0))

  return examples
end

def parse_examples(output)
  # get examples
  cmds = output.partition('Example:').last.split(" \n ")

  # remove outputs
  cmds = cmds.map {|c| c[0, c.rindex("\e[0m\n")]}

  # clean up
  cmds = cmds.map {|c| c.gsub("\e[1m", '') }.
              map {|c| c.gsub("\e[0m", '') }.
              map {|c| c.gsub("\n", '') }.
              map {|c| c.gsub("\\", '') }.
              map {|c| c[c.index("$") + 1, c.size] }. # remove starting $
              map {|c| c.strip }
  return cmds
end
