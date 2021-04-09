require 'bundler/setup'
require 'open3'
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

# Helpers
def cmd(cmd)
  # running commands against bash because not all are compatible with shell
  out, err, status = Open3.capture3("bash -c #{cmd}")
  return [out, err, status, cmd]
end

def ca_cmd(*args)
  cmd("cardano-address #{args.join(' ')}")
end

def get_examples(output)
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
  cmds
end
