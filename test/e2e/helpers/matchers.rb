require "rspec/expectations"

RSpec::Matchers.define :succeed do
    match do |out_arr|
      out_arr[2].success? == true
    end
    failure_message do |out_arr|
      out = out_arr[0]
      err = out_arr[1]
      cmd = out_arr[3]
      %(
        Following command failed!!!
        --------------------------------------------
        #{cmd}

        StdErr:
        --------------------------------------------
        #{err}

        StdOut:
        --------------------------------------------
        #{out}
      )
    end
  end
