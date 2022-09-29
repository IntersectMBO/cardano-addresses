RSpec.describe "CLI examples" do

  it "cardano-address address payment" do
    examples = get_examples('address', 'payment')
    run_examples(examples)
  end

  it "cardano-address address bootstrap" do
    examples = get_examples('address', 'bootstrap')
    run_examples(examples)
  end

  it "cardano-address address stake" do
    examples = get_examples('address', 'stake')
    run_examples(examples)
  end

  it "cardano-address address delegation" do
    examples = get_examples('address', 'delegation')
    run_examples(examples)
  end

  it "cardano-address address pointer" do
    examples = get_examples('address', 'pointer')
    run_examples(examples)
  end

  it "cardano-address script" do
    examples = get_examples('script')
    run_examples(examples)
  end

  it "cardano-address key" do
    examples = get_examples('key')
    run_examples(examples)
  end

  it "cardano-address key from-recovery-phrase" do
    examples = get_examples('key', 'from-recovery-phrase')
    # Omit interactive example but run others
    examples_to_run = examples.first.split("Example:").last.strip.
                               split("$").
                               reject(&:empty?).
                               map {|e| e.strip }
    run_examples(examples_to_run)
  end

end
