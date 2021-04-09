RSpec.describe "CLI examples" do

  it "cardano-address address payment" do
    o, e = ca_cmd('address', 'payment')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address address bootstrap" do
    o, e = ca_cmd('address', 'bootstrap')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address address stake" do
    o, e = ca_cmd('address', 'stake')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address address delegation" do
    o, e = ca_cmd('address', 'delegation')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address address pointer" do
    o, e = ca_cmd('address', 'pointer')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address script" do
    o, e = ca_cmd('script')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

  it "cardano-address key" do
    o, e = ca_cmd('key')
    examples = get_examples(e)
    examples.each do |example|
      expect(cmd(example)).to succeed
    end
  end

end
