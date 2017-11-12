#!/usr/bin/ruby -w

class HuffmanCode
  attr_reader :code
  attr_reader :entropy
  attr_reader :cost

  def initialize(message)
    freqs = frequencies(message)

    @code = if freqs.count > 1
              get_leafs(create_tree(freqs)).to_h
            else
              Hash.new
            end

    @entropy =
      freqs.map { |symbol, count| Float(count) / message.length }
           .inject(0.0) { |sum, freq| sum + - freq * Math.log2(freq) }
    @cost =
      freqs.map { |symbol, count| Float(count) / message.length * @code[symbol].length }
           .inject(0.0) { |sum, a| sum + a }
  end

  def redundancy
    @cost - @entropy
  end

  private

  def frequencies(message)
    message.split(//)
           .group_by { |i| i }
           .map { |symbol, symbols| [symbol, symbols.length] }
           .sort_by { |_, count| count }
  end

  def create_tree(freqs)
    nodes = freqs.map { |symbol, _| [symbol, nil] }

    while nodes.length != 1 do
      bit = 1
      nodes = nodes.inject([]) do |tree, node|
        tree << [[], nil] if bit == 1
        node[1] = bit
        tree.last[0] << node
        bit = bit == 0 ? 1 : 0
        tree
      end
      nodes[nodes.length - 1] = nodes.last[0][0] if nodes.last[0].length == 1
    end
    nodes = nodes[0]
  end

  def get_leafs(node)
    unless node[0].is_a?(Array)
      # a leaf
      [[node[0], node[1].to_s]]
    else
      # not a leaf
      (get_leafs(node[0][0]) + get_leafs(node[0][1]))
        .map { |symbol, bits| [symbol, node[1].to_s + (bits || "")]}
    end
  end
end

code = HuffmanCode.new($stdin.read)
p code.code
p "Entropy: #{code.entropy}"
p "Cost: #{code.cost}"
p "Redundancy: #{code.redundancy}"
