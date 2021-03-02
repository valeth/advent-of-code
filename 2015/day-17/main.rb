#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

Container = Struct.new(:id, :cap)

# dumps result of previous solution to avoid recomputing
def memo(file)
  dump_path = "#{file.basename}.rbdump"

  if File.exist?(dump_path)
    File.open(dump_path, "rb") { |df| Marshal.load(df) }
  else
    File.open(dump_path, "wb") do |dumpfile|
      result = yield
      dumpfile.write(Marshal.dump(result))
      result
    end
  end
end

# quite inefficient, probably needs a DP solution
def combinations(amount, containers, stack = [], results = Set[])
  stack_sum = stack.sum { |x| x.cap }

  if stack_sum > amount
    return results
  elsif stack_sum == amount
    results.add(stack.dup)
    return results
  end

  return results if containers.empty?

  containers.each_with_index do |container, idx|
    combinations(amount, containers[(idx+1)..], [*stack, container], results)
  end

  combinations(amount, containers[1..], [], results)
end

def main(amount, file_paths)
  amount = amount.to_i

  file_paths.each do |file_path|
    puts "Files: #{file_path}"

    containers = file_path.each_line.with_index.map { |x, i| Container.new(i, x.to_i) }.freeze

    result = memo(file_path) { combinations(amount, containers) }

    solution1 = result.size
    puts "  Solution 1: #{solution1}"

    solution2 = result.min_by { |x| x.size }.size
      .then { |mincomb| result.select { |x| x.size == mincomb } }
      .size
    puts "  Solution 2: #{solution2}"
  end
end

main(ARGV[0], ARGV[1..].map { |x| Pathname(x) })
