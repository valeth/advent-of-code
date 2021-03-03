#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

Container = Struct.new(:id, :cap)

def combinations(amount, containers, stack = [], results = Set[])
  next_sum = stack.sum(&:cap)

  return results if next_sum > amount
  return results.add(stack.dup) if next_sum == amount
  return results if containers.empty?

  containers.each_with_index do |container, idx|
    combinations(amount, containers[(idx+1)..], [*stack, container], results)
  end

  results
end

def main(amount, file_paths)
  amount = amount.to_i

  file_paths.each do |file_path|
    puts "Files: #{file_path}"

    containers = file_path.each_line.with_index.map { |x, i| Container.new(i, x.to_i) }.freeze

    result = combinations(amount, containers)

    solution1 = result.size
    puts "  Solution 1: #{solution1}"

    solution2 = result
      .group_by { |x| x.size }
      .min { |(a, _), (b, _)| a <=> b }
      .last
      .size
    puts "  Solution 2: #{solution2}"
  end
end

main(ARGV[0], ARGV[1..].map { |x| Pathname(x) })
