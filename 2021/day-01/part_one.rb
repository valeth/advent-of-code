#!/usr/bin/env ruby

def part_one(measurements)
  measurements
    .each_cons(2)
    .reduce(0) do |acc, (a, b)|
      a < b ? acc + 1 : acc
    end
end

content = File.readlines(ARGV.first, chomp: true).map(&:to_i)
result = part_one(content)
puts(result)
