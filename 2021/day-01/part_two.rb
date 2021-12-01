#!/usr/bin/env ruby

def part_two(measurements)
  measurements
    .each_cons(3)
    .map { |(x, y, z)| x + y + z }
    .each_cons(2)
    .reduce(0) do |acc, (a, b)|
      a < b ? acc + 1 : acc
    end
end

content = File.readlines(ARGV.first, chomp: true).map(&:to_i)
result = part_two(content)
puts(result)
