#!/usr/bin/env ruby

require_relative "common"

def part_two(chars)
  chars.each.with_index(1).reduce(0) do |acc, (char, index)|
    acc += match_paren(char)
    return index if acc == -1
    acc
  end
  nil
end

result = part_two(INPUT)
puts result
