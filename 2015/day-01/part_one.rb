#!/usr/bin/env ruby

require_relative "common"

def part_one(chars)
  chars.sum { |x| match_paren(x) }
end

result = part_one(INPUT)
puts result
