#!/usr/bin/env ruby

require_relative "common"

def part_one(dims)
  dims.sum do |sides, *|
    smallest = sides.min
    sides.sum { |x| x * 2 } + smallest
  end
end

puts part_one(INPUT)
