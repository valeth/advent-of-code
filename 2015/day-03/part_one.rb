#!/usr/bin/env ruby

require "set"
require_relative "common"

def part_one(chars)
  position = [0, 0]
  chars
    .each_with_object(Set[position]) do |direction, visited|
      next_pos = apply_direction(position, direction)
      visited.add(next_pos)
      position = next_pos
    end
    .size
end

puts part_one(INPUT.first)
