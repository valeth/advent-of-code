#!/usr/bin/env ruby

require "set"
require_relative "common"

def part_two(chars)
  position_s = [0, 0]
  position_r = position_s.dup

  chars
    .each_with_object(Set[position_s.dup])
    .with_index(1) do |(direction, visited), index|
      if index.even?
        next_pos = apply_direction(position_s, direction)
        visited.add(next_pos)
        position_s = next_pos
      else
        next_pos = apply_direction(position_r, direction)
        visited.add(next_pos)
        position_r = next_pos
      end
    end
    .size
end

puts part_two(INPUT.first)
