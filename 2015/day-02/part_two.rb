#!/usr/bin/env ruby

require_relative "common"

def part_two(dims)
  dims.sum do |sides, length, width, height|
    circumference =
      case sides.map.with_index.min.last
      when 0 then length * 2 + width * 2
      when 1 then width * 2 + height * 2
      when 2 then height * 2 + length * 2
      end
    circumference + length * width * height
  end
end

puts part_two(INPUT)
