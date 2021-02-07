#!/usr/bin/env ruby

require "pathname"

INPUTS = [
  Pathname("inputs/test.txt"),
  Pathname("inputs/puzzle.txt"),
].freeze

def parse_input(data)
  data.chomp.split("x").map(&:to_i)
end

def sides(length, width, height)
  [length * width, width * height, height * length]
end

def solve_part_1(input)
  input.lines.sum do |line|
    sides = sides(*parse_input(line))
    smallest = sides.min
    sides.sum { |x| x * 2 } + smallest
  end
end

def solve_part_2(input)
  input.lines.sum do |line|
    length, width, height = parse_input(line)
    sides = sides(length, width, height)
    circumference =
      case sides.map.with_index.min.last
      when 0 then length * 2 + width * 2
      when 1 then width * 2 + height * 2
      when 2 then height * 2 + length * 2
      end
    circumference + length * width * height
  end
end

def main(files)
  files.each do |file|
    puts "File: #{file}"
    content = file.read

    result = solve_part_1(content.chomp)
    puts "  Wrapping Paper: #{result}"
    puts " -" * 15

    result = solve_part_2(content.chomp)
    puts "  Ribbon Length: #{result}"
    puts " -" * 15
  end
end

main(INPUTS)
