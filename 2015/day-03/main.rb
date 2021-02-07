#!/usr/bin/env ruby

require "pathname"
require "set"

INPUTS = [
  Pathname("inputs/test.txt"),
  Pathname("inputs/puzzle.txt"),
].freeze

def position_modifier(direction)
  case direction
  when ">" then [1, 0]
  when "<" then [-1, 0]
  when "^" then [0, 1]
  when "v" then [0, -1]
  end
end

def apply_direction(position, change)
  position.map.with_index { |v, i| v + change[i] }
end

def next_position(position, direction)
  apply_direction(position, position_modifier(direction))
end

def solve_part_1(input)
  input.lines.map do |line|
    puts "  " + "-" * 10

    position = [0, 0]

    houses =
      line.chomp.each_char.each_with_object(Set[position]) do |direction, visited|
        next_pos = next_position(position, direction)
        visited.add(next_pos)
        position = next_pos
      end

    puts "  Houses: #{houses.size}"
  end
end

def solve_part_2(input)
  input.lines.map do |line|
    puts "  " + "-" * 10

    position_s = [0, 0]
    position_r = position_s.dup

    houses =
      line.chomp.each_char.each_with_object(Set[position_s.dup]).with_index(1) do |(direction, visited), index|
        if index.even?
          next_pos = next_position(position_s, direction)
          visited.add(next_pos)
          position_s = next_pos
        else
          next_pos = next_position(position_r, direction)
          visited.add(next_pos)
          position_r = next_pos
        end
      end

    puts "  Houses: #{houses.size}"
  end
end

def main(files)
  files.each do |file|
    puts "File: #{file}"
    solve_part_1(file.read.chomp)
    puts "=" * 15
    solve_part_2(file.read.chomp)
  end
end

main(INPUTS)
