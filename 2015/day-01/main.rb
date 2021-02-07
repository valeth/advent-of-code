#!/usr/bin/env ruby

require "pathname"

INPUTS = ["inputs/sample.txt", "inputs/puzzle.txt"].map { |x| Pathname(x) }

def match_paren(char)
    case char
    when "(" then 1
    when ")" then -1
    end
end

def solve_part_1(content)
  content.chars.sum { |x| match_paren(x) }
end

def solve_part_2(content)
  content.chars.each.with_index(1).reduce(0) do |acc, (char, index)|
    acc += match_paren(char)
    return index if acc == -1
    acc
  end
  nil
end

def main(files)
  files.each do |file|
    puts "File: #{file}"
    file.read.lines do |line|
      next if line.empty?
      result = solve_part_1(line.chomp)
      puts "  Floor: #{result}"
      result = solve_part_2(line.chomp)
      puts "  Basement Floor Position: #{result}"
      puts " -" * 15
    end
  end
end

main(INPUTS)
