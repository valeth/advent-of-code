#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"

INPUTS = [
  Pathname("inputs/test.txt"),
  Pathname("inputs/puzzle.txt"),
].freeze

def solve_part_1(file)
  size = file.each_line.sum do |line|
    line = line.chomp
    line.size - line.undump.size
  end

  puts "\tSize 1: #{size}"
end

def solve_part_2(file)
  size = file.each_line.sum do |line|
    line = line.chomp
    line.dump.size - line.size
  end

  puts "\tSize 2: #{size}"
end

def main(files)
  files.each do |file|
    puts "File: #{file}"

    solve_part_1(file)
    solve_part_2(file)
  end
end

main(INPUTS)
