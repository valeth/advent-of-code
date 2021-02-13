#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "stringio"

def look_and_say(input)
  counter = 1

  output = input.each_char.map(&:to_i).chain([nil]).each_cons(2).reduce(StringIO.new) do |acc, (val1, val2)|
    if val1 == val2
      counter += 1
    else
      acc.write counter
      acc.write val1
      counter = 1
    end

    acc
  end

  output.rewind
  output
end

def solve_part1(content)
  puts 40.times.reduce(StringIO.new(content)) { |acc| look_and_say(acc) }.size
end

def solve_part2(content)
  puts 50.times.reduce(StringIO.new(content)) { |acc, _| look_and_say(acc) }.size
end

def main(files)
  files.each do |file|
    content = file.read.chomp
    solve_part1(content)
    solve_part2(content)
  end
end

main(ARGV.map { |x| Pathname(x) })
