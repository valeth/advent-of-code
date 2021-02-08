#!/usr/bin/env -S ruby

require "pathname"
require "set"

INPUTS = [
  Pathname("inputs/test.txt"),
  Pathname("inputs/puzzle.txt"),
].freeze
VOWELS = %[a e i o u].freeze

def vowel_count(input)
  input.each_char.sum do |char|
    VOWELS.include?(char) ? 1 : 0
  end
end

def twin_letters(input)
  input.each_char.each_cons(2).sum do |(char, next_char)|
    char == next_char ? 1 : 0
  end
end

def letter_pairs_count(input)
  characters = input
    .each_char
    .each_cons(4)
    .reject { |(a, b, c, d)| (a == b && b == c || b == c && c == d ) && a != d }

  last_index = characters.size
  pairs = characters
    .flat_map
    .with_index(1) { |(a, b, c, d), i| i == last_index ? [a+b, b+c, c+d] : [a+b] }

  pairs.tally.values.max || 0
end

def surrounded_letter_count(input)
  input
    .each_char
    .each_cons(3)
    .count { |(a, _, b)| a == b }
end

def is_nice?(input)
  return false if input.match?(/ab|cd|pq|xy/)
  return false unless vowel_count(input) >= 3
  return false unless twin_letters(input) >= 1

  true
end

def is_really_nice?(input)
  return false if letter_pairs_count(input) < 2
  return false if surrounded_letter_count(input).zero?

  true
end

def count_nice(file)
  file.each_line.sum do |line|
    line = line.chomp # chomp: true keyword argument emits warnings
    next 0 if line.empty?
    yield(line) ? 1 : 0
  end
end

def solve_part_1(file)
  nice_lines = count_nice(file) { |l| is_nice?(l) }
  puts "\tThere are #{nice_lines} nice lines"
end

def solve_part_2(file)
  nice_lines = count_nice(file) { |l| is_really_nice?(l) }
  puts "\tThere are #{nice_lines} really nice lines"
end

def main(files)
  files.each do |file|
    puts "File: #{file}"
    solve_part_1(file)
    solve_part_2(file)
  end
end

main(INPUTS)
