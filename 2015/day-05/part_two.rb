#!/usr/bin/env ruby

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

def is_really_nice?(input)
  return false if input.empty?
  return false if letter_pairs_count(input) < 2
  return false if surrounded_letter_count(input).zero?

  true
end

result = File.readlines(ARGV.first, chomp: true).sum do |line|
  is_really_nice?(line) ? 1 : 0
end

puts result
