#!/usr/bin/env ruby

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

def is_nice?(input)
  return false if input.empty?
  return false if input.match?(/ab|cd|pq|xy/)
  return false unless vowel_count(input) >= 3
  return false unless twin_letters(input) >= 1

  true
end

result = File.readlines(ARGV.first, chomp: true).sum do |line|
  is_nice?(line) ? 1 : 0
end

puts result
