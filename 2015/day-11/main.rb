#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

ValidationFailed = Class.new(StandardError)

class LengthInvalid < ValidationFailed
  def initialize(pwd) = super("#{pwd} length is not 8")
end

class NoStraightFound < ValidationFailed
  def initialize(pwd) = super("#{pwd} doesn't include an increasing straight")
end

class ForbiddenLetter < ValidationFailed
  def initialize(pwd) = super("#{pwd} has forbidden letters 'i', 'o', or 'l'")
end

class NotEnoughPairs < ValidationFailed
  def initialize(pwd) = super("#{pwd} doesn't include at least two different pairs")
end

def validate(input)
  raise LengthInvalid, input unless input.size == 8

  result = input
    .each_char
    .chain([nil])
    .each_cons(3)
    .each_with_object(straight: false, pairs: Set.new) do |(a, b, c), res|
      raise ForbiddenLetter, input unless ([a, b, c] & %w[i o l]).empty?
      res[:straight] ||= (b == a.succ && c == b.succ)
      res[:pairs].add(a) if (a == b && b != c)
    end

  raise NoStraightFound, input unless result[:straight]
  raise NotEnoughPairs, input unless result[:pairs].size >= 2

  input
end

def next_valid_password(input)
  validate(input)
rescue LengthInvalid
  nil
rescue ForbiddenLetter => e
  idx = input.index(/[iol]/)
  input = input[0..idx].succ + input[(idx+1)..]
  retry
rescue ValidationFailed
  input = input.succ
  retry
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"

    file_path.each_line do |line|
      line = line.chomp
      solution1 = next_valid_password(line)
      puts "solution 1: #{line} -> #{solution1}"
      solution2 = next_valid_password(solution1.succ)
      puts "solution 2: #{solution1} -> #{solution2}"
    end
  end
end

main(ARGV.map { |x| Pathname(x) })
