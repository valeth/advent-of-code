#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

def uppercase?(char)
  char.ord.between?(65, 90)
end

def parse_file(file)
  rules = []
  input = nil

  rules_parsed = false
  file.each_line do |line|
    line = line.chomp
    if line.empty?
      rules_parsed = true
    elsif rules_parsed
      input = line.each_char.slice_before { |x| uppercase?(x) }.map(&:join)
    else
      line.split(" => ") => [lhs, rhs]
      replacement = rhs.each_char.slice_before { |x| uppercase?(x) }.map(&:join)
      rules.push([lhs, replacement])
    end
  end

  [input, rules]
end

def apply_rules(input, rules)
  molecules = Set.new

  input.each_with_index do |item, idx|
    rules.select { |(x, _)| x == item }.each do |(_, re)|
      head = input[...idx].join
      tail = input[(idx+1)..].join

      molecules.add("#{head}#{re.join}#{tail}")
    end
  end

  molecules
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"
    input, rules = parse_file(file_path)

    solution1 = apply_rules(input, rules).size
    puts "  Solution 1: #{solution1}"
  end
end

main(ARGV.map { |x| Pathname(x) })
