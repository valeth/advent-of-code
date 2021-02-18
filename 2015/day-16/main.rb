#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"

Aunt = Struct.new(
  :id, :children, :cats,
  :samoyeds, :pomeranians, :akitas, :vizslas,
  :goldfish, :trees, :cars, :perfumes,
  keyword_init: true
)

KNOWN = Aunt.new(
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1
).freeze

def parse_file(file)
  file.each_line.map do |line|
    line.partition(": ") => [name, _, tail]
    things = tail.split(", ").map { |x| x.split(": ") }.map { |k, v| [k.to_sym, v.to_i] }.to_h
    Aunt.new(id: name.split.last.to_i, **things)
  end
end

def find_aunt(aunts)
  aunts.select do |aunt|
    KNOWN.each_pair.all? { |prop, val| !(val && aunt[prop]) || aunt[prop] == val }
  end
end

def find_real_aunt(aunts)
  aunts.select do |aunt|
    KNOWN.each_pair.all? do |prop, val|
      next true unless val && aunt[prop]

      case prop
      when :cats, :trees then aunt[prop] > val
      when :promeranians, :goldfish then aunt[prop] < val
      else aunt[prop] == val
      end
    end
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"

    aunts = parse_file(file_path)

    solution1 = find_aunt(aunts).first&.id
    puts "  solution 1: #{solution1}"

    solution2 = find_real_aunt(aunts).first&.id
    puts "  solution 2: #{solution2}"
  end
end

main(ARGV.map { |x| Pathname(x) })
