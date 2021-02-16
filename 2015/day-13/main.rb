#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

def parse_file(file)
  file.each_line.each_with_object({}) do |line, people|
    name, _, rest = line.chomp.partition(" would ")
    change, neighbor = rest.split(" happiness units by sitting next to ")
    change = change.split.then { |v| v.first == "gain" ?  v.last.to_i : -v.last.to_i }
    people[name] ||= {}
    people[name][neighbor.delete_suffix(".")] = change
  end
end

def optimized_seating(people)
  people.map do |start|
    visited = Set.new
    happiness = {}

    name, neighbors = start
    start_name = name

    loop do
      visited.add(name)
      happiness[name] ||= 0

      nb_name, hn_change = neighbors
        .reject { |n, _| visited.include?(n) }
        .max { |(an, ac), (bn, bc)| (ac + people[an][name]) <=> (bc + people[bn][name]) }

      nb_name ||= start_name
      hn_change ||= neighbors[start_name]

      happiness[nb_name] ||= 0
      happiness[name] += hn_change
      happiness[nb_name] += people[nb_name][name]

      name = nb_name
      neighbors = people[name]

      break if visited.size == people.size
    end

    happiness.values.sum
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"
    people = parse_file(file_path)

    solution1 = optimized_seating(people).max
    puts "  solution 1: #{solution1}"

    people["Self"] = people.each_with_object({}) { |(n, _), o| o[n] = 0 }
    people.each { |(_, nb)| nb["Self"] = 0 }
    solution2 = optimized_seating(people).max
    puts "  solution 2: #{solution2}"
  end
end

main(ARGV.map { |x| Pathname(x) })
