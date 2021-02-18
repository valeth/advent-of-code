#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"

Ingredient = Struct.new(:name, :capacity, :durability, :flavor, :texture, :calories, keyword_init: true)

def parse_file(file)
  file.each_line.map do |line|
    line.split(": ") => [name, tail]
    tail.split(", ").map(&:split).map { |(k, v)| [k.to_sym, v.to_i] }.to_h => properties
    Ingredient.new(name: name, **properties)
  end
end

# Not the most efficient solution, but it does its job
def calculate_scores(ingredients)
  size = ingredients.size

  (1..(100 - size + 1)).to_a.repeated_permutation(size).select { |x| x.sum == 100 }.map do |perm|
    total_score = %i[capacity durability flavor texture].reduce(1) do |total_score, property|
      score = ingredients.each_with_index.sum { |ing, idx| perm[idx] * ing[property] }
      total_score * [score, 0].max
    end

    calories = ingredients.each_with_index.sum { |ing, idx| perm[idx] * ing.calories }

    [total_score, calories]
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"
    ingredients = parse_file(file_path)

    scores = calculate_scores(ingredients)

    solution1 = scores.max_by { |(score, _)| score }.first
    puts "  solution 1: #{solution1}"

    solution2 = scores.select { |(_, cal)| cal == 500 }.max_by { |(score, _)| score }.first
    puts "  solution 2: #{solution2}"
  end
end

main(ARGV.map { |x| Pathname(x) })
