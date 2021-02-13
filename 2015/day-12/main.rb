#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "json"

def sum_of_numbers(json)
  case json
  when Array then json.sum { |e| sum_of_numbers(e) }
  when Hash then json.sum { |(_, v)| sum_of_numbers(v) }
  when Numeric then json
  else 0
  end
end

def sum_of_numbers_nored(json)
  case json
  when Array then json.sum { |e| sum_of_numbers_nored(e) }
  when Hash then json.value?("red") ? 0 : json.sum { |(_, v)| sum_of_numbers_nored(v) }
  when Numeric then json
  else 0
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    json = JSON.parse(file_path.read)
    puts "#{file_path} / solution 1: #{sum_of_numbers(json)}"
    puts "#{file_path} / solution 2: #{sum_of_numbers_nored(json)}"
  end
end

main(ARGV.map { |x| Pathname(x) })
