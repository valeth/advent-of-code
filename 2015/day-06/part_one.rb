#!/usr/bin/env ruby

require_relative "common"

grid = Grid.new()

File.readlines(ARGV.first, chomp: true).each do |line|
  grid.apply(*parse_instruction(line))
end

puts grid.lights_on
