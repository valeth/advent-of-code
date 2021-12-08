#!/usr/bin/env ruby

require_relative "common"

grid = Grid.new()

grid.set_command(:turn_on, ->(area) { area + 1 })
grid.set_command(:turn_off, ->(area) { [area - 1, 0].max() })
grid.set_command(:toggle, ->(area) { area + 2 })

File.readlines(ARGV.first, chomp: true).each do |line|
  grid.apply(*parse_instruction(line))
end

puts grid.brightness
