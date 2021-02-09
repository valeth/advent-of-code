#!/usr/bin/env ruby

require "pathname"

INPUTS = [
  Pathname("inputs/test.txt"),
  Pathname("inputs/puzzle.txt"),
].freeze

class Grid
  def initialize = @area = Array.new(1000) { Array.new(1000) { 0 } }

  def apply(cmd, coords)
    coords => [[x, y], [w, h]]
    (y..h).each do |i|
      (x..w).each do |j|
        @area[i][j] = send(cmd, @area[i][j])
      end
    end
  end

  def lights_on = @area.sum { |r| r.count { |x| x != 0 }}
  def brightness = @area.sum { |r| r.sum }

  private

  def turn_on(area) = 1
  def toggle(area) = area ^ 0x1
  def turn_off(area) = 0

  def turn_on2(area) = area + 1
  def turn_off2(area) = [area - 1, 0].max()
  def toggle2(area) = area + 2
end

def parse_instruction(input)
    input = input.chomp
    left, right = input.split("through")
    cmd, _, start_coords = left.strip.rpartition(" ").map(&:strip)
    start_coords = start_coords.split(",").map(&:to_i)
    end_coords = right.strip.split(",").map(&:to_i)

    [cmd.tr(" ", "_"), [start_coords, end_coords]]
end

def solve_part_1(file)
  grid = Grid.new()

  file.each_line do |line|
    grid.apply(*parse_instruction(line))
  end

  puts "\tLights on: #{grid.lights_on}"
end

def solve_part_2(file)
  grid = Grid.new()

  file.each_line do |line|
    parse_instruction(line) => [cmd, coords]
    grid.apply("#{cmd}2", coords)
  end

  puts "\tBrightness: #{grid.brightness}"
end

def main(files)
  files.each do |file|
    puts "File: #{file}"
    solve_part_1(file)
    solve_part_2(file)
  end
end

main(INPUTS)
