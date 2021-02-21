#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"
require "set"

class Grid
  NEIGHBOR = {
    n:  [0, -1],
    s:  [0, 1],
    e:  [1, 0],
    w:  [-1, 0],
    ne: [1, -1],
    se: [1, 1],
    nw: [-1, -1],
    sw: [-1, 1],
  }.map(&:freeze).to_h.freeze

  def add_row(row)
    @area ||= []
    row = row.map { |x| x == "#" ? 1 : 0 }
    @width ||= row.size
    @area.push(row[0..@height])
  end

  def to_s = @area.map(&:to_s).join("\n")
  def row_count = @area.size
  def col_count = @width

  def dup
    grid = self.class.new
    grid.instance_variable_set(:@area, @area.map(&:dup).dup)
    grid.instance_variable_set(:@width, @width.dup)
    grid.instance_variable_set(:@sticky, @sticky&.dup)
    grid
  end

  def []=(x, y, val)
    return if @sticky&.include?([x, y])
    @area[y][x] = val
  end

  def [](x, y)
    @area[y][x]
  end

  def make_sticky(x, y, val = 1)
    @sticky ||= Set.new
    self[x, y] = val
    @sticky.add([x, y])
  end

  def count_on
    @area.sum { |r| r.sum }
  end

  def advance(steps)
    steps.times do |step|
      change_count = advance_step()
      return step if change_count.zero?
    end
  end

  private

  def advance_step
    changes = []

    @area.each_with_index do |row, y|
      row.each_with_index do |_, x|
        nb_on_count = neighbors_that_are_on(x, y)

        if self[x, y] == 1 && (nb_on_count < 2 || nb_on_count > 3)
          changes.push([x, y, 0])
        elsif self[x, y] == 0 && nb_on_count == 3
          changes.push([x, y, 1])
        end
      end
    end

    changes.each do |(x, y, val)|
      self[x, y] = val
    end

    changes.size
  end

  def neighbors_that_are_on(x, y)
    NEIGHBOR.keys.sum { |d| neighbor_on?(x, y, d) ? 1 : 0 }
  end

  def neighbor_on?(x, y, direction)
    nb_x, nb_y = NEIGHBOR[direction].then { |(nx, ny)| [x + nx, y + ny] }
    return false if nb_x < 0 || nb_y < 0

    neighbor = @area.dig(nb_y, nb_x)
    return false unless neighbor

    neighbor == 1
  end
end

def parse_file(file)
  file.each_line.each_with_object(Grid.new) do |line, grid|
    grid.add_row(line.chomp.chars)
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"

    grid1 = parse_file(file_path)
    grid2 = grid1.dup

    grid1.advance(100)
    solution1 = grid1.count_on
    puts "  Solution 1: #{solution1}"

    grid2.make_sticky(0, 0)
    grid2.make_sticky(0, grid2.col_count - 1)
    grid2.make_sticky(grid2.row_count - 1, 0)
    grid2.make_sticky(grid2.row_count - 1, grid2.col_count - 1)
    grid2.advance(100)

    solution2 = grid2.count_on
    puts "  Solution 2: #{solution2}"
  end
end

main(ARGV.map { |x| Pathname(x) })
