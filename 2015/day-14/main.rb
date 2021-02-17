#!/usr/bin/env ruby
# frozen_string_literal: true

require "pathname"

Item = Struct.new(:name, :speed, :active_time, :recovery_time)

def parse_file(file)
  file.each_line.map do |line|
    line.partition(" can fly ") => [name, _, tail]
    tail.partition(" km/s for ") => [speed, _, tail]
    tail.partition(" seconds, but then must rest for ") => [active_time, _, tail]
    tail.split.first => recovery_time
    Item.new(name, speed.to_i, active_time.to_i, recovery_time.to_i)
  end
end

def race(items, time)
  active_times = items.map { |i| [i, 1..i.active_time] }.to_h

  (1..time).each_with_object(items.map {|i| [i, {score: 0, distance: 0}] }.to_h) do |sec, stats|
    stats.each do |item, stat|
      active = active_times[item]

      stat[:distance] += item.speed if active.include?(sec)

      next unless sec > active.last

      active_start = sec + item.recovery_time
      active_end = active_start - 1 + item.active_time
      active_times[item] = active_start..active_end
    end

    leading = stats.values.max_by { |v| v[:distance] }[:distance]
    stats.each do |n, s|
      s[:score] += 1 if s[:distance] == leading
    end
  end
end

def main(file_paths)
  file_paths.each do |file_path|
    puts "File: #{file_path}"
    items = parse_file(file_path)

    results = race(items, 2503)

    results.values.max_by { |v| v[:distance] } => {distance:}
    puts "solution 1: #{distance}"

    results.values.max_by { |v| v[:score] } => {score:}
    puts "solution 2: #{score}"
  end
end

main(ARGV.map { |x| Pathname(x) })
