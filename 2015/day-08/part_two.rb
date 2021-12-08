#!/usr/bin/env ruby

size = File.readlines(ARGV.first, chomp: true).sum do |line|
  line.dump.size - line.size
end

puts size
