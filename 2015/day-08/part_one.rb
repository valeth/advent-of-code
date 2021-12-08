#!/usr/bin/env ruby

size = File.readlines(ARGV.first, chomp: true).sum do |line|
  line.size - line.undump.size
end

puts size
