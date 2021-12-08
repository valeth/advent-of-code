#!/usr/bin/env ruby
# frozen_string_literal: true

require "stringio"
require_relative "common"

content = File.read(ARGV.first).chomp
puts 50.times.reduce(StringIO.new(content)) { |acc| look_and_say(acc) }.size
