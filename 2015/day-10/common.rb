# frozen_string_literal: true

require "stringio"

def look_and_say(input)
  counter = 1

  output = input.each_char.map(&:to_i).chain([nil]).each_cons(2).reduce(StringIO.new) do |acc, (val1, val2)|
    if val1 == val2
      counter += 1
    else
      acc.write counter
      acc.write val1
      counter = 1
    end

    acc
  end

  output.rewind
  output
end
