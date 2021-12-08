INPUT = File
  .readlines(ARGV.first, chomp: true)
  .lazy.map do |line|
    line.each_char.map { |d| position_modifier(d) }
  end

def position_modifier(direction)
  case direction
  when ">" then [1, 0]
  when "<" then [-1, 0]
  when "^" then [0, 1]
  when "v" then [0, -1]
  end
end

def apply_direction(position, change)
  position.map.with_index { |v, i| v + change[i] }
end
