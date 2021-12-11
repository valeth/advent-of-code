alias OctopusMap = Array(Array(Int32))
alias FlashMap = Array(Array(Bool))

SURROUNDING = [-1, 0, 1]
  .repeated_permutations(2)
  .reject { |v| v == [0, 0] }
  .map { |v| Tuple(Int32, Int32).from(v) }
  .to_set

def parse_file(path : String) : OctopusMap
  File.read_lines(path, chomp: true).map &.chars.map(&.to_i)
end

def print_map(octopi : OctopusMap)
  puts "-" * octopi.first.size
  octopi.each do |row|
    puts row
  end
  puts "-" * octopi.first.size
end

def flash(x : Int32, y : Int32, octopi : OctopusMap, flashes : FlashMap) : Int32
  flashes[y][x] = true
  octopi[y][x] = 0

  return 1 + mark_surrounding(x, y, octopi, flashes) do |nx, ny|
    next 0 if flashes[ny][nx]
    next flash(nx, ny, octopi, flashes) if octopi[ny][nx] > 9
    0
  end
end

def mark_surrounding(x : Int32, y : Int32, octopi : OctopusMap, flashes : FlashMap) : Int32
  valid_surrounding = SURROUNDING.reject do |(dx, dy)|
    val = false
    val ||= dx == -1 if x == 0
    val ||= dx == 1 if x == octopi.first.size - 1
    val ||= dy == -1 if y == 0
    val ||= dy == 1 if y == octopi.size - 1
    val
  end

  return valid_surrounding.sum do |(dx, dy)|
    octopi[y + dy][x + dx] += 1 unless flashes[y + dy][x + dx]
    yield x + dx, y + dy
  end
end

def generation(octopi : OctopusMap, flashes : FlashMap) : Int32
  gen_flash_count = 0

  octopi.each_with_index do |row, y|
    row.each_with_index do |_, x|
      next if flashes[y][x]
      octopi[y][x] += 1
      gen_flash_count += flash(x, y, octopi, flashes) if octopi[y][x] > 9
    end
  end

  gen_flash_count
end

def generations(steps : Int32, octopi : OctopusMap, flashes : FlashMap, flash_count : Int32) : Int32
  return flash_count if steps == 0

  flash_count += generation(octopi, flashes)

  flashes = octopi.map &.map { false }
  return generations(steps - 1, octopi, flashes, flash_count)
end
