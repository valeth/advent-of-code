require "./common"

def main(path)
  octopi = parse_file(path)
  flashes = octopi.map &.map { false }

  puts generations(100, octopi, flashes, 0)
end

main(ARGV.first)
