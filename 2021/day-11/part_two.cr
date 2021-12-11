require "./common"

def main(path)
  octopi = parse_file(path)

  req_flashes = octopi.size * octopi.first.size

  (1..).each do |generation|
    flashes = octopi.map &.map { false }
    gen_flashes = generation(octopi, flashes)
    if gen_flashes == req_flashes
      puts generation
      break
    end
  end
end

main(ARGV.first)
