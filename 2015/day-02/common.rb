INPUT = File.readlines(ARGV.first, chomp: true).lazy.map do |num|
  dims = num.split("x").map(&:to_i)
  [sides(*dims), *dims]
end

def sides(l, w, h) = [l * w, w * h, h * l]
