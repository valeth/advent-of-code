class Grid
  def initialize
    @area = Array.new(1000) { Array.new(1000) { 0 } }
    @commands = {
      turn_on: ->(area) { 1 },
      toggle: ->(area) { area ^ 0x1 },
      turn_off: -> (area) { 0 }
    }
  end

  def set_command(name, callback) = @commands[name.to_sym] = callback

  def apply(cmd, coords)
    coords => [[x, y], [w, h]]
    (y..h).each do |i|
      (x..w).each do |j|
        @area[i][j] = @commands[cmd.to_sym].(@area[i][j])
      end
    end
  end

  def lights_on = @area.sum { |r| r.count { |x| x != 0 }}
  def brightness = @area.sum { |r| r.sum }
end

def parse_instruction(input)
    left, right = input.split("through")
    cmd, _, start_coords = left.strip.rpartition(" ").map(&:strip)
    start_coords = start_coords.split(",").map(&:to_i)
    end_coords = right.strip.split(",").map(&:to_i)

    [cmd.tr(" ", "_"), [start_coords, end_coords]]
end
