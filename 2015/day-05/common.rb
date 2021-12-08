LINES = File.each_line(ARGV.first, chomp: true)

def count_nice(lines)
  LINES.sum do |line|
    # line = line.chomp # chomp: true keyword argument emits warnings
    next 0 if line.empty?
    yield(line) ? 1 : 0
  end
end
