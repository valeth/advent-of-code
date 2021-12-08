INPUT = File.readlines(ARGV.first, chomp: true).first.chars

def match_paren(char)
    case char
    when "(" then 1
    when ")" then -1
    end
end
