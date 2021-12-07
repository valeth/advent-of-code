import Foundation

// Force-duplicated code, unable to import from another file
func read_file(path: String) throws -> [Int] {
    let content = try String(contentsOfFile: path)
    return content
        .trimmingCharacters(in: CharacterSet.whitespacesAndNewlines)
        .split(separator: ",")
        .map { Int($0)! }
        .sorted()
}

func part_one() {
    let path = CommandLine.arguments[1]
    let crab_positions = try! read_file(path: path)
    let my_pos = crab_positions[crab_positions.count / 2]

    let fuel_cost = crab_positions.reduce(0, {
        $0 + abs(my_pos - $1)
    })

    print(fuel_cost)
}

part_one()
