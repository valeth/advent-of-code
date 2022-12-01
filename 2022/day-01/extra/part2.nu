source sums.nu

def main [path = "../inputs/puzzle.txt"] {
    sums $path
        | sort -r
        | take 3
        | math sum
}
