source sums.nu

def main [path = "../inputs/puzzle.txt"] {
    sums $path
        | math max
}
