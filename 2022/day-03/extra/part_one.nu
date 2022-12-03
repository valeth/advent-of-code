def is-downcase? [] {
    find --regex '[A-Z]' | is-empty
}

def item-priority [item: string] {
    let val = ($item | into int -r 36) - 9

    if ($item | is-downcase?) {
        $val
    } else {
        $val + 26
    }
}

def main [path = "../inputs/puzzle.txt"] {
    open $path
    | lines
    | each {
        let chars = split chars
        let len = ($chars | length)
        let mid = ($len / 2)

        let df = ($chars | into df)
        let left = ($df | slice 0 $mid)
        let right = ($df | slice $mid $len)

        let df = ($left | rename "0" "left" | with-column $right --name "right")
        let mask = ($df.left | is-in $df.right)

        $left
        | rename "0" "left"
        | with-column $right --name "right"
        | get "left"
        | filter-with $mask
        | unique
        | into nu
        | get "left"
        | each { |i| item-priority $i }
        | math sum
    }
    | math sum
}
