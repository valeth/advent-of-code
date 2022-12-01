def sums [path: string] {
    open $path
        | split row "\n\n"
        | each {
            split row "\n"
                | into int
                | math sum
        }
}
