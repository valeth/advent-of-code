package main

import (
    "os"
    "fmt"
    . "aoc/aoc"
)

func find_winner(numbers []int, boards []Board) (*Board, int) {
    for _, number := range numbers {
        for b, _ := range boards {
            if MarkBoard(&boards[b], number) {
                return &boards[b], number
            }
        }
    }

    return nil, -1
}

func main() {
    path := os.Args[1]

    numbers, boards, err := ReadFile(path)

    if err != nil {
        os.Exit(1)
    }

    board, last_number := find_winner(numbers, boards)

    var unmarked_sum = 0
    for _, row := range board.Fields {
        for _, col := range row {
            if !col.Marked {
                unmarked_sum += col.Value;
            }
        }
    }

    fmt.Println(unmarked_sum * last_number)
}
