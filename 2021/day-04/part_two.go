package main

import (
    "os"
    "fmt"
    . "aoc/aoc"
)

func find_winner(numbers []int, boards []Board) (*Board, int) {
    var boards_left = len(boards)
    for _, number := range numbers {
        for b, _ := range boards {
            if boards[b].Completed { continue }

            if MarkBoard(&boards[b], number) {
                if boards_left == 1 {
                    return &boards[b], number
                }
                boards[b].Completed = true
                boards_left -= 1;
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
