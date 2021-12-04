package aoc

import (
    "os"
    "io"
    "errors"
    "bufio"
    "strings"
    "strconv"
)

type Field struct {
    Marked bool
    Value int
}

type Board struct {
    Fields [5][5]Field
    Completed bool
}

const (
    READ_NUMBERS = 0
    READ_BOARD = 1
)

func strs_to_ints(num_strs []string) []int {
    var numbers []int

    for _, num_str := range num_strs {
        i, _ := strconv.Atoi(num_str)
        numbers = append(numbers, i)
    }

    return numbers
}

func ReadFile(path string) ([]int, []Board, error) {
    file, err := os.Open(path)
    defer file.Close()

    if err != nil {
        return nil, nil, err
    }

    reader := bufio.NewReader(file)

    var state = READ_NUMBERS

    var numbers []int

    var boards []Board
    var cur_fields [5][5]Field
    var row_idx = 0

    reader:for {
        line, err := reader.ReadString('\n')
        var end_of_file = errors.Is(err, io.EOF)
        if err != nil && !end_of_file { break }

        var state_change = line == "\n" || end_of_file
        line = strings.TrimRight(line, "\n")

        switch state {
            case READ_NUMBERS:
                if state_change {
                    state = READ_BOARD
                    continue
                }
                var num_strs = strings.Split(line, ",")
                numbers = strs_to_ints(num_strs)
            case READ_BOARD:
                if state_change {
                    var board = Board{Fields: cur_fields, Completed: false}
                    boards = append(boards, board)
                    row_idx = 0
                    if end_of_file { break reader }
                    continue
                }
                var num_strs = strings.Fields(line)
                for col_idx, value := range strs_to_ints(num_strs) {
                    cur_fields[row_idx][col_idx] = Field{Value: value, Marked: false}
                }
                row_idx += 1
        }
    }

    return numbers, boards, nil
}

func check_row(i int, board *Board) bool {
    for _, field := range board.Fields[i] {
        if !field.Marked { return false }
    }
    return true
}

func check_col(j int, board *Board) bool {
    for _, row := range board.Fields {
        if !row[j].Marked { return false }
    }
    return true
}

func MarkBoard(board *Board, number int) bool {
    for i, row := range board.Fields {
        for j, _ := range row {
            var field = &board.Fields[i][j]
            if field.Value == number {
                field.Marked = true
                if check_row(i, board) || check_col(j, board) {
                    return true
                }
            }
        }
    }

    return false
}
