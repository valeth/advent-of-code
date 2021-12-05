import std/strutils
import std/sequtils

type
    Point* = (int, int)
    Line* = (Point, Point)
    LinesMap* = object
        fields: seq[int]
        width: int
        height: int


proc readFile*(path: string): (seq[Line], int, int) =
    let file = open(path)
    defer: file.close()

    var lines: seq[Line] = @[]
    var max_x = 0
    var max_y = 0

    for line in lines file:
        let tuples = line.split(" -> ")
        let s = tuples[0].split(',')
        let e = tuples[1].split(',')

        let x1 = s[0].parseInt()
        let x2 = e[0].parseInt()
        max_x = max(max(x1, x2), max_x)

        let y1 = s[1].parseInt()
        let y2 = e[1].parseInt()
        max_y = max(max(y1, y2), max_y)

        lines.add(((x1, y1), (x2, y2)))

    (lines, max_x + 1, max_y + 1)

proc newLinesMap*(width: int, height: int): LinesMap =
    LinesMap(
        fields: newSeqWith(width * height, 0),
        height: height,
        width: width
    )

proc incMapPos*(map: var LinesMap, x: int, y: int) =
    let pos = x + (y * map.width)
    map.fields[pos] += 1

proc printMap*(map: LinesMap) =
    for j in 0..(map.height - 1):
        let p = j * map.height
        let w = p + map.width - 1
        echo map.fields[p..w]

proc countOverlaps*(map: LinesMap): int =
    map.fields.countIt(it >= 2)
