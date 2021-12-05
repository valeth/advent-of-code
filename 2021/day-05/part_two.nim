import os
import common

iterator point_range(s: Point, e: Point): Point =
    var x1 = s[0]
    var y1 = s[1]
    var x2 = e[0]
    var y2 = e[1]

    for i in 0..abs(x2 - x1):
        yield (x1, y1)

        if x1 > x2:
            x1 -= 1
        elif x1 < x2:
            x1 += 1

        if y1 > y2:
            y1 -= 1
        elif y1 < y2:
            y1 += 1

proc main() =
    let path = commandLineParams()[0]
    let (lines, width, height) = common.readFile(path)

    var lines_map = newLinesMap(width, height)

    for (s, e) in lines:
        if s[0] == e[0]:
            for y in min(s[1], e[1])..max(s[1], e[1]):
                lines_map.incMapPos(s[0], y)
        elif s[1] == e[1]:
            for x in min(s[0], e[0])..max(s[0], e[0]):
                lines_map.incMapPos(x, s[1])
        else:
            for x, y in point_range(s, e):
                lines_map.incMapPos(x, y)

    echo lines_map.countOverlaps()

main()
