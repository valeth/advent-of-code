import os
import common

proc main() =
    let path = commandLineParams()[0]
    let (lines, width, height) = common.readFile(path)

    var lines_map = newLinesMap(width, height)

    for (s, e) in lines:
        # ignore diagonal lines
        if s[0] != e[0] and s[1] != e[1]:
            continue

        let x1 = min(s[0], e[0])
        let x2 = max(s[0], e[0])
        let y1 = min(s[1], e[1])
        let y2 = max(s[1], e[1])

        for x in x1..x2:
            for y in y1..y2:
                lines_map.incMapPos(x, y)


    echo lines_map.countOverlaps()

main()
