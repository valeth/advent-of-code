import "dart:io";
import "dart:math";

List<List<int>> parseFile(String path) {
    final file = File(path);

    final content = file.readAsStringSync();

    final coordinates = content
        .trim().split(" ")
        .skip(2).where((e) => e.isNotEmpty)
        .map((elem) {
            return elem
                .split("=").last
                .replaceFirst(",", "").split("..")
                .map(int.parse)
                .toList();
        })
        .toList();

  return coordinates;
}

List<int> step(int x, int y, int xv, int yv) {
    final dx = x + xv;
    final dy = y + yv;
    final dxv = xv + (xv > 0 ? -1 : (xv < 0 ? 1 : 0));
    final dyv = yv - 1;
    return [dx, dy, dxv, dyv];
}

bool hasMissed(int x, int y, List<List<int>> area) {
    var missed = (x - area[0][1]) > 0;
    return missed || (y - area[1][0]) < 0;
}

bool isInTargetArea(int x, int y, List<List<int>> area) {
    return
        x >= area[0][0] &&
        x <= area[0][1] &&
        y >= area[1][0] &&
        y <= area[1][1];
}

bool landsInTargetArea(int init_vx, int init_vy, List<List<int>> area) {
    var x = 0;
    var y = 0;
    var vx = init_vx;
    var vy = init_vy;

    while (true) {
        final result = step(x, y, vx, vy);
        if (isInTargetArea(result[0], result[1], area)) return true;
        if (hasMissed(result[0], result[1], area)) return false;
        x = result[0];
        y = result[1];
        vx = result[2];
        vy = result[3];
    }
}

int? findMaxHeight(int init_vx, int init_vy, List<List<int>> area) {
    var x = 0;
    var y = 0;
    var vx = init_vx;
    var vy = init_vy;
    var max_height = 0;

    while (true) {
        final result = step(x, y, vx, vy);
        max_height = max(result[1], max_height);
        if (isInTargetArea(result[0], result[1], area)) return max_height;
        if (hasMissed(result[0], result[1], area)) return null;
        x = result[0];
        y = result[1];
        vx = result[2];
        vy = result[3];
    }
}
