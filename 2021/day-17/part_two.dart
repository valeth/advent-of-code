import "dart:math";
import "common.dart";

main(List<String> args) {
    final path = args.first;

    final target_area = parseFile(path);

    final max_xv = target_area[0][1];
    final max_yv = target_area[1][0];
    var velocity_count = 0;

    var yv = max_yv;

    while (yv < max_yv.abs()) {
        var xv = max_xv;

        while (xv > 0) {
            if (landsInTargetArea(xv, yv, target_area)) {
                velocity_count += 1;
            }

            xv -= 1;
        }

        yv += 1;
    }

    print(velocity_count);
}
