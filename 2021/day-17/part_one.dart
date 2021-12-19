import "dart:math";
import "common.dart";

main(List<String> args) {
    final path = args.first;

    final target_area = parseFile(path);

    final max_xv = target_area[0][1];
    final max_yv = target_area[1][0].abs();
    var max_height = 0;

    Iterable.generate(max_xv, (v) => v + 1).forEach((x) {
        Iterable.generate(max_yv, (v) => v + 1).forEach((y) {
            final result = findMaxHeight(x, y, target_area);
            if (result != null) {
                max_height = max(result, max_height);
            }
        });
    });

    print(max_height);
}
