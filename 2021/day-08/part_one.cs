using System;
using System.Linq;

using Common;
using static Common.SegmentDisplay;

class PartOne {
    static readonly int[] SEGMENT_COUNT = {2, 4, 3, 7};

    public static void Main(string[] args) {
        var seg_display = new SegmentDisplay(args[0]);

        var digit_count = seg_display.Wirings.Sum(w =>
            w.Outputs.Count(o => SEGMENT_COUNT.Contains(o.Count))
        );

        Console.WriteLine(digit_count);
    }

}
