using System;
using System.Collections.Generic;
using System.Linq;

using Common;
using static Common.SegmentDisplay;
using static Common.Permutation;

class PartTwo {
    private static readonly HashSet<int>[] VALID_POSITIONS = {
        new HashSet<int>() {0, 1, 2, 4, 5, 6},
        new HashSet<int>() {2, 5},
        new HashSet<int>() {0, 2, 3, 4, 6},
        new HashSet<int>() {0, 2, 3, 5, 6},
        new HashSet<int>() {1, 2, 3, 5},
        new HashSet<int>() {0, 1, 3, 5, 6},
        new HashSet<int>() {0, 1, 3, 4, 5, 6},
        new HashSet<int>() {0, 2, 5},
        new HashSet<int>() {0, 1, 2, 3, 4, 5, 6},
        new HashSet<int>() {0, 1, 2, 3, 5, 6},
    };

    public static void Main(string[] args) {
        var seg_display = new SegmentDisplay(args[0]);

        var output_sum = seg_display.Wirings.Sum(w => OutputNumber(w));

        Console.WriteLine(output_sum);
    }

    private static int OutputNumber(Wiring wiring) {
        var inputs = wiring.Inputs.OrderBy(x => x.Count).ToList();
        var digit = 0;

        if (Analyze(inputs).GetValue(out var segments)) {
            var digit_idx = wiring.Outputs.Count - 1;

            foreach (var output in wiring.Outputs) {
                var segment_digit = GetOutputDigit(output, segments);

                digit += segment_digit * (int)Math.Pow(10, digit_idx);

                digit_idx--;
            }
        }

        return digit;
    }

    private static int GetOutputDigit(HashSet<char> output, Dictionary<char, int> segments) {
        var mapped_output = output.Select(v => segments[v]);

        switch (output.Count) {
            case 2: return 1;
            case 3: return 7;
            case 4: return 4;
            case 7: return 8;
            case 5:
                foreach (var n in new[]{2, 3, 5}) {
                    if (!VALID_POSITIONS[n].Except(mapped_output).Any()) return n;
                }
                break;
            case 6:
                foreach (var n in new[]{0, 6, 9}) {
                    if (!VALID_POSITIONS[n].Except(mapped_output).Any()) return n;
                }
                break;
            default:
                throw new Exception("Invalid output length");
        }

        throw new Exception("No valid mapping for output");
    }

    private static Option<Dictionary<char, int>> Analyze(IEnumerable<HashSet<char>> inputs) {
        return Analyze(inputs, new Dictionary<char, int>());
    }

    private static Option<Dictionary<char, int>> Analyze(
        IEnumerable<HashSet<char>> inputs,
        Dictionary<char, int> segments
    ) {
        if (!inputs.Any()) return new Option<Dictionary<char, int>>.Some(segments);

        var input = inputs.First();
        var segment_count = input.Count;
        var unmapped_input = input.Except(segments.Keys);
        var mapped_input = input.Intersect(segments.Keys).Select(v => segments[v]);

        switch (segment_count) {
            case 2: return Analyze(1, unmapped_input, mapped_input, inputs, segments);
            case 3: return Analyze(7, unmapped_input, mapped_input, inputs, segments);
            case 4: return Analyze(4, unmapped_input, mapped_input, inputs, segments);
            case 7: return new Option<Dictionary<char, int>>.Some(segments);
            case 5:
                foreach (var n in new[]{2, 3, 5}) {
                    var retval = Analyze(n, unmapped_input.ToList(), mapped_input.ToList(), inputs, segments);
                    if (retval.IsSome()) return retval;
                }
                break;
            case 6:
                foreach (var n in new[]{0, 6, 9}) {
                    var retval = Analyze(n, unmapped_input.ToList(), mapped_input.ToList(), inputs, segments);
                    if (retval.IsSome()) return retval;
                }
                break;
            default:
                throw new Exception("Invalid input length");
        }

        return new Option<Dictionary<char, int>>.None();
    }

    private static Option<Dictionary<char, int>> Analyze(
        int num,
        IEnumerable<char> unmapped_input,
        IEnumerable<int> mapped_input,
        IEnumerable<HashSet<char>> inputs,
        Dictionary<char, int> segments
    ) {
        if (!mapped_input.Except(VALID_POSITIONS[num]).Any()) {
            foreach (var perm in Permutations(unmapped_input)) {
                var new_segments = new Dictionary<char, int>(segments);

                foreach (var pair in perm.Zip(VALID_POSITIONS[num].Except(mapped_input), (a, b) => (a, b))) {
                    new_segments.Add(pair.Item1, pair.Item2);
                }

                var retval = Analyze(inputs.Skip(1).ToList(), new_segments);

                if (retval.IsSome()) return retval;
            }
        }

        return new Option<Dictionary<char, int>>.None();
    }
}
