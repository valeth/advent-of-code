using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Common {
    public class Wiring {
        public List<HashSet<char>> Inputs { get; private set; }
        public List<HashSet<char>> Outputs { get; private set; }

        public Wiring(string text) {
            var parts = text.Split(" | ");
            Inputs = ParseSegments(parts[0]);
            Outputs = ParseSegments(parts[1]);
        }

        private static List<HashSet<char>> ParseSegments(string text) {
            var char_sets = new List<HashSet<char>>();

            foreach (var str in text.Split(" ")) {
                var char_set = new HashSet<char>();
                foreach (var chr in str.ToCharArray()) {
                    char_set.Add(chr);
                }
                char_sets.Add(char_set);
            }

            return char_sets;
        }
    }

    public class SegmentDisplay {
        public List<Wiring> Wirings
        { get; private set; }

        public SegmentDisplay(string path) {
            Wirings = new List<Wiring>();

            var lines = File.ReadAllLines(path);

            foreach (var line in lines) {
                Wirings.Add(new Wiring(line));
            }
        }
    }

    public static class Permutation {
        public static IEnumerable<T[]> Permutations<T>(this IEnumerable<T> input) {
            return Permute(input, Enumerable.Empty<T>());
        }

        private static IEnumerable<T[]> Permute<T>(IEnumerable<T> memo, IEnumerable<T> pfx) {
            if (memo.Any()) {
                return
                    from t in memo.Select((a, b) => (a, b))
                    let next_memo = memo.Take(t.Item2).Concat(memo.Skip(t.Item2 + 1)).ToArray()
                    let next_pfx = pfx.Append(t.Item1)
                    from perms in Permute(next_memo, next_pfx)
                    select perms;

            } else {
                return new[] { pfx.ToArray() };
            }
        }
    }

    public abstract class Option<T> {
        private Option() {}

        public abstract bool IsSome();

        public bool GetValue(out T val) {
            if (this is Some s) {
                val = s.Value;
                return true;
            }

            val = default(T);
            return false;
        }

        public sealed class Some : Option<T> {
            public Some(T val) => Value = val;
            public T Value { get; }
            public override bool IsSome() => true;
        }

        public sealed class None : Option<T> {
            public override bool IsSome() => false;
        }
    }
}
