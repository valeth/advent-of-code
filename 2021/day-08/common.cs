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
}
