import std.stdio;
import common : CaveMap, Cave;

void main(string[] args)
{
    auto immutable path = args[1];

    auto cave_map = CaveMap(path);
    auto graph = Cave(cave_map);

    writeln(graph.lowestRisk());
}
