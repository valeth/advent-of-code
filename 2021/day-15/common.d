module common;

import std.algorithm.iteration : filter, map;
import std.algorithm.searching : minElement, canFind;
import std.array : split;
import std.container.array : Array;
import std.conv : to;
import std.stdio;
import std.string : stripRight;
import std.typecons : tuple, Tuple;

private alias Buffer = Array!(Array!(uint));

struct CaveMap
{
    private Buffer positions;

    this(string path)
    {
        positions = parseFile(path);
    }

    private Array!(Array!uint) parseFile(string path)
    {
        auto file = File(path, "r");

        auto buffer = Buffer();
        string line;
        while ((line = file.readln()) !is null)
        {
            auto line_buffer = Array!uint();

            foreach (weight; split(stripRight(line), "").map!(to!uint))
            {
                line_buffer.insertBack(weight);
            }

            buffer.insertBack(line_buffer);
        }

        return buffer;

    }
}

private alias NodeId = Tuple!(uint, uint);

private struct Node
{
    private NodeId id;
    private uint weight;

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("(");
        sink(to!string(this.id[0]));
        sink(",");
        sink(to!string(this.id[1]));
        sink(";");
        sink(to!string(this.weight));
        sink(")");
    }
}

struct Cave
{
    private Array!Node[NodeId] nodes;
    private NodeId start_node;
    private NodeId end_node;

    this(CaveMap cave_map)
    {
        auto node_id = NodeId(0, 0);
        start_node = node_id;

        uint y = 0;
        foreach (row; cave_map.positions)
        {
            uint x = 0;
            foreach (weight; row)
            {
                node_id = tuple(x, y);
                nodes[node_id] = getNeighbors(x, y, cave_map.positions);

                x++;
            }

            y++;
        }

        end_node = node_id;
    }

    uint lowestRisk()
    {
        auto safest_path = dijkstraSlow();
        return safest_path[1][end_node];
    }

    private Tuple!(NodeId[NodeId], uint[NodeId]) dijkstraSlow()
    {
        bool[NodeId] unvisited;
        uint[NodeId] distance;
        NodeId[NodeId] previous;

        foreach (node_id; nodes.byKey())
        {
            unvisited[node_id] = true;
            distance[node_id] = uint.max;
        }

        distance[start_node] = 0;

        unvisited.rehash;
        distance.rehash;

        while (unvisited.length != 0)
        {
            auto cur_node = unvisited.keys.minElement!(x => distance[x]);

            if (cur_node == end_node)
            {
                break;
            }

            unvisited.remove(cur_node);

            foreach (neighbor; nodes[cur_node])
            {
                if ((neighbor.id in unvisited) is null)
                    continue;

                auto total_distance = distance[cur_node] + neighbor.weight;

                if (total_distance < distance[neighbor.id])
                {
                    distance[neighbor.id] = total_distance;
                    previous[neighbor.id] = cur_node;
                }
            }
        }

        return tuple(previous, distance);
    }

    private immutable int[2][4] DIRECTIONS = [
        [-1, 0], [1, 0],
        [0, -1], [0, 1],
    ];

    private Array!Node getNeighbors(uint x, uint y, Buffer buf)
    {
        auto nodes = Array!Node();

        foreach (dir; DIRECTIONS)
        {
            int dx = x + dir[0];
            int dy = y + dir[1];

            if (dx < 0 || dy < 0)
                continue;
            if (dx >= buf[0].length() || dy >= buf.length())
                continue;

            Node node = {id: tuple(dx, dy), buf[dy][dx]};

            nodes.insertBack(node);
        }

        return nodes;
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        foreach (id; this.nodes.byKey())
        {
            auto nodes = this.nodes[id];
            sink("(");
            sink(to!string(id[0]));
            sink(",");
            sink(to!string(id[1]));
            sink(") { ");
            foreach (node; nodes)
            {
                sink(to!string(node));
                sink(" ");
            }
            sink("}\n");
        }
    }
}
