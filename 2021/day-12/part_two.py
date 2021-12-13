#!/usr/bin/env python3

from common import parse_file, cave_is_small


def search_paths(cave_system, cave="start", marked = set(), paths = [], path = [], revisit = True):
    path = [*path, cave]

    if cave == "end":
        return [*paths, path]

    if cave_is_small(cave):
        if cave in marked and revisit:
            revisit = False

        marked.add(cave)

    for connected_cave in cave_system[cave]:
        if connected_cave == "start":
            continue

        if connected_cave in marked and not revisit:
            continue

        paths = search_paths(cave_system, connected_cave, set(marked), paths, path, revisit)

    return paths


if __name__ == "__main__":
    import sys

    cave_system = parse_file(sys.argv[1])

    paths = search_paths(cave_system)

    print(len(paths))
