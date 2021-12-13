#!/usr/bin/env python3

from common import parse_file, cave_is_small


def search_paths(cave_system, cave="start", marked = set(), paths = [], path = []):
    path = [*path, cave]

    if cave == "end":
        return [*paths, path]

    if cave_is_small(cave):
        marked.add(cave)

    for connected_cave in cave_system[cave]:
        if connected_cave in marked:
            continue

        paths = search_paths(cave_system, connected_cave, set(marked), paths, path)

    return paths


def short_path_filter(path):
    small_caves = []

    for cave in path[1:-1]:
        if not cave_is_small(cave):
            if cave in small_caves:
                return False
            small_caves.append(cave)

    return True


def only_short_paths(paths):
    return list(filter(short_path_filter, paths))


if __name__ == "__main__":
    import sys

    cave_system = parse_file(sys.argv[1])

    paths = search_paths(cave_system)

    print(len(only_short_paths(paths)))
