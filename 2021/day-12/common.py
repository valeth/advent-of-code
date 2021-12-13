def cave_is_small(cave: str) -> bool:
    return cave.islower()


def add_connection(cave_system, a: str, b: str):
    if a not in cave_system.keys():
        cave_system[a] = set([b])
    else:
        cave_system[a].add(b)


def parse_file(path: str):
    cave_system = {}

    with open(path, "r") as f:
        for line in f.readlines():
            cave_a, cave_b = line.strip().split("-")
            add_connection(cave_system, cave_a, cave_b)
            add_connection(cave_system, cave_b, cave_a)

    return cave_system
