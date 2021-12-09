#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "common.h"

#define BASIN_COUNT 1000
int BASIN_IDX = 0;
int BASINS[BASIN_COUNT] = { 0 };

int scout_basin(int x, int y, int marked[100][100]) {
    marked[y][x] = 1;
    int count = 1;

    if (y > 0 && MAP[y - 1][x] != 9 && !marked[y - 1][x]) {
        count += scout_basin(x, y - 1, marked);
    }

    if (y < MAP_HEIGHT - 1 && MAP[y + 1][x] != 9 && !marked[y + 1][x]) {
        count +=  scout_basin(x, y + 1, marked);
    }

    if (x > 0 && MAP[y][x - 1] != 9 && !marked[y][x - 1]) {
        count += scout_basin(x - 1, y, marked);
    }

    if (x < MAP_WIDTH - 1 && MAP[y][x + 1] != 9 && !marked[y][x + 1]) {
        count += scout_basin(x + 1, y, marked);
    }

    return count;
}

int calculate_basin_size(int x, int y, int cur) {
    int marked[100][100] = { 0 };
    int basin_size = scout_basin(x, y, marked);
    BASINS[BASIN_IDX] = basin_size;
    BASIN_IDX++;
    return 0;
}

int cmp_basin_size(const void* a, const void* b) {
    if (*(int*) a < *(int*) b) {
        return 1;
    } else if (*(int*) a > *(int*) b) {
        return -1;
    }

    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 2) return EXIT_FAILURE;

    char* path = argv[1];

    read_file(path);

    with_low_point(calculate_basin_size);

    qsort(&BASINS, BASIN_COUNT, sizeof(int), cmp_basin_size);

    int result = 1;
    for (int i = 0; i < 3; i++) {
        result *= BASINS[i];
    }

    printf("%d\n", result);

    return EXIT_SUCCESS;
}
