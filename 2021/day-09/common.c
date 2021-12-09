#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include "common.h"

int MAP_WIDTH = 0;
int MAP_HEIGHT = 0;
int MAP[MAP_MAX_Y][MAP_MAX_X];

void read_file(char* path) {
    FILE* fp = fopen(path, "r");
    if (fp == NULL) exit(EXIT_FAILURE);

    int y = 0;
    int x = 0;

    while (true) {
        char chr = fgetc(fp);

        if (chr == EOF) break;

        if (chr == '\n') {
            y++;

            if (MAP_WIDTH == 0) MAP_WIDTH = x;
            x = 0;

            continue;
        }

        MAP[y][x] = chr - 48;

        assert(MAP[x][y] < 10 && MAP[x][y] >= 0);

        x++;
    }

    MAP_HEIGHT = y;

    fclose(fp);
}

void print_map(int map[100][100]) {
    for (int y = 0; y < MAP_HEIGHT; y++) {
        printf("%d: ", y);
        for (int x = 0; x < MAP_WIDTH; x++) {
            printf("%d ", map[y][x]);
        }
        printf("\n");
    }
}

int with_low_point(result_fn fn) {
    int result = 0;

    for (int y = 0; y < MAP_HEIGHT; y++) {
        for (int x = 0; x < MAP_WIDTH; x++) {
            int cur = MAP[y][x];
            bool is_low_point = true;

            if (y > 0) {
                is_low_point = is_low_point && MAP[y - 1][x] > cur;
            }

            if (y < MAP_HEIGHT - 1) {
                is_low_point = is_low_point && MAP[y + 1][x] > cur;
            }

            if (x > 0) {
                is_low_point = is_low_point && MAP[y][x - 1] > cur;
            }

            if (x < MAP_WIDTH - 1) {
                is_low_point = is_low_point && MAP[y][x + 1] > cur;
            }

            if (is_low_point) {
                result += fn(x, y, cur);
            }

        }
    }

    return result;
}
