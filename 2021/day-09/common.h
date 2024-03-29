#pragma once

#define MAP_MAX_Y 100
#define MAP_MAX_X 100
extern int MAP_WIDTH;
extern int MAP_HEIGHT;
extern int MAP[MAP_MAX_Y][MAP_MAX_X];

typedef int (*result_fn)(int, int, int);

typedef int (*collect_fn)(int, int);

void read_file(char* path);

void print_map();

int with_low_point(result_fn fn);
