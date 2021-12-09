#pragma once

extern int MAP_WIDTH;
extern int MAP_HEIGHT;
extern int MAP[100][100];

typedef int (*result_fn)(int, int, int);

typedef int (*collect_fn)(int, int);

void read_file(char* path);

void print_map();

int with_low_point(result_fn fn);
