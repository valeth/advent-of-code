#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "common.h"

int calculate_risk_level(int x, int y, int cur) {
    return cur + 1;
}

int main(int argc, char *argv[]) {
    if (argc != 2) return EXIT_FAILURE;

    char* path = argv[1];

    read_file(path);

    int result = with_low_point(calculate_risk_level);

    printf("%d\n", result);

    return EXIT_SUCCESS;
}
