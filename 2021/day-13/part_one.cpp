#include <string>
#include <iostream>
#include "common.hpp"

using std::string;

int main([[maybe_unused]] int argc, char *argv[]) {
    string path = argv[1];

    Sheet sheet {path};

    sheet.fold_once();

    std::cout << sheet.count_dots() << std::endl;
}
