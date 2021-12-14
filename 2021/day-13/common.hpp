#pragma once

#include <string>
#include <vector>
#include <tuple>

using std::vector;
using std::tuple;
using std::string;

enum class Axis { X, Y };

using Coordinate = tuple<size_t, size_t>;
using Instruction = tuple<Axis, size_t>;

class Sheet {
private:
    vector<bool> map;
    size_t width = 0;
    size_t height = 0;
    vector<Instruction> instructions;

public:
    Sheet(string path);

    void fold_once();

    void fold_all();

    int count_dots();

    void print();

private:
    void fold(Axis axis, size_t pos);

    void fold_up(size_t pos);

    void fold_left(size_t pos);
};

tuple<vector<Coordinate>, vector<Instruction>> parse_file(string path);
