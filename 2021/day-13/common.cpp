#include <fstream>
#include <iostream>
#include "common.hpp"

using std::ifstream;
using std::getline;

enum class ParseState { ReadCoordinates, ReadInstruction };

Sheet::Sheet(string path) {
    auto parsed = parse_file(path);
    auto coordinates = std::get<0>(parsed);
    auto instructions = std::get<1>(parsed);

    for (size_t i = 0; i < 2; i++) {
        auto instruction = instructions.at(i);
        switch (std::get<0>(instruction)) {
            case Axis::X:
                this->width = std::get<1>(instruction) * 2 + 1;
                break;
            case Axis::Y:
                this->height = std::get<1>(instruction) * 2 + 1;
        }
    }

    this->instructions = instructions;

    this->map.resize(width * height, false);

    for (auto coord : coordinates) {
        auto x = std::get<0>(coord);
        auto y = std::get<1>(coord);
        this->map[x + (y * this->width)] = true;
    }
}

void Sheet::fold(Axis axis, size_t pos) {
    switch (axis) {
        case Axis::X:
            this->fold_left(pos);
            break;
        case Axis::Y:
            this->fold_up(pos);
            break;
    }
}

void Sheet::fold_up(size_t pos) {
    vector<bool> top(this->width * pos, false);

    for (size_t row = 0; row < pos; ++row) {
        for (size_t col = 0; col < this->width; ++col) {
            size_t top_pos = col + (row * this->width);
            size_t bottom_pos = col + (this->map.size() - ((row + 1) * this->width));

            top[top_pos] = this->map[top_pos] || this->map[bottom_pos];
        }
    }

    this->height = pos;
    this->map = top;
}

void Sheet::fold_left(size_t pos) {
    vector<bool> left(this->height * pos, false);

    for (size_t row = 0; row < this->height; ++row) {
        for (size_t col = 0; col < pos; ++col) {
            size_t left_pos = col + (row * this->width);
            size_t right_pos = (this->width - 1 - col) + (row * this->width);

            left[col + (row * pos)] = this->map[left_pos] || this->map[right_pos];
        }
    }

    this->width = pos;
    this->map = left;
}

void Sheet::fold_once() {
    auto instruction = this->instructions.at(0);
    this->fold(std::get<0>(instruction), std::get<1>(instruction));
}

void Sheet::fold_all() {
    for (auto instruction : this->instructions) {
        this->fold(std::get<0>(instruction), std::get<1>(instruction));
    }
}

int Sheet::count_dots() {
    int dots = 0;

    for (auto dot : this->map) {
        if (dot) dots++;
    }

    return dots;
}

void Sheet::print() {
    for (size_t i = 0; i < this->map.size(); ++i) {
        auto chr = this->map[i] ? "#" : ".";
        std::cout << chr << " ";
        if ((i + 1) % this->width == 0) {
            std::cout << std::endl;
        }
    }
}

Coordinate parse_coordinates(string str) {
    const string delim = ",";

    auto x = std::stoi(str.substr(0, str.find(delim)));
    str.erase(0, str.find(delim) + delim.length());
    auto y = std::stoi(str);

    return std::make_tuple(x, y);
}

Instruction parse_instruction(string str) {
    const string delim = "=";

    str.erase(0, 11);
    auto axis = str.substr(0, str.find(delim)) == "x" ? Axis::X : Axis::Y;
    str.erase(0, str.find(delim) + delim.length());
    auto where = std::stoi(str);

    return std::make_tuple(axis, where);
}

tuple<vector<Coordinate>, vector<Instruction>> parse_file(string path) {
    vector<Coordinate> coordinates;
    vector<Instruction> instructions;

    ifstream file;
    file.open(path);

    auto state = ParseState::ReadCoordinates;
    string line;
    while (getline(file, line)) {
        switch(state) {
            case ParseState::ReadCoordinates: {
                if (line.empty()) {
                    state = ParseState::ReadInstruction;
                    continue;
                }
                auto coord = parse_coordinates(line);
                coordinates.push_back(coord);
                break;
            }
            case ParseState::ReadInstruction: {
                auto instr = parse_instruction(line);
                instructions.push_back(instr);
                break;
            }
        }

    }

    file.close();

    return std::make_tuple(coordinates, instructions);
}
