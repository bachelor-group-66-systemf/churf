#pragma once

#include <cstdlib>

class HeapObj {
public:
    HeapObj(size_t size_) {
        size = size_;
    }

    ~HeapObj() { }

private:
    size_t size { 0 };
    bool marked { false };
};