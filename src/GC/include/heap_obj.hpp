#pragma once

#include <cstdlib>

class HeapObj {
public:
    HeapObj(void *start_, size_t size_) {
        start = start_;
        size = size_;
    }

    ~HeapObj() { }

    void *getAddr() { return start; }

    size_t getSize() { return size; }

    bool isMarked() { return marked; }
    void mark() { marked = true; }

private:
    void *start = 0;
    size_t size { 0 };
    bool marked { false };
};