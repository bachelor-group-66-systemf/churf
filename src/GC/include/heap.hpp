#pragma once

#include <assert.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "allocator.hpp"

#define HEAP_SIZE   65536

namespace GC {

class Heap {
public:

    static Heap &the() {
        if (s_instance)
            return *s_instance;
        s_instance = new Heap();
        return *s_instance;
    }

    ~Heap() {
        for (auto *alloc : h_allocs)
            delete alloc;
    }

    size_t getHeapSize() {
        return size;
    }

    // helt onödig
    Allocator *getAllocator(size_t size) {
        for (auto *alloc : h_allocs) {
            if (alloc->getSize() >= size)
                return alloc;
        }
        // std::cout << "Object too big" << std::endl;
        assert(false && "TODO: Object too big");
    }

    void *alloc(size_t size) {
        auto allocator = getAllocator(size);
        return allocator->alloc();
    }

    void collect();

private:
    inline static Heap *s_instance = nullptr;

    Heap() {
        h_allocs.push_back(new Allocator(16));
        h_allocs.push_back(new Allocator(32));
        h_allocs.push_back(new Allocator(64));
        h_allocs.push_back(new Allocator(128));
        h_allocs.push_back(new Allocator(256));
        h_allocs.push_back(new Allocator(512));
        h_allocs.push_back(new Allocator(1024));
    }

    char _heap[HEAP_SIZE] = {0};
    size_t size = 0;
    std::vector<Allocator *> h_allocs;
};

}