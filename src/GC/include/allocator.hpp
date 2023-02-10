#pragma once

#include <assert.h>
#include <memory>
#include <stdlib.h>

#include "include/heap.hpp"

namespace GC {

class Allocator {
public:
    Allocator(size_t size) {
        alloc_size = size;
    }

    ~Allocator() { }

    size_t getSize() {
        return alloc_size;
    }

    void *alloc() {
        auto heap = Heap::the();

        assert(heap.getHeapSize() + alloc_size <= HEAP_SIZE);

    }

private:
    size_t alloc_size;


};

}