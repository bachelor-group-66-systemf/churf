#pragma once

#include <assert.h>
#include <memory>
#include <stdlib.h>

namespace GC {

class Allocator {
public:
    Allocator(size_t size) {
        alloc_size = size;
    }

    size_t getSize() {
        return alloc_size;
    }

    void *alloc();

private:
    size_t alloc_size;


};

}