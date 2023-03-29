#pragma once

#include <stdlib.h>

#include "heap.hpp"
#include "cheap.h"

struct cheap {
    void *obj;
};

cheap_t *cheap_the() {
    cheap_t *c;
    GC::Heap *heap;

    c = (cheap_t *)malloc(sizeof(*c));
    heap = &GC::Heap::the();
    c->obj = heap;

    return c;
}

void cheap_init() {
    GC::Heap::init();
}