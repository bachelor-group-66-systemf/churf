#include <stdlib.h>
#include <iostream>

#include "heap.hpp"
#include "cheap.h"

#ifndef WRAPPER_DEBUG
struct cheap
{
    void *obj;
};
#endif

cheap_t *cheap_the()
{
    cheap_t *c;
    GC::Heap *heap;

    c = static_cast<cheap_t *>(malloc(sizeof(cheap_t)));
    heap = &GC::Heap::the();
    c->obj = heap;

    return c;
}

void cheap_init()
{
    GC::Heap::init();
}

void cheap_dispose()
{
    std::cout << "In dispose\n";
    GC::Heap::dispose();
    std::cout << "Out dispose" << std::endl;
}

void *cheap_alloc(unsigned long size)
{
    return GC::Heap::alloc(size);
}

void cheap_set_profiler(cheap_t *cheap, bool mode)
{
    GC::Heap *heap = static_cast<GC::Heap *>(cheap->obj);

    heap->set_profiler(mode);
}