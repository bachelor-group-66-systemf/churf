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
    GC::Heap::dispose();
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

void cheap_profiler_log_options(cheap_t *cheap, unsigned long flags)
{
    GC::Heap *heap = static_cast<GC::Heap *>(cheap->obj);

    GC::RecordOption cast_flag;
    if (flags == FuncCallsOnly)
        cast_flag = GC::FunctionCalls;
    else if (flags == ChunkOpsOnly)
        cast_flag = GC::ChunkOps;
    else
        cast_flag = GC::AllOps;

    heap->set_profiler_log_options(cast_flag);
}