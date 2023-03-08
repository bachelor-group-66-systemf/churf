#pragma once

#include <stdlib.h>

namespace GC
{

    struct Chunk
    {
        bool marked;
        uintptr_t *start;
        size_t size;
    };

}