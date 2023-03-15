#pragma once

#include <stdlib.h>

namespace GC
{

    struct Chunk
    {
        bool marked;
        uintptr_t *start;
        size_t size;

        // Default constructor
        Chunk() 
        {}

        // -- Temporary --
        // A copy constructor, keep track of how many times the vectors that hold chunks
        // are copied.
        // Shouldn't be all that relevant if we use vectors with Chunk-pointers.
        Chunk(const Chunk& c) : marked(c.marked), start(c.start), size(c.size)
        {
            std::cout << "Chunk was copied" << std::endl;
        }
    };
}