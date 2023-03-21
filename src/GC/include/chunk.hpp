#pragma once

#include <stdint.h>
#include <stdlib.h>

namespace GC
{
    /**
     * The basic element of what can be stored on
     * the heap. A chunk contains a start address
     * on the actual heap, the size of memory that
     * is allocated at that address and if the
     * chunk is reachable (marked).
    */
    struct Chunk
    {
        bool m_marked {false};
        uintptr_t *const m_start {nullptr};
        const size_t m_size {0};

        Chunk(size_t size, uintptr_t *start) : m_size(size), m_start(start) {}
        Chunk(const Chunk *const c) : m_marked(c->m_marked), m_start(c->m_start), m_size(c->m_size) {}
        Chunk(const Chunk& c) : m_marked(c.m_marked), m_start(c.m_start), m_size(c.m_size) {}
    };
}