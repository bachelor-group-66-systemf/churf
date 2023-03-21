#pragma once

#include <ctime>
#include <iostream>
#include <list>
#include <time.h>

#include "chunk.hpp"

namespace GC
{

    enum GCEventType
    {
        HeapInit,
        AllocStart,
        CollectStart,
        MarkStart,
        ChunkMarked,
        ChunkSwept,
        ChunkFreed,
        NewChunk,
        ReusedChunk,
        ProfilerDispose
    };

    class GCEvent
    {
    private:
        const GCEventType m_type;
        const std::time_t m_timestamp {std::time(NULL)};
        const Chunk *m_chunk {nullptr};
        const size_t m_size {0};

    public:
        GCEvent(GCEventType type) : m_type(type) {}
        GCEvent(GCEventType type, Chunk *chunk) : m_type(type), m_chunk(chunk) {}
        GCEvent(GCEventType type, size_t size) : m_type(type), m_size(size) {}

        ~GCEvent() {
            if (m_chunk != nullptr)
                delete m_chunk;
        }

        GCEventType get_type();
        std::time_t get_time_stamp();
        const Chunk *get_chunk();
        size_t get_size();
        const char *type_to_string();
    };
}