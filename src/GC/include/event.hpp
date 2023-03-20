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
        // make const
        GCEventType m_type;
        std::time_t m_timestamp;
        Chunk *m_chunk;
        size_t m_size;

    public:
        GCEvent(GCEventType type)
        {
            m_type = type;
            m_timestamp = std::time(NULL);
            m_chunk = nullptr;
            m_size = 0;
        }

        GCEvent(GCEventType type, Chunk *chunk) : GCEvent(type)
        {
            m_chunk = chunk;
        }

        GCEvent(GCEventType type, size_t size) : GCEvent(type)
        {
            m_size = size;
        }

        ~GCEvent() {
            if (m_chunk != nullptr)
                delete m_chunk;
        }

        GCEventType get_type();
        std::time_t get_time_stamp();
        Chunk *get_chunk();
        const char *type_to_string();
    };
}