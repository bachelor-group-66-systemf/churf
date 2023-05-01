#pragma once

#include <ctime>

#include "chunk.hpp"

namespace GC
{
    /**
     * Types of events that can occur on the heap.
    */
    enum GCEventType
    {
        HeapInit        = 1 << 0,
        AllocStart      = 1 << 1,
        CollectStart    = 1 << 2,
        MarkStart       = 1 << 3,
        SweepStart      = 1 << 4,
        ChunkMarked     = 1 << 5,
        ChunkSwept      = 1 << 6,
        ChunkFreed      = 1 << 7,
        NewChunk        = 1 << 8,
        ReusedChunk     = 1 << 9,
        ProfilerDispose = 1 << 10,
        FreeStart       = 1 << 11
    };

    /**
     * Stores metadeta about an event on the heap.
    */
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