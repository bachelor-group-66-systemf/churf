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