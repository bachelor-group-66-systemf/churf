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
        CollectStart,
        MarkStart,
        ChunkMarked,
        ChunkSwept,
        ChunkFreed,
        NewChunk,
        ReusedChunk
    };

    class GCEvent
    {
    private:
        // make const
        GCEventType m_type;
        std::time_t m_timestamp;
        Chunk *m_chunk = nullptr;

    public:
        GCEvent(GCEventType type)
        {
            m_type = type;
            m_timestamp = std::time(NULL);
        }

        GCEvent(GCEventType type, Chunk *chunk)
        {
            m_type = type;
            m_timestamp = std::time(NULL);
            m_chunk = chunk;
        }

        GCEventType getType();
        std::time_t getTimeStamp();
        Chunk *getChunk();
        const char *TypeToString();
    };
}