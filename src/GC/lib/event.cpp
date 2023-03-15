#include <chrono>
#include <iostream>
#include <list>

#include "chunk.hpp"
#include "event.hpp"
#include "heap.hpp"

namespace GC
{

    GCEventType GCEvent::getType()
    {
        return m_type;
    }

    std::time_t GCEvent::getTimeStamp()
    {
        return m_timestamp;
    }

    Chunk *GCEvent::getChunk()
    {
        return m_chunk;
    }

    // Try to remove inline
    const char *GCEvent::TypeToString()
    {
        switch (m_type)
        {
            case CollectStart:  return "CollectStart";
            case MarkStart:     return "MarkStart";
            case ChunkMarked:   return "ChunkMarked";
            case ChunkSwept:    return "ChunkSwept";
            case ChunkFreed:    return "ChunkFreed";
            case NewChunk:      return "NewChunk";
            case ReusedChunk:   return "ReusedChunk";
            default:            return "[Unknown]";
        }
    }
}