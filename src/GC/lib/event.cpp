#include <chrono>
#include <iostream>
#include <list>

#include "chunk.hpp"
#include "event.hpp"
#include "heap.hpp"

namespace GC
{

    GCEventType GCEvent::get_type()
    {
        return m_type;
    }

    std::time_t GCEvent::get_time_stamp()
    {
        return m_timestamp;
    }

    Chunk *GCEvent::get_chunk()
    {
        return m_chunk;
    }

    // Try to remove inline
    const char *GCEvent::type_to_string()
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