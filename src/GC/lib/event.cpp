#include <chrono>
#include <iostream>
#include <list>

#include "chunk.hpp"
#include "event.hpp"

namespace GC
{
    /**
     * @returns The type of the event
    */
    GCEventType GCEvent::get_type()
    {
        return m_type;
    }

    /**
     * @returns The time the event happened in
     *          the form of time_t.
    */
    std::time_t GCEvent::get_time_stamp()
    {
        return m_timestamp;
    }

    /**
     * If the event is related to a chunk, this
     * function returns the chunk that it is
     * related to. If the event is independent
     * of a chunk, it returns the nullptr.
     * 
     * @returns A chunk pointer or the nullptr.
    */
    const Chunk *GCEvent::get_chunk()
    {
        return m_chunk;
    }
    
    /**
     * If the event is an AllocStart event, this
     * returns the size of the alloc() request.
     * otherwise this returns 0.
     * 
     * @returns A number representing the number
     *          of bytes requested to alloc()
     *          or 0 if the event is not an
     *          AllocStart event.
    */
    size_t GCEvent::get_size()
    {
        return m_size;
    }

    /**
     * @returns The string conversion of the event type.
    */
    const char *GCEvent::type_to_string()
    {
        switch (m_type)
        {
            case HeapInit:          return "HeapInit";
            case AllocStart:        return "AllocStart";
            case CollectStart:      return "CollectStart";
            case MarkStart:         return "MarkStart";
            case ChunkMarked:       return "ChunkMarked";
            case ChunkSwept:        return "ChunkSwept";
            case ChunkFreed:        return "ChunkFreed";
            case NewChunk:          return "NewChunk";
            case ReusedChunk:       return "ReusedChunk";
            case ProfilerDispose:   return "ProfilerDispose";
            default:                return "[Unknown]";
        }
    }
}