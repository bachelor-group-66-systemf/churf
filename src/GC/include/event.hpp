#pragma once

#include <chrono>
#include <iostream>
#include <list>

#include "chunk.hpp"

using namespace std;

namespace GC {

    enum GCEventType {
        CollectStart,
        MarkStart,
        ChunkMarked,
        ChunkSwept,
        ChunkFreed
    };

    using TimeStamp = chrono::_V2::system_clock::time_point;

    class GCEvent {
    private:
        // make const
        GCEventType m_type;
        TimeStamp m_timestamp;
        Chunk *m_chunk;
    public:
        GCEvent(GCEventType type) {
            m_type = type;
            m_timestamp = chrono::system_clock::now();
        }

        GCEvent(GCEventType type, Chunk *chunk) {
            m_type = type;
            m_timestamp = chrono::system_clock::now();
            m_chunk = chunk;
        }

        GCEventType getType();
        TimeStamp getTimeStamp();
        Chunk *getChunk();

        void printShort();
        void printFull();
    };
}