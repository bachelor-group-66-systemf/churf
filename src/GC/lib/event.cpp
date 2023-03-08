#include <chrono>
#include <iostream>
#include <list>

#include "chunk.hpp"
#include "event.hpp"
#include "heap.hpp"

// using namespace std;

namespace GC {
    
    GCEventType GCEvent::getType() {
        return m_type;
    }

    TimeStamp GCEvent::getTimeStamp() {
        return m_timestamp;
    }

    Chunk *GCEvent::getChunk() {
        return m_chunk;
    }

    void GCEvent::print(std::ostream &out) {
        assert(false && "TODO: unimplemented");
    }
}