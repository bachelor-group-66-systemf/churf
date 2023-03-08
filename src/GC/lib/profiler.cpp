#include <iostream>
#include <vector>

#include "chunk.hpp"
#include "event.hpp"
#include "heap.hpp"
#include "profiler.hpp"

namespace GC {
    void Profiler::record(GCEventType type) {
        auto event = new GCEvent(type);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::record(GCEventType type, Chunk *chunk) {
        auto event = new GCEvent(type, chunk);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::printTrace() {
        auto profiler = Profiler::the();
        auto start = profiler->m_events.begin();
        auto end = profiler->m_events.end();
        while (start != end) {
            auto event = *start++;
            event->print(std::cout);
        }
    }
}