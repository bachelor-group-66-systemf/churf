#include <ctime>
#include <cstring>
#include <iostream>
#include <fstream>
#include <time.h>
#include <vector>

#include "chunk.hpp"
#include "event.hpp"
#include "heap.hpp"
#include "profiler.hpp"

namespace GC
{
    void Profiler::record(GCEventType type)
    {
        auto event = new GCEvent(type);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::record(GCEventType type, Chunk *chunk)
    {
        auto event = new GCEvent(type, chunk);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::dumpTrace()
    {
        auto profiler = Profiler::the();
        auto start = profiler->m_events.begin();
        auto end = profiler->m_events.end();

        std::ofstream fstr = profiler->createFileStream();

        while (start != end)
        {
            auto event = *start++;

            fstr << "--------------------------------"
                 << event->TypeToString()
                 << event->getTimeStamp
        }
    }

    std::ofstream createFileStream()
    {
        std::time_t tt = std::time(NULL);
        std::tm *ptm = std::localtime(&tt);

        char buffer[32];
        std::strftime(buffer, 32, "/profiler/log_%a_%H_%M_%S.txt", ptm);

        std::ofstream fstr(buffer);
        return fstr;
    }
}