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
        char buffer[22];
        std::tm *btm;
        std::time_t tt;
        Chunk *chunk;

        while (start != end)
        {
            auto event = *start++;

            tt = event->getTimeStamp(); 
            btm = std::localtime(&tt);
            std::strftime(buffer, 22, "%a %T", btm);

            fstr << "--------------------------------\n"
                 << buffer
                 << "\nEvent:\t" << event->TypeToString(); 

            chunk = event->getChunk(); 

            if (chunk) {
                fstr << "\nChunk:\t" << chunk->start
                     << "\n  Size: " << chunk->size
                     << "\n  Mark: " << chunk->marked;
            }
            fstr << "--------------------------------\n" << std::endl;
        }
    }

    std::ofstream Profiler::createFileStream()
    {
        std::time_t tt = std::time(NULL);
        std::tm *ptm = std::localtime(&tt);
        char buffer[32];
        std::strftime(buffer, 32, "/profiler/log_%a_%H_%M_%S.txt", ptm);
        std::string filename(buffer);
        
        //const std::string ABS_PATH = "/home/virre/dev/systemF/org/language/src/GC/";
        const std::string ABS_PATH = "/Users/valtermiari/Desktop/DV/Bachelors/code/language/src/GC";
        std::string fullpath = ABS_PATH + filename;

        std::ofstream fstr(fullpath);
        return fstr;
    }
}