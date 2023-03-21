#include <ctime>
#include <cstring>
#include <iostream>
#include <fstream>
#include <time.h>
#include <vector>
#include <unistd.h>
#include <stdexcept>

#include "chunk.hpp"
#include "event.hpp"
#include "profiler.hpp"

namespace GC
{
    void Profiler::record(GCEventType type)
    {
        auto event = new GCEvent(type);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::record(GCEventType type, size_t size)
    {
        auto event = new GCEvent(type, size);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::record(GCEventType type, Chunk *chunk)
    {
        auto chunk_copy = new Chunk(chunk);
        auto event = new GCEvent(type, chunk_copy);
        auto profiler = Profiler::the();
        profiler->m_events.push_back(event);
    }

    void Profiler::dump_trace()
    {
        auto profiler = Profiler::the();
        auto start = profiler->m_events.begin();
        auto end = profiler->m_events.end();

        std::ofstream fstr = profiler->create_file_stream(); 
        char buffer[22];
        std::tm *btm;
        std::time_t tt;
        Chunk *chunk;

        while (start != end)
        {
            auto event = *start++;

            tt = event->get_time_stamp(); 
            btm = std::localtime(&tt);
            std::strftime(buffer, 22, "%a %T", btm);

            fstr << "--------------------------------\n"
                 << buffer
                 << "\nEvent:\t" << event->type_to_string(); 
            


            chunk = event->get_chunk(); 

            if (chunk) {
                fstr << "\nChunk:  " << chunk->start
                     << "\n  Size: " << chunk->size
                     << "\n  Mark: " << chunk->marked;
            }
            fstr << "\n--------------------------------" << std::endl;
        }
    }

    void Profiler::dispose() {
        Profiler::record(ProfilerDispose);
        Profiler::dump_trace();
        auto profiler = Profiler::the();
        delete profiler;
    }

    std::ofstream Profiler::create_file_stream()
    {
        // get current time
        std::time_t tt = std::time(NULL);
        std::tm *ptm = std::localtime(&tt);

        // format to string
        char buffer[32];
        std::strftime(buffer, 32, "/log_%a_%H_%M_%S.txt", ptm);
        std::string filename(buffer);
        
        // const std::string ABS_PATH = "/home/virre/dev/systemF/org/language/src/GC/";
        // // const std::string ABS_PATH = "/Users/valtermiari/Desktop/DV/Bachelors/code/language/src/GC";
        // std::string fullpath = ABS_PATH + filename;

        const std::string fullpath = get_log_folder() + filename;

        std::ofstream fstr(fullpath);
        return fstr;
    }

    std::string Profiler::get_log_folder()
    {
        char buffer[1024];
        // chars read from path
        ssize_t len = readlink("/proc/self/exe", buffer, sizeof(buffer)-1);

        // if readlink fails
        if (len == -1)
        {
            throw std::runtime_error(std::string("Error: readlink failed on '/proc/self/exe/'"));
        }

        buffer[len] = '\0';

        // convert to string for string operators
        auto path = std::string(buffer);
        
        // remove filename
        size_t last_slash = path.find_last_of('/');
        std::string folder = path.substr(0, last_slash);

        return folder + "/logs";
    }
}