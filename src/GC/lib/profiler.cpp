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

// #define MAC_OS

namespace GC
{
    Profiler& Profiler::the()
    {
        static Profiler instance;
        return instance;
    }

    RecordOption Profiler::log_options()
    {
        Profiler &prof = Profiler::the();
        return prof.flags;        
    }

    void Profiler::set_log_options(RecordOption flags)
    {
        Profiler &prof = Profiler::the();
        prof.flags = flags;
    }

    void Profiler::record_data(GCEvent *event)
    {
        Profiler &prof = Profiler::the();
        prof.m_events.push_back(event);

        if (prof.m_last_prof_event->m_type == event->get_type())
            prof.m_last_prof_event->m_n++;
        else
        {
            prof.m_prof_events.push_back(prof.m_last_prof_event);
            prof.m_last_prof_event = new ProfilerEvent(event->get_type());
        }
    }

    /**
     * Records an event independent of a chunk.
     * 
     * @param type  The type of event to record.
    */
    void Profiler::record(GCEventType type)
    {
        Profiler &prof = Profiler::the();
        if (prof.flags & type)
            Profiler::record_data(new GCEvent(type));
        // auto event = new GCEvent(type);
        // auto profiler = Profiler::the();
        // profiler.m_events.push_back(event);
    }

    /**
     * This overload is only used with an AllocStart
     * event.
     * 
     * @param type  The type of event to record.
     * 
     * @param size  The size of requested to alloc().
    */
    void Profiler::record(GCEventType type, size_t size)
    {
        Profiler &prof = Profiler::the();
        if (prof.flags & type)
            Profiler::record_data(new GCEvent(type, size));
        // auto event = new GCEvent(type, size);
        // auto profiler = Profiler::the();
        // profiler.m_events.push_back(event);
    }

    void Profiler::dump_trace()
    {
        Profiler &prof = Profiler::the();
        if (prof.flags & FunctionCalls)
            dump_prof_trace();
        else
            dump_chunk_trace();
    }

    /**
     * Records an event related to a chunk.
     * 
     * @param type  The type of event to record.
     * 
     * @param chunk The chunk the event is connected
     *              to.
    */
    void Profiler::record(GCEventType type, Chunk *chunk)
    {
        // Create a copy of chunk to store in the profiler
        // because in free() chunks are deleted and cannot
        // be referenced by the profiler. These copied
        // chunks are deleted by the profiler on dispose().
        Profiler &prof = Profiler::the();
        if (prof.flags & type)
        {
            auto chunk_copy = new Chunk(chunk);
            auto event = new GCEvent(type, chunk_copy);
            Profiler::record_data(event);
        }
        // auto profiler = Profiler::the();
        // profiler.m_events.push_back(event);
    }

    void Profiler::dump_prof_trace()
    {
        Profiler &prof = Profiler::the();
        prof.m_prof_events.push_back(prof.m_last_prof_event);
        auto start = prof.m_prof_events.begin();
        auto end = prof.m_prof_events.end();

        char buffer[22];
        std::ofstream fstr = prof.create_file_stream();

        while (start != end)
        {
            auto event = *start++;

            fstr << "\n--------------------------------\n"
                << Profiler::type_to_string(event->m_type)
                << "\nTimes:\t" << event->m_n; 
        }
        fstr << "\n--------------------------------";
    }

    /**
     * Prints the history of the recorded events
     * to a log file in the /tests/logs folder.
    */
    void Profiler::dump_chunk_trace()
    {
        Profiler &prof = Profiler::the();
        auto start = prof.m_events.begin();
        auto end = prof.m_events.end();

        // Buffer for timestamp
        char buffer[22];

        while (start != end)
        {
            auto event = *start++;
            auto e_type = event->get_type();

            prof.print_chunk_event(event, buffer);
        }
    }

    void Profiler::print_chunk_event(GCEvent *event, char buffer[22])
    {
        Profiler &prof = Profiler::the();
        // File output stream
        std::ofstream fstr = prof.create_file_stream(); 
        std::time_t tt = event->get_time_stamp(); 
        std::tm *btm = std::localtime(&tt);
        std::strftime(buffer, 22, "%a %T", btm);

        fstr << "--------------------------------\n"
             << buffer
             << "\nEvent:\t" << Profiler::type_to_string(event->get_type());
             // event->type_to_string(); 
        


        const Chunk *chunk = event->get_chunk(); 

        if (event->get_type() == AllocStart)
        {
            fstr << "\nSize: " << event->get_size();
        }
        else if (chunk)
        {
            fstr << "\nChunk:  " << chunk->m_start
                 << "\n  Size: " << chunk->m_size
                 << "\n  Mark: " << chunk->m_marked;
        }
        fstr << "\n";
    }

    /**
     * Deletes the profiler singleton and all
     * the events recorded after recording
     * the ProfilerDispose event and dumping
     * the history to a log file.
    */
    void Profiler::dispose()
    {
        Profiler::record(ProfilerDispose);
        Profiler::dump_trace();
    }

    /**
     * Creates a filestream for the future
     * log file to print the history to in
     * dump_trace().
     * 
     * @returns The output stream to the file.
    */
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

    /**
     * This function retrieves the path to the folder
     * of the executable to use for log files.
     * 
     * @returns The path to the logs folder.
     * 
     * @throws  A runtime error if the call
     *          to readlink() fails.
    */
    std::string Profiler::get_log_folder()
    {
#ifndef MAC_OS
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
#else
        auto folder = std::string("/Users/valtermiari/Desktop/DV/Bachelors/code/language/src/GC/tests");
#endif
        return folder + "/logs";
    }

    const char *Profiler::type_to_string(GCEventType type)
    {
        switch (type)
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
            case SweepStart:        return "SweepStart";
            case FreeStart:         return "FreeStart";
            default:                return "[Unknown]";
        }
    }
}