#pragma once

#include <iostream>
#include <vector>
#include <chrono>

#include "chunk.hpp"
#include "event.hpp"

// #define FunctionCallTypes   
// #define ChunkOpsTypes        

namespace GC {

    enum RecordOption
    {
        TimingInfo      = 0,
        FunctionCalls   = (GC::AllocStart | GC::CollectStart | GC::MarkStart | GC::SweepStart | GC::FreeStart),
        ChunkOps        = (GC::ChunkMarked | GC::ChunkSwept | GC::ChunkFreed | GC::NewChunk | GC::ReusedChunk),
        AllOps          = 0xFFFFFF
    };

    struct ProfilerEvent
    {
        uint m_n {1};
        const GCEventType m_type;

        ProfilerEvent(GCEventType type) : m_type(type) {}
    };

    class Profiler {
    private:
        Profiler() {}
        ~Profiler()
        {
            for (GCEvent *c : m_events)
                delete c;
        }

        static Profiler &the();
        inline static Profiler *m_instance {nullptr};
        std::vector<GCEvent *> m_events;
        ProfilerEvent *m_last_prof_event {new ProfilerEvent(HeapInit)};
        std::vector<ProfilerEvent *> m_prof_events;
        RecordOption flags {AllOps};

        std::chrono::microseconds alloc_time {0};
        // size_t alloc_counts {0};
        std::chrono::microseconds collect_time {0};
        // size_t collect_counts {0};

        static void record_data(GCEvent *type);
        std::ofstream create_file_stream();
        std::string get_log_folder();
        static void dump_trace();
        static void dump_prof_trace(bool timing_only);
        static void dump_chunk_trace();
        // static void dump_trace_short();
        // static void dump_trace_full();
        static void print_chunk_event(GCEvent *event, char buffer[22]);
        static const char *type_to_string(GCEventType type);

    public:
        static RecordOption log_options();
        static void set_log_options(RecordOption flags);
        static void record(GCEventType type);
        static void record(GCEventType type, size_t size);
        static void record(GCEventType type, Chunk *chunk);
        static void record(GCEventType type, std::chrono::microseconds time);
        static void dispose();
    };
}