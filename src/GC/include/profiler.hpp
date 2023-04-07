#pragma once

#include <iostream>
#include <vector>

#include "chunk.hpp"
#include "event.hpp"

namespace GC {

    class Profiler {
    private:
        Profiler() {}
        ~Profiler()
        {
            for (GCEvent *c : m_events)
                delete c;
        }

        /**
         * Returns the instance of the Profiler singleton.
         * If m_instance is the nullptr and the profiler
         * is not initialized yet, initialize it and return
         * the pointer to it. Otherwise return the previously
         * initialized pointer.
         * 
         * @returns The pointer to the profiler singleton.
        */
        static Profiler *the()
        {
            if (m_instance)
                return m_instance;
            m_instance = new Profiler();
            return m_instance;
        }

        inline static Profiler *m_instance {nullptr};
        std::vector<GCEvent *> m_events;

        std::ofstream create_file_stream();
        std::string get_log_folder();
        static void dump_trace();

    public:
        static void record(GCEventType type);
        static void record(GCEventType type, size_t size);
        static void record(GCEventType type, Chunk *chunk);
        static void dispose();
    };
}