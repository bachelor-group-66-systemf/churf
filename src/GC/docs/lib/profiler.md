# profiler.hpp & profiler.cpp

## Members
`inline static Profiler *m_instance`: The pointer to the profiler
singleton instance.

`std::vector<GCEvent *> m_events`: A vector of events recorded
by the profiler. The contents are always sorted by time.

## Constructors
`Profiler()`: Default constructor, declared private because of
the singleton pattern.

## Destructors
`~Profiler()`: Default destructor, declared private because of
the singleton pattern. This destructor also deletes any events
that were recorded by the profiler to free memory.

## Functions
`static void record(GCEventType type)`: Records an event independent
of a size and a chunk (like `ProfilerDispose`).

`static void record(GCEventType type, size_t size)`: Records an event independent
of a chunk but not a size (only `AllocStart`).

`static void record(GCEventType type, Chunk *chunk)`: Records an event independent
of a size but not a chunk (like `NewChunk`).

`static void dispose()`: Disposes the profiler by dumping a log file of all
events and deleting events to free memory.