# event.hpp & event.cpp

An event class used by the profiler to track actions
on the heap.

## Members
`const GCEventType m_type`: The type of event recorded.

`const std::time_t m_timestamp`: The timestamp of the event,
initialized to the current time by `std::time(NULL)`.

`const Chunk *m_chunk`: The chunk an event is related to.
For example, in `alloc` when a new chunk is created, a
new event is recorded with the type of `NewChunk` and
`m_chunk` then contains a copied version of that new chunk.
If an event is not related to a chunk this member is initialized
to a nullptr.

`const size_t m_size`: In an `AllocStart` event, this member
stores the amount of bytes requested to `alloc`. Otherwise
this member is initialized to 0.

## Constructors
`GCEvent(GCEventType type)`: Used for creating events that are
independent of a chunk and size (like `ProfilerDispose`).

`GCEvent(GCEventType type, Chunk *chunk)`: Used for creating events
that are connected to a chunk (like `ChunkMarked`).

`GCEvent(GCEventType type, size_t size)`: Used for creating events
that are related to a size (only `AllocStart`).

## Destructors
`~GCEvent()`: Default destructor and also frees the member
`m_chunk` if it's not the `nullptr`.

## Functions
`GCEventType get_type()`: Getter for the type of the event.

`std::time_t get_time_stamp()`: Getter for the timestamp of
the event.

`const Chunk *get_chunk()`: Getter for the Chunk the event
is related to. The chunk data is constant.

`const char *type_to_string()`: Translates the type of the
event to a string.