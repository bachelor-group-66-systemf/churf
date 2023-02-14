#pragma once

#include <stdlib.h>

#define CHUNK_LIST_CAP 1024

namespace GC {
  
  struct Chunk {
    bool marked;
    void *start;
    size_t size;
  };

}