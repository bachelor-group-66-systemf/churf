#pragma once

#include <assert.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>
using namespace std;

#include "chunk.hpp"

#define HEAP_SIZE   65536

namespace GC {

  class Heap {
  public:

    // Singleton
    static Heap &the() {
      if (m_instance)
	      return *m_instance;
      m_instance = new Heap();
      return *m_instance;
    }

    ~Heap() {
      free((char *)m_heap);
    }

    void *alloc(size_t size);
    void print_contents();

  private:

    Heap() {
      m_heap = reinterpret_cast<char *>(malloc(HEAP_SIZE));
      m_size = 0;
      m_allocated_size = 0;
    }

    void collect();
    void compact();
    void mark(uintptr_t *start, const uintptr_t *end, std::vector<Chunk *> worklist);

    bool compareChunks(Chunk *c1, Chunk *c2);
      
    inline static Heap *m_instance = nullptr;
    const char *m_heap;
    size_t m_size;
    size_t m_allocated_size;

    std::vector<Chunk *> m_allocated_chunks;
    std::vector<Chunk *> m_freed_chunks;

  };
}