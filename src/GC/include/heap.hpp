#pragma once

#include <assert.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "chunk.hpp"

#define HEAP_SIZE   65536

#define MARK        (uint) 0x1
#define SWEEP       (uint) 0x2
#define FREE        (uint) 0x4
#define COLLECT_ALL (uint) 0x7

#define FREE_THRESH (uint) 20

namespace GC {

  class Heap {

  private:

    Heap() {
      m_heap = reinterpret_cast<char *>(malloc(HEAP_SIZE));
      m_size = 0;
      m_allocated_size = 0;
    }

    void collect(Heap *heap);
    void sweep(Heap *heap);
    uintptr_t *try_recycle_chunks(Heap *heap, size_t size);
    void free(Heap *heap);
    void free_overlap(Heap *heap);
    void mark(uintptr_t *start, const uintptr_t *end, std::vector<Chunk *> worklist);
    void print_line(Chunk *chunk);
    void print_worklist(std::vector<Chunk *> list);

    inline static Heap *m_instance = nullptr;
    const char *m_heap;
    size_t m_size;
    size_t m_allocated_size;
    uintptr_t *m_stack_end = nullptr;

    std::vector<Chunk *> m_allocated_chunks;
    std::vector<Chunk *> m_freed_chunks;

  public:

    static Heap *the() {
      if (m_instance)
        return m_instance;
      m_instance = new Heap();
      return m_instance;
    }

    // BEWARE only for testing, this should be adressed
    ~Heap() {
      std::free((char *)m_heap);
    }

    void *alloc(size_t size);
    void init();
    
    // DEBUG ONLY
    void check_init();
    void collect(uint flags);
    void print_contents();
  };
}