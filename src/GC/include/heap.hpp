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

    //Private constructor according to the singleton pattern
    Heap() {
      m_heap = reinterpret_cast<char *>(malloc(HEAP_SIZE));
      m_size = 0;
      m_allocated_size = 0;
    }

    // BEWARE only for testing, this should be adressed
    ~Heap() {
      std::free((char *)m_heap);
    }

    inline static Heap *the() { // TODO: make private
      if (m_instance) // if m_instance is not a nullptr
        return m_instance;
      m_instance = new Heap();
      return m_instance;
    }

    inline static Chunk *getAt(std::vector<Chunk *> list, size_t n) {
      auto iter = list.begin();
      if (!n)
        return *iter;
      std::advance(iter, n);
      return *iter;
    }

    void collect();
    void sweep(Heap *heap);
    uintptr_t *try_recycle_chunks(size_t size);
    void free(Heap* heap);
    void free_overlap(Heap *heap);
    void mark(uintptr_t *start, const uintptr_t *end, std::vector<Chunk *> worklist);
    void print_line(Chunk *chunk);
    void print_worklist(std::vector<Chunk *> list);

    inline static Heap *m_instance = nullptr;
    const char *m_heap;
    size_t m_size;
    size_t m_allocated_size;
    uintptr_t *m_stack_top = nullptr;
    bool m_profiler_enable = false;

    // maybe change to std::list
    std::vector<Chunk *> m_allocated_chunks;
    std::vector<Chunk *> m_freed_chunks;

  public:

    /**
     * These are the only two functions which are exposed
     * as the API for LLVM. At the absolute start of the
     * program the developer has to call init() to ensure
     * that the address of the topmost stack frame is
     * saved as the limit for scanning the stack in collect.
    */
    static void init();
    static void dispose();
    static void *alloc(size_t size);
    
    // DEBUG ONLY
    static inline Heap *debug_the() { // TODO: make private
      if (m_instance) // if m_instance is not a nullptr
        return m_instance;
      m_instance = new Heap();
      return m_instance;
    }
    void collect(uint flags); // conditional collection
    void check_init();        // print dummy things
    void print_contents();    // print dummy things
    void set_profiler(bool mode);
  };
}