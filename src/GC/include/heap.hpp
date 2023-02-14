#pragma once

#include <assert.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

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

    size_t getSize();
    void *alloc(size_t size);

  private:

    Heap() {
      m_heap = reinterpret_cast<char *>(malloc(HEAP_SIZE));
      m_size = 0;
      m_allocated_size = 0;
    }

    void collect();
      
    inline static Heap *m_instance = nullptr;
    char *m_heap;
    size_t m_size;
    size_t m_allocated_size;
  }
}