#pragma once

#include <assert.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "include/heap.hpp"

namespace GC {

  size_t Heap::getSize() {
    return m_size;
  }

  void *Heap::alloc(size_t size) {
    auto heap = Heap::the();
    assert(heap.getSize() + size <= HEAP_SIZE);

    return m_heap + m_size;
  }

  void Heap::collect() {
    // TODO
  }

}
