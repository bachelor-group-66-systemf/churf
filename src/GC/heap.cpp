#pragma once

#include <algorithm>
#include <assert.h>
#include <cstring>
#include <execinfo.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "include/heap.hpp"

namespace GC {

  // alloc assumes that after the collect phase, the aligned memory in the heap is compacted from the start,
  void *Heap::alloc(size_t size) { 
    auto heap = Heap::the();
    assert(size > 0 && "Heap: Cannot alloc less than 0B");
    if (heap.m_size + size > HEAP_SIZE) {
      // try collect
      collect();
      assert(heap.m_size + size <= HEAP_SIZE && "Heap: Out Of Memory");
    }

    // kolla freed chunks innan
    for (size_t i = 0; i < m_freed_chunks.size(); i++) {
      auto cp = m_freed_chunks.at(i);
      if (cp->size > size)
      {
        // dela upp chunken och sno ena delen
        size_t diff = cp->size - size;
        
        auto chunk_complement = new Chunk;
        chunk_complement->size = diff;
        chunk_complement->start = cp->start + cp->size;

        m_freed_chunks.erase(m_freed_chunks.begin() + i);
        m_freed_chunks.push_back(chunk_complement);
        m_allocated_chunks.push_back(cp);
        
        return cp->start;
      }
      else if (cp->size == size)
      {
        // sno hela chunken
        m_freed_chunks.erase(m_freed_chunks.begin()+i);
        m_allocated_chunks.push_back(cp);
        return cp->start;
      }
    }
    
    // Om inga free chunks finns, skapa ny chunk
    auto new_chunk = new Chunk;
    new_chunk->size = size;
    new_chunk->start = (void *)m_heap + m_size;

    m_size += size;

    m_allocated_chunks.push_back(new_chunk);

    return new_chunk->start;
  }

  bool compareChunks(Chunk *c1, Chunk *c2) {
    return c1->start < c2->start;
  }

  void Heap::collect() {
    // mark all objs
    // compact();
    // free all unmarked (m_freed_chunks.add(jalkdsj))

    // get the frame adress, whwere local variables and saved registers are located    
    auto stack_start = reinterpret_cast<const uintptr_t *>(__builtin_frame_address(0));
    auto stack_end = reinterpret_cast<const uintptr_t *>(0ul);
    

    //release free chunks
    while (m_freed_chunks.size()) {
      auto chunk_pointer = m_freed_chunks.back();
      m_freed_chunks.pop_back();
      delete chunk_pointer;
    }
  }
  
  void Heap::compact() {

    // sort alloced_chunks by their start-addresses
    std::sort(m_allocated_chunks.begin(), m_allocated_chunks.end(), compareChunks);
    
    // move all chunks to the start of the heap
    auto heap_curr = (char *)m_heap;
    for (auto space : m_allocated_chunks) {
      if (space->start != heap_curr) {
        memmove(heap_curr, space->start, space->size);
        space->start = heap_curr;
        heap_curr += space->size;
      } else {
        heap_curr += space->size;
      }
    }
  }

  void Heap::mark(uintptr_t *start, uintptr_t *end) {
    for (; start < end; start += 1) { // start < end???
      for (auto chunk : m_allocated_chunks) {
        if (chunk->start <= start && start < chunk->start + chunk->size) {
          if (!chunk->marked) {
            chunk->marked = true;
            //mark(chunk) //
            //TODO
          }
        }
      }
    }
  }
}