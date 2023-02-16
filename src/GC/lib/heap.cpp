#include <algorithm>
#include <assert.h>
#include <cstring>
#include <execinfo.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "heap.hpp"

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
        m_freed_chunks.erase(m_freed_chunks.begin() + i);
        m_allocated_chunks.push_back(cp);
        return cp->start;
      }
    }
    
    // Om inga free chunks finns, skapa ny chunk
    auto new_chunk = new Chunk;
    new_chunk->size = size;
    new_chunk->start = (uintptr_t *)m_heap + m_size;

    m_size += size;

    m_allocated_chunks.push_back(new_chunk);

    return new_chunk->start;
  }

  void Heap::collect() {

    // get the frame adress, whwere local variables and saved registers are located
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    // looking at 10 stack frames back
    const uintptr_t *stack_end = (uintptr_t *)0;
    
    // denna segfaultar om arg för __b_f_a är > 2
    // reinterpret_cast<const uintptr_t *>(__builtin_frame_address(10));
    auto work_list = m_allocated_chunks;
    mark(stack_start, stack_end, work_list);

    sweep();

    // compact();

    // BELOW SHOULD BE INCLUDED (commented only for tests, to see which objects freed)
    
    //release free chunks
    while (m_freed_chunks.size()) {
      auto chunk_pointer = m_freed_chunks.back();
      m_freed_chunks.pop_back();
      delete chunk_pointer; // deletes chunk object, doesn't free memory to the OS
    }
  }

  void Heap::collect(uint flags) {

    std::cout << "DEBUG COLLECT\nFLAGS: " << flags << std::endl;
    // get the frame adress, whwere local variables and saved registers are located
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));

    const uintptr_t *stack_end = (uintptr_t *)0; // dummy value
    
    // denna segfaultar om arg för __b_f_a är > 2
    // reinterpret_cast<const uintptr_t *>(__builtin_frame_address(10));

    auto work_list = m_allocated_chunks;

    if (flags & MARK) {
      mark(stack_start, stack_end, work_list);
    }

    if (flags & SWEEP) {
      sweep();
    }
    
    //release free chunks
    if (flags & FREE) {
      while (m_freed_chunks.size()) {
        auto chunk_pointer = m_freed_chunks.back();
        m_freed_chunks.pop_back();
        delete chunk_pointer; // deletes chunk object, doesn't free heap memory to the OS
      }
    }
  }

  // Not optimal for now, it doesn't have to loop over all objects
  // but mark needs some refinements before this can be optimised
  // sweep apparently only sweeps chunks of odd size, eventhough the're not marked
  void Heap::sweep() {
/*     for (char i = 0; i < m_allocated_chunks.size();) {
      auto chunk = m_allocated_chunks.at(i);
      std::cout << "Current chunk: " << chunk->start << std::endl;
      if (chunk->marked == false) {
        std::cout << "The current chunk was removed" << std::endl;
        m_allocated_chunks.erase(m_allocated_chunks.begin() + i);
        m_freed_chunks.push_back(chunk);
      } 
    } */
    // This captures the wierd behavior or my lack of knowledge of vectors
    for (auto chunk : m_allocated_chunks) {
        if (!chunk->marked) {
            auto it = std::find(m_allocated_chunks.begin(), m_allocated_chunks.end(), chunk);
            if (it != m_allocated_chunks.end())
                m_allocated_chunks.erase(it);
            m_freed_chunks.push_back(chunk);
        }
    }
  }

  // TODO: return the worklist filtered on mark = true
  void Heap::mark(uintptr_t *start, const uintptr_t *end, std::vector<Chunk*> work_list) {
    for (; start < end; start++) { // to find adresses thats in the worklist
      for (size_t i = 0; i < work_list.size(); i++) { // fix this
      auto chunk = work_list.at(i);
        if (chunk->start <= start && start < chunk->start + chunk->size) {
          if (!chunk->marked) {
            chunk->marked = true;
            work_list.erase(work_list.begin() + i);
            mark(reinterpret_cast<uintptr_t *>(chunk->start + chunk->size), end, work_list); //
            return;
          }
        }
      }
    }
  }
  

  /* Alternative marking, pseudocode
  mark_from_roots():
    worklist <- empty
    for fld in Roots
      ref <- *fld
      if ref ≠ null && !marked(ref)
        set_marked(ref)
        worklist.add(ref)
        mark()

  mark():
    while size(worklist) > 0
      ref <- remove_first(worklist)
      for fld in Pointers(ref)
        child <- *fld
        if child ≠ null && !marked(child)
          set_marked(child)
          worklist.add(child)
  */
  // For testing purposes
  void Heap::print_line(Chunk *chunk) {
    std::cout << "Marked: " << chunk->marked << "\nStart adr: " << chunk->start << "\nSize: " << chunk->size << " B\n" << std::endl;
  }

  void Heap::print_contents() {
    if (m_allocated_chunks.size()) {
      std::cout << "\nALLOCATED CHUNKS #" << m_allocated_chunks.size() << std::endl;
      for (auto chunk : m_allocated_chunks) {
          print_line(chunk);
      }
    } else {
      std::cout << "NO ALLOCATIONS\n" << std::endl;
    }
    if (m_freed_chunks.size()) {
      std::cout << "\nFREED CHUNKS #" <<  m_freed_chunks.size() << std::endl;
      for (auto fchunk : m_freed_chunks) {
          print_line(fchunk);
      }
    } else {
      std::cout << "NO FREED CHUNKS" << std::endl;
    }      
  }
}