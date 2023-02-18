#include <algorithm>
#include <assert.h>
#include <cstring>
#include <execinfo.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "heap.hpp"
using namespace std;

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
    // denna är helt onödig just nu, freed chunks kommer alltid va tom
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

    // new_chunk should probably be a unique pointer, if that isn't implicit already 
    return new_chunk->start;
  }

  void Heap::collect() {

    // get the frame adress, whwere local variables and saved registers are located
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    // looking at 10 stack frames back
    const uintptr_t *stack_end = (uintptr_t *)0; // temporary
    
    // denna segfaultar om arg för __b_f_a är > 2
    // reinterpret_cast<const uintptr_t *>(__builtin_frame_address(10));
    auto work_list = m_allocated_chunks;
    mark(stack_start, stack_end, work_list);

    sweep();

    // compact();

    free();

    // We shouldn't do this, since then m_freed_chunks doesnt' have any real purpose,
    // it should be used in alloc, it isn't if we delete *all* of its contentes
    // release free chunks
    // while (m_freed_chunks.size()) {
    //   auto chunk_pointer = m_freed_chunks.back();
    //   m_freed_chunks.pop_back();
    //   delete chunk_pointer; // deletes chunk object, doesn't free memory to the OS
    // }
  }

  void Heap::free() {
    if (m_freed_chunks.size() > FREE_THRESH) {
      while (m_freed_chunks.size()) {
        auto chunk = m_freed_chunks.back();
        m_freed_chunks.pop_back();
        delete chunk;
      }
    } else {
      free_overlap();
    }
  }

  void Heap::free_overlap() {
    std::vector<Chunk *> filtered;
    size_t i = 0;
    filtered.push_back(m_freed_chunks.at(i++));
    for (; i < m_freed_chunks.size(); i++) {
      auto prev = filtered.back();
      auto next = m_freed_chunks.at(i);
      if (next->start > (prev->start + prev->size)) {
        filtered.push_back(next);
      }
    }
    m_freed_chunks.swap(filtered);
  }

  void Heap::collect(uint flags) {

    cout << "DEBUG COLLECT\nFLAGS: ";
    if (flags & MARK)
      cout << "\n - MARK";
    if (flags & SWEEP)
      cout << "\n - SWEEP";
    if (flags & FREE)
      cout << "\n - FREE";
    cout << endl;
    // get the frame adress, whwere local variables and saved registers are located
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    cout << "Stack start:\t" << stack_start << endl;

    const uintptr_t *stack_end = (uintptr_t *) stack_start - 40; // dummy value
    
    // denna segfaultar om arg för __b_f_a är > 2
    // reinterpret_cast<const uintptr_t *>(__builtin_frame_address(10));

    auto work_list = m_allocated_chunks;
    // print_worklist(work_list);

    if (flags & MARK) {
      mark(stack_start, stack_end, work_list);
    }

    if (flags & SWEEP) {
      sweep();
    }
    
    if (flags & FREE) {
      free();
    }

    //release free chunks
    // if (flags & FREE) {
    //   while (m_freed_chunks.size()) {
    //     auto chunk_pointer = m_freed_chunks.back();
    //     m_freed_chunks.pop_back();
    //     delete chunk_pointer; // deletes chunk object, doesn't free heap memory to the OS
    //   }
    // }
  }

  // Not optimal for now, it doesn't have to loop over all objects
  // but mark needs some refinements before this can be optimised
  void Heap::sweep() {
    for (auto it = m_allocated_chunks.begin(); it != m_allocated_chunks.end();) {
        auto chunk = *it;
        if (!chunk->marked) {
            m_freed_chunks.push_back(chunk);
            it = m_allocated_chunks.erase(it);
        }
        else { 
            ++it;
        }
    }
  }

  // TODO: return the worklist filtered on mark = true
  // This assumes that there are no chains of pointers, will be fixed later on
  void Heap::mark(uintptr_t *start, const uintptr_t *end, vector<Chunk*> worklist) {
    for (; start > end; start--) { // to find adresses thats in the worklist
      if (*start % 8 == 0) { // all pointers must be aligned as double words
        for (size_t i = 0; i < worklist.size(); i++) { // fix this
          auto chunk = worklist.at(i);
          uintptr_t c_start = reinterpret_cast<uintptr_t>(chunk->start);
          uintptr_t c_end = reinterpret_cast<uintptr_t>(chunk->start + chunk->size);
          if (c_start <= *start && *start < c_end) {
            uintptr_t c_start = reinterpret_cast<uintptr_t>(chunk->start);
            if (!chunk->marked) {
              chunk->marked = true;
              worklist.erase(worklist.begin() + i);
              auto new_stack_start = reinterpret_cast<uintptr_t *>(start);
              mark(new_stack_start, end, worklist); //
              return;
            }
          }
        }
      }
    }
  }

/*     void mark_test(vector<Chunk *> worklist) {
        while (worklist.size() > 0) {
            Chunk *ref = worklist.pop_back();
            Chunk *child = (Chunk*) *ref;
            if (child != NULL && !child->marked) {
                child->marked = true;
                worklist.push_back(child);
                mark_test(worklist);
            }
    }
  }

  void mark_from_roots(uintptr_t *start, const uintptr_t *end) {
    vector<Chunk *> worklist;
    for (;start > end; start--) {
        Chunk *ref = *start;
        if (ref != NULL && !ref->marked) {
            ref->marked = true;
            worklist.push_back(ref);
            mark_test(worklist);
        }
    }
  } */

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
    cout << "Marked: " << chunk->marked << "\nStart adr: " << chunk->start << "\nSize: " << chunk->size << " B\n" << endl;
  }

  void Heap::print_worklist(std::vector<Chunk *> list) {
    for (auto cp : list) {
      cout << "Chunk at:\t" << cp->start << "\nSize:\t\t" << cp->size << endl;
    }
  }

  void Heap::print_contents() {
    if (m_allocated_chunks.size()) {
      cout << "\nALLOCATED CHUNKS #" << m_allocated_chunks.size() << endl;
      for (auto chunk : m_allocated_chunks) {
          print_line(chunk);
      }
    } else {
      cout << "NO ALLOCATIONS\n" << endl;
    }
    if (m_freed_chunks.size()) {
      cout << "\nFREED CHUNKS #" <<  m_freed_chunks.size() << endl;
      for (auto fchunk : m_freed_chunks) {
          print_line(fchunk);
      }
    } else {
      cout << "NO FREED CHUNKS" << endl;
    }      
  }
}