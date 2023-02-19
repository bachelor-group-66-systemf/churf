#include <algorithm>
#include <assert.h>
#include <cstring>
#include <execinfo.h>
#include <iostream>
#include <setjmp.h>
#include <stdlib.h>
#include <vector>

#include "../include/heap.hpp"
using namespace std;

namespace GC {

  /**
   * Initialises the heap singleton and saves the address
   * of the calling stack frame as the stack_end. Presumeably
   * this address points to the stack frame of the compiled
   * LLVM executable after linking. (NOT CONFIRMED)
  */
  void Heap::check_init() {
    auto heap = Heap::the();
    cout << "GC m_stack_end:\t" << heap->m_stack_end << endl;
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    cout << "GC stack_start:\t" << stack_start << endl;
  }

  void Heap::init() {
    Heap *heap = Heap::the();
    heap->m_stack_end = reinterpret_cast<uintptr_t *>(__builtin_frame_address(1));
  }

  /**
   * Allocates a given amount of bytes on the heap.
   * 
   * @param size The amount of bytes to be allocated.
   * 
   * @return  A pointer to the address where the memory
   *          has been allocated. This pointer is supposed
   *          to be casted to and object pointer.
  */
  void *Heap::alloc(size_t size) {

    // Singleton
    Heap *heap = Heap::the();
    
    if (size < 0) {
      cout << "Heap: Cannot alloc less than 0B. No bytes allocated." << endl;
      return nullptr;
    }

    if (heap->m_size + size > HEAP_SIZE) {
      collect(heap);
      // If collect failed, crash with OOM error
      assert(heap->m_size + size <= HEAP_SIZE && "Heap: Out Of Memory");
    }

    // If a chunk was recycled, return the old chunk address
    uintptr_t *reused_chunk = try_recycle_chunks(heap, size);
    if (reused_chunk != nullptr) {
      return (void *)reused_chunk;
    }
    
    // If no free chunks was found (reused_chunk is a nullptr),
    // then create a new chunk
    auto new_chunk = new Chunk;
    new_chunk->size = size;
    new_chunk->start = (uintptr_t *)(heap->m_heap + m_size);

    heap->m_size += size;

    heap->m_allocated_chunks.push_back(new_chunk);

    // new_chunk should probably be a unique pointer, if that isn't implicit already 
    return new_chunk->start;
  }

  uintptr_t *Heap::try_recycle_chunks(Heap *heap, size_t size) {
    // Check if there are any freed chunks large enough for current request
    for (size_t i = 0; i < heap->m_freed_chunks.size(); i++) {
      auto cp = heap->m_freed_chunks.at(i);
      if (cp->size > size)
      {
        // Split the chunk, use one part and add the remaining part to
        // the list of freed chunks
        size_t diff = cp->size - size;
        
        auto chunk_complement = new Chunk;
        chunk_complement->size = diff;
        chunk_complement->start = cp->start + cp->size;

        heap->m_freed_chunks.erase(m_freed_chunks.begin() + i);
        heap->m_freed_chunks.push_back(chunk_complement);
        heap->m_allocated_chunks.push_back(cp);
        
        return cp->start;
      }
      else if (cp->size == size)
      {
        // Reuse the whole chunk
        heap->m_freed_chunks.erase(m_freed_chunks.begin() + i);
        heap->m_allocated_chunks.push_back(cp);
        return cp->start;
      }
    }
    return nullptr;
  }

  void Heap::collect(Heap *heap) {
    // Get the adress of the current stack frame

    uintptr_t *stack_end;

    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    if (heap->m_stack_end != nullptr)
      stack_end = heap->m_stack_end;
    else
      stack_end = (uintptr_t *)0; // temporary

    auto work_list = heap->m_allocated_chunks;
    mark(stack_start, stack_end, work_list);

    sweep(heap);

    free(heap);
  }

  void Heap::free(Heap *heap) {
    if (heap->m_freed_chunks.size() > FREE_THRESH) {
      while (heap->m_freed_chunks.size()) {
        auto chunk = heap->m_freed_chunks.back();
        heap->m_freed_chunks.pop_back();
        delete chunk;
      }
    } else {
      free_overlap(heap);
    }
  }

  void Heap::free_overlap(Heap *heap) {
    std::vector<Chunk *> filtered;
    size_t i = 0;
    filtered.push_back(heap->m_freed_chunks.at(i++));
    for (; i < heap->m_freed_chunks.size(); i++) {
      auto prev = filtered.back();
      auto next = heap->m_freed_chunks.at(i);
      if (next->start > (prev->start + prev->size)) {
        filtered.push_back(next);
      }
    }
    heap->m_freed_chunks.swap(filtered);
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
    
    auto heap = Heap::the();

    // get the frame adress, whwere local variables and saved registers are located
    auto stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    cout << "Stack start:\t" << stack_start << endl;
    uintptr_t *stack_end;

    if (heap->m_stack_end != nullptr)
      stack_end = heap->m_stack_end;
    else
      stack_end = (uintptr_t *) stack_start - 40; // dummy value

    auto work_list = heap->m_allocated_chunks;
    // print_worklist(work_list);

    if (flags & MARK) {
      mark(stack_start, stack_end, work_list);
    }

    if (flags & SWEEP) {
      sweep(heap);
    }
    
    if (flags & FREE) {
      free(heap);
    }
  }

  // Not optimal for now, it doesn't have to loop over all objects
  // but mark needs some refinements before this can be optimised
  void Heap::sweep(Heap *heap) {
    for (auto it = heap->m_allocated_chunks.begin(); it != heap->m_allocated_chunks.end();) {
        auto chunk = *it;
        if (!chunk->marked) {
            heap->m_freed_chunks.push_back(chunk);
            it = heap->m_allocated_chunks.erase(it);
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
    auto heap = Heap::the();
    if (heap->m_allocated_chunks.size()) {
      cout << "\nALLOCATED CHUNKS #" << heap->m_allocated_chunks.size() << endl;
      for (auto chunk : heap->m_allocated_chunks) {
          print_line(chunk);
      }
    } else {
      cout << "NO ALLOCATIONS\n" << endl;
    }
    if (heap->m_freed_chunks.size()) {
      cout << "\nFREED CHUNKS #" <<  heap->m_freed_chunks.size() << endl;
      for (auto fchunk : heap->m_freed_chunks) {
          print_line(fchunk);
      }
    } else {
      cout << "NO FREED CHUNKS" << endl;
    }      
  }
}