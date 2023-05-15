#include <iostream>
#include <stdexcept>
#include <stdlib.h>
#include <vector>
#include <unordered_map>
#include <chrono>
#include <queue>
#include <set>

#include "heap.hpp"

#define time_now	std::chrono::high_resolution_clock::now()
#define to_us		std::chrono::duration_cast<std::chrono::microseconds>

using std::cout, std::endl, std::vector, std::hex, std::dec, std::unordered_map;

namespace GC
{
	/**
	 * This implementation of the() guarantees laziness
	 * on the instance and a correct destruction with
	 * the destructor.
	 * 
	 * @returns The singleton object.
	*/
	Heap& Heap::the()
	{
		static Heap instance;
		return instance;
	}

	/**
	 * Initialises the heap singleton and saves the address
	 * of the calling function's stack frame as the stack_top.
	 * Presumeably this address points to the stack frame of
	 * the compiled LLVM executable after linking.
	 */
	void Heap::init()
	{
		Heap &heap = Heap::the();
		if (heap.profiler_enabled())
			Profiler::record(HeapInit);
// clang complains because arg for __b_f_a is not 0 which is "unsafe"
#pragma clang diagnostic ignored "-Wframe-address"
		heap.m_stack_top = static_cast<uintptr_t *>(__builtin_frame_address(1));
		// TODO: handle this below
		//heap.m_heap_top = heap.m_heap;
	}

	void Heap::set_profiler_log_options(RecordOption flags)
	{
		Profiler::set_log_options(flags);
	}

	/**
	 * Disposes the heap and the profiler at program exit
	 * which also triggers a heap log file dumped if the
	 * profiler is enabled.
	 */
	void Heap::dispose()
	{
		Heap &heap = Heap::the();
		if (heap.profiler_enabled())
			Profiler::dispose();
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
	void *Heap::alloc(size_t size)
	{
		auto a_start = time_now;
		// Singleton
		Heap &heap = Heap::the();
		bool profiler_enabled = heap.profiler_enabled();
		
		if (profiler_enabled)
			Profiler::record(AllocStart, size);

		if (size == 0)
		{
			cout << "Heap: Cannot alloc 0B. No bytes allocated." << endl;
			return nullptr;
		}

		if (heap.m_size + size > HEAP_SIZE)
		{
			// auto a_ms = to_us(c_start - a_start);
			// Profiler::record(AllocStart, a_ms);
			auto stack_bottom = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
			heap.collect(stack_bottom);
			// If memory is not enough after collect, crash with OOM error
			if (heap.m_size > HEAP_SIZE)
			{
				throw std::runtime_error(std::string("Error: Heap out of memory"));
			}
			//throw std::runtime_error(std::string("Error: Heap out of memory"));
		}
		if (heap.m_size + size > HEAP_SIZE)
		{
			if (profiler_enabled)
				Profiler::dispose();
			throw std::runtime_error(std::string("Error: Heap out of memory"));
		}

		// If a chunk was recycled, return the old chunk address
		Chunk *reused_chunk = heap.try_recycle_chunks(size);
		if (reused_chunk != nullptr)
		{
			if (profiler_enabled)
				Profiler::record(ReusedChunk, reused_chunk);
			auto a_end = time_now;
			auto a_ms = to_us(a_end - a_start);
			Profiler::record(AllocStart, a_ms);
			return static_cast<void *>(reused_chunk->m_start);
		}

		// If no free chunks was found (reused_chunk is a nullptr),
		// then create a new chunk
		auto new_chunk = new Chunk(size, (uintptr_t *)(heap.m_heap + heap.m_size));

		heap.m_size += size;
		// TODO: handle this below
		//heap.m_total_size += size;
		heap.m_allocated_chunks.push_back(new_chunk);

		if (profiler_enabled)
			Profiler::record(NewChunk, new_chunk);

		auto a_end = time_now;
		auto a_ms = to_us(a_end - a_start);
		Profiler::record(AllocStart, a_ms);
		return new_chunk->m_start;
	}

	/**
	 * Tries to recycle used and freed chunks that are
	 * already allocated objects by the OS but freed
	 * from our Heap. This reduces the amount of GC
	 * objects slightly which saves time from malloc'ing
	 * memory from the OS.
	 *
	 * @param size  Amount of bytes needed for the object
	 *              which is about to be allocated.
	 *
	 * @returns If a chunk is found and recycled, a
	 *          pointer to the allocated memory for
	 *          the object is returned. If not, a
	 *          nullptr is returned to signify no
	 *          chunks were found.
	 */
	Chunk *Heap::try_recycle_chunks(size_t size)
	{
		Heap &heap = Heap::the();
		// Check if there are any freed chunks large enough for current request
		for (size_t i = 0; i < heap.m_freed_chunks.size(); i++)
		{
			//auto chunk = Heap::get_at(heap.m_freed_chunks, i);
			auto chunk = heap.m_freed_chunks[i];
			auto iter = heap.m_freed_chunks.begin();
			i++;
			//advance(iter, i);
			if (chunk->m_size > size)
			{
				// Split the chunk, use one part and add the remaining part to
				// the list of freed chunks
				size_t diff = chunk->m_size - size;
				auto chunk_complement = new Chunk(diff, chunk->m_start + chunk->m_size);

				heap.m_freed_chunks.erase(iter);
				heap.m_freed_chunks.push_back(chunk_complement);
				heap.m_allocated_chunks.push_back(chunk);

				return chunk;
			}
			else if (chunk->m_size == size)
			{
				// Reuse the whole chunk
				heap.m_freed_chunks.erase(iter);
				heap.m_allocated_chunks.push_back(chunk);
				return chunk;
			}
		}
		// If no chunk was found, return nullptr
		return nullptr;
	}

	/**
	 * Returns a bool whether the profiler is enabled
	 * or not.
	 * 
	 * @returns True or false if the profiler is enabled
	 * 			or disabled respectively.
	*/
	bool Heap::profiler_enabled() {
		Heap &heap = Heap::the();
		return heap.m_profiler_enable;
	}

	/**
	 * Collection phase of the garbage collector. When
	 * an allocation is requested and there is no space
	 * left on the heap, a collection is triggered. This
	 * function is private so that the user cannot trigger
	 * a collection unneccessarily.
	 */
	void Heap::collect(uintptr_t *stack_bottom)
	{
		auto c_start = time_now;

		Heap &heap = Heap::the();

		if (heap.profiler_enabled())
			Profiler::record(CollectStart);

		// get current stack frame
		stack_bottom = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));

		if (heap.m_stack_top == nullptr)
			throw std::runtime_error(std::string("Error: Heap is not initialized, read the docs!"));

		// uintptr_t *stack_top = heap.m_stack_top;

		//auto work_list = heap.m_allocated_chunks;
		//mark(stack_bottom, stack_top, work_list);

		// Testing mark_hash, previous woking implementation above
		// create_table();
		// mark_hash(stack_bottom, stack_top);

		create_table();
		vector<uintptr_t *> roots;
		// cout << "\nb4 find_roots\n";
		find_roots(stack_bottom, roots);

		// cout << "b4 mark\n";''
		mark(roots);

		// cout << "b4 sweep\n";
		sweep(heap);

		// cout << "b4 free\n";
		free(heap);
		
		auto c_end = time_now;
		
		Profiler::record(CollectStart, to_us(c_end - c_start));
	}

	void Heap::find_roots(uintptr_t *stack_bottom, vector<uintptr_t *> &roots)
	{
		auto heap_bottom = reinterpret_cast<const uintptr_t>(m_heap);
		auto heap_top = reinterpret_cast<const uintptr_t>(m_heap + HEAP_SIZE);

		while (stack_bottom < m_stack_top)
		{
			if (heap_bottom < *stack_bottom && *stack_bottom < heap_top)
			{
				roots.push_back(stack_bottom);
			}
			stack_bottom++;
		}
	}
	
	void Heap::mark(vector<uintptr_t *> &roots)
	{
		bool prof_enabled = m_profiler_enable;
		if (prof_enabled)
			Profiler::record(MarkStart);

		auto iter = roots.begin(), end = roots.end();
		std::queue<std::pair<uintptr_t, uintptr_t>> chunk_spaces;

		while (iter != end)
		{
			find_chunks(*iter++, chunk_spaces);
		}

		while (!chunk_spaces.empty())
		{
			auto range = chunk_spaces.front();
			chunk_spaces.pop();

			auto addr_bottom = reinterpret_cast<uintptr_t *>(range.first);
			auto addr_top = reinterpret_cast<uintptr_t *>(range.second);

			while (addr_bottom < addr_top)
			{
				find_chunks(addr_bottom, chunk_spaces);
				addr_bottom++;
			}
		}
	}

	void Heap::find_chunks(uintptr_t *stack_addr, std::queue<std::pair<uintptr_t, uintptr_t>> &chunk_spaces)
	{
		Heap &heap = Heap::the();

		auto it = heap.m_chunk_table.find(*stack_addr);
		if (it != heap.m_chunk_table.end())
		{
			auto chunk = it->second;

			if (!chunk->m_marked) 
			{
				auto c_start = reinterpret_cast<uintptr_t>(chunk->m_start);
				auto c_size  = reinterpret_cast<uintptr_t>(chunk->m_size);
				auto c_end   = reinterpret_cast<uintptr_t>(c_start + c_size);

				chunk->m_marked = true;
				chunk_spaces.push(std::make_pair(c_start, c_end));
			}
			
		}

/* 		auto iter = m_allocated_chunks.begin();
		auto end = m_allocated_chunks.end();

		while (iter != end)
		{
			auto chunk = *iter++;
			
			if (chunk->m_marked)
				continue;

			auto c_start = reinterpret_cast<uintptr_t>(chunk->m_start);
			auto c_size  = reinterpret_cast<uintptr_t>(chunk->m_size);
			auto c_end   = reinterpret_cast<uintptr_t>(c_start + c_size);

			if (c_start < *stack_addr && *stack_addr < c_end)
			{
				chunk->m_marked = true;
				chunk_spaces.push(std::make_pair(c_start, c_end));
			}
		} */

	}

	void Heap::create_table() 
	{
		Heap &heap = Heap::the();
		unordered_map<uintptr_t, Chunk*> chunk_table;
		for (auto chunk : heap.m_allocated_chunks) {
			auto pair = std::make_pair(reinterpret_cast<uintptr_t>(chunk->m_start), chunk);
			heap.m_chunk_table.insert(pair);		
		}
	}

	void Heap::mark_hash(uintptr_t *start, const uintptr_t* const end) 
	{
		Heap &heap = Heap::the();

		bool profiler_enabled = heap.m_profiler_enable;
		if (profiler_enabled)
			Profiler::record(MarkStart);

		for (; start <= end; start++) 
		{
			auto search = heap.m_chunk_table.find(*start);
			if (search != heap.m_chunk_table.end())
			{
				Chunk *chunk = search->second;
				auto c_start = reinterpret_cast<uintptr_t>(chunk->m_start);
				auto c_size = reinterpret_cast<uintptr_t>(chunk->m_size);
				auto c_end = reinterpret_cast<uintptr_t*>(c_start + c_size);
				if (!chunk->m_marked) 
				{
					chunk->m_marked = true;

					if (profiler_enabled)
						Profiler::record(ChunkMarked, chunk);

					//mark_hash(chunk->m_start, c_end);
					Chunk *next = find_pointer_hash((uintptr_t *) c_start, (uintptr_t *) c_end);
					while (next != NULL) 
					{
						if (!next->m_marked) 
						{
							next->m_marked = true;

							if (profiler_enabled)
								Profiler::record(ChunkMarked, chunk);

							auto c_start = reinterpret_cast<uintptr_t>(next->m_start);
							auto c_size  = reinterpret_cast<uintptr_t>(next->m_size);
							auto c_end   = reinterpret_cast<uintptr_t>(c_start + c_size);
							next = find_pointer_hash((uintptr_t *) c_start, (uintptr_t *) c_end);
						}
					}
				}
			}
		}
	}

	/**
	 * Sweeps the heap, unmarks the marked chunks for the next cycle,
	 * adds the unmarked nodes to the list of freed chunks; to be freed.
	 *
	 * Time complexity: O(N^2), where N is the number of allocated chunks. 
	 * 					It is quadratic, in the worst case, 
	 * 					since each call to erase() is linear.  
	 *
	 * @param heap Pointer to the heap singleton instance.
	 */
	void Heap::sweep(Heap &heap)
	{
		bool profiler_enabled = heap.m_profiler_enable;
		if (profiler_enabled)
			Profiler::record(SweepStart);
		auto iter = heap.m_allocated_chunks.begin();
		// std::cout << "Chunks alloced: " << heap.m_allocated_chunks.size() << std::endl;
		// This cannot "iter != stop", results in seg fault, since the end gets updated, I think.
		while (iter != heap.m_allocated_chunks.end())
		{
			Chunk *chunk = *iter;

			// Unmark the marked chunks for the next iteration.
			if (chunk->m_marked)
			{
				chunk->m_marked = false;
				++iter;
			}
			else
			{
				// Add the unmarked chunks to freed chunks and remove from
				// the list of allocated chunks
				if (profiler_enabled)
					Profiler::record(ChunkSwept, chunk);
				heap.m_freed_chunks.push_back(chunk);
				iter = heap.m_allocated_chunks.erase(iter);
				heap.m_size -= chunk->m_size;
				// cout << "Decremented total heap size with: " << chunk->m_size << endl;
				// cout << "Total size is: " << heap.m_size << endl;
			}
		}
		// std::cout << "Chunks left: " << heap.m_allocated_chunks.size() << std::endl;
	}

	/**
	 * Frees chunks that was moved to the list m_freed_chunks
	 * by the sweep phase. If there are more than a certain
	 * amount of free chunks, delete the free chunks to
	 * avoid cluttering.
	 * 
	 * Time complexity: O(N^2), where N is the freed chunks.
	 * 					If free_overlap() is called, it runs in O(N^2),
	 * 					otherwise O(N).
	 *
	 * @param heap  Heap singleton instance, only for avoiding
	 *              redundant calls to the singleton get
	 */
	void Heap::free(Heap &heap)
	{
		bool profiler_enabled = heap.m_profiler_enable;
		if (profiler_enabled)
			Profiler::record(FreeStart);
		if (heap.m_freed_chunks.size() > FREE_THRESH)
		{
			bool profiler_enabled = heap.profiler_enabled();
			while (heap.m_freed_chunks.size())
			{
				auto chunk = heap.m_freed_chunks.back();
				heap.m_freed_chunks.pop_back();
				if (profiler_enabled)
					Profiler::record(ChunkFreed, chunk);
				// heap.m_size -= chunk->m_size;
				// cout << "Decremented total heap size with: " << chunk->m_size << endl;
				// cout << "Total size is: " << heap.m_size << endl;
				delete chunk;
			}
		}
		// if there are chunks but not more than FREE_THRESH
		else if (heap.m_freed_chunks.size())
		{
			// essentially, always check for overlap between
			// chunks before finishing the allocation
			free_overlap(heap);
		}
	}

	/**
	 * Checks for overlaps between freed chunks of memory
	 * and removes overlapping chunks while prioritizing
	 * the chunks at lower addresses.
	 * 
	 * Time complexity: O(N^2), where N is the number of freed chunks.
	 * 					At each iteration get_at() is called, which is linear.
	 *
	 * @param heap  Heap singleton instance, only for avoiding
	 *              redundant calls to the singleton get
	 *
	 * @note Maybe this should be changed to prioritizing
	 *       larger chunks. Should remove get_at() to indexing,
	 * 		 since that's constant.
	 */
	void Heap::free_overlap(Heap &heap) // borde göra en record(ChunkFreed) på onödiga chunks
	{
		std::vector<Chunk *> filtered;
		size_t i = 0;
		//auto prev = Heap::get_at(heap.m_freed_chunks, i++);
		auto prev = heap.m_freed_chunks[i++];
		prev->m_marked = true;
		filtered.push_back(prev);
		// cout << filtered.back()->m_start << endl;
		for (; i < heap.m_freed_chunks.size(); i++)
		{
			prev = filtered.back();
			//auto next = Heap::get_at(heap.m_freed_chunks, i);
			auto next = heap.m_freed_chunks[i];
			auto p_start = (uintptr_t)(prev->m_start);
			auto p_size = (uintptr_t)(prev->m_size);
			auto n_start = (uintptr_t)(next->m_start);
			if (n_start >= (p_start + p_size))
			{
				next->m_marked = true;
				filtered.push_back(next);
			}
		}
		heap.m_freed_chunks.swap(filtered);
		
		bool profiler_enabled = heap.m_profiler_enable;
		// After swap m_freed_chunks contains still available chunks
		// and filtered contains all the chunks, so delete unused chunks
		for (Chunk *chunk : filtered)
		{
			// if chunk was filtered away, delete it
			if (!chunk->m_marked)
			{
				if (profiler_enabled)
					Profiler::record(ChunkFreed, chunk);
				heap.m_size -= chunk->m_size;
				cout << "Decremented total heap size with: " << chunk->m_size << endl;
				cout << "Total size is: " << heap.m_size << endl;
				delete chunk;
			}
			else
			{
				chunk->m_marked = false;
			}
		}
	}

	void Heap::set_profiler(bool mode)
	{
		Heap &heap = Heap::the();
		heap.m_profiler_enable = mode;
	}

	Chunk* find_pointer(uintptr_t *start, const uintptr_t* const end, vector<Chunk *> &worklist) {
		for (; start <= end; start++) {
			auto it = worklist.begin();
			auto stop = worklist.end();
			while (it != stop)
			{
				Chunk *chunk = *it;
				auto c_start = reinterpret_cast<uintptr_t>(chunk->m_start);
				auto c_size  = reinterpret_cast<uintptr_t>(chunk->m_size);
				auto c_end   = reinterpret_cast<uintptr_t>(c_start + c_size);

				// Check if the stack pointer points to something within the chunk
				if (c_start <= *start && *start < c_end)
				{
					return chunk;	
				}
				return NULL;
			}
		}
	}

	// Checks if a given chunk points to another chunk and returns it
	Chunk* Heap::find_pointer_hash(uintptr_t *start, const uintptr_t* const end) {
		Heap &heap = Heap::the();
		for (; start <= end; start++) {
			auto search = heap.m_chunk_table.find(*start);
			if (search != heap.m_chunk_table.end()) {
				return search->second;
			}
			return NULL;
		}
	}

#ifdef HEAP_DEBUG
	/**
	 * Prints the result of Heap::init() and a dummy value
	 * for the current stack frame for reference.
	 */
	void Heap::check_init()
	{
		Heap &heap = Heap::the();
		cout << "Heap addr:\t" << &heap << "\n";
		cout << "GC m_stack_top:\t" << heap.m_stack_top << "\n";
		auto stack_bottom = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
		cout << "GC stack_bottom:\t" << stack_bottom << endl;
	}

	/**
	 * Conditional collection, only to be used in debugging
	 *
	 * @param flags Bitmap of flags
	 */
	void Heap::collect(CollectOption flags)
	{
		set_profiler(true);

		Heap &heap = Heap::the();

		if (heap.m_profiler_enable)
			Profiler::record(CollectStart);

		cout << "DEBUG COLLECT\nFLAGS: ";
		if (flags & MARK)
			cout << "\n - MARK";
		if (flags & SWEEP)
			cout << "\n - SWEEP";
		if (flags & FREE)
			cout << "\n - FREE";
		cout << "\n";

		// get the frame adress, whwere local variables and saved registers are located
		auto stack_bottom = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
		cout << "Stack bottom in collect:\t" << stack_bottom << "\n";
		uintptr_t *stack_top = heap.m_stack_top;

		cout << "Stack end in collect:\t " << stack_top << endl;
		auto work_list = heap.m_allocated_chunks;

		if (flags & MARK)
			mark(stack_bottom, stack_top, work_list);

		if (flags & SWEEP)
			sweep(heap);

		if (flags & FREE)
			free(heap);
	}

	// Mark child references from the root references
	void mark_test(vector<Chunk *> &worklist)
	{
		while (worklist.size() > 0)
		{
			Chunk *ref = worklist.back();
			worklist.pop_back();
			Chunk *child = (Chunk *)ref; // this is probably not correct
			if (child != nullptr && !child->m_marked)
			{
				child->m_marked = true;
				worklist.push_back(child);
				mark_test(worklist);
			}
		}
	}

	// Mark the root references and look for child references to them
	void mark_from_roots(uintptr_t *start, const uintptr_t *end)
	{
		vector<Chunk *> worklist;
		for (; start > end; start--)
		{
			if (*start % 8 == 0)
			{ // all pointers must be aligned as double words
				Chunk *ref = (Chunk *)*start;
				if (ref != nullptr && !ref->m_marked)
				{
					ref->m_marked = true;
					worklist.push_back(ref);
					mark_test(worklist);
				}
			}
		}
	}

	// For testing purposes
	void Heap::print_line(Chunk *chunk)
	{
		cout << "Marked: " << chunk->m_marked << "\nStart adr: " << chunk->m_start << "\nSize: " << chunk->m_size << " B\n"
			 << endl;
	}

	void Heap::print_worklist(std::vector<Chunk *> &list)
	{
		for (auto cp : list)
			cout << "Chunk at:\t" << cp->m_start << "\nSize:\t\t" << cp->m_size << "\n";
		cout << endl;
	}

	void Heap::print_contents()
	{
		Heap &heap = Heap::the();
		if (heap.m_allocated_chunks.size())
		{
			cout << "\nALLOCATED CHUNKS #" << dec << heap.m_allocated_chunks.size() << endl;
			for (auto chunk : heap.m_allocated_chunks)
				print_line(chunk);
		}
		else
		{
			cout << "NO ALLOCATIONS\n" << endl;
		}
		if (heap.m_freed_chunks.size())
		{
			cout << "\nFREED CHUNKS #" << dec << heap.m_freed_chunks.size() << endl;
			for (auto fchunk : heap.m_freed_chunks)
				print_line(fchunk);
		}
		else
		{
			cout << "NO FREED CHUNKS" << endl;
		}
	}

	void Heap::print_summary()
	{
		Heap &heap = Heap::the();
		if (heap.m_allocated_chunks.size())
		{
			cout << "\nALLOCATED CHUNKS #" << dec << heap.m_allocated_chunks.size() << endl;
		}
		else
		{
			cout << "NO ALLOCATIONS\n" << endl;
		}
		if (heap.m_freed_chunks.size())
		{
			cout << "\nFREED CHUNKS #" << dec << heap.m_freed_chunks.size() << endl;
		}
		else
		{
			cout << "NO FREED CHUNKS" << endl;
		}
	}

	void Heap::print_allocated_chunks(Heap *heap) {
		cout << "--- Allocated Chunks ---\n" << endl;
		for (auto chunk : heap->m_allocated_chunks) {
			print_line(chunk);
		}
	}

	Chunk *Heap::try_recycle_chunks_new(size_t size)
	{
		Heap &heap = Heap::the();
		// Check if there are any freed chunks large enough for current request
		for (size_t i = 0; i < heap.m_freed_chunks.size(); i++)
		{
			auto chunk = heap.m_freed_chunks[i]; //Heap::get_at(heap.m_freed_chunks, i);
			auto iter = heap.m_freed_chunks.begin();
			//advance(iter, i);
			i++;
			if (chunk->m_size > size)
			{
				// Split the chunk, use one part and add the remaining part to
				// the list of freed chunks
				size_t diff = chunk->m_size - size;
				auto chunk_complement = new Chunk(diff, chunk->m_start + chunk->m_size);

				heap.m_freed_chunks.erase(iter);
				heap.m_freed_chunks.push_back(chunk_complement);
				heap.m_allocated_chunks.push_back(chunk);

				return chunk;
			}
			else if (chunk->m_size == size)
			{
				// Reuse the whole chunk
				heap.m_freed_chunks.erase(iter);
				heap.m_allocated_chunks.push_back(chunk);
				return chunk;
			}
		}
		// If no chunk was found, return nullptr
		return nullptr;
	}

	void Heap::free_overlap_new(Heap &heap) // borde göra en record(ChunkFreed) på onödiga chunks
	{
		std::vector<Chunk *> filtered;
		size_t i = 0;
		auto prev = heap.m_freed_chunks[i++]; //Heap::get_at(heap.m_freed_chunks, i++);
		prev->m_marked = true;
		filtered.push_back(prev);
		cout << filtered.back()->m_start << endl;
		for (; i < heap.m_freed_chunks.size(); i++)
		{
			prev = filtered.back();
			auto next = heap.m_freed_chunks[i]; //Heap::get_at(heap.m_freed_chunks, i);
			auto p_start = (uintptr_t)(prev->m_start);
			auto p_size = (uintptr_t)(prev->m_size);
			auto n_start = (uintptr_t)(next->m_start);
			if (n_start >= (p_start + p_size))
			{
				next->m_marked = true;
				filtered.push_back(next);
			}
		}
		heap.m_freed_chunks.swap(filtered);
		
		bool profiler_enabled = heap.m_profiler_enable;
		// After swap m_freed_chunks contains still available chunks
		// and filtered contains all the chunks, so delete unused chunks
		for (Chunk *chunk : filtered)
		{
			// if chunk was filtered away, delete it
			if (!chunk->m_marked)
			{
				if (profiler_enabled)
					Profiler::record(ChunkFreed, chunk);
				delete chunk;
			}
			else
			{
				chunk->m_marked = false;
			}
		}
	}

#endif
}