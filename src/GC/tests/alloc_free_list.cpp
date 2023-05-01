#include <iostream>
#include <list>

#include "heap.hpp"

using GC::Chunk;

void alloc_test();
void add_to_free_list(Chunk *chunk);
void merge_free_list(Chunk *chunk, bool do_merge);
void do_merge_list();
void print_free_list();

std::list<Chunk *> m_free_list;

int main()
{
    alloc_test();

    // std::list<int> test;

    // test.push_back(1);
    // test.push_back(2);
    // test.push_back(3);
    // test.push_back(4);
    // test.push_back(5);

    // auto iter = test.begin();

    // std::cout << "First? " << *(iter++) << "\n";
    // std::cout << "Second? " << *(iter--) << "\n";
    // std::cout << "First? " << *iter << std::endl;

    // auto i = test.begin();
    // while (i != test.end())
    // {
    //     std::cout << *i << " ";
    //     ++i;
    // }

    // if (i == test.end())
    //     std::cout << "great success!";

    // std::cout << std::endl;

    return 0;
}

void alloc_test()
{
    auto tmp = static_cast<uintptr_t *>(__builtin_frame_address(0));

    auto c1 = new Chunk((size_t)(8), tmp);
    auto c2 = new Chunk((size_t)(4), c1->m_start + (size_t)(8));
    auto c3 = new Chunk((size_t)(16), c2->m_start + (size_t)(4));
    auto c4 = new Chunk((size_t)(4), c3->m_start + (size_t)(16));
    auto c5 = new Chunk((size_t)(32), c4->m_start + (size_t)(4));

    // std::cout << "test: " << (uintptr_t *)(tmp + (size_t)(2)) << std::endl;

    std::cout << "tmp: " << tmp << "\ntmp: " << (tmp + (size_t)(28)) << std::endl;

    // add_to_free_list(c1);
    // add_to_free_list(c2);
    // add_to_free_list(c3);
    // add_to_free_list(c4);
    // add_to_free_list(c5);

    merge_free_list(c1, false);
    merge_free_list(c2, false);
    merge_free_list(c3, false);
    merge_free_list(c4, false);
    merge_free_list(c5, false);

    std::cout << "----- BEFORE MERGE ----------------------";
    // print_free_list();

    do_merge_list();

    std::cout << "----- AFTER MERGE -----------------------";
    // print_free_list();
}

void add_to_free_list(Chunk *chunk)
{
    Chunk *curr;
    auto iter = m_free_list.begin();
    uintptr_t *prev_start = nullptr;
    uintptr_t *prev_end = nullptr;

    if (m_free_list.size() == 0)
    {
        m_free_list.push_back(chunk);
        return;
    }        

    while (iter != m_free_list.end())
    {
        curr = *iter;

        // If the curr chunk is aligned before param
        if (curr->m_start + curr->m_size == chunk->m_start)
        {
            Chunk *merged = new Chunk(
                curr->m_size + chunk->m_size,
                curr->m_start);
            iter = m_free_list.erase(iter);
            m_free_list.insert(iter, merged);
            return;
        }

        // If the curr chunk is aligned after param
        if (chunk->m_start + chunk->m_size == curr->m_start)
        {
            Chunk *merged = new Chunk(
                curr->m_size + chunk->m_size,
                chunk->m_start);
            iter = m_free_list.erase(iter);
            m_free_list.insert(iter, merged);
            return;
        }

        // If the first chunk starts after param
        if (prev_start == nullptr && curr->m_start > chunk->m_start)
        {
            m_free_list.insert(iter, chunk);
            return;
        }

        if (prev_end < chunk->m_start && (chunk->m_start + chunk->m_size) < curr->m_start)
        {
            m_free_list.insert(iter, chunk);
            return;
        }

        prev_start = curr->m_start;
        prev_end = prev_start + curr->m_size;
        iter++;
    }

    // This is only reachable if the chunk is at the end
    m_free_list.push_back(chunk);
}

void merge_free_list(Chunk *chunk, bool do_merge)
{
    auto i = m_free_list.begin();
    uintptr_t *prev_start = nullptr, *prev_end;
    bool chunk_inserted = false;

    while (i != m_free_list.end())
    {

        // if chunk is left-aligned
        if ((*i)->m_start + (*i)->m_size == chunk->m_start)
        {
            m_free_list.insert(++i, chunk);
            chunk_inserted = true;
            break;
        }

        // if chunk is right-aligned
        if (chunk->m_start + chunk->m_size == (*i)->m_start)
        {
            m_free_list.insert(i, chunk);
            chunk_inserted = true;
            break;
        }

        // is new first
        if (prev_start == nullptr && (*i)->m_start > chunk->m_start)
        {
            m_free_list.insert(i, chunk);
            chunk_inserted = true;
            break;
        }

        // if between chunks
        if (prev_end < chunk->m_start && (chunk->m_start + chunk->m_size) < (*i)->m_start)
        {
            m_free_list.insert(i, chunk);
            chunk_inserted = true;
            break;
        }

        prev_start = (*i)->m_start;
        prev_end = (*i)->m_start + (*i)->m_size;
        i++;
    }

    // is new last
    if (!chunk_inserted && i == m_free_list.end())
        m_free_list.push_back(chunk);

    if (do_merge)
        do_merge_list();
}

void do_merge_list()
{
    std::cout << "DO MERGE" << std::endl;
    auto i = m_free_list.begin();
    Chunk *prev = *(i++), *curr;
    print_free_list();
    
    while (i != m_free_list.end())
    {
        curr = *i;

        if ((prev->m_start + prev->m_size) == curr->m_start)
        {
            Chunk *merged = new Chunk(
                prev->m_size + curr->m_size,
                prev->m_start
            );

            // replace current and previous with merged
            i = m_free_list.erase(i);
            i = m_free_list.erase(--i);
            m_free_list.insert(i, merged);

            prev = merged;
        }
        else
        {
            prev = curr;
            i++;
        }
        print_free_list();
    }
    print_free_list();
}

void print_free_list()
{
    std::cout << "free-list count: " << m_free_list.size() << "\n";

    auto iter = m_free_list.begin();
    size_t cnt = 1;

    while (iter != m_free_list.end())
    {
        std::cout << "C" << cnt << ":\n\tstart: " << (*iter)->m_start
            << "\n\tsize: " << (*iter)->m_size << "\n";
        iter++;
        cnt++;
    }

    std::cout << std::endl;
}