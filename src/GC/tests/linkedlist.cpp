#include <stdio.h>
#include <stdint.h>

#include "heap.hpp"

#define allocNode   static_cast<Node *>(GC::Heap::alloc(sizeof(Node)))

using std::cout, std::endl;

struct Node // sizeof(Node) = 16
{
    int value;
    Node *next {nullptr};
};

Node *create_list(size_t length)
{
    Node *head = allocNode;
    head->value = 0;

    Node *prev = head;
    Node *next;

    for (size_t i = 1; i < length; i++)
    {
        next = allocNode;
        next->value = i;
        prev->next = next;
        prev = next;
    }

    return head;
}

#define LIST_SIZE   1000

void list_test1()
{
    Node *list_1 = create_list(LIST_SIZE);
}

int main()
{
    GC::Heap::init();
    GC::Heap &heap = GC::Heap::the();
    heap.set_profiler(true);
    // GC::Profiler::set_log_options(GC::FunctionCalls);
    // GC::Profiler::set_log_options(GC::ChunkOps);
    GC::Profiler::set_log_options(GC::TimingInfo);

    // make_test();
    for (int i = 0; i < 1000; i++)
        list_test1();

    GC::Heap::dispose();

    return 0;
}