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

    for (size_t i = 1; i < length; i++)
    {
        Node *next = allocNode;
        next->value = i;
        prev->next = next;
        prev = next;
    }

    return head;
}

void print_list(Node* head)
{
    cout << "\nPrinting list...\n";
    while (head != nullptr)
    {
        cout << head->value << " ";
        head = head->next;
    }
    cout << endl;
}

void clear_list(Node *head)
{
    while (head != nullptr)
    {
        Node *tmp = head->next;
        head->next = nullptr;
        head = tmp;
    }
}

void run_list_test1()
{
    Node *list_a = create_list(10);
    print_list(list_a);
}

void run_list_test2()
{
    Node *list_b = create_list(10);
    print_list(list_b);
}

void run_list_test3()
{
    Node *list_c = create_list(10);
    print_list(list_c);
}

int main()
{
    GC::Heap::init();
    GC::Heap &heap = GC::Heap::the();
    heap.set_profiler(true);
    GC::Profiler::set_log_options(GC::FunctionCalls);

    run_list_test1();
    run_list_test2();
    run_list_test3();

    GC::Heap::dispose();

    return 0;
}