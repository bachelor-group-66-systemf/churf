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

#define LIST_SIZE   10

void list_test1()
{
    Node *list_1 = create_list(LIST_SIZE);
    // print_list(list_1);
}

void list_test2()
{
    Node *list_2 = create_list(LIST_SIZE);
    // print_list(list_2);
}

void list_test3()
{
    Node *list_3 = create_list(LIST_SIZE);
    // print_list(list_3);
}

void list_test4()
{
    Node *list_4 = create_list(LIST_SIZE);
    // print_list(list_4);
}

void list_test5()
{
    Node *list_5 = create_list(LIST_SIZE);
    // print_list(list_5);
}

void list_test6()
{
    Node *list_6 = create_list(LIST_SIZE);
    // print_list(list_6);
}

void make_test() {
    list_test1();
    list_test2();
    list_test3();
    list_test4();
    list_test5();
    list_test6();
}

int main()
{
    GC::Heap::init();
    GC::Heap &heap = GC::Heap::the();
    heap.set_profiler(true);
    GC::Profiler::set_log_options(GC::FunctionCalls);
    // GC::Profiler::set_log_options(GC::AllOps);

    make_test();

    GC::Heap::dispose();

    return 0;
}