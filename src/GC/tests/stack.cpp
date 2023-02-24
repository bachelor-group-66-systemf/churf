#include <algorithm>
#include <cstring>
#include <iostream>
#include <vector>

/*
 *  Stack.cpp
 *  - Tests stack scanning and stack pointers
 *  
 *  Goal:   Find the values of the following variables
 *          and their position on the stack
 *          - unsigned long a
 *          - unsigned long b
 *          - unsigned long global_1
 *          - unsigned long global_2
 * 
 *  Result: Passed
*/




std::vector<uintptr_t *> iv;

void collect() {
    std::cout << "in collect" << std::endl;

    uintptr_t *stack_start = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));

    // denna orsakar segfault om man ger __b_f_a ett värde större än 2
    // uintptr_t *stack_end = reinterpret_cast<uintptr_t *>(__builtin_frame_address(100)); 

    std::cout << "SP1:\t" << stack_start << "\nSP2:\t" << (stack_start - 1*sizeof(int)) << std::endl;
    std::cout << "SP-:\t" << --stack_start << std::endl;

    const uintptr_t *stack_end = (stack_start + 30*sizeof(int));
    int vars_found = 0;

    while (stack_start < stack_end) {

        if (std::find(iv.begin(), iv.end(), stack_start) != iv.end()) {
            vars_found++;
            std::cout << "Found " << *(reinterpret_cast<unsigned long *>(stack_start)) << " at " << stack_start << std::endl;
        }

        // std::cout << "SP address:\t\t" << stack_start << "\nSP value:\t\t" << *(reinterpret_cast<unsigned long *>(stack_start)) << std::endl;
        
        stack_start++;
    }

    if (vars_found == 0) {
        std::cout << "Found nothing" << std::endl;
    }
}

int add(unsigned long a, unsigned long b) {
    iv.push_back(reinterpret_cast<uintptr_t *>(&a));
    iv.push_back(reinterpret_cast<uintptr_t *>(&b));
    std::cout << "'a':\t" << &a << "\n'b':\t" << &b << std::endl;
    collect();
    return a + b;
}

int main() {

    unsigned long global_1 = 16;
    unsigned long global_2 = 32;

    iv.push_back(&global_1);
    iv.push_back(&global_2);

    std::cout << "'g1':\t" << &global_1 << "\n'g2':\t" << &global_2 << std::endl;

    add(3,2);
    return 0;
}