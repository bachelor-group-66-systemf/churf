#include <cstring>
#include <iostream>

void dummy1();
void dummy2();

int main() {
    
    uintptr_t *prev1 = reinterpret_cast<uintptr_t *>(__builtin_frame_address(0));
    uintptr_t *prev2 = static_cast<uintptr_t *>(__builtin_frame_address(0));

    std::cout << "reinterpret:\t" << prev1 << "\nstatic:\t\t" << prev2 << std::endl;

    std::cout << "Start:\t\t" << prev1 << std::endl;
#pragma clang diagnostic ignored "-Wframe-address"
    uintptr_t *tmp = reinterpret_cast<uintptr_t *>(__builtin_frame_address(1));
    std::cout << "Frame 1:\t" << tmp << "\t\tDiff:\t" << std::hex << "0x"<< tmp - prev1 << std::endl;
    prev1 = tmp;

#pragma clang diagnostic ignored "-Wframe-address"
    tmp = reinterpret_cast<uintptr_t *>(__builtin_frame_address(2));
    std::cout << "Frame 2:\t" << tmp << "\tDiff:\t" << std::hex << "0x" << tmp - prev1 << std::endl;
    prev1 = tmp;

// arg > 2 for __builtin_frame_address() results in segfault
// #pragma clang diagnostic ignored "-Wframe-address"
//     tmp = reinterpret_cast<uintptr_t *>(__builtin_frame_address(3));
//     std::cout << "Frame 3:\t" << tmp << "\tDiff:\t" << std::hex << "0x" << prev1 - tmp << std::endl;
    
    dummy1();

    return 0;
}

void dummy1() {
    std::cout << "D1 SFrame:\t" << __builtin_frame_address(0);
#pragma clang diagnostic ignored "-Wframe-address"
    std::cout << "\t\tPrev:\t" << __builtin_frame_address(1) << std::endl;
    std::cout << "D1 RA:\t\t" << std::hex << __builtin_return_address(0) << std::endl;
    dummy2();
}

void dummy2() {
    std::cout << "Frame:\t\t" << __builtin_frame_address(0);
#pragma clang diagnostic ignored "-Wframe-address"
    std::cout << "\t\tPrev:\t" << __builtin_frame_address(1) << std::endl;
    void *ra = __builtin_return_address(0);
    std::cout << "D2 RA:\t\t" << std::hex << ra << std::endl;
    // gives same value as pure 'ra'
    // std::cout << "D2 ERA:\t\t" << std::hex << __builtin_extract_return_addr(ra) << std::endl;
}