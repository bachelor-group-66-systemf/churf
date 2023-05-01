#include <chrono>
#include <cstring>
#include <iostream>
#include <list>
#include <time.h>
#include <stdlib.h>

// void time_test()
// {
//     using TimeStamp = std::chrono::_V2::system_clock::time_point;

//     std::list<char> l;
//     char c = 'a';
//     for (int i = 1; i <= 5; i++) {
//         l.push_back(c++);
//     }

//     auto iter = l.begin();
//     auto stop = l.end();

//     while (iter != stop) {
//         std::cout << *iter << " ";
     
//         iter++;
//     }
//     std::cout << std::endl;
//     iter = l.begin();
//     while (*iter != *stop) {
//         std::cout << *iter << " ";
//         iter++;
//     }
//     std::cout << std::endl;

//     std::cout << "rebased" << std::endl;
//     std::cout << "iter: " << *iter << "\nstop: " << *stop << std::endl;

//     TimeStamp ts = std::chrono::system_clock::now();
//     std::time_t tt = std::chrono::system_clock::to_time_t(ts);
//     std::string tstr = std::ctime(&tt);
//     tstr.resize(tstr.size()-1);
//     std::cout << tstr << std::endl;
// }

void iter_test()
{
    std::list<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(4);
    list.push_back(5);

    auto iter = list.begin();

    while (iter != list.end())
    {
        if (*iter == 4)
        {
            iter = list.erase(iter);
            std::cout << *iter << "\n";
            list.insert(iter, 3);
            // list.insert(iter, 3);
            // std::cout << "n: " << *(++iter) << "\n";
            // iter = list.erase(++iter);
        }
        iter++;
    }

    for (int i : list)
    {
        std::cout << i << " ";
    }
    std::cout << std::endl;
}



int main() {
    std::cout << "hello" << std::endl;
    
    iter_test();

    return 0;
}