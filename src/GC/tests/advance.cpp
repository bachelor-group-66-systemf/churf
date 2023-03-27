#include <chrono>
#include <cstring>
#include <iostream>
#include <list>
#include <time.h>
#include <stdlib.h>

int main() {
    using namespace std;
    using TimeStamp = std::chrono::_V2::system_clock::time_point;

    list<char> l;
    char c = 'a';
    for (int i = 1; i <= 5; i++) {
        l.push_back(c++);
    }

    auto iter = l.begin();
    auto stop = l.end();

    while (iter != stop) {
        cout << *iter << " ";
     
        iter++;
    }
    cout << endl;
    iter = l.begin();
    while (*iter != *stop) {
        cout << *iter << " ";
        iter++;
    }
    cout << endl;

    cout << "rebased" << endl;
    cout << "iter: " << *iter << "\nstop: " << *stop << endl;

    TimeStamp ts = std::chrono::system_clock::now();
    std::time_t tt = std::chrono::system_clock::to_time_t(ts);
    std::string tstr = std::ctime(&tt);
    tstr.resize(tstr.size()-1);
    std::cout << tstr << std::endl;

    return 0;
}