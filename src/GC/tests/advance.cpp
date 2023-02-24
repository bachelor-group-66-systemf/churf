#include <iostream>
#include <list>
#include <stdlib.h>

using namespace std;

int main() {
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

    // cout << "iter: " << *iter << "\nstop: " << *stop << endl;

    return 0;
}