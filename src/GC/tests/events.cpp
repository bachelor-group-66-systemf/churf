#include <iostream>
#include <stdio.h>

using namespace std;
// broken :(
// [event_source(native)]
class ESource {
public:
    __event void TestEvent(int eValue);
};

// [event_receiver(native)]
class EReceiver {
public:
    void Handler1(int eValue) {
        cout << "Handler1 with: " << eValue << endl;
    }

    void Handler2(int eValue) {
        cout << "Handler2 with: " << eValue << endl;
    }

    void hookEvent(ESource *eSource) {
        __hook(&ESource::TestEvent, eSource, &EReceiver::Handler1);
        __hook(&ESource::TestEvent, eSource, &EReceiver::Handler2);    
    }

    void unhookEvent(ESource *eSource) {
        __unhook(&ESource::TestEvent, eSource, &EReceiver::Handler1);
        __unhook(&ESource::TestEvent, eSource, &EReceiver::Handler2);
    }
};

int main() {

    ESource src;
    EReceiver rcv;

    rcv.hookEvent(&src);
    __raise src.TestEvent(12);
    rcv.unhookEvent(&src);

    return 0;
}