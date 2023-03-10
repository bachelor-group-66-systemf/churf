#include <ctime>
#include <fstream>
#include <iostream>
#include <string>
#include <time.h>

int main()
{
    char buffer[31];
    std::time_t tt = std::time(NULL);
    std::tm *ptm = std::localtime(&tt);
    std::strftime(buffer, 31, "/logs/log_%a_%H_%M_%S.txt", ptm);
    std::cout << buffer << std::endl;

    const std::string TESTS_PATH = "/home/virre/dev/systemF/org/language/src/GC/tests/";

    std::string path = TESTS_PATH + "/testlog.txt";

    std::ofstream testF(path);
    
    testF << "hellow yorld";

    testF.close();

    return 0;
}