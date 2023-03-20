#include <ctime>
#include <fstream>
#include <iostream>
#include <string>
#include <time.h>
#include <unistd.h>

void time_string(char *buffer);
void print_log_file(const std::string TESTS_PATH);
void readlink_test();

int main()
{
    // char time_buffer[31];
    // time_string(time_buffer);

    // const std::string TESTS_PATH = "/home/virre/dev/systemF/org/language/src/GC/tests/";
    // print_log_file(TESTS_PATH);
    
    readlink_test();

    return 0;
}

void time_string(char *const buffer)
{
    std::time_t tt = std::time(NULL);
    std::tm *ptm = std::localtime(&tt);
    std::strftime(buffer, 31, "/logs/log_%a_%H_%M_%S.txt", ptm);
    std::cout << buffer << std::endl;
}

void print_log_file(const std::string TESTS_PATH)
{
    std::string path = TESTS_PATH + "/testlog.txt";

    std::ofstream testF(path);
    
    testF << "hellow york";

    testF.close();
}

void readlink_test()
{
    char buffer[1024];
    ssize_t len = readlink("/proc/self/exe", buffer, sizeof(buffer)-1);
    if (len == -1)
    {
        std::cout << "readlink error" << std::endl;
        return;
    }

    buffer[len] = '\0';
    std::cout << "readlink:\n" << "'''" << buffer << "'''"; // << std::endl;

    auto path = std::string(buffer);
    std::cout << path << "\nlen: " << path.size() << "\ncap:" << path.capacity();

    size_t last_slash = path.find_last_of('/');
    std::string folder = path.substr(0, last_slash);

    std::cout << "\n" << folder;

    std::string log_path = folder + "/log_file_bla.txt";
    std::cout << "\n" << log_path << std::endl;

}