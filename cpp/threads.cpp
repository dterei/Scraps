// It is an error to not call either `detach` or `join` on all threads.
// detach -- thread will run independently of thread that created it and the
//           lexical scope of its `thread` objects lifetime.
// join -- caller will wait for thread to terminate before returning.

#include <iostream>
#include <chrono>
#include <thread>
 
void independentThread() 
{
    std::cout << "Starting concurrent thread.\n";
    std::this_thread::sleep_for(std::chrono::seconds(2));
    std::cout << "Exiting concurrent thread.\n";
}
 
void threadCaller() 
{
    std::cout << "Starting thread caller.\n";
    std::thread t(independentThread);
    t.detach();
    std::this_thread::sleep_for(std::chrono::seconds(1));
    std::cout << "Exiting thread caller.\n";
}
 
int main() 
{
    threadCaller();
    std::this_thread::sleep_for(std::chrono::seconds(3));
    std::cout << "Exiting main.\n";
}

