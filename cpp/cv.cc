// Bug in clang/gcc or perhaps libc/nptl around static linking and condition
// variables.
//
// Fails: clang++ -std=c++11 -static -o cv cv.cc -lpthread
//
// Workaround:
// clang++ -std=c++11 -static -o cv cv.cc -Wl,--whole-archive -lpthread \
//   -Wl,--no-whole-archive 
//
#include <condition_variable>
#include <iostream>

using namespace std;

int main( void )
{
  { condition_variable cv; }
  cout << "No fault!" << endl;
  return 0;
}
