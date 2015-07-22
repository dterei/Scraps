// Testing GNU libstdc++ provided allocators

#include <iostream>
#include <vector>

#include <ext/array_allocator.h>
#include <ext/malloc_allocator.h>
#include <ext/mt_allocator.h>
#include <ext/new_allocator.h>
#include <ext/pool_allocator.h>

using namespace std;

int main(void)
{
  // deprecated! need to pass it an array to allocate from, by default array is
  // nullptr and allocation will segfault.
  // vector<int, __gnu_cxx::array_allocator<int>> vint;

  // C++ standard allocators
  // vector<int, __gnu_cxx::malloc_allocator<int>> vint;
  // vector<int, __gnu_cxx::new_allocator<int>> vint;

  // High-performance slab-like allocators
  vector<int, __gnu_cxx::__mt_alloc<int>> vint;
  // vector<int, __gnu_cxx::__pool_alloc<int>> vint;

  vint.push_back(0);
  vint.push_back(1);
  vint.push_back(2);
  vint.push_back(3);
  vint.push_back(4);
  vint.push_back(5);

  for ( auto & v : vint ) {
    cout << "V: " << v << endl;
  }
}
