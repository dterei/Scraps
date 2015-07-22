// Testing out C++ Allocator concept

#include <iostream>
#include <memory>
#include <vector>

using namespace std;

// Minimum allocator type
template <class T>
struct SimpleAlloc
{
  using value_type = T;
  
  SimpleAlloc( void ) {}

  SimpleAlloc( const SimpleAlloc & other ) {}

  T * allocate( size_t n )
  {
    T * t = static_cast<T *>( malloc( n * sizeof( T ) ) );
    cout << "allocate: " << t << ", " << n << endl;
    return t;
  }

  void deallocate( T *  t, size_t n )
  {
    cout << "deallocate: " << t << ", " << n << endl;
    free( t );
  }
};

template <class T>
bool operator==(const SimpleAlloc<T> & a, const SimpleAlloc<T> b)
{
  return true;
}

template <class T>
bool operator!=(const SimpleAlloc<T> & a, const SimpleAlloc<T> b)
{
  return not (a == b);
}

using VInt = vector<int, SimpleAlloc<int>>;

int main( void )
{
  // custom allocator for a vector
  VInt vint;

  vint.push_back(1);
  vint.push_back(2);
  vint.push_back(3);
  vint.push_back(4);
  vint.push_back(5);
  vint.push_back(6);
  vint.push_back(7);

  for ( auto & v : vint ) {
    cout << "V: " << v << endl;
  }

  // using it directly
  SimpleAlloc<int> xalloc;
  int * xints = xalloc.allocate( 3 );
  xints[2] = 2;
  xints[1] = 1;
  xints[0] = 0;

  // using allocator_traits to fill in optional methods
  SimpleAlloc<string> xsalloc;
  allocator_traits<decltype( xsalloc )> xxalloc;

  string * ss = xxalloc.allocate( xsalloc, 1 );
  xxalloc.construct( xsalloc, ss, "Yes?" );
  cout << ss[0] << endl;


  // standard C++ allocator -- used by STL containers
  allocator<int> std_alloc;

  int * ints = std_alloc.allocate( 10 );

  ints[9] = 9;
  std_alloc.construct( ints + 8, 8 );

  cout << ints[9] << endl;
  cout << ints[8] << endl;

  allocator<string> salloc;
  string * strings = salloc.allocate( 2 );
  salloc.construct( strings, "Hello World" );
  cout << strings[0] << endl;
  
  return 0;
}

