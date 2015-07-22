// Test out boost memory pools
//
// Note that Boost will add in some alignment (seems to want to align to 8 byte
// blocks), so our 90 byte records will actually take up 96 bytes.

#include <boost/pool/pool_alloc.hpp>

using namespace std;

using rec_value = char[90];

struct Rec {
  char data[90];
} __attribute__((packed));

struct MyTag {};

int main( void )
{
  auto ba1 = boost::fast_pool_allocator<int>();

  int * i1 = ba1.allocate();
  int * i2 = ba1.allocate();
  int * i3 = ba1.allocate();

  auto ba2 = boost::fast_pool_allocator
    < int
    , boost::default_user_allocator_new_delete
    , boost::details::pool::default_mutex
    // , boost::details::pool::null_mutex
    , 32
    , 0
    >
    ();

  int * i4 = ba2.allocate();

  cout << "- ints -" << endl;
  cout << i1 << endl;
  cout << i2 << endl;
  cout << i3 << endl;
  cout << i4 << endl;
  cout << endl;

  auto ba3 = boost::fast_pool_allocator<rec_value>();

  rec_value * r1 = ba3.allocate();
  rec_value * r2 = ba3.allocate();
  rec_value * r3 = ba3.allocate();
  rec_value * r4 = ba3.allocate();

  cout << "- recs -" << endl;
  cout << r1 << endl;
  cout << r2 << endl;
  cout << r3 << endl;
  cout << r4 << endl;
  cout << endl;

  ba3.deallocate( r1 );
  
  rec_value * r5 = ba3.allocate();

  cout << r1 << endl;
  cout << r2 << endl;
  cout << r3 << endl;
  cout << r4 << endl;
  cout << r5 << endl;
  cout << endl;

  cout << sizeof( char[90] ) << endl;
  cout << alignof( char[90] ) << endl;

  auto ba4 = boost::fast_pool_allocator
    < Rec
    // , boost::default_user_allocator_new_delete
    , boost::default_user_allocator_malloc_free
    , boost::details::pool::default_mutex
    // , boost::details::pool::null_mutex
    , 32
    , 0
    >
    ();

  cout << "- Rec -" << endl;

  Rec * x1 = ba4.allocate(1);
  Rec * x2 = ba4.allocate(1);
  Rec * x3 = ba4.allocate(2);
  Rec * x4 = ba4.allocate(1);

  cout << x1 << endl;
  cout << x2 << endl;
  cout << &x3[0] << endl;
  cout << &x3[1] << endl;
  cout << x4 << endl;

  cout << sizeof( Rec ) << endl;
  cout << alignof( Rec ) << endl;

  return 0;
}

