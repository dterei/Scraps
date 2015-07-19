/* Play with new
 * 
 * here we are using the 'placement new' syntax.
 * 
 * `new ( expression-list ) new-type-id ( optional-initializer-expression-list )`
 * 
 * There is no equivalent placement delete syntax.
 *
 * However, the underlying new and delete operators both have overloaded
 * versions for supporting placement functions.
 *
 * The C++ standard provides:
 * - new ( size_t )
 * - new ( size_t, nothrow_t )
 * - new ( size_t, void * )
 * - new[] ( size_t )
 * - new[] ( size_t, nothrow_t )
 * - new[] ( size_t, void * )
 *
 * And equivalent ones for delete.
 *
 * https://en.wikipedia.org/wiki/Placement_syntax
 * http://www.scs.stanford.edu/~dm/home/papers/c++-new.html
 */
#include <new> // needed for placement new
#include <cstdlib>
#include <iostream>

using namespace std;

void * operator new( size_t s, const nothrow_t & nt ) noexcept
{
  cout << "My allocator: " << s << endl;
  return malloc( s );
}

// If not defined, new (nothrow) T[] will call the standard new overload
// defined above.
void * operator new[]( size_t s, const nothrow_t & nt ) noexcept
{
  cout << "My allocator[]: " << s << endl;
  return malloc( s );
}

class A
{
private:
  int a_;

public:
  A( void )
  {
    cout << "A Ctor" << endl;
    a_ = 1;
  }

  void * operator new( size_t s )
  {
    cout << "A Allocator: " << s << endl;
    return malloc( s );
  }
};

class B
{
private:
  int b_;
public:
  B( void )
  {
    cout << "B Ctor" << endl;
    b_ = 1;
  }
};

int main( void )
{
  // call standard `new`
  char * c = new char( 'c' );
  cout << "C: " << c << endl;

  // call our overloaded `new`
  char * d = new (nothrow) char( 'd' );
  cout << "D: " << d << endl;

  // call our overloaded `new`
  char * e = new (nothrow) char[10];

  // call standard `new` + A.ctor
  A * a = new A;

  // call new operator directly
  void * raw1 = operator new( 10 );

  // call our overloaded new operator directly
  void * raw2 = operator new( 10, nothrow );

  // seperate `new` into allocation & ctor
  void * b_mem = operator new( sizeof( B ) );
  // - allows us to call B Ctor without an allocation.
  B * b = new (b_mem) B()

  return 0;
}
