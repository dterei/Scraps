/* Testing templates with non-type parameters */
#include <iostream>
#include <cstring>

using namespace std;

template <int x> void f( void );

// non-type template parameter
template <int x>
void f( void )
{
  cout << "X: " << x << endl;
}

// specialize a non-type template parameter
template <>
void f<2>( void )
{
  cout << "Z: 2" << endl;
}

// Compile-time fixed size stack
// - non-type template parameter
// - default value
template <size_t N = 100>
struct Stack {
  size_t count_ = N;
  char data_[N];
};

// Compile-time arithmetic
template <size_t N1, size_t N2>
Stack<N1+N2> append( Stack<N1> s1, Stack<N2> s2 )
{
  Stack<N1+N2> ss;
  memcpy( ss.data_, s1.data_, N1 );
  memcpy( ss.data_ + N1, s2.data_, N2 );
  return ss;
}

int main( void )
{
  f<1>();
  f<2>();
  f<3>();

  Stack<10> s1;
  Stack<7> s2;
  auto s3 = append( s1, s2 );

  cout << "S1: " << s1.count_ << endl;
  cout << "S2: " << s2.count_ << endl;
  cout << "S3: " << s3.count_ << endl;

  return 0;
}

