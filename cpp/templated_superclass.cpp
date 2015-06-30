#include <iostream>

using namespace std;

class A
{
private:
  /* allow our output function private access to A. */
  friend ostream & operator<<( ostream & , const A & );
  int a_;
public:
  explicit A( void ) : a_{0} {}
  explicit A( int a ) : a_{a} {}
};

ostream & operator<<( ostream & out, const A & a )
{
  out << "A{ " << a.a_ << " }";
  return out;
}

class B
{
private:
  friend ostream & operator<<( ostream & , const B & );
  char b_;
public:
  explicit B( void ) : b_{'0'} {}
  explicit B( char b ) : b_{b} {}

  void f(void) {};
};

ostream & operator<<( ostream & out, const B & b )
{
  out << "B{ " << b.b_ << " }";
  return out;
}

template <typename Super>
class X : public Super
{
public:
  using Super::Super;
};

int main(void)
{
  // standard A class
  A a {1};
  cout << "A: " << a << endl;

  // X using A as superclass
  X<A> xa {2};
  cout << "X: " << xa << endl;

  // X using B as superclass
  X<B> xb {'b'};
  cout << "X: " << xb << endl;
  xb.f();

  return 0;
}
