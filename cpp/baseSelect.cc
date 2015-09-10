#include <iostream>

using namespace std;

class A
{
public:
  virtual void f( int x, int y ) = 0;

  void f( int x = 10 )
  {
    cout << "A.f': " << x << endl;
  }
};

class B : public A
{
public:
  virtual void f( int x, int y ) override
  {
    cout << "B.f: " << x << endl;
  }
};

int main( void )
{
  B b;
  // Need to qualify A::f().
  b.A::f();
  return 0;
}
