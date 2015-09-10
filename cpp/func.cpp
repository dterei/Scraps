#include <iostream>

using namespace std;

class C
{
private:
  function<void(void)> f_ = []() {};

public:
  C() {};

  void set_cb( function<void(void)> && f )
  {
    f_ = move( f );
  }

  void operator()( void ) {
    f_();
  }
};

int main( void )
{
  cout << "hello world" << endl;

  auto f = []() {
    cout << "lambda" << endl;
  };

  C c;
  c.set_cb( f );
  c();

  return 0;
}
