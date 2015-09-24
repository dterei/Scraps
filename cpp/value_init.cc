#include <iostream>

using namespace std;

constexpr size_t ARRAY_SIZE = 10;

class C
{
public:
  char c_[ARRAY_SIZE];
  C( void ) : c_{} {}; // value-init will zero out the array.
  C ( bool f ) {}
};

int main( void )
{
  C c1;
  C c2( false );

  cout << "C1: ";
  for ( size_t i = 0; i < ARRAY_SIZE; i++ ) {
    cout << (size_t) c1.c_[i] << " ";
  }
  cout << endl << "C2: ";
  for ( size_t i = 0; i < ARRAY_SIZE; i++ ) {
    cout << (size_t) c2.c_[i] << " " << endl;
  }
  cout << endl;
  return 0;
}
