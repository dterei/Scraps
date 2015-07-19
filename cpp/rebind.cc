// Exploring the curious rebind functionality of std::allocator

#include <iostream>

using namespace std;

template <typename T>
struct MyT
{
  using pointer = T*;

  T val_1;
  T val_2;

  template <typename U>
  struct rebind
  {
    using other = MyT<U>;
  };

  void print( void )
  {
    cout << "Val: " << val_1 << endl;
  }

  pointer ptr( void ) { return &val_1; }
};

using MInt = MyT<uint16_t>;

using MChar = MyT<char>;

using MBind1 = MInt::rebind<char>::other;
using MBind2 = MChar::rebind<uint16_t>::other;

int main( void )
{
  MInt t1;
  MChar t2;

  t1.print();  
  t2.print();
  
  // MBind = MyT<Char> = MChar -- but got type through rebinding
  MBind1 t3 = t2;
  
  // Can use rebinded type as type for pointer from t2
  MBind1::pointer p = t2.ptr();

  // Convert t1 -> MChar type.
  MInt * t4 = (MBind2 *) ( &t2 );
  t4->print();

  return 0;
}
