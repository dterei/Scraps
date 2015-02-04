#include <iostream>
#include <typeinfo>

template< typename T >
struct is_void {
  static const bool value = false;
};

template<>
struct is_void< void > {
  static const bool value = true;
};

// template<>
// struct is_void< int > {
//   static const bool value = true;
// };

template< typename A >
struct TestVoid {
  using type_value = A;
  void operator()() {
    std::cout << "Is void? [" << typeid(type_value).name() << "] "
      << is_void<type_value>::value << std::endl;
  }
};

int main()
{
  TestVoid<bool>()();
  TestVoid<int>()();
  TestVoid<void>()();
  return 0;
}

