#include <iostream>

int add1(int x, int y) {
  return x + y;
}

// NOT LEGAL -- can only use 'auto' with trailing return type.
// auto add2(int x, int y) {
//   return x + y;
// }

// NOT LEGAL -- requires an 'auto' specifier as a place holder for return type.
// add3(int x, int y) -> int {
//   return x + y;
// }

// LEGAL -- auto is just to satisfy the parser really...
auto add4(int x, int y) -> int {
  return x + y;
}

auto main(void) -> int {
  std::cout << "add1: " << add1(1,4) << std::endl;
  // std::cout << "add2: " << add2(1,4);
  // std::cout << "add3: " << add3(1,4);
  std::cout << "add4: " << add4(1,4) << std::endl;
  return 0;
}

