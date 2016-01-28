#include <iostream>

/**
 * Need to use 'auto' return type to return a lambda since we can't type it's
 * type. This is only possible in C++14 or later.
 */

auto func(void)
{
  auto lmb = [](int x){ return x+1; };
  return lmb;
}

int main(void)
{
  auto lmb = func();

  std::cout << "Lambda: " << lmb(1) << std::endl;

  return 0;
}
