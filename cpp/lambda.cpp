#include <iostream>
#include <functional>

std::function<int (int)> genF(void)
{
  int y = 10;
  // copy capture
  auto f = [y](int a){ return y + a; };
  // reference capture -- error, as y is on stack
  // auto f = [&y](int a){ return y + a; };
  return f;
}

int main(void)
{
  int x = 10;
  auto f1 = [&](int a){ return a < x; };
  
  bool y {f1(11)};
  std::cout << "f1(11) = " << y << std::endl;

  auto f2 = genF();
  int z = {f2(11)};
  std::cout << "f2(11) = " << z << std::endl;

  return 0;
}

