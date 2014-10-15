#include <iostream>

class C {
  public:
    C() { std::cout << "C Created" << std::endl; }
    ~C() { std::cout << "C Destroyed" << std::endl; }
};

int main(void)
{
  {
    C c;
    // delete c; -- will throw segfault
  }
  return 0;
}
