#include <iostream>

void call1(std::function<int (int)> f)
{
  f(1);
}

int main(void)
{
  int y = 1;
  call1([y] (auto x) mutable {
    // need mutable keyword above to allow modifying 'y' since we captured by
    // value/copy. I believe this is to ensure the developer hasn't accidently
    // captured by value a variable they wanted to capture by reference.
    y += 1;
    return y;
  });
  return 0;
}
