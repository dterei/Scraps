#include <iostream>

void call1(std::function<int (int)> f)
{
  f(1);
}

void call2(int (*f)(int))
{
  f(1);
}

using F = char (*)(char);
void call3(F f)
{
  f(1);
}

template<typename T> T id(T t) { return t; }

int main(void)
{
  // templated / polymorphic function
  call1(id<int>);
  call1(id<char>);

  // by using 'auto' we can generate templated lambdas
  auto f = [] (auto x) { return x; };
  call1(f);
  call3(f);
}
