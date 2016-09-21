#include <functional>
#include <iostream>

// template class
template<typename> class function; // undefined base

// instance
template<typename R>
struct function<R(int)>
{
  void operator()(void) {
    std::cout << "int\n";
  };
};

// // instance - not allowed as ambiguous instance
// template<typename Arg1>
// struct function<int(Arg1)>
// {
//   void operator()(void) {
//     std::cout << "int\n";
//   };
// };

// instance
template<typename R, typename Arg1>
struct function<R(Arg1)>
{
  void operator()(void) {
    std::cout << "Arg1\n";
  };
};

// instance
template<typename R, typename ...Args>
struct function<R(Args...)>
{
  using Fun = R (*)(Args...);
  Fun f;

  function<R(Args...)> & operator=(Fun f_)
  {
    // TODO: how to capture lambda?
    f = f_;
    return this;
  }

  R operator()(Args... args) {
    std::cout << "Args...\n";
    return f(args...);
  };
};

void call1(std::function<int (int)> f)
{
  f(1);
}

int main(void)
{
  auto f = [] (auto x) { return x; };
  call1(f);

  // appears to resolve to most specialized
  function<int (int)> f1;
  f1();

  function<int (int, int)> f2 = [] (int x, int y) { return x + y; };
  f2(1,1);
}
