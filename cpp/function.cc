#include <functional>
#include <iostream>

// base - undefined
template<typename T1, typename T2> struct C;

// specialize to T2 = int
template<typename S1> struct C<S1,int> {};

// specialize to T1 = function of type S1 -> S2, T2 = int
template<typename S1, typename S2> struct C<S1(S2), int> {};

// template class
template<typename> struct function; // undefined base

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
    // Near magic (not clear if programmer can themselves implement
    // std::function) as each lambda has a unique (compiler-generated) type. So
    // usually you use 'auto' type to deal with them.
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

// To efficiently call all the different callable types (e.g., lambdas,
// functors, free function pointers, member function pointers), we make use of
// templates. Other approach is using 'std::function', which can be fairly
// expensive (both in construction and indirection for calling).
template<typename F, typename ...Args>
void call2(F, Args... args)
{
  f(args...);
}

std::function<int (int)> getF1(int x)
{
  return [x](int y) { return x + y; };
}

auto getF2(int x)
{
  // C++14 allows auto return type---mostly useful to return lambdas.
  return [x](int y) { return x + y; };
}

int main(void)
{
  auto f = [] (auto x) { return x; };
  call1(f);

  // appears to resolve to most specialized
  function<int (int)> f1;
  f1();

  // function<int (int, int)> f2 = [] (int x, int y) { return x + y; };
  // f2(1,1);
  
  auto f2 = getF2(1);
  f2(4);
}
