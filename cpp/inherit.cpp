#include <iostream>

class Base {
  public:
    virtual void f(double x) { std::cout << "Base::f(" << x << ")" << std::endl; }
    virtual void g(int x) { std::cout << "Base::g(" << x << ")" << std::endl; }
};

void requireBase(Base b)
{
  return;
}

class Derived : public Base {
  public:
    // without 'using' we don't inherit Base::f methods as below is considered
    // overwriting all of them even though different types.
    using Base::f;
    void f(int x) { std::cout << "Derived::f(" << x << ")" << std::endl; }
    void h(int x) { std::cout << "Derived::h(" << x << ")" << std::endl; }

    void requireBaseD(Derived b)
    {
      // requires Derived 'protected' or 'public' inherits from Base.
      requireBase(b);
    }
};

int main(void)
{
  Derived d;
  d.f(1.2);
  d.g(1);
  d.h(1);

  // requires Derived 'protected' or 'public' inherits from Base.
  d.requireBaseD(d);

  // requires Derived 'public' inherits from Base
  requireBase(d);

  return 0;
}

