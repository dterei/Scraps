#include <iostream>

class C
{
private:
  int i_;
public:
  /* construct */
  C(int i):i_{i}    { std::cout << "C(" << i_ << ")\n"; }

  /* copy */
  C(const C& c):i_{c.i_} { std::cout << "C(&)\n"; }

  /* move */
  C(C&& c):i_{c.i_} { std::cout << "C(&&)\n"; }

  /* destructor */
  ~C() { std::cout << "~C(" << i_ << ")\n"; }

  C& operator=(const C& c) {
    std::cout << "op=(&)\n";
    if (this == &c) { return *this; }
    i_ = c.i_;
    return *this;
  }

  C& operator=(C&& c) {
    std::cout << "op=(&&)\n";
    if (this == &c) { return *this; }
    i_ = c.i_;
    c.i_ = -c.i_;
    return *this;
  }

};

// invalid since actually return stack memory, and c is destructed on function
// return.
C &mkc1()
{
  C c {1};
  return c;
}

// valid -- but copies to return
C mkc2()
{
  C c {2};
  return c;
}

// still invalid as we make a copy of heap allocated 'c' and return a reference
// to that, copy lives on stack.
C &mkc3()
{
  C* c = new C(3);
  return *c;
}

C &&mkc4()
{
  return {4};
}

void test(void)
{
  // return lvalue reference
  std::cout << "mkc1 -- C&\n";
  C& cc1 = mkc1();

  // cc1 has already been destructed!

  // return by value
  std::cout << "mkc2 -- C\n";
  C cc2 = mkc2();

  // Construct by copy
  C cc3 = cc2;

  // return by reference (but heap allocated)
  std::cout << "mkc3 -- C*\n";
  C cc4 = mkc3();

  C cc5 {5};

  // assingment operator
  cc5 = cc2;

  // move operator
  td::cout << "mkc4 -- C*\n";
  C cc6 = mkc4();
  cc6 = std::move(cc5); // static_cast<C&&>(cc5);
  cc6 = {6};
}

int main(void)
{
  test();
  return 0;
}

