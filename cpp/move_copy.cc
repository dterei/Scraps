#include <iostream>

using namespace std;

class C
{
public:
  int i_;

  C(int i) : i_{i}         { cout << "C(" << i_ << ")\n"; }
  C(const C& c) : i_{c.i_} { cout << "C(&)\n"; }
  C(C&& c) : i_{c.i_}      { cout << "C(&&)\n"; }
  ~C()                     { cout << "~C(" << i_ << ")\n"; i_ = -99; }

  C& operator=(const C& c) {
    std::cout << "op=(&)\n";
    if (this != &c) { i_ = c.i_; }
    return *this;
  }

  C& operator=(C&& c) {
    std::cout << "op=(&&)\n";
    if (this != &c) { i_ = c.i_; c.i_ = -1; }
    return *this;
  }
};

static C global_c { 0 };

// return by value (copy)
C valueC() { return C( 1 ); }
// ^ although this is suggested way to return as C++ mandates that compilers
// perform copy elision of return values (return value optimization [RVO]).

// heap allocate and return ptr
C * ptrC() { return new C( 2 ); }

// invalid since actually return stack memory, and c is destructed on function
// return.
C & ref1C()
{
  C c {3};
  return c;
}

// valid! a reference is just a pointer that can't be null, so just returning
// that.
C & ref2C()
{
  C* c = new C(4);
  return *c;
}

// valid as life-time greater than ref3C().
C & ref3C() { return global_c; }

// return rvalue refernce -- invalid as dcon called on return
C && rvalC() { return {5}; }

void test(void)
{
  {
    cout << "\nTest return semantics:" << endl;
    C    cc1 = valueC();
    C *  cc2 = ptrC();
    C &  cc3 = ref1C();
    C &  cc4 = ref2C();
    delete &cc4;
    C &  cc5 = ref3C();
    C && cc6 = rvalC();
  }

  {
    cout << "\nTest copy semantics:" << endl;
    C cc1 { 1 };

    C cc2 { cc1 }; // copy-conc

    // uses copy constructor! copy-assign only used when overriding existing C.
    C cc3 = const_cast<const C &>( cc1 );

    // RVO used, so just runs regular constructor
    C cc4 { valueC() };

    C cc5 { move( cc1 ) }; // uses move-constructor
    cc2 = cc1; // uses copy-assign
    cc2 = move( cc3 ); // uses move-assign
  }

  cout << "\nTest global semantics:" << endl;
}

int main(void)
{
  test();
  return 0;
}

