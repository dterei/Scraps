// SFINAE = Substitution Failure Is Not An Error
//
// For some situations (as defined by C++ standard), when template substitution
// fails, rather than treat this as a failure, we simply remove this instance
// from the 'overloaded set'.
//
// 'overloaded set'---The set of all available instances that a instantiation
// could resolve to.

#include <iostream>

struct X
{
    void foo() const { std::cout << "foo\n"; }
};

struct Y
{
    void bar() const { std::cout << "bar\n"; }
};

struct Z
{
    void foo() const { std::cout << "foo\n"; }
    void bar() const { std::cout << "bar\n"; }
};

template<typename C> 
auto foobar(C& c) -> decltype(c.foo())
{
    return c.foo();
}

template<typename C> 
auto foobar(C& c) -> decltype(c.bar())
{
    return c.bar();
}

int main(void)
{
    X x;
    Y y;

    foobar(x);
    foobar(y);

    // Z z;
    // foobar(z); // fails as ambiguous since overloaded set can't be reduced.

    return 0;
}
