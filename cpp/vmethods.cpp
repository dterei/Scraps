#include <iostream>

class Foo
{
public:
    void f()
    {
        std::cout << "Foo::f()" << std::endl;
    }
    virtual void g()
    {
        std::cout << "Foo::g()" << std::endl;
    }
}

class Bar : public Foo
{
public:
    void f()
    {
        std::cout << "Bar::f()" << std::endl;
    }
    virtual void g()
    {
        std::cout << "Bar::g()" << std::endl;
    }
}

int main()
{
    Foo foo;
    Bar bar;

    Foo *baz = &bar;
    Bar *quux = &bar;

    foo.f(); // "Foo::f()"
    foo.g(); // "Foo::g()"

    bar.f(); // "Bar::f()"
    bar.g(); // "Bar::g()"

    // So far everything we would expect...

    baz->f();  // "Foo::f()"
    baz->g();  // "Bar::g()"

    quux->f(); // "Bar::f()"
    quux->g(); // "Bar::g()"

    return 0;
}
	
