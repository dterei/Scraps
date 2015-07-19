#include <iostream>

using namespace std;

class MyClass
{
private:
  // if wasn't mutable, const method can't access
  mutable int counter;
  int myid;
public:
  MyClass() : counter(0), myid(0) {}

  void Foo()
  {
    counter++;
    cout << "Foo" << endl;    
  }

  void Foo() const
  {
    counter++;
    // Goo();
    cout << "Foo const" << endl;
  }

  void Goo()
  {
    myid = 12;
  }

  int GetInvocations() const
  {
    return counter;
  }
};

int main(void)
{
  MyClass* cc = new MyClass();
  const MyClass* ccc = cc;
  cc->Foo();
  ccc->Foo();
  cout
    << "The MyClass instance has been invoked" << ccc->GetInvocations()
    << " times" << endl; 
  delete cc;
  ccc = NULL;
  return 0;
}

