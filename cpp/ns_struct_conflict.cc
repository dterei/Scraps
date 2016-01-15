// Can't have conflicting symbols of namepsace and struct!

namespace S
{
  inline void f(void)
  {
    return;
  }
}

struct S
{
  int x_;
};
