#include <iostream>
#include <vector>

// seems templates are duck-typed -- we require the += operator for a template
// but that is only determined by it's use, not by its type.

class Z {
    int z;
  public:
    Z(int zz): z{zz} {}
    Z() :z{0} {}

    Z operator+=(Z zz) {
      return {z += zz.z};
    }

    int unpack() { return z; }
};

template <typename Container, typename Value>
Value sum(const Container& c, Value v)
{
  for(auto x:c) { v+= x; }
  return v;
}

int main(void)
{
  std::vector<int> v {1,2,3,4};
  int x {sum(v, 0)};
  char b = sum(v, 'c');
  std::cout << "Sum of v (x) = " << x << std::endl;
  std::cout << "Sum of v (b) = " << b << std::endl;

  Z z = sum(v, Z {1});
  std::cout << "Sum of v (z) = " << z.unpack() << std::endl;

  return 0;
}
