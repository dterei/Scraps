// is using a type or newtype? sadly only can be used as type
using myint = int;

myint f(myint x)
{
  return x+1;
}

int main(void)
{
  int x = 0;
  x = f(x);
  return x;
}
