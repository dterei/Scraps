#include<iostream>
#include<iterator>
#include<string>

std::ostream_iterator<std::string> oo {std::cout};

int main(void)
{
  *oo = "Hello";
  ++oo; // seems to be a noop
  *oo = "World\n";
}

