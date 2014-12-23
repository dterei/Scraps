// Test of branch prediction on CPU's.
#include <algorithm>
#include <ctime>
#include <iostream>
#include <unistd.h>

const unsigned ARRAY_SIZE = 32768;
const unsigned LOOPS = 100000;

void randomFill(int data[], unsigned len)
{
  for (auto c = 0; c < len; ++c) {
    data[c] = std::rand() % 256;
  }
}

void sortArray(int data[], unsigned len)
{
  std::sort(data, data + len);
}

long long sumArray(int data[], unsigned len)
{
  long long sum = 0;

  for (auto i = 0; i < LOOPS; ++i) {
    for (auto c = 0; c < len; ++c) {
      // with uniformly random data, should branch-mispredict 50% of time
      if (data[c] >= 128) { sum += data[c]; }
    }
  }

  return sum;
}

int main()
{
    // Generate data
    int data[ARRAY_SIZE];
    randomFill(data, ARRAY_SIZE);
    auto p = getpid();
    std::cout << "pid = " << p << std::endl;

    // Sorting the array greatly improves performance as it ensures very low
    // branch misprediction rates. Efectivness depends on compiler
    // optimizations as compiler may be smart enough to use conditional moves
    // and avoid mispredict penatly.
    sortArray(data, ARRAY_SIZE);

    auto start = clock();
    auto sum = sumArray(data, ARRAY_SIZE);
    auto elapsedTime = static_cast<double>(clock() - start) / CLOCKS_PER_SEC;

    std::cout << elapsedTime << std::endl;
    std::cout << "sum = " << sum << std::endl;

    return 0;
}

