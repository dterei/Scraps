#include <algorithm>
#include <ctime>
#include <iostream>
#include <unistd.h>

const unsigned ARRAY_SIZE = 65536;

unsigned sum = 0;

void randomFill(int data[], unsigned len)
{
  for (auto i = 0; i < len; i++) {
    data[i] = std::rand() % 10;
  }
}

void a() { sum += 0; }
void b() { sum += 1; }
void c() { sum += 2; }
void d() { sum += 3; }
void e() { sum += 4; }
void f() { sum += 5; }
void g() { sum += 6; }
void k() { sum += 7; }
void i() { sum += 8; }
void j() { sum += 9; }

void runData(int data[], unsigned len)
{
  for (auto x = 0; x < len; x++) {
    switch (data[x]) {
    case 0: a(); break;
    case 1: b(); break;
    case 2: c(); break;
    case 3: d(); break;
    case 4: e(); break;
    case 5: f(); break;
    case 6: g(); break;
    case 7: k(); break;
    case 8: i(); break;
    case 9: j(); break;
    }
  }
}

int main()
{
    // Generate data
    int data[ARRAY_SIZE];
    randomFill(data, ARRAY_SIZE);

    auto p = getpid();
    std::cout << "pid = " << p << std::endl;

    auto start = clock();
    runData(data, ARRAY_SIZE);
    auto elapsedTime = static_cast<double>(clock() - start) / CLOCKS_PER_SEC;

    std::cout << elapsedTime << "s" << std::endl;
    std::cout << "sum = " << sum << std::endl;

    return 0;
}

