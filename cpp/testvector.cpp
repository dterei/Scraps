#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

#include <cassert>
#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

typedef unsigned long long ticks;

using namespace std;

static __inline__ ticks startRDTSC (void) {
    unsigned cycles_low, cycles_high;
    asm volatile ("CPUID\n\t"
            "RDTSC\n\t"
            "mov %%edx, %0\n\t"
            "mov %%eax, %1\n\t": "=r" (cycles_high), "=r" (cycles_low)::
            "%rax", "%rbx", "%rcx", "%rdx");
    return (static_cast<ticks>(cycles_high) << 32) | cycles_low;
}

static __inline__ ticks stopRDTSCP (void) {
    unsigned cycles_low, cycles_high;
    asm volatile("RDTSCP\n\t"
            "mov %%edx, %0\n\t"
            "mov %%eax, %1\n\t"
            "CPUID\n\t": "=r" (cycles_high), "=r" (cycles_low):: "%rax",
            "%rbx", "%rcx", "%rdx");
    return (static_cast<ticks>(cycles_high) << 32) | cycles_low;
}

class CPUBenchmark
{
private:
  ticks ticktime;

public:
  CPUBenchmark() :
    ticktime(0) {
    start();
  }

  void start() {
    ticktime = startRDTSC();
  }

  ticks stop() {
    return stopRDTSCP() - ticktime;
  }
};

int vpushback(size_t N) {
  vector<int> bigarray;
  for(unsigned int k = 0; k<N; ++k)
    bigarray.push_back(k);
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int vreserve_pushback(size_t N) {
  vector<int> bigarray;
  bigarray.reserve(N);
  for(unsigned int k = 0; k<N; ++k)
    bigarray.push_back(k);
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int vreserve_emplaceback(size_t N) {
  vector<int> bigarray;
  bigarray.reserve(N);
  for(unsigned int k = 0; k<N; ++k)
    bigarray.emplace_back(k);
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int init_index(size_t N) {
  vector<int> bigarray(N);
  for(unsigned int k = 0; k<N; ++k)
    bigarray[k] = k;
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

// This method is unsafe as it access the underlying data array directly and so
// doesn't update the known size of vector. After all these inserts,
// bigarary.size() == 0.
//
// So really, this isn't any safer than using an array directly.
int reserve_index(size_t N) {
  vector<int> bigarray;
  bigarray.reserve(N);
  for(unsigned int k = 0; k<N; ++k)
    bigarray[k] = k;// unsafe
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int carray(size_t N) {
  int * bigarray = new int[N];
  for(unsigned int k = 0; k<N; ++k)
    bigarray[k] = k;
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  delete [] bigarray;
  return sum;
}

int vnoalloc(size_t N, int * bigarray) {
  for(unsigned int k = 0; k<N; ++k)
    bigarray[k] = k;// unsafe
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

template <typename T>
struct iota_iterator : std::iterator<std::forward_iterator_tag, T> {
  iota_iterator(T value = T()) : value(value) { }

  iota_iterator& operator ++() {
    ++value;
    return *this;
  }

  iota_iterator operator ++(int) {
    iota_iterator copy = *this;
    ++*this;
    return copy;
  }

  T const& operator *() const {
    return value;
  }

  T const* operator ->() const {
    return &value;
  }

  friend bool operator ==(iota_iterator const& lhs, iota_iterator const& rhs) {
    return lhs.value == rhs.value;
  }

  friend bool operator !=(iota_iterator const& lhs, iota_iterator const& rhs) {
    return not (lhs == rhs);
  }

private:
  T value;
};

int viterator(size_t N) {
  // Extra parentheses to prevent most vexing parse.
  vector<int> bigarray((iota_iterator<int>()), iota_iterator<int>(N));
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int vitoa_algo(size_t N) {
  vector<int> bigarray(N);
  iota(bigarray.begin(), bigarray.end(), 0);
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int vgen_algo(size_t N) {
  int x = 0;
  vector<int> bigarray(N);
  generate(bigarray.begin(), bigarray.end(), [&]{ return x++; });
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int vinsert(size_t N) {
  vector<int> bigarray;
  bigarray.reserve(N);
  bigarray.insert( bigarray.begin(),  iota_iterator<int>(), iota_iterator<int>(N) );
  int sum = 0;
  for(unsigned int k = 0; k<N; ++k)
    sum += bigarray[k];
  return sum;
}

int main() {
  CPUBenchmark time;
  const size_t N = 100 * 1000 * 1000 ;
  time.start();
  cout.precision(3);
  cout<<" report speed in CPU cycles per integer"<<endl;
  cout<<endl<<"ignore this:"<<vpushback(N)<<endl;
  cout<<"with push_back:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<vreserve_pushback(N)<<endl;
  cout<<"with push_back and reserve:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<vreserve_emplaceback(N)<<endl;
  cout<<"with emplace_back and reserve:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<init_index(N)<<endl;
  cout<<"init first:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<reserve_index(N)<<endl;
  cout<<"reserve (unsafe) first:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<carray(N)<<endl;
  cout<<"C++ new:"<<(time.stop()*1.0/N)<<endl;
  int * bigarray = new int[N];
  time.start();
  cout<<endl<<"ignore this:"<<vnoalloc(N,bigarray)<<endl;
  cout<<"without alloc:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<viterator(N)<<endl;
  cout<<"generator iterators:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<vitoa_algo(N)<<endl;
  cout<<"itoa algorithm:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<vgen_algo(N)<<endl;
  cout<<"generator algo:"<<(time.stop()*1.0/N)<<endl;
  time.start();
  cout<<endl<<"ignore this:"<<vinsert(N)<<endl;
  cout<<"insert:"<<(time.stop()*1.0/N)<<endl;

  return 0;
}
