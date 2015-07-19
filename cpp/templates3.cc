/* Testing templates with template template parameters */
#include <iostream>

// two arrays to store a key-value mapping
class Map_fixed {
  char * * key;
  char * * val;
};

// abstracting to an array type
class array_fixed {
  char * data;  
};

class Map_array {
  array_fixed key;
  array_fixed val;
};

// abstract array to a templated type
template <typename T>
class array {
  T data[];
};

template <typename T>
class Map_array_T {
  array<T> key;
  array<T> val;
};

// allow key and value types to differ
template <typename K, typename V>
class Map_array_KV {
  array<K> key;
  array<V> val;
};

// right now, we have a fairly good abstraction, but we have fixed the
// underlying array implementation, what if we want to allow it to change as
// well?
template <typename T> class Array {};

template <typename K, typename V, typename KA = Array<K>, typename KV = Array<V>>
class Map_Array {
  KA key;
  KV val;
};

// sort of works, although need to express relationship of K to KV through a
// default choice.

// template template parameters allow us to express the relationship directly!

template <typename K, typename V, template<typename> class A = Array>
class Map_flexible {
  A<K> key;
  A<V> val;
};

