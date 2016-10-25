#!/usr/bin/python

# must define functions before usage
def fib(n):
  a, b = 0, 1
  while a < n:
    print(a, end=' ') # use ' ' as EOL, not '\n'
    a, b = b, a+b
  print()

fib(2000)
