#!/usr/bin/python

squares = [1, 4, 9, 16, 25]
print(squares[0])

# by default, we copy by reference
ref = squares
ref[0] = 2
print(ref)
print(squares)

# slicing returns a new (shallow) copy of the list
copy = squares[:]
copy[0] = 4
print(copy)
print(squares)

# len also applies to lists
print(len(copy))

# + also applies to lists
print(squares + [36, 49, 64])

