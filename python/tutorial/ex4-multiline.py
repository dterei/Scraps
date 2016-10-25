#!/usr/bin/python

print("""
Multi-line strings can be constructed with triple-qoutes:
   * \"\"\"
   * '''
""")

print("strings next to" "each other"
    "are automatically concatenated")

print("but only for literals, not for variables")

var1 = "hello"
var2 = 'world'

print("use + for variables: " + var1 + " " + var2)

print("python has no 'char' type, just a string of length 1")

print("var1[1] = " + var1[1])
print("var2[0:3] = " + var2[0:3])
print("var2[:3] = " + var2[:3])
print("var2[3:] = " + var2[3:])
print("var2[-2:] = " + var2[-2:])

# python strings are immutable
print("len(var1) = " + str(len(var1)))

