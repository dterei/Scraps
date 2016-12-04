#!/usr/bin/python

# multi-assignment
a, b = 0, 1

# while-loop - indentation determines block
while b < 10:
  print(b)
  a,b = b, a+b

# if-statement
if a < 5:
  print("a < 5")
elif a == 5:
  print("a == 5")
else:
  print("a > 5")

# for-each-loop
words = ["cat", "dog", "bird"]
for w in words:
  print(w, len(w))

# for i loop
for i in range(5):
  print(i)

# for i loop - jump by 2
for i in range(2,10, 2):
  # break / continue work as normal
  if i == 6: break
  print(i)

# cast iterator/generator to list
print(list(range(5)))

# for loops (and while) can have else statements, executed at end of loop
# iteration (if no break issued).
n = 5
for i in range(1,n):
  x = i
else:
  print(i) # i == 4, terminal value

# pass = undefined
def initlog(*args):
  pass # TODO: Implement!
