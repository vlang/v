from ctypes import *
import os

so_file="./test.so"
if os.name=="nt":
  so_file="./test.dll"
my_functions = CDLL(so_file)
print(my_functions.square(10))
