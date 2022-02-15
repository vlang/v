from ctypes import *
import os

so_file="./test.so"
if os.name=="nt":
  so_file="./test.dll"
my_functions = CDLL(so_file)
print(my_functions.square(10))

my_functions.sqrt_of_sum_of_squares.restype = c_double
print(my_functions.sqrt_of_sum_of_squares(c_double(1.1), c_double(2.2)))