from ctypes import *
so_file="./test.so"
my_functions = CDLL(so_file)
print(my_functions.square(10))
