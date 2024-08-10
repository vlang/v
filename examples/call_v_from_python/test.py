from ctypes import *
import math, os

## Load the V shared library:
so_file="./test.so"
if os.name=="nt":
  so_file="./test.dll"
lib = CDLL(so_file)

## Pass an integer to a V function, and receiving back an integer:
print("lib.square(10) result is", lib.square(10))
assert lib.square(10) == 100, "Cannot validate V square()."

## Pass a floating point number to a V function:
lib.sqrt_of_sum_of_squares.restype = c_double
assert lib.sqrt_of_sum_of_squares(c_double(1.1), c_double(2.2)) == math.sqrt(1.1*1.1 + 2.2*2.2), "Cannot validate V sqrt_of_sum_of_squares()."

## Passing a V string to a V function, and receiving back a V string:
class VString(Structure):
  _fields_ = [("str", c_char_p), ("len", c_int)]

lib.process_v_string.argtypes = [VString]
lib.process_v_string.restype = VString

assert lib.process_v_string(VString(b'World', 5)).str == b'v World v'
print('Hello', str(lib.process_v_string(VString(b'World', 5)).str, 'utf-8'))
