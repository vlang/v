## Description:

`bitfield` is a module for
manipulating arrays of bits, i.e. series of zeroes and ones spread across an
array of storage units (unsigned 32-bit integers).

## BitField structure

Bit arrays are stored in data structures called 'BitField'. The structure is
'opaque', i.e. its internals are not available to the end user. This module
provides API (functions and methods) for accessing and modifying bit arrays.
