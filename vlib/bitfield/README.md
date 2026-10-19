## Description

`bitfield` is a module for manipulating arrays of bits, i.e. a series of zeroes and ones spread
across an array of storage units.

## BitField Structure

Bit arrays are stored in data structures called `BitField`. The structure is 'opaque', i.e. its
internals are not available to the end user. This module provides API (functions and methods) for
accessing and modifying bit arrays.

`BitField.slice(start, end)` clamps a negative start to zero. An end beyond the bitfield
size is clamped to the size, and reversed bounds produce an empty bitfield.
