## Description

`encoding.binary` contains utility functions for converting between an array of bytes (`[]u8`)
and unsigned integers of various widths (`u16`, `u32`, and `u64`).

There are two ways in which bytes can be encoded:

1. Little endian: The least significant bytes are stored first, followed by the most
   significant bytes.
2. Big endian: The most significant bytes are stored first, opposite to the little endian
   convention.

For example, let us take the number `0x12345678`. In little endian, the bytes are extracted as
`0x78`, `0x56`, `0x34`, and `0x12`. In big endian, the bytes are `0x12`, `0x34`, `0x56`,
and `0x78`.

We follow a similar procedure when we want to go the other way around. Consider the second
sequence of bytes in the previous example: `0x12`, `0x34`, `0x56`, and `0x78`. If we encode
this sequence in little endian format, we get the integer `0x78563412`. If we encode this
sequence in big endian, we get `0x12345678`.

**NOTE:** The functions in this module assume appropriately sized u8 arrays. If the sizes
are not valid, the functions will panic.
