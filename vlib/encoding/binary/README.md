## Description

`encoding.binary` contains utility functions for converting between an array of bytes (`[]u8`)
and unsigned integers of various widths (`u16`, `u32`, and `u64`).

Also, it provide functions `encode_binary[T]()` and `decode_binary[T]()` which can converting 
between an array of bytes (`[]u8`) and generic type `T`.

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

> **Note**
> The functions in this module assume appropriately sized u8 arrays. If the sizes
> are not valid, the functions will panic.

For generic `T` data encoding/decoding, you can use `encode_binary[T]()` and `decode_binary[T]()`:

```v
module main

import encoding.binary

struct MyStruct {
	g_u8 u8
}

struct ComplexStruct {
mut:
	f_u8      u8
	f_u32     u32 @[serialize: '-'] // this field will be skipped
	f_u64     u64
	f_string  string
	f_structs []MyStruct
	f_maps    []map[string]string
}

fn main() {
	a := ComplexStruct{
		f_u8:      u8(10)
		f_u32:     u32(1024)
		f_u64:     u64(2048)
		f_string:  'serialize me'
		f_structs: [
			MyStruct{
				g_u8: u8(1)
			},
			MyStruct{
				g_u8: u8(2)
			},
			MyStruct{
				g_u8: u8(3)
			},
		]
		f_maps:    [
			{
				'abc': 'def'
			},
			{
				'123': '456'
			},
			{
				',./': '!@#'
			},
		]
	}

	b := binary.encode_binary(a)!
	mut c := binary.decode_binary[ComplexStruct](b)!

	// because there skipped field in `a`, a != c
	assert a != c

	c.f_u32 = u32(1024)
	assert a == c
}
```
