## Description

`encoding.base32` encodes and decodes Base32 data using the standard or hexadecimal alphabet.
Decoders reject bytes outside their alphabet.

Custom alphabets must contain exactly 32 distinct bytes, excluding CR, LF, and the padding byte.
Encoding constructors panic when these requirements are violated.

With `no_padding`, the final group may contain 2, 4, 5, or 7 symbols, or a complete group of 8.
Incomplete groups of 1, 3, or 6 symbols return an illegal Base32 data error.

Decoding ignores CR and LF bytes in the input, including inside an encoded quantum and after
padding. Other whitespace is not stripped.
