## Description

`encoding.base32` encodes and decodes Base32 data using the standard or hexadecimal alphabet.

With `no_padding`, the final group may contain 2, 4, 5, or 7 symbols, or a complete group of 8.
Incomplete groups of 1, 3, or 6 symbols return an illegal Base32 data error.
