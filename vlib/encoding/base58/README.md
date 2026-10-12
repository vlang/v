## Description

`encoding.base58` encodes and decodes Base58 data using standard or custom alphabets.

`new_alphabet` requires exactly 58 distinct ASCII characters and returns an error for invalid input.

The byte and string decode functions return an error for non-ASCII digits, as they do for
other characters outside the selected alphabet.
