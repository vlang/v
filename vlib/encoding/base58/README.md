## Description

`encoding.base58` encodes and decodes Base58 data using standard or custom alphabets.

`new_alphabet` requires exactly 58 distinct ASCII characters and returns an error for invalid input.

`decode_int` and `decode_int_walpha` return an error if the encoded value exceeds `max_int`.
They accumulate integer digits exactly and reject characters outside the selected alphabet.

The byte and string decode functions return an error for non-ASCII digits, as they do for
other characters outside the selected alphabet.
