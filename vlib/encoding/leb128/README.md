The LEB128 decoders return the decoded integer and the number of input bytes consumed.
All four decoders return `(0, 0)` for empty or truncated input. Overflow returns zero and the
negative number of bytes inspected (at most 5 for 32-bit values and 10 for 64-bit values).
Signed decoders validate that unused bits in the final byte extend the sign correctly.
Non-minimal encodings are accepted when their value fits the target integer type.
