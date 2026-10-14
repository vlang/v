## Description

`io` provides common interfaces for buffered reading/writing of data.

## ByteBuffer

`new_byte_buffer(capacity)` creates a growable cursor-based buffer. `put_u8`,
`put_u16_be`/`put_u16_le`, `put_u32_be`/`put_u32_le`, `put_u64_be`/`put_u64_le`, and
`put_bytes` append values. Corresponding `get_*` methods consume bytes and return errors
on underflow, including negative or oversized byte counts, without moving the cursor.

`flip()` and `rewind()` reset the read cursor to the start. `compact()` discards consumed
bytes, and `clear()` discards all bytes. `remaining()` counts unread bytes; `len()` counts
all buffered bytes. There is no separate limit or write mode: puts always append and
reads consume the region from the cursor through the current length.
