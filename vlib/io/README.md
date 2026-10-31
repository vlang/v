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

`limited_reader` and `section_reader` cap the remaining bytes without changing the caller's
buffer length. `tee_reader` mirrors read bytes and reports write errors or short writes.
`multi_reader` advances only at `Eof`; it preserves other errors and zero-byte reads from the
current reader. Reader combinators reject negative counts or counts larger than the buffer.
`read_full` fills the buffer, propagates reader errors, and errors on a zero-byte read.
`read_byte` reads one byte. `BufferedReader.unread_byte` permits one undo of a successful
buffered `read_byte`; another read operation, peek, or failed read clears that permission.

`read_exact(mut reader, mut buffer)!` fills the supplied buffer across short reads.
An empty buffer succeeds without reading. Early `Eof` or a zero-byte read returns `Eof`;
other reader errors propagate. Negative counts or counts larger than the remaining buffer
return errors. A failure may leave the bytes read so far in the buffer.

`BufferedReader.lines()` produces a `LineIterator`; `next()` returns a line without its
newline delimiter and handles CRLF and a final line without a newline.
Iteration ends with `none` at EOF or any read error. The iterator uses the original reader,
which must remain valid until iteration finishes.
`Seeker` describes `seek(position, os.SeekMode)!`; `os.File` implements it.
