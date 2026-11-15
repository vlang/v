## Description

`io` provides common interfaces for buffered reading/writing of data.

## Copying bytes and streams

`copy_bytes(mut dst, src)` copies `min(dst.len, src.len)` bytes and returns that count.
It leaves the destination length unchanged, ignores unused capacity, and supports overlapping
slices. It can also be stored in a function value.

```v
import io

mut bytes := 'abcdef'.bytes()
assert io.copy_bytes(mut bytes[2..], bytes[..4]) == 4
assert bytes.bytestr() == 'ababcd'
```

`copy(mut dst, mut src)!` copies from a `Reader` into a `Writer` until the reader reports
`io.Eof`. It returns the total byte count on success and uses a 32 KiB buffer.
`copy_buffer(mut dst, mut src, mut buffer)!` uses the supplied buffer instead. An empty buffer
is replaced with a 32 KiB buffer; a nonempty buffer keeps its length.

`copy_n(mut dst, mut src, n)!` copies exactly `n` bytes without reading beyond that limit.
If `n <= 0`, it returns zero without reading or writing. Early `io.Eof` returns an unexpected-EOF
error.

All three stream helpers handle short reads and retry short writes. Negative or oversized counts,
zero-byte reads, and zero-byte writes return errors. A zero-byte read is a no-progress error,
not a completed stream. Other reader and writer errors propagate unchanged, including a writer's
`io.Eof`. Errors may leave partial output in the writer; the result carries a byte count only
on success.

## Synchronous pipes

`pipe()` returns a connected `PipeReader` and `PipeWriter`. A nonempty read waits for
data or a close. A nonempty write waits until one or more reads consume all its bytes;
parallel writes are serialized. Run the producer and consumer in separate workers.
Reads, writes, and closes may run concurrently. Do not modify a write's input while it runs.

Closing the writer makes reads return `Eof`; `close_with_error(err)` supplies another error.
Closing the reader makes writes fail, forwarding its custom error while the writer is open.
An operation on its own closed end returns a pipe-closed error. Each end retains its first
close error, and closing either end wakes blocked operations. An interrupted write may have
already delivered some bytes; `!int` carries a count only on success. Empty reads and writes
are no-ops that return zero, even after a close.

## Discarding output

`Discard` implements `Writer` by accepting every byte without storing data. Writes never
fail and return the input length, including zero for an empty input. Copy `io.discard`
to a mutable local, or construct `io.Discard{}`, when a writer is required.

```v
import io

mut sink := io.discard
assert sink.write('ignored'.bytes())! == 7
```

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

## Writing strings

`write_string(mut writer, text)!` writes a string through a `Writer`. It calls `write`
once and returns the writer's byte count or error, including a short count without
retrying. Counts refer to UTF-8 bytes, not Unicode characters. Empty strings are passed
to the writer too.

```v
import io
import strings

mut buffer := strings.new_builder(16)
assert io.write_string(mut buffer, 'hello')! == 5
assert buffer.str() == 'hello'
```
