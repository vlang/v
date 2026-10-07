## Description

`uuid` generates and parses UUIDs (Universally Unique Identifiers) as defined in
[RFC 9562](https://www.rfc-editor.org/rfc/rfc9562.html).

A `uuid.UUID` is a `[16]u8`, so UUIDs can be compared with `==` and used as map keys.
The random bits of new UUIDs come from the cryptographically secure random number
generator of the operating system (`crypto.rand`).

- `uuid.new()` returns a new UUID made with an algorithm suitable for most purposes;
  at this time it is the same as `uuid.new_v4()`.
- `uuid.new_v4()` returns a random (version 4) UUID, with 122 random bits.
- `uuid.new_v7()` returns a time-ordered (version 7) UUID: the Unix time in
  milliseconds, a 12-bit fraction of the millisecond and 62 random bits. The UUIDs
  it returns always sort in increasing order, except when the system clock moves
  backwards.
- `uuid.parse(s)` accepts `f81d4fae-7dec-11d0-a765-00a0c91e6bf6`,
  `{f81d4fae-7dec-11d0-a765-00a0c91e6bf6}`, `urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6`
  and `f81d4fae7dec11d0a76500a0c91e6bf6`, with hexadecimal digits in any case.
- `u.str()` returns the lowercase hex-and-dash form, and `u.compare(v)` orders UUIDs
  by their bytes, as RFC 9562 does.
- `uuid.nil_uuid` and `uuid.max_uuid` are the Nil and the Max UUIDs.

The module is a port of the `uuid` package of Go.

## Examples

```v
import uuid

fn main() {
	id := uuid.parse('{F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6}') or { panic(err) }
	println(id.str())
	println(id == uuid.parse('f81d4fae7dec11d0a76500a0c91e6bf6') or { panic(err) })

	mut ids := [uuid.max_uuid, id, uuid.nil_uuid]
	ids.sort_with_compare(fn (a &uuid.UUID, b &uuid.UUID) int {
		return a.compare(*b)
	})
	for u in ids {
		println(u.str())
	}

	mut seen := map[uuid.UUID]bool{}
	seen[uuid.new_v7()] = true
	println(seen.len)
}
```

```
f81d4fae-7dec-11d0-a765-00a0c91e6bf6
true
00000000-0000-0000-0000-000000000000
f81d4fae-7dec-11d0-a765-00a0c91e6bf6
ffffffff-ffff-ffff-ffff-ffffffffffff
1
```
