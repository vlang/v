// `buf << s.bytes()` on a `[]u8` appends the string bytes without the temporary array;
// other shapes keep the ordinary path. Both must match appending a materialized copy.
type Text = string

fn (t Text) bytes() []u8 {
	return [u8(`!`)]
}

@[heap]
struct Counter {
mut:
	n int
}

struct Db {
mut:
	cmd_buf []u8
}

struct Outer {
mut:
	db Db
}

fn reference_append(prefix []u8, s string) []u8 {
	mut out := prefix.clone()
	copy_bytes := s.bytes()
	for b in copy_bytes {
		out << b
	}
	return out
}

fn append_param(mut out []u8, s string) {
	out << s.bytes()
}

fn test_contents_match_reference() {
	inputs := ['', 'a', 'hello', 'a\x00b', 'é∑', '\x00', 'x'.repeat(10000)]
	for prefix_len in [0, 1, 15, 16, 17, 100] {
		prefix := []u8{len: prefix_len, init: u8(index)}
		for s in inputs {
			mut got := prefix.clone()
			got << s.bytes()
			assert got == reference_append(prefix, s), 'prefix=${prefix_len} s=${s.len}'
			mut via_param := prefix.clone()
			append_param(mut via_param, s)
			assert via_param == got
		}
	}
}

fn test_exact_capacity_growth_and_spare_capacity() {
	mut a := []u8{cap: 3}
	a << 'abc'.bytes()
	assert a.len == 3
	a << 'defg'.bytes()
	assert a.bytestr() == 'abcdefg'
	mut b := []u8{cap: 100}
	b << 'xyz'.bytes()
	assert b.bytestr() == 'xyz'
	assert b.cap == 100
	mut empty := []u8{}
	empty << ''.bytes()
	assert empty.len == 0
}

fn test_fields_and_results() {
	mut db := Db{}
	db.cmd_buf << '*${2}\r\n'.bytes()
	db.cmd_buf << 42.str().bytes()
	db.cmd_buf << '\r\n'.bytes()
	assert db.cmd_buf.bytestr() == '*2\r\n42\r\n'
	mut o := Outer{}
	o.db.cmd_buf << rune(`∑`).str().bytes()
	assert o.db.cmd_buf == [u8(0xe2), 0x88, 0x91]
}

fn test_string_evaluated_once() {
	mut c := &Counter{}
	next := fn [mut c] () string {
		c.n++
		return 'ab'
	}
	mut buf := []u8{}
	buf << next().bytes()
	buf << next().bytes()
	assert buf.bytestr() == 'abab'
	assert c.n == 2
}

fn test_unsafe_self_alias_is_copied_first() {
	for start_cap in [4, 5, 64] {
		mut buf := []u8{cap: start_cap}
		buf << 'abcd'.bytes()
		view := unsafe { tos(&u8(buf.data), buf.len) }
		buf << view.bytes()
		assert buf.bytestr() == 'abcdabcd', 'cap=${start_cap}'
		tail := unsafe { tos(&u8(buf.data) + 2, 4) }
		buf << tail.bytes()
		assert buf.bytestr() == 'abcdabcdcdab', 'cap=${start_cap}'
	}
}

fn test_unsafe_header_alias_is_copied_first() {
	for start_cap in [1, 64] {
		mut buf := []u8{len: 1, cap: start_cap}
		// View the capacity field without copying it; appending may change that field.
		view := unsafe {
			string{
				str: &u8(&buf.cap)
				len: int(sizeof(buf.cap))
			}
		}
		expected := view.bytes()
		buf << view.bytes()
		assert buf[1..] == expected, 'cap=${start_cap}'
	}
}

fn test_unsafe_slice_header_alias_is_copied_first() {
	mut base := []u8{len: 128, init: u8(index)}
	mut buf := unsafe { base[1..2] }
	// View the header from its first field; unsharing can change its pointer and flags.
	view := unsafe {
		string{
			str: &u8(&buf.data)
			len: int(sizeof(buf))
		}
	}
	expected := view.bytes()
	buf << view.bytes()
	assert buf[1..] == expected
	assert base == []u8{len: 128, init: u8(index)}
}

fn test_slices_keep_independent_storage() {
	mut base := 'abcdef'.bytes()
	mut head := unsafe { base[..2] }
	head << 'XY'.bytes()
	assert head.bytestr() == 'abXY'
	assert base.bytestr() == 'abcdef'
	mut copy := base.clone()
	copy << 'Z'.bytes()
	assert base.bytestr() == 'abcdef'
	assert copy.bytestr() == 'abcdefZ'
}

fn test_fallbacks() {
	// Custom `bytes`, index destinations and non-`[]u8` destinations keep the ordinary path.
	mut buf := []u8{}
	buf << Text('abc').bytes()
	assert buf == [u8(`!`)]
	mut bufs := [][]u8{len: 2}
	mut c := &Counter{}
	pick := fn [mut c] () int {
		c.n++
		return 1
	}
	bufs[pick()] << 'q'.bytes()
	assert bufs[1].bytestr() == 'q'
	assert c.n == 1
	mut nested := [][]u8{}
	nested << 'ab'.bytes()
	assert nested == [[u8(`a`), `b`]]
}
