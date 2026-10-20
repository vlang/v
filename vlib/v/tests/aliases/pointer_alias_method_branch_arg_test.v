// A method declared on an alias of a pointer type (`type Handle = &Foo`) is called on a receiver
// that is snapshotted to keep evaluation order, because a later argument is an `if`/`match`
// value. The snapshot must keep the alias, or the call resolved to `Foo.put`, which does not
// exist, and the C build failed at the link (`undefined reference to Foo__put`).
struct Foo {
	n int
}

type Handle = &Foo

fn (h Handle) put(s string) int {
	return h.n + s.len
}

enum Kind {
	short
	long
}

fn get_handle(n int) Handle {
	return Handle(&Foo{
		n: n
	})
}

struct Holder {
	h Handle
}

fn test_variable_receiver_with_a_match_argument() {
	h := Handle(&Foo{
		n: 10
	})
	k := Kind.long
	assert h.put(match k {
		.short { 'a' }
		.long { 'abc' }
	}) == 13
}

fn test_variable_receiver_with_an_if_argument() {
	h := Handle(&Foo{
		n: 1
	})
	long := false
	assert h.put(if long { 'abcd' } else { 'ab' }) == 3
}

fn test_index_receivers() {
	hs := [Handle(&Foo{
		n: 2
	})]
	fixed := [Handle(&Foo{
		n: 3
	}), Handle(&Foo{
		n: 4
	})]!
	k := Kind.short
	assert hs[0].put(match k {
		.short { 'a' }
		.long { 'abc' }
	}) == 3
	assert fixed[1].put(match k {
		.short { 'a' }
		.long { 'abc' }
	}) == 5
	m := {
		'x': Handle(&Foo{
			n: 5
		})
	}
	assert unsafe { m['x'] }.put(if k == .short { 'ab' } else { 'abc' }) == 7
}

fn test_field_and_call_receivers() {
	w := Holder{
		h: Handle(&Foo{
			n: 6
		})
	}
	k := Kind.long
	assert w.h.put(match k {
		.short { 'a' }
		.long { 'abc' }
	}) == 9
	assert get_handle(7).put(if k == .long { 'abcd' } else { 'a' }) == 11
}
