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

// The same through an alias of the pointer alias: the method is declared on the outer alias, and
// both sides of the comparison must be followed to the end of the chain.
type BaseHandle = &Foo

type ChainHandle = BaseHandle

fn (h ChainHandle) put(s string) int {
	return h.n + s.len + 100
}

fn get_chain_handle(n int) ChainHandle {
	return ChainHandle(BaseHandle(&Foo{
		n: n
	}))
}

fn test_chained_alias_receivers() {
	h := ChainHandle(BaseHandle(&Foo{
		n: 1
	}))
	k := Kind.long
	assert h.put(if k == .long { 'ab' } else { 'a' }) == 103
	assert h.put(match k {
		.short { 'a' }
		.long { 'abc' }
	}) == 104
	hs := [h]
	assert hs[0].put(match k {
		.short { 'a' }
		.long { 'abcd' }
	}) == 105
	assert hs[0].put(if k == .long { 'a' } else { 'ab' }) == 102
	assert get_chain_handle(2).put(if k == .long { 'a' } else { 'ab' }) == 103
	assert get_chain_handle(3).put(match k {
		.short { 'a' }
		.long { 'ab' }
	}) == 105
}

// When the aliased type declares the method too, the snapshotted call must run the method the
// direct call runs: the alias's for a variable or an alias chain, the pointee's for a call
// returning the pointee. For an index or a call returning the alias, the test pins the hoisted
// call to whatever the direct one selects.
struct Bar {
	n int
}

type BarHandle = &Bar

type ChainBarHandle = BarHandle

fn (b &Bar) put(s string) string {
	return 'Bar.put(${s})'
}

fn (h BarHandle) put(s string) string {
	return 'BarHandle.put(${s})'
}

fn (h ChainBarHandle) put(s string) string {
	return 'ChainBarHandle.put(${s})'
}

fn (h BarHandle) raw() &Bar {
	return &Bar(h)
}

fn (h BarHandle) same() BarHandle {
	return h
}

fn test_both_declare_the_method_variable_and_chain() {
	h := BarHandle(&Bar{})
	c := ChainBarHandle(BarHandle(&Bar{}))
	k := Kind.long
	assert h.put('x') == 'BarHandle.put(x)'
	assert h.put(if k == .long { 'x' } else { 'y' }) == 'BarHandle.put(x)'
	assert h.put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == 'BarHandle.put(x)'
	assert c.put('x') == 'ChainBarHandle.put(x)'
	assert c.put(if k == .long { 'x' } else { 'y' }) == 'ChainBarHandle.put(x)'
	assert c.put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == 'ChainBarHandle.put(x)'
}

fn test_both_declare_the_method_call_receivers() {
	h := BarHandle(&Bar{})
	k := Kind.long
	assert h.raw().put('x') == 'Bar.put(x)'
	assert h.raw().put(if k == .long { 'x' } else { 'y' }) == 'Bar.put(x)'
	assert h.raw().put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == 'Bar.put(x)'
	direct := h.same().put('x')
	assert h.same().put(if k == .long { 'x' } else { 'y' }) == direct
	assert h.same().put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == direct
}

fn test_both_declare_the_method_index_receivers() {
	hs := [BarHandle(&Bar{})]
	fixed := [BarHandle(&Bar{})]!
	m := {
		'a': BarHandle(&Bar{})
	}
	k := Kind.long
	direct := hs[0].put('x')
	assert hs[0].put(if k == .long { 'x' } else { 'y' }) == direct
	assert hs[0].put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == direct
	assert fixed[0].put(if k == .long { 'x' } else { 'y' }) == fixed[0].put('x')
	assert fixed[0].put(match k {
		.short { 'y' }
		.long { 'x' }
	}) == fixed[0].put('x')
	assert unsafe { m['a'] }.put(if k == .long { 'x' } else { 'y' }) == unsafe { m['a'] }.put('x')
}
