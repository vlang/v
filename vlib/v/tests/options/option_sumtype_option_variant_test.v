type Any = ?int | string | ?string

fn test_main() {
	b := ?Any(?string('bar'))
	assert b?.str() == "Any(Option('bar'))"

	c := ?Any(string('baz'))
	assert c?.str() == "Any('baz')"

	d := ?Any('baz')
	assert d?.str() == "Any('baz')"
}

struct Bar {
	i int
}

struct Baz {}

type Foo = ?Bar | Baz

// `as`/`is` on an option-type sumtype variant must reference the correct
// union member (`_option_...`), not the plain one. See vlang/v#27478.
fn test_as_is_option_variant() {
	f := Foo(?Bar(Bar{
		i: 5
	}))
	assert f is ?Bar
	got := f as ?Bar
	assert got != none
	assert got?.i == 5

	n := Foo(?Bar(none))
	assert n is ?Bar
	got2 := n as ?Bar
	assert got2 == none

	b := Foo(Baz{})
	assert b is Baz
	assert b !is ?Bar
}
