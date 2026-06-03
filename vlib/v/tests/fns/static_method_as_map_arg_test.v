// Regression test for a static method used as a first-class value in the
// builtin array methods (`map`/`filter`/...). It is parsed as an `ast.EnumVal`
// (same syntax as an enum value, e.g. `Color.red`); when the static method is
// not yet registered while parsing the call site (forward reference here, or a
// cross-module reference), the parser cannot emit a function `ast.Ident`, so the
// checker has to rewrite the resolved value into one. See issue #27328.

fn test_static_method_as_map_arg() {
	nums := [1, 2, 3]
	foos := nums.map(Foo.from)
	assert foos.map(it.x) == [1, 2, 3]
}

fn test_static_method_as_map_arg_explicit_call() {
	nums := [1, 2, 3]
	foos := nums.map(Foo.from(it))
	assert foos.map(it.x) == [1, 2, 3]
}

fn test_static_method_as_filter_arg() {
	nums := [1, 2, 3, 4]
	odds := nums.filter(is_odd)
	assert odds == [1, 3]
}

fn test_aliased_static_method_as_map_arg() {
	// the static method is reached through a type alias, so it must be resolved
	// by its real fkey (`Foo__static__from`), not the alias name
	nums := [1, 2, 3]
	foos := nums.map(FooAlias.from)
	assert foos.map(it.x) == [1, 2, 3]
}

type FooAlias = Foo

// Defined *after* the call sites on purpose, to exercise the forward-reference
// path (the function is not in the table yet when the calls above are parsed).
struct Foo {
	x int
}

fn Foo.from(n int) Foo {
	return Foo{
		x: n
	}
}

fn is_odd(n int) bool {
	return n % 2 == 1
}
