// This tests, whether type aliased values, can be used in place of their non aliased versions,
// when implementing interfaces. This is important for allowing type aliases in one module,
// to be used as substitutes to keep backwards compatibility, when migrating functionality
// from one module to another, which was the case, when deprecating `gx` in favor of moving
// its functionality into `gg`, while still keeping old code using `gx` compilable for a while,
// by making `gx` import `gg` and declare a few shallow type aliases to the new `gg` types,
// so that older `gx` importers and their CIs (like the `ui` module), can continue to work
// (with just deprecation notices), without forcing an immediate change.
struct Param1 {
	x int
}

struct Param2 {
	y f32
}

type AliasParam1 = Param1
type AliasParam2 = Param2

interface MyInterface {
	method1(p1 Param1, p2 Param2) f32
}

struct ImplDirect {
	v int
}

fn (i ImplDirect) method1(p1 Param1, p2 Param2) f32 {
	println(i)
	println(p1)
	println(p2)
	return i.v + p1.x + p2.y
}

struct ImplWithAliasedParams {
	v int
}

fn (i ImplWithAliasedParams) method1(p1 AliasParam1, p2 AliasParam2) f32 {
	println(i)
	println(p1)
	println(p2)
	return i.v + p1.x + p2.y
}

fn test_interface_method_can_be_called_with_aliased_type_values() {
	isdirect := MyInterface(ImplDirect{1000})
	isalias := MyInterface(ImplWithAliasedParams{2000})

	assert isdirect.method1(AliasParam1{123}, AliasParam2{1.1}) == 1124.1
	assert isalias.method1(AliasParam1{456}, AliasParam2{2.2}) == 2458.2
}
