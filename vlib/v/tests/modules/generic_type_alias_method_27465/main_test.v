module main

import candidates27465 as se

// Regression test for https://github.com/vlang/v/issues/27465
//
// A type alias of a generic instantiation (`type Candidates = BitSet[u32]`) that
// defines its own method whose name collides with a method on the generic parent
// (`set`) used to make the alias method's call sites emit a bogus generic suffix,
// e.g. `candidates27465__Candidates_set_T_u32`, an undefined C symbol.

fn test_alias_method_in_module() {
	// se.make() calls the alias method `Candidates.set` from inside its own module.
	c := se.make()
	assert c.bits == u32(0b111)
}

fn test_alias_method_cross_module() {
	// Cross-module call to the alias method via a fully qualified `se.Candidates{}`.
	mut c := se.Candidates{}
	c.set(3, 5)
	assert c.bits == u32(0b101000)
}

fn test_inherited_generic_method_still_works() {
	// The inherited generic method keeps its proper generic suffix.
	c := se.full()
	assert c.bits == ~u32(0)
}
