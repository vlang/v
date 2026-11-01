// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
import v.reflection

fn test_platform_integer_kinds_preserve_legacy_ordinals() {
	assert int(reflection.VKind.int) == 9
	assert int(reflection.VKind.isize) == 10
	assert reflection.type_of(42).sym.kind == .int
	assert reflection.type_of(isize(-42)).sym.kind == .isize
	assert reflection.type_of(uint(42)).sym.kind == .uint
}
