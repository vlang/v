import strconv

// An architecture-qualified test file (`foo_test.<arch>.v`) must be treated as a test
// by every compiler stage, so its `test_` fns may propagate with `!` like a plain
// `foo_test.v` fn can.
fn test_propagation_in_arch_qualified_test_file() {
	value := strconv.atoi('42')!
	assert value == 42
}
