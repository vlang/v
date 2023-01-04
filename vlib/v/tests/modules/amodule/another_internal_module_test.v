module amodule

// This tests whether _test.v files can be *internal* to a
// module, and thus have access to its guts.
// Note: the function test_private_isub() is defined both here
// and inside internal_module_test.v . That is done on purpose,
// with the goal of ensuring that _test.v files are compiled
// *independently* from each other.
//
// _test.v files should *only* import all the other normal .v
// files from the same folder, NOT other _test.v files from it.
fn test_private_isub() {
	assert private_isub(7, 5) == 2
}
