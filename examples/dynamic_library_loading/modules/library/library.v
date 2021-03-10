module library

// add_1 is exported with the C name `add_1`.
// It can be called by external programs, when the module is compiled
// as a shared library.
// It is exported, because the function is declared as public with `pub`.
// The exported C name is `add_1`, because of the export: tag.
// (Normally, the exported name is a V mangled version based on the module
// name followed by __, followed by the fn name, i.e. it would have been
// `library__add_1`, if not for the export: tag).
[export: 'add_1']
pub fn add_1(x int, y int) int {
	return my_private_function(x + y)
}

// this function is not exported and will not be visible to external programs.
fn my_private_function(x int) int {
	return 1 + x
}
