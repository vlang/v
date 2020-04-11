// This file tests whether v can generate default string methods for both:
// a) an array of custom structs,
// b) also for the custom struct itself (when the .str() for it is missing).
//
// NB: this is very simillar to string_interpolation_struct_test.v
// but they should NOT be merged into 1 file.  If you merge it with
// string_interpolation_struct_test.v, which tests whether the compiler
// can generate the default method for a struct, then the b) case of
// this test will be done by *that* test, and so the testing will
// be incomplete.
struct Man {
	name      string
	age       int
	interests []string
}

fn test_default_struct_array_of_structs_interpolation() {
	people := [
		Man{'Superman', 30, ['flying','fighting evil','being nice']},
		Man{'Bilbo Baggins', 111, ['exploring', 'hiding']},
	]
	s := '$people' // the compiler should generate code for both a) and b)
	assert s.contains('Man {')
	assert s.contains('name: Superman')
	assert s.contains('age: 30')
	assert s.contains('"being nice"')
	assert s.contains('}, Man {')
	assert s.contains('name: Bilbo Baggins')
	assert s.contains('age: 111')
	assert s.contains('interests: ["exploring", "hiding"]')
	assert s.contains('}]')
	// println(s)
}
