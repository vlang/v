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

fn test_array_of_structs_interpolation() {
	people := [
		Man{'Superman', 30, ['flying','fighting evil','being nice']},
		Man{'Bilbo Baggins', 111, ['exploring', 'hiding']},
	]
	s := '$people' // the compiler should generate code for both a) and b)
	assert s.contains('Man{')
	assert s.contains("name: 'Superman'")
	assert s.contains('age: 30')
	assert s.contains("'being nice'")
	assert s.contains('}, Man{')
	assert s.contains("name: 'Bilbo Baggins'")
	assert s.contains('age: 111')
	assert s.contains("interests: ['exploring', 'hiding']")
	assert s.contains('}]')
	// println(s)
}

fn test_array_of_floats_interpolation() {
	// f64 array
	aa := [1.2, 3.4, 5.67]
	assert '$aa' == '[1.2, 3.4, 5.67]'
	// f32 array
	bb := [f32(1.2), 3.4, 5.67]
	assert '$bb' == '[1.2, 3.4, 5.67]'
}

fn test_array_of_bools_interpolation() {
	aa := [true, false, true]
	assert '$aa' == '[true, false, true]'
}

fn test_array_of_ints_interpolation() {
	// int
	a1 := [11, 22, 33]
	assert '$a1' == '[11, 22, 33]'
	// u32
	a2 := [u32(11), 22, 33]
	assert '$a2' == '[11, 22, 33]'
	// i16
	b1 := [i16(11), 22, 33]
	assert '$b1' == '[11, 22, 33]'
	// u16
	b2 := [u16(11), 22, 33]
	assert '$b2' == '[11, 22, 33]'
	// i64
	c1 := [i64(11), 22, 33]
	assert '$c1' == '[11, 22, 33]'
	// u64
	c2 := [u64(11), 22, 33]
	assert '$c2' == '[11, 22, 33]'
}

fn test_array_of_runes_interpolation() {
	aa := [`a`, `b`, `c`]
	assert '$aa' == '[`a`, `b`, `c`]'
}

fn test_array_of_strings_interpolation() {
	aa := ['aa', 'bb', 'cc']
	assert '$aa' == "['aa', 'bb', 'cc']"
}

fn test_array_of_map_interpolation() {
	mut a := []map[string]int{}
	a << {'a': int(1), 'b': 2}
	a << {'c': int(3), 'd': 4}
	assert '$a' == "[{'a': 1, 'b': 2}, {'c': 3, 'd': 4}]"
}

fn test_array_initialization_with_interpolation() {
	sysroot := '/usr'
	a := [
		'abcd'
		'$sysroot/xyz'
		'u$sysroot/vw'
		'/rr$sysroot'
		'lmno'
	]
	assert '$a' == "['abcd', '/usr/xyz', 'u/usr/vw', '/rr/usr', 'lmno']"
	b := [
		'a${sysroot:5}/r'
		'ert'
	]
	assert '$b' == "['a /usr/r', 'ert']"
	c := ['xy', 'r$sysroot', '$sysroot/t', '>$sysroot<']
	assert '$c' == "['xy', 'r/usr', '/usr/t', '>/usr<']"
}
