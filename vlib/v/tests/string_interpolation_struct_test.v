// This file tests whether V can generate a convenience default .str() method
// for a custom struct, when the developer has not defined one himself.
// The .str() methods are used for string interpolation and for println() calls.
struct Man {
	name      string
	age       int
	interests []string
}

fn test_default_struct_string_interpolation() {
	superman := Man{'Superman', 30, ['flying', 'fighting evil', 'being nice']}
	s := '$superman'
	assert s.contains('Man {')
	assert s.contains('name: "Superman"')
	assert s.contains('age: 30')
	assert s.contains('interests: [')
	assert s.contains('"being nice"')
	assert s.contains('}')
	// println(s)
}
