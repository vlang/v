// This file tests whether V can generate a convenience default .str() method
// for var args of a custom type, when the developer has NOT defined one.
// Although similar to string_interpolation_struct_test.v, they should not be
// merged.
struct Man {
	name      string
	age       int
	interests []string
}

fn my_variadic_function(x ...Man) string {
	return '$x'	// this interpolation should generate .str() methods for Man
}

fn test_vargs_string_interpolation() {
	man := Man{'Me', 38, ['programming', 'reading', 'hiking']}
	superman := Man{'Superman', 30, ['flying', 'fighting evil', 'being nice']}
	results := my_variadic_function(superman, man)
	assert results.contains('Man {')
	//
	assert results.contains("name: 'Superman'")
	assert results.contains('age: 30')
	assert results.contains('}, Man {')
	//
	assert results.contains("interests: ['programming'")
	assert results.contains("name: 'Me'")
	//
	assert results.contains('}]')
	//
	println(results)
}
