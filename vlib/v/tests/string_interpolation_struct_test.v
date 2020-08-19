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
	assert s.starts_with('Man {')
	assert s.contains("name: 'Superman'")
	assert s.contains('age: 30')
	assert s.contains('interests: [')
	assert s.contains("'being nice'")
	assert s.ends_with('}')
	// println(s)
}

struct S1 {
mut:
	a []int = [7]
}

fn (s S1) m() int {
	return s.a[0]
}

fn test_array_interpolation() {
	a := [2,3]
	assert '$a[1]' == '3'
	mut s := S1{}
	assert '$s.a[0]' == '7'
	assert '$s.m()' == '7'
	b := [s]
	assert '$b[0].a' == '[7]'
	s.a[0]++
	assert '$b[0].a[0]' == '8'
}

struct Context {
pub mut:
	vb [8]f64
}

fn test_fixed_array_struct_string_interpolation() {
	mut ctx := Context{}
	x := 2.32
	ctx.vb = [1.1, x, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]!!
	s := '$ctx'
	assert s.starts_with('Context {')
	assert s.contains('vb: [1.1, 2.32, 3.3, 4.4, 5, 6, 7, 8.9]')
	assert s.ends_with('}')
}

struct Info {
	name string
	dict map[string]int
}

fn test_struct_map_field_string_interpolation() {
	info := Info{
		name: 'test'
		dict: {'a': int(1), 'b': 2}
	}
	s := '$info'
	assert s.starts_with('Info {')
	assert s.contains("name: 'test'")
	assert s.contains("dict: {'a': 1, 'b': 2}")
	assert s.ends_with('}')
}
