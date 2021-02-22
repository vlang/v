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
	assert s.starts_with('Man{')
	assert s.contains("name: 'Superman'")
	assert s.contains('age: 30')
	assert s.contains('interests: [')
	assert s.contains("'being nice'")
	assert s.ends_with('}')
	// println(s)
}

struct Context {
pub mut:
	vb [8]f64
}

fn test_fixed_array_struct_string_interpolation() {
	mut ctx := Context{}
	x := 2.32
	ctx.vb = [1.1, x, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]!
	s := '$ctx'
	assert s.starts_with('Context{')
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
	assert s.starts_with('Info{')
	assert s.contains("name: 'test'")
	assert s.contains("dict: {'a': 1, 'b': 2}")
	assert s.ends_with('}')
}

struct Circular {
mut:
	next &Circular
}

fn test_stack_circular_elem_auto_str() {
	mut elem := Circular{0}
	elem.next = &elem
	s := '$elem'.replace('\n', '|')
	assert s == 'Circular{|    next: &<circular>|}'
}

fn test_heap_circular_elem_auto_str() {
	mut elem := &Circular{0}
	elem.next = elem
	s := '$elem'.replace('\n', '|')
	assert s == '&Circular{|    next: &<circular>|}'
}
