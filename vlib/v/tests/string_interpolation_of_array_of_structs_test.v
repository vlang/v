module main

struct Anything {
mut:
	name  string
	keepo int
}

fn (a Anything) str() string {
	return a.name
}

fn test_array_of_ptrs_to_structs_can_be_printed() {
	mut testing := []&Anything{}
	testing << &Anything{
		name: 'Hehe'
	}
	testing << &Anything{
		name: 'other'
	}
	testing << &Anything{
		name: 'test'
	}
	for test in testing {
		println(test)
		assert true
	}
	println('testing: ${testing}')
	println(testing)
	assert true
}

// At the same time, this should also work:
// (note the str method defined on (a &T), instead on (a T))
struct PstrAnything {
mut:
	name  string
	keepo int
}

fn (a &PstrAnything) str() string {
	return a.name
}

fn test_array_of_ptrs_to_structs_can_be_printed_when_structs_have_str_with_ptr() {
	mut testing := []&PstrAnything{}
	testing << &PstrAnything{
		name: 'abc'
	}
	testing << &PstrAnything{
		name: 'def'
	}
	testing << &PstrAnything{
		name: 'ghi'
	}
	for test in testing {
		println(test)
		assert true
	}
	println('testing: ${testing}')
	println(testing)
	assert true
}

//
fn test_stack_array_of_structs_can_be_printed_when_structs_have_ordinary_str() {
	mut t := [3]Anything{}
	t[0] = Anything{
		name: '012'
	}
	t[1] = Anything{
		name: '345'
	}
	t[2] = Anything{
		name: '678'
	}
	for test in t[0..3] {
		println(test)
		assert true
	}
	println('t: ${t}')
	println(t)
	println('t[0] := ${t[0]}')
	assert true
}

fn test_stack_array_of_structs_can_be_printed_when_structs_have_str_with_ptr() {
	// this generates a C error
	mut pt := [3]PstrAnything{}
	pt[0] = PstrAnything{
		name: 'P012'
	}
	pt[1] = PstrAnything{
		name: 'P345'
	}
	pt[2] = PstrAnything{
		name: 'P678'
	}
	for test in pt[0..3] {
		println(test)
		assert true
	}
	println('pt: ${pt}')
	println(pt)
	print('pt[0] := ')
	print(pt[0])
	println('')
	assert true
	$if debug_buggy_println ? {
		// TODO: fix string interpolation for structs with `fn (t &T) str() string` too:
		println('pt[0] := ${pt[0]}')
	}
}
