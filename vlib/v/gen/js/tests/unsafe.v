struct Bar {
	value int
}

struct Foo1 {
mut:
	name &string
}

struct Foo2 {
mut:
	name ?&string
}

struct Foo3 {
mut:
	bar &Bar
}

fn main() {
	// Basic test of assigning nil to a string pointer
	mut str := 'hi!'
	mut str_ptr := &str
	unsafe {
		str_ptr = nil
	}
	println(str_ptr) // should print '&nil'
	assert str_ptr == unsafe { nil }

	// Test initializing a pointer field with nil
	f1 := Foo1{
		name: unsafe { nil }
	}
	assert f1.name == unsafe { nil }

	// Test assigning nil to an optional pointer field
	mut f2 := Foo2{}
	unsafe {
		f2.name = nil
	}
	if f2.name != none {
		assert f2.name == unsafe { nil }
	} else {
		assert false
	}

	// Test assigning nil to a struct pointer
	mut f3 := &Foo2{}
	unsafe {
		f3 = nil
	}
	assert f3 == unsafe { nil }

	// Test with custom struct fields
	mut f4 := Foo3{
		bar: &Bar{42}
	}
	unsafe {
		f4.bar = nil
	}
	assert f4.bar == unsafe { nil }

	// Test with nil pointers in arrays
	mut ptrs := []&string{len: 3, init: unsafe { nil }}
	p0, p1 := 'hello', 'world'
	ptrs[0] = &p0
	ptrs[1] = &p1
	unsafe {
		ptrs[2] = nil
	}
	assert ptrs[0] != unsafe { nil }
	assert ptrs[1] != unsafe { nil }
	assert ptrs[2] == unsafe { nil }
}
