type MySumType = []MyStructA | []MyStructB

struct ParentStruct {
	parent_field string
}

fn (f ParentStruct) foo() int {
	return 123
}

struct MyStructA {
	ParentStruct
	a int = 1
}

struct MyStructB {
	ParentStruct
	a int = 2
}

fn check(mut t MySumType) {
	match mut t {
		[]MyStructA, []MyStructB {
			for v in t {
				println(v)
				println(v.a)
				println(v.parent_field)
				println(v.foo())
			}
		}
	}
}

fn test_a() {
	s := MyStructA{
		parent_field: 'common'
	}

	mut t := MySumType([s])
	check(mut t)
	assert true
}

fn test_b() {
	s := MyStructB{
		parent_field: 'common2'
	}

	mut t := MySumType([s])
	check(mut t)
	assert true
}
