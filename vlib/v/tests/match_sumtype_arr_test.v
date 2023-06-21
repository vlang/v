type MySumType = []MyStructA | []MyStructB

struct ParentStruct {
	parent_field string
}

struct MyStructA {
	ParentStruct
}

struct MyStructB {
	ParentStruct
}

fn check(mut t MySumType) {
	match mut t {
		[]MyStructA, []MyStructB {
			println(t[0].parent_field)
		}
	}
}

fn test_main() {
	s := MyStructA{
		parent_field: 'common'
	}

	mut t := MySumType([s])

	check(mut t)
	assert true
}
