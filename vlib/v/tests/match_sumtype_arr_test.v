type MySumType = []MyStructA | []MyStructB
type MySumTypePtr = []&MyStructA | []&MyStructB

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

fn check2(mut t MySumTypePtr) {
	match mut t {
		[]&MyStructA, []&MyStructB {
			println(t[0].parent_field)
		}
	}
}

fn test_main() {
	s := MyStructA{
		parent_field: 'common'
	}

	mut t := MySumType([s])
	mut t2 := MySumTypePtr([&s])

	check(mut t)
	check2(mut t2)
	assert true
}
