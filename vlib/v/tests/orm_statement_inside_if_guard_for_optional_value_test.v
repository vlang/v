import db.sqlite

@[table: 'foos']
struct Foo {
	value int
}

struct State {
	val ?int
}

fn test_main() {
	val := ?int(none)
	state := State{val}

	db := sqlite.connect(':memory:')!

	if state.val != none {
		v := sql db {
			select from Foo where value < state.val
		}!
		println(v)
		assert false
	} else {
		assert true
	}
}
