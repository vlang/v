// vtest vflags: -autofree -gc none
struct Identifier {
	name string
}

struct Cursor {
	table ?Identifier
}

fn test_if_guard_keeps_local_option() {
	mut table := ?Identifier(none)
	table = Identifier{'t'.repeat(2)}
	if t := table {
		assert t.name == 'tt'
	}
	c := &Cursor{
		table: table
	}
	ct := c.table or { panic('table was moved out by the if guard') }
	assert ct.name == 'tt'
}

fn test_if_guard_in_loop_keeps_local_option() {
	table := ?Identifier(Identifier{'t'.repeat(2)})
	mut seen := 0
	for _ in 0 .. 3 {
		if t := table {
			assert t.name == 'tt'
			seen++
		}
	}
	assert seen == 3
}
