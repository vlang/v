module main

import alpha
import owner
import sorter
import zeta

// A closure capturing a local of type `[]owner.Row` (from a field written as
// `[]Row` in `owner`) must keep `owner.Row`, even when other imported modules
// declare a `Row` too.
fn test_closure_capture_keeps_the_field_type_module() {
	assert alpha.Row{'a'}.name == 'a'
	assert zeta.Row{'z'}.name == 'z'
	log := owner.Log{
		rows: [owner.Row{
			t_s: 2
		}, owner.Row{
			t_s: 1
		}, owner.Row{
			t_s: 3
		}]
	}
	assert sorter.order(log) == [1, 0, 2]
}
