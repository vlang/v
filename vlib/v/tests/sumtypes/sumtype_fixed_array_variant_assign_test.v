type Val = f64 | [3]f64 | []Val

type Num = int | [2]int

fn test_fixed_array_variant_assigned_to_map_value() {
	mut m := map[string]Val{}
	m['p'] = [1.0, 2.0, 3.0]!
	v := m['p'] or { panic('missing key') }
	if v is [3]f64 {
		assert v == [1.0, 2.0, 3.0]!
	} else {
		assert false
	}
}

fn test_fixed_array_variant_assigned_to_var() {
	mut v := Val(0.0)
	v = [4.0, 5.0, 6.0]!
	match v {
		[3]f64 { assert v == [4.0, 5.0, 6.0]! }
		else { assert false }
	}
}

fn test_fixed_array_var_assigned_to_sumtype() {
	arr := [7.0, 8.0, 9.0]!
	mut v := Val(0.0)
	v = arr
	if v is [3]f64 {
		assert v == [7.0, 8.0, 9.0]!
	} else {
		assert false
	}
}

fn test_fixed_array_variant_assigned_to_array_element() {
	mut a := [Val(0.0)]
	a[0] = [1.0, 1.5, 2.0]!
	if a[0] is [3]f64 {
		assert a[0] == [1.0, 1.5, 2.0]!
	} else {
		assert false
	}
}

struct Holder {
mut:
	val Num
}

fn test_fixed_array_variant_assigned_to_struct_field() {
	mut h := Holder{}
	h.val = [3, 4]!
	if h.val is [2]int {
		assert h.val == [3, 4]!
	} else {
		assert false
	}
}

fn test_plain_fixed_array_assignment_still_works() {
	mut a := [3]f64{}
	a = [1.0, 2.0, 3.0]!
	assert a == [1.0, 2.0, 3.0]!
	b := [4.0, 5.0, 6.0]!
	a = b
	assert a == [4.0, 5.0, 6.0]!
	mut m := map[string][3]f64{}
	m['k'] = [1.0, 2.0, 3.0]!
	assert m['k'] == [1.0, 2.0, 3.0]!
}

fn test_match_branch_on_fixed_array_variant() {
	v := Val([1.0, 2.0, 3.0]!)
	mut got := ''
	match v {
		[3]f64 { got = 'vec ${v}' }
		[]Val { got = 'list' }
		else { got = 'other' }
	}
	assert got == 'vec [1.0, 2.0, 3.0]'

	n := Num([3, 4]!)
	match n {
		[2]int { assert n == [3, 4]! }
		int { assert false }
	}
}

// A `[N]T` match branch is a type pattern, but `[a, b]!` is still a fixed array
// literal *value* to match against.
fn test_match_branch_on_fixed_array_literal_value() {
	pick := fn (a [2]int) string {
		return match a {
			[1, 2]! { 'onetwo' }
			[3, 4]! { 'threefour' }
			else { 'other' }
		}
	}
	assert pick([1, 2]!) == 'onetwo'
	assert pick([3, 4]!) == 'threefour'
	assert pick([9, 9]!) == 'other'
}
