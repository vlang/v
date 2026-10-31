// An assignment of a value of the variant a sum type value is narrowed to keeps
// it narrowed, as in V 0.5.2; the checker and the generated code must agree on
// that. Any other value ends the narrowing.
type Val = int | string

struct Box {
mut:
	v Val
}

struct Holder {
mut:
	box Box
}

type Item = Box | int

fn test_member_assigned_the_narrowed_variant_stays_narrowed() {
	mut b := Box{
		v: Val(1)
	}
	if mut b.v is int {
		b.v = 5
		y := b.v + 1
		assert y == 6
		assert b.v == 5
		assert '${b.v}' == '5'
		b.v = b.v * 2
		assert b.v + 0 == 10
	}
	assert b.v == Val(10)
}

fn test_local_assigned_the_narrowed_variant_stays_narrowed() {
	mut x := Val(1)
	if mut x is int {
		x = 7
		y := x + 1
		assert y == 8
		assert x == 7
		assert '${x}' == '7'
	}
	assert x == Val(7)
}

fn test_assignment_of_another_variant_ends_the_narrowing() {
	mut b := Box{
		v: Val(1)
	}
	if mut b.v is int {
		b.v = 'str'
		assert b.v == Val('str')
		assert '${b.v}' == "Val('str')"
	}
	assert b.v == Val('str')
	mut x := Val(1)
	if mut x is int {
		x = 'str'
		assert x == Val('str')
	}
	assert x == Val('str')
}

fn nested_block_assignment(mut b Box, c bool) string {
	if mut b.v is int {
		if c {
			b.v = 3
			assert b.v + 1 == 4
			for b.v < 6 {
				b.v = b.v + 2
			}
			return 'kept ${b.v * 2}'
		}
		b.v = 'str'
		return 'ended ${b.v}'
	}
	return 'not int'
}

fn test_assignment_in_a_nested_block() {
	mut b := Box{
		v: Val(1)
	}
	assert nested_block_assignment(mut b, true) == 'kept 14'
	assert nested_block_assignment(mut b, false) == "ended Val('str')"
	assert nested_block_assignment(mut b, true) == 'not int'
}

fn (mut b Box) set_through_receiver() int {
	if mut b.v is int {
		b.v = 40
		return b.v + 2
	}
	return 0
}

fn test_member_reached_through_a_pointer_receiver() {
	mut b := &Box{
		v: Val(1)
	}
	assert b.set_through_receiver() == 42
	assert b.v == Val(40)
}

fn test_nested_member() {
	mut h := Holder{
		box: Box{
			v: Val(1)
		}
	}
	if mut h.box.v is int {
		h.box.v = 3
		assert h.box.v * 2 == 6
	}
	assert h.box.v == Val(3)
}

fn test_replacing_the_parent_ends_the_narrowing_of_its_fields() {
	mut it := Item(Box{
		v: Val(1)
	})
	if mut it is Box {
		if mut it.v is int {
			it = Box{
				v: Val('s')
			}
			assert it.v == Val('s')
		}
	}
	assert '${it}' == "Item(Box{\n    v: Val('s')\n})"
}

fn match_set(mut b Box) string {
	match mut b.v {
		int {
			b.v = b.v * 10
			return 'int ${b.v + 1}'
		}
		string {
			b.v = b.v.len
			return 'len ${b.v}'
		}
	}
	return ''
}

fn test_match_branch() {
	mut b := Box{
		v: Val(2)
	}
	assert match_set(mut b) == 'int 21'
	b.v = 'hello'
	assert match_set(mut b) == 'len Val(5)'
}

fn count_up(mut b Box) int {
	mut n := 0
	for b.v is int && b.v < 5 {
		n++
		b.v = b.v + 1
		if b.v == 3 {
			n += 100
		}
	}
	return n
}

fn test_for_condition() {
	mut b := Box{
		v: Val(0)
	}
	assert count_up(mut b) == 105
	assert b.v == Val(5)
}

fn after_guard(mut b Box) string {
	if b.v !is int {
		return 'not int'
	}
	b.v = 5
	y := b.v + 1
	if b.v == 5 {
		return 'five ${y}'
	}
	return 'other'
}

fn test_after_a_guard() {
	mut b := Box{
		v: Val(1)
	}
	assert after_guard(mut b) == 'five 6'
	b.v = 'x'
	assert after_guard(mut b) == 'not int'
}
