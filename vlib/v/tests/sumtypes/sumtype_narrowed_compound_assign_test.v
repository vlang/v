// `++`, `--` and compound assignments update the variant a sum type value is
// narrowed to, and leave it narrowed.
type Val = int | string

struct Box {
mut:
	v Val
}

fn test_member_increment_and_decrement() {
	mut b := Box{
		v: Val(1)
	}
	if mut b.v is int {
		b.v++
		assert b.v == 2
		b.v--
		b.v--
		assert b.v == 0
	}
	assert b.v == Val(0)
}

fn test_local_increment_and_decrement() {
	mut x := Val(1)
	if mut x is int {
		x++
		x++
		assert x == 3
		x--
		assert x + 0 == 2
	}
	assert x == Val(2)
}

fn test_member_compound_assignment() {
	mut b := Box{
		v: Val(1)
	}
	if mut b.v is int {
		b.v += 2
		assert b.v == 3
		assert '${b.v}' == '3'
		b.v -= 1
		b.v *= 10
		b.v /= 4
		b.v %= 3
		assert b.v == 2
		b.v <<= 3
		b.v |= 1
		assert b.v + 0 == 17
	}
	assert b.v == Val(17)
}

fn test_local_compound_assignment() {
	mut x := Val(1)
	if mut x is int {
		x += 10
		x--
		assert x == 10
		assert x * 2 == 20
	}
	assert x == Val(10)
}

fn test_string_variant_concatenation() {
	mut s := Val('a')
	if mut s is string {
		s += 'b'
		assert s.len == 2
	}
	assert s == Val('ab')
	mut b := Box{
		v: Val('x')
	}
	if mut b.v is string {
		b.v += 'y'
		assert b.v.len == 2
		assert b.v == 'xy'
	}
	assert b.v == Val('xy')
}

fn (mut b Box) bump(n int) {
	if mut b.v is int {
		b.v++
		b.v += n
	}
}

fn bump_param(mut b Box) {
	if mut b.v is int {
		b.v -= 5
	}
}

fn test_member_reached_through_a_pointer() {
	mut b := &Box{
		v: Val(5)
	}
	if mut b.v is int {
		b.v++
		b.v += 2
	}
	assert b.v == Val(8)
	b.bump(10)
	assert b.v == Val(19)
	bump_param(mut b)
	assert b.v == Val(14)
	mut c := Box{
		v: Val(0)
	}
	c.bump(1)
	assert c.v == Val(2)
}

fn test_match_branch() {
	mut b := Box{
		v: Val(4)
	}
	match mut b.v {
		int {
			b.v *= 3
			assert b.v - 2 == 10
		}
		string {
			b.v += '!'
		}
	}
	assert b.v == Val(12)
}
