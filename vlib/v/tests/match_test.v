enum Color {
	red
	green
	blue
}

pub fn (c Color) str() string {
	return 'tmp'
}

fn test_match_integers() {
	mut a := 3
	mut b := 0
	match a
	2 {
		println('two')
	}
	3 {
		println('three')
		b = 3
	}
	4 {
		println('four')
	}
	else {
		println('???')
	}
	assert b == 3
	assert match 2 {
		1 { 2 }
		2 { 3 }
		else { 5 }
	} == 3
	assert match 0 {
		1 { 2 }
		2 { 3 }
		else { 5 }
	} == 5
	assert match 1 {
		else { 5 }
	} == 5
	a = 0
	match 2 {
		0 {
			a = 1
		}
		1 {
			a = 2
		}
		else {
			a = 3
			println('a is $a')
		}
	}
	assert a == 3
	a = 0
	match 1 {
		0 {
			a = 1
		}
		1 {
			a = 2
			a = a + 2
			a = a + 2
		}
		else {}
	}
	assert a == 6
	a = 0
	match 1 {
		else { a = -2 }
	}
	assert a == -2
}

fn test_match_multiple() {
	assert match 9 {
		1, 2, 3 { '1-3' }
		4, 5 { '4-5' }
		6...9 { '6-9' }
		else { 'other' }
	} == '6-9'
}

fn test_match_range() {
	assert match `f` {
		`0`...`9` { 'digit' }
		`A`...`Z` { 'uppercase' }
		`a`...`z` { 'lowercase' }
		else { 'other' }
	} == 'lowercase'
}

fn test_match_enums() {
	mut b := Color.red
	match b {
		.red {
			b = .green
		}
		.green {
			b = .blue
		}
		else {
			println('b is $b.str()')
			b = .red
		}
	}
	assert b == .green
	match b {
		.red {
			b = .green
		}
		else {
			println('b is $b.str()')
			b = .blue
		}
	}
	assert b == .blue
}

struct Counter {
mut:
	val int
}

fn (mut c Counter) next() int {
	c.val++
	return c.val
}

fn test_method_call() {
	mut c := Counter{
		val: 1
	}
	assert match c.next() {
		1 { false }
		2 { true }
		3 { false }
		else { false }
	}
}

type Sum = A1 | B1

struct A1 {
	pos int
}

struct B1 {
	val string
}

fn f(s Sum) string {
	match s {
		A1 { return typeof(s).name }
		B1 { return '' }
	}
	return ''
}

fn test_sum_type_name() {
	a := A1{
		pos: 22
	}
	assert f(a) == 'A1'
}

fn f_else(s Sum) string {
	match s {
		A1 { return typeof(s).name }
		else { return '' }
	}
}

fn test_sum_type_else() {
	a := A1{
		pos: 22
	}
	assert f_else(a) == 'A1'
}

struct Alfa {
	char rune = `a`
}

fn (a Alfa) letter() rune {
	return a.char
}

struct Bravo {
	// A field so that Alfa and Bravo structures aren't the same
	dummy_field int
	char        rune = `b`
}

fn (b Bravo) letter() rune {
	return b.char
}

struct Charlie {
	char rune = `c`
}

type NATOAlphabet = Alfa | Bravo | Charlie

fn test_match_sumtype_multiple_types() {
	a := Alfa{}
	l := NATOAlphabet(a)
	match l {
		Alfa, Bravo {
			assert l.char == `a`
			// TODO make methods work
			// assert l.letter() == `a`
		}
		Charlie {
			assert false
		}
	}
	// test one branch
	match l {
		Alfa, Bravo, Charlie {
			assert l.char == `a`
		}
	}
}

fn test_sub_expression() {
	b := false && match 1 {0 {true} else {true}}
	assert !b
	c := true || match 1 {0 {false} else {false}}
	assert c
}

const (
	one = 'one'
)

fn test_match_constant_string() {
	match one {
		one {
			assert true
		}
		else {
			assert false
		}
	}
}

type WithArray = []WithArray | int

fn test_sumtype_with_array() {
	fa := [WithArray(0)]
	f := WithArray(fa)
	match f {
		[]WithArray {
			assert true
		}
		int {
			assert false
		}
	}
	match f {
		int {
			assert false
		}
		[]WithArray {
			assert true
		}
	}
}

fn test_match_expression_add() {
	a := match true { true {1} false {2} } + 3
	assert a == 4
}
