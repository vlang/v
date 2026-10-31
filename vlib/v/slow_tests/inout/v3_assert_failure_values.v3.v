struct Point {
	x int
	y int
}

struct Name {
	value string
}

fn (n Name) str() string {
	return 'Name(${n.value})'
}

enum Color {
	red
	green
}

fn foo() string {
	return 'zzz'
}

fn next(mut calls []string) string {
	calls << 'next'
	return 'call ${calls.len}'
}

@[assert_continues]
fn check(mut values []int) {
	mut calls := []string{}
	assert next(mut calls) == 'call 2', 'calls: ${calls}'
	assert Point{1, 2} == Point{1, 3}
	assert Name{'a'} != Name{'a'}
	assert values == [1, 2]
	assert values.len > 5
	color := Color.red
	assert color == .green
	ratio := 0.5
	assert ratio >= 1.5
	ok := false
	assert ok, 'ok should be true'
}

fn main() {
	mut values := [3]
	check(mut values)
	assert foo() == 'www'
	println('unreachable')
}
