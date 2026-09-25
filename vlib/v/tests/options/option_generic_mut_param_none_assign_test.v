// vtest vflags: -new-compiler
// Assigning `none` to a generic `mut` parameter whose concrete type is an Option must
// clear the caller's value (and generate valid C).
// See https://github.com/vlang/v/issues/28826
struct NoneAssignPoint {
	x int
	y int
}

struct NoneAssignHolder {
mut:
	value ?int
	name  ?string
}

fn clear_if_option[T](mut out T) {
	$if T is $option {
		out = none
	}
}

fn clear_option[T](mut out ?T) {
	out = none
}

fn clear_through[T](mut out T) {
	clear_if_option(mut out)
}

fn test_none_assign_to_generic_mut_option_param() {
	mut a := ?int(5)
	clear_if_option(mut a)
	assert a == none
	mut b := ?string('hi')
	clear_if_option(mut b)
	assert b == none
	mut c := ?NoneAssignPoint(NoneAssignPoint{1, 2})
	clear_if_option(mut c)
	assert c == none
	mut d := ?f64(1.5)
	clear_if_option(mut d)
	assert d == none
	mut e := ?[]int([1, 2])
	clear_if_option(mut e)
	assert e == none
}

fn test_none_assign_to_mut_option_of_generic_param() {
	mut a := ?int(3)
	clear_option(mut a)
	assert a == none
	mut b := ?string('hi')
	clear_option(mut b)
	assert b == none
}

fn test_none_assign_skips_non_option_types() {
	mut n := 7
	clear_if_option(mut n)
	assert n == 7
	mut s := 'kept'
	clear_if_option(mut s)
	assert s == 'kept'
}

fn test_none_assign_through_nested_generic_call_and_field() {
	mut a := ?int(9)
	clear_through(mut a)
	assert a == none
	mut holder := NoneAssignHolder{
		value: 1
		name:  'x'
	}
	clear_if_option(mut holder.value)
	clear_if_option(mut holder.name)
	assert holder.value == none
	assert holder.name == none
}
