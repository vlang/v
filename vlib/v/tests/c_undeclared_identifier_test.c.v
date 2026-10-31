#include "@DIR/c_undeclared_identifier_test.h"

// V declares none of the C variables that this file reads: each takes the type
// that the context of its first use expects, and is passed as it is written.

struct C.v_undeclared_point {
mut:
	x int
	y int
}

struct Holder {
	text  &char = unsafe { nil }
	count int
}

fn text_of(p &char) string {
	return unsafe { cstring_to_vstring(p) }
}

fn untyped_text_of(p voidptr) string {
	return unsafe { cstring_to_vstring(&char(p)) }
}

fn twice(n int) int {
	return n * 2
}

fn half(x f64) f64 {
	return x / 2
}

fn move_right(mut p C.v_undeclared_point) {
	p.x++
}

fn sum_of(p &C.v_undeclared_point) int {
	return p.x + p.y
}

fn header_text() &char {
	return C.v_undeclared_text
}

// No checked body reads the C variables of this one.
fn generic_texts[T](x T) []string {
	return [text_of(C.v_undeclared_generic_text), untyped_text_of(C.v_undeclared_generic_name),
		'${x}']
}

fn test_pointer_variable_is_passed_to_a_v_function_as_it_is() {
	assert unsafe { cstring_to_vstring(C.v_undeclared_text) } == 'hello from the header'
	assert text_of(C.v_undeclared_text) == 'hello from the header'
	assert untyped_text_of(C.v_undeclared_text) == 'hello from the header'
	// `Mmm dd yyyy hh:mm:ss`
	assert text_of(C.v_undeclared_build).len == 20
	assert text_of(C.v_undeclared_chars) == 'array text'
}

fn test_identifier_keeps_the_type_of_its_first_use() {
	assert twice(C.v_undeclared_count) == 84
	count := C.v_undeclared_count
	assert count == 42
	assert C.v_undeclared_count + 1 == 43
	assert half(C.v_undeclared_ratio) == 0.75
}

fn test_identifier_in_other_typed_contexts() {
	holder := Holder{
		text:  C.v_undeclared_text
		count: C.v_undeclared_count
	}
	assert text_of(holder.text) == 'hello from the header'
	assert holder.count == 42
	assert text_of(header_text()) == 'hello from the header'
	mut text := &char(unsafe { nil })
	text = C.v_undeclared_chars
	assert text_of(text) == 'array text'
}

fn test_local_with_the_name_of_the_identifier_does_not_hide_it() {
	v_undeclared_text := 'local'
	assert text_of(C.v_undeclared_text) == 'hello from the header'
	assert v_undeclared_text == 'local'
}

fn test_mut_argument_takes_the_address_of_the_object() {
	move_right(mut C.v_undeclared_origin)
	assert C.v_undeclared_origin.x == 4
	assert sum_of(&C.v_undeclared_origin) == 8
}

fn test_identifier_in_a_generic_function() {
	assert generic_texts(1) == ['generic text', 'generic name', '1']
	assert generic_texts('a') == ['generic text', 'generic name', 'a']
}
