module fastc

import v.pref

fn test_option_and_result_use_distinct_return_types() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main
struct Result { number int }
struct IError { message string }
fn (e IError) msg() string { return e.message }
fn error(message string) IError { return IError{message} }
fn optional(n int) ?int {
	if n == 0 { return none }
	return n
}
fn result(n int) !int {
	if n == 0 { return error("failed") }
	return n
}
fn main() {
	_ := Result{number: 7}
	_ := optional(0) or { 42 }
	_ := result(0) or { println(err.msg()); 42 }
}
', 'option_layout.v', prefs)!
	assert source.contains('Option optional('), source
	assert source.contains('__v_result result('), source
	assert source.contains('return (__v_result){.err='), source
	assert source.contains('return (__v_result){.data='), source
	assert !source.contains('(Option){.err='), source
}
