module fastc

import v.pref

fn test_option_method_payload_survives_if_expression_local() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate(r'module main

struct Checker {}

struct Access {
	name string
	pos int
}

fn (checker Checker) write_access() ?Access {
	return Access{name: "write", pos: 1}
}

fn (checker Checker) read_access() ?Access {
	return Access{name: "read", pos: 2}
}

fn choose(checker Checker, write bool) string {
	selected := if write {
		checker.write_access()
	} else {
		checker.read_access()
	}
	if access := selected {
		return "${access.name}"
	}
	return "missing"
}

fn main() {
	_ := choose(Checker{}, true)
	_ := choose(Checker{}, false)
}
', 'option_method_if_expression.v', prefs) or { panic(err) }
	assert source.contains('Access access = *((Access *)'), source
	assert !source.contains('if (access:=selected)'), source
}

fn test_error_interface_none_field_default_uses_none_implementation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

interface IError {
	msg() string
}

struct None__ {}

fn (value None__) msg() string {
	return "none"
}

struct Holder {
	err IError = none
}

fn main() {
	value := Holder{}
	println(value.err.msg())
}
', 'error_interface_none_default.v', prefs) or { panic(err) }
	assert source.contains('._typ=__v_typeid_None__'), source
	assert !source.contains('._typ=__v_typeid_Option'), source
}

fn test_comptime_option_payload_survives_direct_and_parenthesized_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate(r'module main

struct Access {
	name string
}

fn read_access() ?Access {
	return Access{name: "read"}
}

fn main() {
	direct := $if macos { read_access() } $else { read_access() }
	wrapped := ($if macos { read_access() } $else { read_access() })
	if first := direct {
		println("${first.name}")
	}
	if second := wrapped {
		println("${second.name}")
	}
}
', 'comptime_option_expression.v', prefs) or { panic(err) }
	assert source.contains('Access first = *((Access *)'), source
	assert source.contains('Access second = *((Access *)'), source
}
