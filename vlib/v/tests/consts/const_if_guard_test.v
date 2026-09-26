// https://github.com/vlang/v/issues/28944
import os
import if_guard_mod

struct Point {
	x int
	y int
}

fn opt_str(s string) ?string {
	if s == '' {
		return none
	}
	return s
}

fn res_int(ok bool) !int {
	if ok {
		return 42
	}
	return error('no int')
}

fn opt_point(ok bool) ?Point {
	if ok {
		return Point{1, 2}
	}
	return none
}

fn add_one(x int) int {
	return x + 1
}

fn opt_pair(ok bool) ?(int, string) {
	if ok {
		return 7, 'seven'
	}
	return none
}

const env_value = if envver := os.getenv_opt('V_ISSUE_28944_SURELY_UNSET_ENV_VAR') {
	envver
} else {
	'zzz'
}

const opt_some = if v := opt_str('abc') { v } else { 'none' }
const opt_none = if v := opt_str('') { v } else { 'none' }
const opt_discard = if _ := opt_str('abc') { 'some' } else { 'none' }
const opt_mut = if mut v := opt_str('abc') {
	v += '!'
	v
} else {
	'none'
}

const res_ok = if v := res_int(true) { v } else { -1 }
const res_err = if v := res_int(false) { v } else { -1 }
const res_err_msg = if v := res_int(false) { v.str() } else { err.msg() }

const point_some = if p := opt_point(true) { p } else { Point{} }
const point_none = if p := opt_point(false) { p } else { Point{7, 8} }

const pair = if a, b := opt_pair(true) { '${a}:${b}' } else { 'none' }

const lookup = {
	'k': 5
}
const map_hit = if v := lookup['k'] { v } else { 0 }
const map_miss = if v := lookup['missing'] { v } else { -3 }

const numbers = [10, 20, 30]
const array_hit = if v := numbers[1] { v } else { -1 }
const array_miss = if v := numbers[5] { v } else { -1 }

const else_if_second = if a := opt_str('') {
	a
} else if b := opt_str('second') {
	b + '!'
} else {
	'none'
}
const else_if_first = if a := opt_str('first') {
	a
} else if b := opt_str('second') {
	b
} else {
	'none'
}
const else_if_none = if a := opt_str('') {
	a
} else if b := opt_str('') {
	b
} else {
	'none'
}
const else_if_plain = if a := opt_str('') {
	a
} else if opt_some.len == 3 {
	'plain'
} else {
	'none'
}

const in_array = [0, if v := res_int(true) { v } else { -1 }]
const in_call = add_one(if v := res_int(true) { v } else { 0 })
const in_interpolation = 'v=${if v := opt_str('x') { v } else { 'none' }}'
const depends_on_guard_const = opt_some + '/' + env_value

fn test_issue_repro() {
	assert env_value == 'zzz'
}

fn test_option_guards() {
	assert opt_some == 'abc'
	assert opt_none == 'none'
	assert opt_discard == 'some'
	assert opt_mut == 'abc!'
}

fn test_result_guards() {
	assert res_ok == 42
	assert res_err == -1
	assert res_err_msg == 'no int'
	println(res_ok)
}

fn test_struct_payload() {
	assert point_some == Point{1, 2}
	assert point_none == Point{7, 8}
	assert point_some.x + point_none.y == 9
}

fn test_multi_return_payload() {
	assert pair == '7:seven'
}

fn test_map_and_array_index_guards() {
	assert map_hit == 5
	assert map_miss == -3
	assert array_hit == 20
	assert array_miss == -1
}

fn test_else_if_guard_chains() {
	assert else_if_second == 'second!'
	assert else_if_first == 'first'
	assert else_if_none == 'none'
	assert else_if_plain == 'plain'
}

fn test_nested_guards() {
	assert in_array == [0, 42]
	assert in_call == 43
	assert in_interpolation == 'v=x'
	assert depends_on_guard_const == 'abc/zzz'
}

fn test_module_consts() {
	assert if_guard_mod.env_value == 'module default'
	assert if_guard_mod.doubled == 22
}
