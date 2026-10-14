module fastc

import os
import v.cmdexec

fn test_fastc_nested_or_preserves_outer_fallback_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_or_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Item {
	amount int
}

type Value = Item | int

struct Calls {
mut:
	count int
}

fn resolve_type(mut calls Calls) Value {
	calls.count++
	return Value(3)
}

fn touch(mut calls Calls) int {
	calls.count++
	return calls.count
}

fn local_fallback(payload Value) Value {
	return payload
}

fn option_value(present bool, payload Value) ?Value {
	if present {
		return payload
	}
	return none
}

fn result_value(present bool, payload Value) !Value {
	if present {
		return payload
	}
	return error("missing value")
}

fn choose_option(mut calls Calls, outer bool, inner bool) Value {
	return option_value(outer, Value(Item{amount: 10})) or {
		actual := resolve_type(mut calls)
		option_value(inner, Value(Item{amount: 20})) or { actual }
	}
}

fn choose_result(mut calls Calls, outer bool, inner bool) Value {
	return result_value(outer, Value(Item{amount: 10})) or {
		actual := resolve_type(mut calls)
		result_value(inner, Value(Item{amount: 20})) or { actual }
	}
}

fn choose_call(mut calls Calls, present bool) Value {
	return result_value(present, Value(Item{amount: 10})) or {
		actual := resolve_type(mut calls)
		touch(mut calls)
		local_fallback(actual)
	}
}

fn touch_value(mut calls Calls, payload Value, index int) int {
	calls.count += score(payload) + index
	return calls.count
}

fn choose_scoped_calls(mut calls Calls, present bool) Value {
	return result_value(present, Value(Item{amount: 10})) or {
		actual := resolve_type(mut calls)
		if true {
			field := actual
			touch_value(mut calls, field, 4)
		}
		for i in 0 .. 2 {
			for j in 0 .. 2 {
				field := actual
				touch_value(mut calls, field, i + j)
			}
		}
		local_fallback(actual)
	}
}

fn score(value Value) int {
	return match value {
		Item { value.amount }
		int { value }
	}
}

fn main() {
	mut option_calls := Calls{}
	assert score(choose_option(mut option_calls, true, false)) == 10
	assert option_calls.count == 0
	assert score(choose_option(mut option_calls, false, true)) == 20
	assert option_calls.count == 1
	assert score(choose_option(mut option_calls, false, false)) == 3
	assert option_calls.count == 2
	mut result_calls := Calls{}
	assert score(choose_result(mut result_calls, true, false)) == 10
	assert result_calls.count == 0
	assert score(choose_result(mut result_calls, false, true)) == 20
	assert result_calls.count == 1
	assert score(choose_result(mut result_calls, false, false)) == 3
	assert result_calls.count == 2
	mut call_calls := Calls{}
	assert score(choose_call(mut call_calls, true)) == 10
	assert call_calls.count == 0
	assert score(choose_call(mut call_calls, false)) == 3
	assert call_calls.count == 2
	mut scope_calls := Calls{}
	assert score(choose_scoped_calls(mut scope_calls, true)) == 10
	assert scope_calls.count == 0
	assert score(choose_scoped_calls(mut scope_calls, false)) == 3
	assert scope_calls.count == 24
	println("nested fallback works")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'nested fallback works', run.output
}
