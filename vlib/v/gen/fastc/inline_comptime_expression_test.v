module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_inline_comptime_if_keeps_enclosing_expression() {
	root := os.join_path(os.vtmp_dir(), 'fastc_inline_comptime_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for selected in [false, true] {
		mut prefs := pref.new_preferences()
		if selected {
			prefs.user_defines << 'selected'
		}
		c_source := generate('module main
fn add(a int, b int) int { return a + b }
fn main() {
	println(add(100, 2 + \$if selected ? { 40 } \$else { 9 }))
}
', 'inline_comptime_if.v', prefs) or { panic(err) }
		c_file := os.join_path(root, 'program.c')
		program := os.join_path(root, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', program, c_file])
		assert compiled.exit_code == 0, compiled.output
		executed := cmdexec.run(program, [])
		assert executed.exit_code == 0, executed.output
		assert executed.output == if selected { '142\n' } else { '111\n' }
	}
}

fn test_fastc_nested_if_guard_with_inline_comptime_suffix() {
	for selected in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = true
		if selected {
			prefs.user_defines << 'selected'
		}
		c_source := generate('module main
fn find_root() ?string { return "root" }
fn join(root string, suffix string) string { return root + suffix }
fn choose(enabled bool) string {
	fallback := "fallback"
	return if enabled {
		if current_root := find_root() {
			join(current_root, "compiler" + \$if selected ? { ".exe" } \$else { "" })
		} else {
			fallback
		}
	} else {
		fallback
	}
}
fn main() { println(choose(true)) }
', 'nested_if_guard_comptime_suffix.v', prefs) or { panic(err) }
		assert c_source.contains('Option main__find_root('), c_source
		assert c_source.contains('string current_root ='), c_source
		assert c_source.contains('".exe"') == selected, c_source
	}
}

fn test_fastc_nested_if_value_with_interpolation_condition() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main
fn choose(values map[string]string, module_name string) string {
	return if recorded := values["dir"] {
		if recorded == "\${module_name}_test" { "" } else { recorded }
	} else {
		""
	}
}
fn main() { println(choose({"dir": "demo"}, "demo")) }
', 'nested_if_interpolation_condition.v', prefs) or { panic(err) }
	assert c_source.contains('string recorded ='), c_source
	assert c_source.contains('builtin__string_eq('), c_source
}

fn test_fastc_comptime_statement_after_increment_keeps_statement_boundary() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.user_defines << 'selected'
	c_source := generate('module main
fn main() {
	mut depth := 0
	depth++
	\$if selected ? { depth++ }
	println(depth)
}
', 'comptime_statement_after_increment.v', prefs) or { panic(err) }
	assert c_source.count('depth++;') == 2, c_source
}
