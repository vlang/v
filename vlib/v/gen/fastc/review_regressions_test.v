module fastc

import os
import v.cmdexec
import v.pref

fn fastc_review_regression_matches_c(name string, source string, output string) {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_review_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == output, '${backend}: ${run.output}'
	}
}

fn test_fastc_function_field_nil_assignment_requires_unsafe() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source in [
		'module main\nstruct Handles { mut: callback fn () = unsafe { nil } }\nfn main() { mut handles := Handles{}; handles.callback = nil }\n',
		'module main\nstruct Handles { callback fn () = unsafe { nil } }\nfn main() { handles := Handles{callback: nil}; println(isnil(handles.callback)) }\n',
	] {
		mut message := ''
		_ := generate(source, 'callback_nil.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('`nil` outside an `unsafe` block'), message
	}
}

fn test_fastc_negated_mutable_sum_pointer_variant_preserves_nil() {
	fastc_review_regression_matches_c('pointer_sum', 'module main
type Value = int | voidptr
fn read(value Value) int { if value is int { return value }; return -1 }
fn main() {
	pointer := voidptr(unsafe { nil })
	mut value := Value(pointer)
	if value !is voidptr { return }
	println(isnil(value))
	println(usize(value))
	mut alternate := Value(pointer)
	if alternate !is voidptr {
		assert false
	} else {
		println(usize(alternate))
		alternate = Value(13)
	}
	println(read(alternate))
	value = Value(12)
	println(read(value))
	number := 7
	address := voidptr(&number)
	mut nonnil := Value(address)
	if nonnil !is voidptr { return }
	assert usize(nonnil) == usize(address)
	println("identity")
	mut scalar := Value(9)
	if scalar !is int { return }
	println(scalar + 1)
	println(1 + scalar)
	println((scalar + 1) * 2)
	println(scalar & 3)
	println(scalar << 1)
}
', 'true\n0\n0\n13\n12\nidentity\n10\n10\n20\n1\n18')
}

fn test_fastc_voidptr_alias_local_can_be_reset_to_nil() {
	fastc_review_regression_matches_c('voidptr_alias', 'module main
type Raw = voidptr
fn main() {
	value := 7
	mut pointer := Raw(voidptr(&value))
	pointer = nil
	println(isnil(pointer))
	println(isnil(if true { pointer } else { pointer }))
}
', 'true\ntrue')
}

fn test_fastc_constant_dependencies_ignore_inactive_comptime_branches() {
	fastc_review_regression_matches_c('conditional_constant', 'module main
fn initial() int { return 3 }
const a = $if windows { b } $else { initial() }
const b = a + 1
fn main() { println(b) }
', '4')
}

fn test_fastc_channel_runtime_import_accepts_tabs_after_chan() {
	fastc_review_regression_matches_c('tab_channel', 'module main\nfn main() {\n\tc := chan\tint{cap: 1}\n\tc <- 7\n\tvalue := <-c\n\tprintln(value)\n}\n', '7')
}
