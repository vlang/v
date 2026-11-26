import os
import v.pref
import v.tempname

fn test_c_backend_only_runs_generic_and_c_backend_test_files() {
	assert pref.is_test_file_for_backend('/tmp/basic_test.v', 'c')
	assert pref.is_test_file_for_backend('/tmp/basic_test.c.v', 'c')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.js.v', 'c')
	assert pref.is_test_file_for_backend('/tmp/basic_test.arm64.v', 'c')
	assert pref.is_test_file_for_backend('/tmp/basic_test.amd64.v', 'c')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.arm64.v', 'wasm')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.amd64.v', 'native')
}

fn test_c_flag_target_filter_keeps_host_linux_and_drops_wasm() {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_c_flag_target_filter_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_c_flag_target_filter_input.v')
	os.write_file(src,
		"#flag linux -ldl\n#flag termux -L/data/data/com.termux/files/usr/lib -liconv\n#flag qnx qnx_should_not_leak\n#flag haiku haiku_should_not_leak\n#flag serenity serenity_should_not_leak\n#flag vinix vinix_should_not_leak\n#flag wasm32_emscripten --embed-file @VEXEROOT/README.md@/README.md\nfn main() {\n\tprintln('flag-filter-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(root, 'v3_c_flag_target_filter_input')
	compile := os.exec([@VEXE, '-showcc', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('-ldl'), compile.output
	assert !compile.output.contains('termux'), compile.output
	assert !compile.output.contains('/data/data/com.termux'), compile.output
	assert !compile.output.contains('qnx_should_not_leak'), compile.output
	assert !compile.output.contains('haiku_should_not_leak'), compile.output
	assert !compile.output.contains('serenity_should_not_leak'), compile.output
	assert !compile.output.contains('vinix_should_not_leak'), compile.output
	assert !compile.output.contains('wasm32_emscripten'), compile.output
	assert !compile.output.contains('--embed-file'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'flag-filter-ok'
}

fn test_c_flag_target_filter_drops_termux_off_termux() {
	$if termux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_c_flag_termux_filter_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_c_flag_termux_filter_input.v')
	os.write_file(src,
		"#flag termux termux_should_not_leak_to_linker\nfn main() {\n\tprintln('termux-filter-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(root, 'v3_c_flag_termux_filter_input')
	compile := os.exec([@VEXE, '-showcc', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('termux_should_not_leak_to_linker'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'termux-filter-ok'
}

fn test_objective_c_flags_are_applied_to_standalone_build() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_objective_c_flag_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_objective_c_flag_input.v')
	os.write_file(src,
		"#flag darwin -fobjc-arc\n#flag darwin -lobjc\n#include <objc/message.h>\ntype Msg = fn (voidptr, voidptr) voidptr\nfn C.objc_msgSend(obj voidptr, sel voidptr) voidptr\nfn main() {\n\t_ := unsafe { Msg(C.objc_msgSend) }\n\tprintln('objc-flag-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(root, 'v3_objective_c_flag_input')
	compile := os.exec([@VEXE, '-no-memory-limit', '-showcc', '-b', 'c', '-o', bin, '${src}'])
	assert compile.exit_code == 0, compile.output
	cc_lines := compile.output.split_into_lines().filter(it.contains('  > '))
	assert cc_lines.len == 1, compile.output
	cc_line := cc_lines[0]
	assert cc_line.contains('-fobjc-arc'), cc_line
	assert cc_line.contains('-x objective-c'), cc_line
	assert !compile.output.contains('C dylib cache'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'objc-flag-ok'
}

fn test_direct_objective_c_source_flag_skips_tcc() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_direct_objective_c_source_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'shim.m'), '@interface V3DirectObjectiveC
+ (int)answer;
@end
@implementation V3DirectObjectiveC
+ (int)answer { return 69; }
@end
int v3_direct_objective_c_answer(void) { return [V3DirectObjectiveC answer]; }
') or {
		panic(err)
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

#flag @DIR/shim.m
#flag darwin -lobjc

fn C.v3_direct_objective_c_answer() int

fn main() {
	println(C.v3_direct_objective_c_answer())
}
') or {
		panic(err)
	}
	output := os.join_path(root, 'direct_objective_c_source')
	compile := os.exec([@VEXE, '-nocache', '-o', output, source])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '69'
}
