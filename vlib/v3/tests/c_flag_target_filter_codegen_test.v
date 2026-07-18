import os
import v3.pref

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_c_backend_only_runs_generic_and_c_backend_test_files() {
	assert pref.is_test_file_for_backend('/tmp/basic_test.v', 'c')
	assert pref.is_test_file_for_backend('/tmp/basic_test.c.v', 'c')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.js.v', 'c')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.arm64.v', 'c')
	assert !pref.is_test_file_for_backend('/tmp/basic_test.amd64.v', 'c')

	assert pref.is_test_file_for_backend('/tmp/basic_test.arm64.v', 'arm64')
	assert pref.is_test_file_for_backend('/tmp/basic_test.amd64.v', 'amd64')
}

fn test_c_flag_target_filter_keeps_host_linux_and_drops_wasm() {
	$if !linux {
		return
	}
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_flag_target_filter_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_flag_target_filter_input_${pid}.v')
	os.write_file(src,
		"#flag linux -ldl\n#flag termux -L/data/data/com.termux/files/usr/lib -liconv\n#flag qnx qnx_should_not_leak\n#flag haiku haiku_should_not_leak\n#flag serenity serenity_should_not_leak\n#flag vinix vinix_should_not_leak\n#flag wasm32_emscripten --embed-file @VEXEROOT/README.md@/README.md\nfn main() {\n\tprintln('flag-filter-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_flag_target_filter_input_${pid}')
	os.rm(bin) or {}
	os.rmdir_all(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
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

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'flag-filter-ok'
}

fn test_c_flag_target_filter_drops_termux_off_termux() {
	$if termux {
		return
	}
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_flag_termux_filter_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_c_flag_termux_filter_input_${pid}.v')
	os.write_file(src,
		"#flag termux termux_should_not_leak_to_linker\nfn main() {\n\tprintln('termux-filter-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_c_flag_termux_filter_input_${pid}')
	os.rm(bin) or {}
	os.rmdir_all(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('termux_should_not_leak_to_linker'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'termux-filter-ok'
}

fn test_objective_c_flags_skip_tcc_and_select_objective_c_language() {
	$if !macos {
		return
	}
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_objective_c_flag_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_objective_c_flag_input_${pid}.v')
	os.write_file(src, "#flag darwin -fobjc-arc\nfn main() {\n\tprintln('objc-flag-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_objective_c_flag_input_${pid}')
	os.rm(bin) or {}
	os.rmdir_all(bin) or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	assert compile.output.contains('-x objective-c'), compile.output
	assert compile.output.contains('-x none'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'objc-flag-ok'
}

fn test_direct_objective_c_source_flag_skips_tcc() {
	$if !macos {
		return
	}
	pid := os.getpid()
	root := os.join_path(os.vtmp_dir(), 'v3_direct_objective_c_source_${pid}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3_direct_objective_c_driver')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
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
	compile := os.execute('${v3_bin} -nocache -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('tcc.exe'), compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '69'
}
