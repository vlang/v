import os

const channel_close_vexe = @VEXE
const channel_close_tests_dir = os.dir(@FILE)
const channel_close_v3_dir = os.dir(channel_close_tests_dir)
const channel_close_vlib_dir = os.dir(channel_close_v3_dir)
const channel_close_v3_src = os.join_path(channel_close_v3_dir, 'v3.v')

fn channel_close_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn channel_close_build_v3() string {
	v3_bin := channel_close_tmp_path('channel_close_codegen_test')
	build :=
		os.execute('${channel_close_vexe} -gc none -path "${channel_close_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${channel_close_v3_src}')
	if build.exit_code != 0 {
		panic(build.output)
	}
	return v3_bin
}

fn channel_close_run_good(v3_bin string, name string, src string) string {
	src_path := '${channel_close_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	bin_path := channel_close_tmp_path(name)
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	if compile.exit_code != 0 {
		panic('${name}: ${compile.output}')
	}
	if compile.output.contains('C compilation failed') {
		panic('${name}: ${compile.output}')
	}
	run := os.execute(bin_path)
	if run.exit_code != 0 {
		panic('${name}: ${run.output}')
	}
	return run.output.trim_space()
}

fn channel_close_gen_c(v3_bin string, name string, src string) string {
	src_path := '${channel_close_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	c_path := '${channel_close_tmp_path(name)}.c'
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
	if compile.exit_code != 0 {
		panic('${name}: ${compile.output}')
	}
	if !os.exists(c_path) {
		panic('${name}: missing generated C at ${c_path}')
	}
	return os.read_file(c_path) or { panic(err) }
}

fn test_channel_alias_close_method_wins_over_runtime_lowering() {
	v3_bin := channel_close_build_v3()
	out := channel_close_run_good(v3_bin, 'channel_alias_close_method',
		'type MyChan = chan int\n\nfn (c MyChan) close() int {\n\treturn 71\n}\n\nfn main() {\n\tch := MyChan(unsafe { nil })\n\tprintln(int_str(ch.close()))\n}\n')
	if out != '71' {
		panic('alias close output: ${out}')
	}
}

fn test_pointer_channel_close_lowers_to_runtime_with_error_array() {
	v3_bin := channel_close_build_v3()
	c_source := channel_close_gen_c(v3_bin, 'pointer_channel_close_runtime',
		'fn main() {\n\tmut ch := chan bool{cap: 1}\n\tp := &ch\n\tp.close()\n}\n')
	if !c_source.contains('sync__Channel__close(*p,') {
		panic('missing pointer channel close runtime call')
	}
	if !c_source.contains('sizeof(IError)') {
		panic('missing close error array')
	}
	if c_source.contains('sync__Channel__close(*p);') {
		panic('found one-arg pointer channel close')
	}
}
