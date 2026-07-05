import os

const global_decl_vexe = @VEXE
const global_decl_tests_dir = os.dir(@FILE)
const global_decl_v3_dir = os.dir(global_decl_tests_dir)
const global_decl_vlib_dir = os.dir(global_decl_v3_dir)
const global_decl_v3_src = os.join_path(global_decl_v3_dir, 'v3.v')

fn global_decl_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_global_decl_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${global_decl_vexe} -gc none -path "${global_decl_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${global_decl_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn global_decl_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_typed_global_initializers_in_group_keep_type_and_value() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'typed_global_initializers_in_group',
		'import sync.stdatomic\n\n__global (\n\tfirst_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)\n\tsecond_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)\n)\n\nfn main() {\n\tfirst_flag.store(true)\n\tprintln(first_flag.load())\n\tprintln(second_flag.load())\n\tsecond_flag.store(true)\n\tprintln(second_flag.load())\n}\n')
	assert out == 'true\nfalse\ntrue'
}
