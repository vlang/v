import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_join_path_markused_${os.getpid()}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_join_path_lowering_roots_join_path_single() {
	v3_bin := build_v3()
	src := os.join_path(os.temp_dir(), 'v3_join_path_markused_input_${os.getpid()}.v')
	os.write_file(src, "import os

fn main() {
	path := os.join_path('a', 'b', 'c')
	expected := 'a' + os.path_separator + 'b' + os.path_separator + 'c'
	assert path == expected
	println(path)
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_join_path_markused_input_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('os__join_path_single'), generated

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	expected := 'a' + os.path_separator + 'b' + os.path_separator + 'c'
	assert run.output.trim_space() == expected
}
