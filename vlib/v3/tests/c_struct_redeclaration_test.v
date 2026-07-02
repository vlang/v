import os
import rand

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_struct_redeclaration_${os.getpid()}_${rand.ulid()}')
	build := os.execute('${vexe} -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn unique_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn run_v3_source(v3_bin string, name string, src string) os.Result {
	out := unique_temp_path(name)
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	return os.execute('${v3_bin} ${src_path} -b c -o ${out}')
}

fn test_c_struct_redeclaration_checks_field_signature() {
	v3_bin := build_v3()
	bad := run_v3_source(v3_bin, 'bad_c_struct_redeclaration',
		'struct C.dirent {\n\td_name [256]char\n}\n\nstruct C.dirent {\n\td_name byteptr\n}\n\nfn main() {}\n')
	assert bad.exit_code != 0, bad.output
	assert bad.output.contains('cannot redeclare C struct `C.dirent`'), bad.output
	assert !bad.output.contains('C compilation failed'), bad.output
	reordered := run_v3_source(v3_bin, 'bad_c_struct_reordered_fields',
		'struct C.OrderedPair {\n\tleft int\n\tright int\n}\n\nstruct C.OrderedPair {\n\tright int\n\tleft int\n}\n\nfn main() {}\n')
	assert reordered.exit_code != 0, reordered.output
	assert reordered.output.contains('cannot redeclare C struct `C.OrderedPair`'), reordered.output
	assert !reordered.output.contains('C compilation failed'), reordered.output

	good := run_v3_source(v3_bin, 'good_c_struct_redeclaration',
		'@[typedef]\nstruct C.XKeyEvent {\n\tkeycode u32\n\tstate u32\n}\n\nstruct C.XKeyEvent {\n\tkeycode u32\n\tstate u32\n}\n\n@[typedef]\nunion C.XClientMessageData {\nmut:\n\tb [20]u8\n\ts [10]i16\n\tl [5]i64\n}\n\nunion C.XClientMessageData {\nmut:\n\tl [5]i64\n}\n\n@[typedef]\nunion C.XEvent {\nmut:\n\ttype int\n\txkey C.XKeyEvent\n}\n\nunion C.XEvent {\nmut:\n\t@type int\n\txkey C.XKeyEvent\n}\n\nfn main() {}\n')
	assert good.exit_code == 0, good.output
	assert !good.output.contains('C compilation failed'), good.output
}
