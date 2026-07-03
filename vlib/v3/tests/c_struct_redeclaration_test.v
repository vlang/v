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

fn run_v3_project(v3_bin string, name string, files map[string]string) os.Result {
	root := unique_temp_path('${name}_project')
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, src) or { panic(err) }
	}
	out := unique_temp_path(name)
	return os.execute('${v3_bin} ${root} -b c -o ${out}')
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

	alias_good := run_v3_source(v3_bin, 'good_c_struct_alias_field_redeclaration',
		'struct C.Foo {\n\tx byte\n}\n\nstruct C.Foo {\n\tx u8\n}\n\nfn main() {}\n')
	assert alias_good.exit_code == 0, alias_good.output
	assert !alias_good.output.contains('C compilation failed'), alias_good.output

	late_alias_good := run_v3_source(v3_bin, 'good_c_struct_late_alias_field_redeclaration',
		'struct C.LateAlias {\n\tx MyInt\n}\n\ntype MyInt = int\n\nstruct C.LateAlias {\n\tx int\n}\n\nfn main() {}\n')
	assert late_alias_good.exit_code == 0, late_alias_good.output
	assert !late_alias_good.output.contains('C compilation failed'), late_alias_good.output

	cross_file_bad := run_v3_project(v3_bin, 'bad_c_struct_cross_file_redeclaration', {
		'a.v': 'module main\n\nstruct C.Split {\n\tx int\n}\n\nfn main() {}\n'
		'b.v': 'module main\n\nstruct C.Split {\n\tx string\n}\n'
	})
	assert cross_file_bad.exit_code != 0, cross_file_bad.output
	assert cross_file_bad.output.contains('cannot redeclare C struct `C.Split`'), cross_file_bad.output
	assert !cross_file_bad.output.contains('C compilation failed'), cross_file_bad.output
}
