import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const repo_dir = os.dir(vlib_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')
const compiler_src_dir = os.join_path(repo_dir, 'cmd', 'v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_post_merge_review_fixes_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(good_bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_compiler_vexe_env_uses_running_executable() {
	v3_bin := build_v3()
	c_out := os.join_path(os.temp_dir(), 'v3_review_vexe.c')
	os.rm(c_out) or {}
	gen := os.execute('${v3_bin} -o ${c_out} ${compiler_src_dir}')
	assert gen.exit_code == 0, gen.output
	assert os.exists(c_out)
	c_source := os.read_file(c_out) or { panic(err) }
	assert !c_source.contains('v3_vexe_target')
	assert !c_source.contains('fopen(v3_src')
	assert c_source.contains('snprintf(v3_checkout_vexe, sizeof(v3_checkout_vexe),')
	assert c_source.contains('if (access(v3_checkout_vexe, F_OK) == 0) v3_vexe = v3_checkout_vexe;')
	assert c_source.contains('const char* v3_vexe = v3_src_real_result != NULL ? v3_src_real : v3_arg0;')
	assert c_source.contains('_putenv_s("VEXE", v3_vexe);')
	assert c_source.contains('setenv("VEXE", v3_vexe, 1);')
}

fn test_assoc_return_runs_defers() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'assoc_return_runs_defers',
		'struct Point {\n\tx int\n\ty int\n}\n\n__global hit int\n\nfn make_point() Point {\n\tbase := Point{\n\t\tx: 1\n\t\ty: 2\n\t}\n\tdefer {\n\t\thit = 7\n\t}\n\treturn Point{\n\t\t...base\n\t\tx: 5\n\t}\n}\n\nfn main() {\n\tp := make_point()\n\tprintln(int_str(p.x))\n\tprintln(int_str(hit))\n}\n')
	assert out == '5\n7'
}

fn test_qualified_enum_str_requires_exact_receiver() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'qualified_enum_str_exact_receiver', {
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.Color.red.str())\n\tprintln(modb.Color.blue.str())\n}\n'
		'moda/moda.v': 'module moda\n\npub enum Color {\n\tred\n}\n'
		'modb/modb.v': "module modb\n\npub enum Color {\n\tblue\n}\n\npub fn (c Color) str() string {\n\treturn 'custom'\n}\n"
	}, 'main.v')
	assert out == 'red\ncustom'
}
