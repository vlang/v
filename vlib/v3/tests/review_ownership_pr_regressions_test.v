import os

const review_pr_vexe = @VEXE
const review_pr_tests_dir = os.dir(@FILE)
const review_pr_v3_dir = os.dir(review_pr_tests_dir)
const review_pr_vlib_dir = os.dir(review_pr_v3_dir)
const review_pr_v3_src = os.join_path(review_pr_v3_dir, 'v3.v')

fn review_pr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_pr_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${review_pr_vexe} -gc none -path "${review_pr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${review_pr_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn review_pr_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn review_pr_run_project(v3_bin string, name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		review_pr_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_imported_decl_param_uses_callee_module_for_unqualified_type() {
	v3_bin := review_pr_build_v3()
	out := review_pr_run_project(v3_bin, 'imported_decl_param_module', {
		'callee/callee.v': 'module callee\n\npub struct S {\npub:\n\tn int\n}\n\npub fn take(s S) int {\n\treturn s.n + 1\n}\n'
		'main.v':          'module main\n\nimport callee\n\nstruct S {\n\ttext string\n}\n\nfn main() {\n\tprintln(int_str(callee.take(callee.S{\n\t\tn: 4\n\t})))\n}\n'
	})
	assert out == '5'
}

fn test_imported_decl_param_uses_callee_file_selective_import() {
	v3_bin := review_pr_build_v3()
	out := review_pr_run_project(v3_bin, 'imported_decl_param_selective_import', {
		'other/other.v':   'module other\n\npub struct S {\npub:\n\tn int\n}\n'
		'callee/callee.v': 'module callee\n\nimport other { S }\n\npub fn take(s S) int {\n\treturn s.n + 3\n}\n'
		'main.v':          'module main\n\nimport callee\nimport other\n\nstruct S {\n\ttext string\n}\n\nfn main() {\n\tprintln(int_str(callee.take(other.S{\n\t\tn: 4\n\t})))\n}\n'
	})
	assert out == '7'
}

fn test_value_new_chain_uses_checked_return_type_for_generic_receiver() {
	v3_bin := review_pr_build_v3()
	out := review_pr_run_project(v3_bin, 'generic_value_new_chain', {
		'factory/factory.v': 'module factory\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n\npub fn (box Box[T]) get() T {\n\treturn box.value\n}\n\npub struct Factory {}\n\npub fn (factory Factory) new() Box[int] {\n\t_ = factory\n\treturn Box[int]{\n\t\tvalue: 23\n\t}\n}\n'
		'main.v':            'module main\n\nimport factory\n\nfn main() {\n\tfactory := factory.Factory{}\n\tprintln(int_str(factory.new().get()))\n}\n'
	})
	assert out == '23'
}

fn test_fn_ptr_typedef_keeps_concrete_upper_suffix_type() {
	v3_bin := review_pr_build_v3()
	out := review_pr_run_project(v3_bin, 'fn_ptr_concrete_upper_suffix_type', {
		'main.v': 'module main\n\nstruct Foo_T {\n\tn int\n}\n\nfn apply(cb fn (Foo_T) int) int {\n\treturn cb(Foo_T{\n\t\tn: 8\n\t})\n}\n\nfn take(foo Foo_T) int {\n\treturn foo.n + 2\n}\n\nfn main() {\n\tprintln(int_str(apply(take)))\n}\n'
	})
	assert out == '10'
}
