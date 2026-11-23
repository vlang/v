import os
import v.tempname

const review_pr_vexe = @VEXE
const review_pr_tests_dir = os.dir(@FILE)
const review_pr_v3_dir = os.dir(review_pr_tests_dir)
const review_pr_vlib_dir = os.dir(review_pr_v3_dir)
const review_pr_v3_src = os.join_path(review_pr_v3_dir, 'v.v')

const review_pr_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_review_pr_fixtures_${tempname.unique_token()}')
const review_pr_compiler = os.join_path(review_pr_fixture_dir, 'v3') + $if windows { '.exe' } $else { '' }

fn testsuite_begin() {
	os.mkdir_all(review_pr_fixture_dir) or { panic(err) }
	build :=
		os.exec([review_pr_vexe, '-gc', 'none', '-path', '${review_pr_vlib_dir}' + '|@vlib|@vmodules',
			'-o', review_pr_compiler, '${review_pr_v3_src}'])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(review_pr_fixture_dir) or {}
}

fn review_pr_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn review_pr_run_project(name string, files map[string]string) string {
	root := os.join_path(review_pr_fixture_dir, 'v3_${name}_project')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		review_pr_write_file(root, rel, source)
	}
	bin := os.join_path(review_pr_fixture_dir, 'v3_${name}')
	compile := os.exec([review_pr_compiler, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_imported_decl_param_uses_callee_module_for_unqualified_type() {
	out := review_pr_run_project('imported_decl_param_module', {
		'callee/callee.v': 'module callee\n\npub struct S {\npub:\n\tn int\n}\n\npub fn take(s S) int {\n\treturn s.n + 1\n}\n'
		'main.v':          'module main\n\nimport callee\n\nstruct S {\n\ttext string\n}\n\nfn main() {\n\tprintln(int_str(callee.take(callee.S{\n\t\tn: 4\n\t})))\n}\n'
	})
	assert out == '5'
}

fn test_imported_decl_param_uses_callee_file_selective_import() {
	out := review_pr_run_project('imported_decl_param_selective_import', {
		'other/other.v':   'module other\n\npub struct S {\npub:\n\tn int\n}\n'
		'callee/callee.v': 'module callee\n\nimport other { S }\n\npub fn take(s S) int {\n\treturn s.n + 3\n}\n'
		'main.v':          'module main\n\nimport callee\nimport other\n\nstruct S {\n\ttext string\n}\n\nfn main() {\n\tprintln(int_str(callee.take(other.S{\n\t\tn: 4\n\t})))\n}\n'
	})
	assert out == '7'
}

fn test_value_new_chain_uses_checked_return_type_for_generic_receiver() {
	out := review_pr_run_project('generic_value_new_chain', {
		'factory/factory.v': 'module factory\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n\npub fn (box Box[T]) get() T {\n\treturn box.value\n}\n\npub struct Factory {}\n\npub fn (builder Factory) new() Box[int] {\n\t_ = builder\n\treturn Box[int]{\n\t\tvalue: 23\n\t}\n}\n'
		'main.v':            'module main\n\nimport factory\n\nfn main() {\n\tbuilder := factory.Factory{}\n\tprintln(int_str(builder.new().get()))\n}\n'
	})
	assert out == '23'
}

fn test_fn_ptr_typedef_keeps_concrete_upper_suffix_type() {
	out := review_pr_run_project('fn_ptr_concrete_upper_suffix_type', {
		'main.v': 'module main\n\nstruct Foo_T {\n\tn int\n}\n\nfn apply(cb fn (Foo_T) int) int {\n\treturn cb(Foo_T{\n\t\tn: 8\n\t})\n}\n\nfn take(foo Foo_T) int {\n\treturn foo.n + 2\n}\n\nfn main() {\n\tprintln(int_str(apply(take)))\n}\n'
	})
	assert out == '10'
}
