import os
import v.tempname

fn gfp_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn gfp_run_project(name string, files map[string]string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		gfp_write_file(root, rel, source)
	}
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A generic function in an imported module carries a function-typed parameter whose signature
// mentions the type parameter (`cb fn (T)`). When it is specialized from `main` with a program
// type (`Context`) that collides by short name with a same-named type in the callee module,
// the nested `T` inside `fn (T)` must be pinned to the program type (`main.Context`) rather than
// rebased to the callee module's `mid.Context`. Otherwise the generated callback typedef has the
// wrong ABI and the C compiler rejects the call. This guards the fn-signature branch of the
// collision lock (`lock_colliding_main_generic_type_text` recursing through fn param/return
// types, and `specialized_signature_type_text` returning the locked spelling directly).
fn test_fn_typed_generic_param_locks_nested_main_type() {
	out := gfp_run_project('generic_fn_param_main_type', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn wrap[T](cb fn (T), v T) {\n\tcb(v)\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Context {\npub:\n\tname string\n}\n\nfn main() {\n\tc := Context{\n\t\tname: 'hello'\n\t}\n\tmid.wrap[Context](fn (x Context) {\n\t\tprintln('got: ' + x.name)\n\t}, c)\n}\n"
	})
	assert out == 'got: hello'
}

// The nested program type must also be locked when it appears in the return position of a
// function-typed parameter (`cb fn (T) T`), not only in a parameter position.
fn test_fn_typed_generic_param_locks_nested_main_type_in_return() {
	out := gfp_run_project('generic_fn_ret_main_type', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn transform[T](cb fn (T) T, v T) T {\n\treturn cb(v)\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Context {\npub mut:\n\tname string\n}\n\nfn main() {\n\tc := Context{\n\t\tname: 'hi'\n\t}\n\tr := mid.transform[Context](fn (x Context) Context {\n\t\treturn Context{\n\t\t\tname: x.name + '!'\n\t\t}\n\t}, c)\n\tprintln('ret: ' + r.name)\n}\n"
	})
	assert out == 'ret: hi!'
}
