import os

const gmb_vexe = @VEXE
const gmb_tests_dir = os.dir(@FILE)
const gmb_v3_dir = os.dir(gmb_tests_dir)
const gmb_vlib_dir = os.dir(gmb_v3_dir)
const gmb_v3_src = os.join_path(gmb_v3_dir, 'v3.v')

fn gmb_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_main_base_collision_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${gmb_vexe} -gc none -path "${gmb_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gmb_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn gmb_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn gmb_run_project(v3_bin string, name string, files map[string]string) string {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_${name}_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		gmb_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A generic function in an imported module (`mid`) is specialized with a type argument that is
// itself a main-module generic instance whose BASE collides by short name with a same-named generic
// in the callee module — `main.Box[string]` while `mid.Box[T]` also exists. Inside the clone, a
// composite that nests that argument — here the struct literal `other.Cont[T]{}` with T =
// `Box[string]` — is resolved in `mid`, so the bare `Box` base must be locked to `main.Box`.
// Without the lock it binds to `mid.Box[string]` (whose fields differ, `x` not `v`), so storing the
// caller's `main.Box` value into it fails to compile. (Type inference already qualifies a bare
// top-level argument; the collision only surfaces once the bare base flows through a nested
// composite in the clone.)
fn test_generic_arg_locks_colliding_main_generic_base() {
	v3_bin := gmb_build_v3()
	out := gmb_run_project(v3_bin, 'generic_main_base_collision', {
		'other/other.v': 'module other\n\npub struct Cont[T] {\npub mut:\n\tinner T\n}\n'
		'mid/mid.v':     'module mid\n\nimport other\n\npub struct Box[T] {\npub mut:\n\tx T\n}\n\npub fn boxify[T](v T) T {\n\tc := other.Cont[T]{\n\t\tinner: v\n\t}\n\treturn c.inner\n}\n'
		'main.v':        "module main\n\nimport mid\n\nstruct Box[T] {\npub:\n\tv T\n}\n\nfn main() {\n\tb := Box[string]{\n\t\tv: 'hi'\n\t}\n\tgot := mid.boxify(b)\n\tprintln(got.v)\n}\n"
	})
	assert out == 'hi'
}
