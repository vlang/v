import os

const gct_vexe = @VEXE
const gct_tests_dir = os.dir(@FILE)
const gct_v3_dir = os.dir(gct_tests_dir)
const gct_vlib_dir = os.dir(gct_v3_dir)
const gct_v3_src = os.join_path(gct_v3_dir, 'v3.v')

fn gct_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_chan_thread_param_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${gct_vexe} -gc none -path "${gct_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gct_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn gct_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn gct_run_project(v3_bin string, name string, files map[string]string) string {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_${name}_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		gct_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A generic function in an imported module takes a `chan T` parameter. When specialized from
// `main` with a program type (`Context`) that collides by short name with a same-named type in
// the callee module, the channel payload must be pinned to the program type (`main.Context`),
// not rebased to the callee module's `mid.Context`. Two things must hold: `chan T` must have its
// payload substituted at all (otherwise the unresolved `T` lowers to a bogus `int` channel), and
// the substituted `Context` must be collision-locked to `main.` (otherwise the channel and the
// `v T` argument get the callee module's ABI and the C compiler rejects the call). The received
// value carries `.name`, a field only the program `Context` has, so the run proves the program
// type survived end-to-end.
fn test_chan_generic_param_locks_nested_main_type() {
	v3_bin := gct_build_v3()
	out := gct_run_project(v3_bin, 'generic_chan_param_main_type', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn send_one[T](ch chan T, v T) {\n\tch <- v\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Context {\npub:\n\tname string\n}\n\nfn main() {\n\tch := chan Context{cap: 1}\n\tmid.send_one[Context](ch, Context{\n\t\tname: 'hello'\n\t})\n\tgot := <-ch\n\tprintln('got: ' + got.name)\n}\n"
	})
	assert out == 'got: hello'
}

// The same `chan T` generic used with a non-colliding program type must also work: the payload
// is substituted so the channel element is the concrete type rather than a defaulted `int`.
fn test_chan_generic_param_non_colliding() {
	v3_bin := gct_build_v3()
	out := gct_run_project(v3_bin, 'generic_chan_param_non_colliding', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn send_one[T](ch chan T, v T) {\n\tch <- v\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Payload {\npub:\n\tname string\n}\n\nfn main() {\n\tch := chan Payload{cap: 1}\n\tmid.send_one[Payload](ch, Payload{\n\t\tname: 'hello'\n\t})\n\tgot := <-ch\n\tprintln('got: ' + got.name)\n}\n"
	})
	assert out == 'got: hello'
}

// A generic function taking a `thread T` handle must substitute the payload the same way, so the
// specialized signature carries a real thread handle (`__v_thread`) returning the concrete type
// rather than leaving the placeholder `thread T` (which resolves the unsubstituted `T` to a bogus
// `int` thread element). Exercised with a non-colliding program type.
fn test_thread_generic_param_substitutes_payload() {
	v3_bin := gct_build_v3()
	out := gct_run_project(v3_bin, 'generic_thread_param_main_type', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn joiner[T](t thread T) T {\n\treturn t.wait()\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Payload {\npub:\n\tname string\n}\n\nfn make_it() Payload {\n\treturn Payload{\n\t\tname: 'threaded'\n\t}\n}\n\nfn main() {\n\tt := spawn make_it()\n\tc := mid.joiner[Payload](t)\n\tprintln('got: ' + c.name)\n}\n"
	})
	assert out == 'got: threaded'
}

// A generic function that CREATES a channel inside its body (`ch := chan T{cap: 1}`), rather than
// receiving one as a parameter, must lock a colliding program payload to `main.` on the channel
// literal itself. The literal's type is a composite `chan main.Context`, so the collision lock is
// nested, not a leading `main.`; without matching a nested `main.` the literal round-trips through
// the callee module and the channel is built with that module's homonym `Context` (wrong element
// size/ABI), so pushing the program `Context` value into it fails to compile.
fn test_chan_literal_inside_body_locks_nested_main_type() {
	v3_bin := gct_build_v3()
	out := gct_run_project(v3_bin, 'generic_chan_literal_main_type', {
		'mid/mid.v': 'module mid\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn make_and_send[T](v T) chan T {\n\tch := chan T{cap: 1}\n\tch <- v\n\treturn ch\n}\n'
		'main.v':    "module main\n\nimport mid\n\nstruct Context {\npub:\n\tname string\n}\n\nfn main() {\n\tch := mid.make_and_send[Context](Context{\n\t\tname: 'inside'\n\t})\n\tgot := <-ch\n\tprintln('got: ' + got.name)\n}\n"
	})
	assert out == 'got: inside'
}
