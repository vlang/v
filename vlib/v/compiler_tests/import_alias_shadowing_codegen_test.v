import os

// A dotted import binds its last segment in the importing file, so `import
// x.bench` makes `bench` mean `x.bench` there. When the same file also reaches
// the real `bench` module under another name, a type annotation the checker
// recorded as `bench.Thing` used to be alias-expanded a second time on its way
// to C and landed on the same-named struct in `x.bench`.

const alias_shadow_vexe = @VEXE
const alias_shadow_tests_dir = os.dir(@FILE)
const alias_shadow_v3_dir = os.dir(alias_shadow_tests_dir)
const alias_shadow_vlib_dir = os.dir(alias_shadow_v3_dir)
const alias_shadow_v3_src = os.join_path(alias_shadow_v3_dir, 'v.v')

// The fixtures have to be compiled by the V3 compiler built from this checkout.
// Building `vlib/v/v.v` is also what lets the suite's unit-test wrapper hand
// back its shared V3 binary; any other invocation is forwarded to the host
// compiler instead, which would not exercise this code path at all.
fn alias_shadow_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_alias_shadow_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${alias_shadow_vexe} -gc none -path "${alias_shadow_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${alias_shadow_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn alias_shadow_build_and_run(v3_bin string, root string, files map[string]string) os.Result {
	os.rmdir_all(root) or {}
	for path, content in files {
		full := os.join_path(root, path)
		os.mkdir_all(os.dir(full)) or { panic(err) }
		os.write_file(full, content) or { panic(err) }
	}
	main_v := os.join_path(root, 'main.v')
	exe := os.join_path(root, 'prog')
	compile := os.execute('${v3_bin} -nocache ${main_v} -b c -o ${exe}')
	if compile.exit_code != 0 {
		return compile
	}
	return os.execute(exe)
}

fn test_a_shadowed_module_keeps_its_own_type() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_short_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'bench/bench.v':   'module bench\n\npub struct Thing {\npub mut:\n\tn int\n}\n\npub fn start() Thing {\n\treturn Thing{\n\t\tn: 1\n\t}\n}\n'
		'x/bench/bench.v': 'module bench\n\npub struct Thing {\npub mut:\n\tm int\n}\n'
		'main.v':          'module main\n\nimport x.bench\nimport bench as jj\n\nfn main() {\n\tb := jj.start()\n\tprintln(b.n)\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '1', res.output
}

// The canonical name can live in a nested module, so the check has to compare
// whole module paths: with `import x.foo` the first segment alone (`foo`) never
// matches the real module `foo.bar`, and `foo.bar.Thing` would be rebased onto
// `x.foo` and resolve to `x.foo.bar.Thing`.
fn test_a_shadowed_nested_module_keeps_its_own_type() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_nested_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'foo/bar/bar.v':   'module bar\n\npub struct Thing {\npub mut:\n\tn int\n}\n\npub fn start() Thing {\n\treturn Thing{\n\t\tn: 42\n\t}\n}\n'
		'x/foo/foo.v':     'module foo\n\npub fn ping() int {\n\treturn 0\n}\n'
		'x/foo/bar/bar.v': 'module bar\n\npub struct Thing {\npub mut:\n\tm int\n}\n'
		'main.v':          'module main\n\nimport x.foo\nimport x.foo.bar\nimport foo.bar as jj\n\nfn main() {\n\tb := jj.start()\n\tprintln(b.n)\n\tc := bar.Thing{\n\t\tm: 7\n\t}\n\tprintln(c.m)\n\tprintln(foo.ping())\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['42', '7', '0'], res.output
}

// A pointer annotation carries a wrapper, so the module check has to look at the
// bare leaf: `&bench.Thing` otherwise yields the alias `&bench` and a
// whole-wrapper lookup, neither of which is registered, and the pointer keeps
// being rebased onto the shadowing module.
fn test_a_shadowed_module_keeps_its_own_type_through_a_pointer() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_ptr_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'bench/bench.v':   'module bench\n\npub struct Thing {\npub mut:\n\tn int\n}\n\npub fn start() &Thing {\n\treturn &Thing{\n\t\tn: 5\n\t}\n}\n'
		'x/bench/bench.v': 'module bench\n\npub struct Thing {\npub mut:\n\tm int\n}\n'
		'main.v':          'module main\n\nimport x.bench\nimport bench as jj\n\nfn main() {\n\tb := jj.start()\n\tprintln(b.n)\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '5', res.output
}

// The module that owns the canonical annotation need not be imported by the
// current file. A public signature can carry it across a transitive dependency.
fn test_a_transitive_shadowed_module_keeps_its_own_type() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_transitive_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'bench/bench.v':   'module bench\n\npub struct Thing {\npub mut:\n\tn int\n}\n'
		'bridge/bridge.v': 'module bridge\n\nimport bench\n\npub fn start() bench.Thing {\n\treturn bench.Thing{\n\t\tn: 17\n\t}\n}\n'
		'x/bench/bench.v': 'module bench\n\npub struct Thing {\npub mut:\n\tm int\n}\n'
		'main.v':          'module main\n\nimport bridge\nimport x.bench\n\nfn main() {\n\tb := bridge.start()\n\tprintln(b.n)\n\tprintln(bench.Thing{\n\t\tm: 3\n\t}.m)\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['17', '3'], res.output
}

fn test_a_shadowed_module_keeps_its_declared_alias() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_declared_alias_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'bench/bench.v':   'module bench\n\npub struct Thing {\npub mut:\n\tn int\n}\n\npub type Alias = Thing\n\npub fn start() Alias {\n\treturn Alias(Thing{\n\t\tn: 23\n\t})\n}\n'
		'x/bench/bench.v': 'module bench\n\npub struct Thing {\npub mut:\n\tm int\n}\n\npub type Alias = Thing\n'
		'main.v':          'module main\n\nimport x.bench\nimport bench as jj\n\nfn main() {\n\tb := jj.start()\n\tprintln(b.n)\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '23', res.output
}

// A cast's checker sidecar records the canonical target even though its
// `node.value` still has the import spelling used in source.
fn test_a_source_cast_uses_the_shadowing_import() {
	v3_bin := alias_shadow_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_alias_shadow_source_cast_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := alias_shadow_build_and_run(v3_bin, root, {
		'foo/foo.v':   'module foo\n\npub type Value = int\n'
		'x/foo/foo.v': 'module foo\n\npub type Value = u8\n'
		'main.v':      'module main\n\nimport x.foo\nimport foo as jj\n\nfn main() {\n\t_ = jj.Value(1)\n\tmut value := foo.Value(200)\n\tvalue += foo.Value(100)\n\tprintln(value)\n}\n'
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '44', res.output
}
