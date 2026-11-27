module eval

import os
import v.parser
import v.pref

fn vsh_eval_vlib_sources(module_name string, module_source string, app_name string, app_source string) (string, string) {
	vroot := os.real_path(os.dir(os.dir(os.dir(os.dir(@FILE)))))
	mut prefs := pref.new_preferences()
	prefs.vroot = vroot
	mut p := parser.Parser.new(prefs)
	p.parse_text(os.join_path(vroot, 'vlib', module_name, 'synthetic_c_init.c.v'), module_source)
	a := p.parse_text(app_name, app_source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut e := new(prefs)
	e.capture_output = true
	e.run_files(a) or { return e.stdout(), err.msg() }
	return e.stdout(), ''
}

fn test_run_text_parses_in_memory_without_temp_files() {
	nonexistent_tmp := os.join_path(os.vtmp_dir(), 'eval_mem_nonexistent_${os.getpid()}', 'missing')
	mut prev_env := map[string]?string{}
	for key in ['TMPDIR', 'TEMP', 'TMP'] {
		prev_env[key] = os.getenv_opt(key)
		os.setenv(key, nonexistent_tmp, true)
	}
	defer {
		for key, val in prev_env {
			if prev := val {
				os.setenv(key, prev, true)
			} else {
				os.unsetenv(key)
			}
		}
	}
	mut e := create()
	e.run_text('fn main() {\n\tprintln(2 + 40)\n}\n') or { panic(err) }
	assert e.stdout() == '42\n'
	file_nodes := e.a.nodes.filter(it.kind == .file)
	assert file_nodes.len > 0
	assert file_nodes.all(it.value == '<eval>.v'), 'run_text must parse in memory as <eval>.v, got `${file_nodes.map(it.value)}`'
}

fn test_eval_empty_vsh_allowed_and_empty_v_rejected() {
	prefs := pref.new_preferences()
	mut p_vsh := parser.Parser.new(prefs)
	a_vsh := p_vsh.parse_text('empty.vsh', '')
	mut e_vsh := new(prefs)
	e_vsh.capture_output = true
	e_vsh.run_files(a_vsh) or { panic(err) }
	assert e_vsh.stdout() == ''

	// Extensionless scripts set `has_vsh_source` (e.g. via `-raw-vsh-tmp-prefix`).
	mut p_extless := parser.Parser.new(prefs)
	mut a_extless := p_extless.parse_text('empty_script', '')
	a_extless.has_vsh_source = true
	mut e_extless := new(prefs)
	e_extless.capture_output = true
	e_extless.run_files(a_extless) or { panic(err) }
	assert e_extless.stdout() == ''

	mut p_v := parser.Parser.new(prefs)
	a_v := p_v.parse_text('empty.v', '')
	mut e_v := new(prefs)
	e_v.capture_output = true
	mut failed := false
	e_v.run_files(a_v) or {
		failed = true
		assert err.msg().contains('missing main.main entry point'), err.msg()
		[]Value{}
	}
	assert failed, 'empty .v file without main must still fail'
}

fn test_eval_vsh_dispatches_bare_os_calls_and_respects_local_and_selective_imports() {
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_text('bare.vsh', "println(join_path('alpha', 'beta'))\n")
	mut e := new(prefs)
	e.capture_output = true
	e.run_files(a) or { panic(err) }
	assert e.stdout() == '${os.join_path('alpha', 'beta')}\n'

	// Local declaration shadows implicit os fallback.
	mut p_shadow := parser.Parser.new(prefs)
	a_shadow := p_shadow.parse_text('shadow.vsh',
		"fn join_path(a string, b string) string { return a + ':' + b }\nprintln(join_path('alpha', 'beta'))\n")
	mut e_shadow := new(prefs)
	e_shadow.capture_output = true
	e_shadow.run_files(a_shadow) or { panic(err) }
	assert e_shadow.stdout() == 'alpha:beta\n'

	// Explicit selective import takes precedence over implicit os fallback.
	mut p_sel := parser.Parser.new(prefs)
	p_sel.parse_text('probe.v',
		"module probe\npub fn join_path(a string, b string) string { return a + ':' + b }\n")
	a_sel := p_sel.parse_text('selective.vsh',
		"import probe { join_path }\nprintln(join_path('a', 'b'))\n")
	mut e_sel := new(prefs)
	e_sel.capture_output = true
	e_sel.run_files(a_sel) or { panic(err) }
	assert e_sel.stdout() == 'a:b\n'

	// Bare call to a V-defined os function (not in the os builtin shim list) uses
	// os parameter types for enum shorthand and mut writeback.
	out, msg := vsh_eval_vlib_sources('os',
		'module os\npub enum SyntheticMode {\n\tok = 7\n}\npub fn synthetic_v_os_helper(m SyntheticMode, mut acc int) int {\n\tacc += int(m)\n\treturn acc\n}\n',
		'bare_v_os.vsh',
		'mut n := 3\nres := synthetic_v_os_helper(.ok, mut n)\nprintln(res)\nprintln(n)\n')
	assert msg == '', msg
	assert out == '10\n10\n', out
}

fn test_eval_optional_enum_zero_values_remain_none() {
	for typ in ['Kind', 'Alias', 'Nested'] {
		mut e := create()
		e.run_text('enum Kind { first = 7 }
type Alias = Kind
type Nested = Alias
struct Box { kind ?${typ} }
fn main() {
	println(Box{}.kind == none)
	println(Box{kind: .first}.kind == none)
}
') or {
			panic(err)
		}
		assert e.stdout() == 'true\nfalse\n', typ
	}

	out, msg := vsh_eval_vlib_sources('probe',
		'module probe\npub enum Kind {\n\tunknown = C._IOLBF\n\tknown = 7\n}\n',
		'optional_enum.v',
		'import probe\ntype Mode = probe.Kind\nstruct Box { kind ?Mode }\nfn main() {\n\tprintln(Box{}.kind == none)\n\tprintln(Box{kind: .known}.kind == none)\n}\n')
	assert msg == '', msg
	assert out == 'true\nfalse\n', out
}

fn test_eval_defers_vlib_c_initializers_without_fabricating_values() {
	vroot := os.real_path(os.dir(os.dir(os.dir(os.dir(@FILE)))))
	mut prefs := pref.new_preferences()
	prefs.vroot = vroot
	c_vlib_path := os.join_path(vroot, 'vlib', 'os', 'synthetic_c_init.c.v')
	c_vlib_src := 'module os\npub enum SyntheticC {\n\ta = C.MISSING_CONST\n\tb\n\tc = 7\n}\nfn helper_with_param(leaked_param int) int {\n\treturn C.missing_fn(leaked_param)\n}\n__global synthetic_c_global = helper_with_param(99)\nfn init() {\n\tC.missing_init()\n}\n'

	mut p := parser.Parser.new(prefs)
	p.parse_text(c_vlib_path, c_vlib_src)
	a := p.parse_text('app.v', 'import os\nfn main() {\n\tprintln(int(os.SyntheticC.c))\n}\n')
	mut e := new(prefs)
	e.capture_output = true
	e.run_files(a) or { panic(err) }
	assert e.stdout() == '7\n'
	assert e.call_stack.len == 0, 'deferred vlib C init must unwind call_stack, got ${e.call_stack}'
	assert e.scopes.len == 0, 'deferred vlib C init must unwind scopes, got ${e.scopes.len}'

	// Parameters from a failed vlib C global helper must not leak into user scope.
	mut p_leak := parser.Parser.new(prefs)
	p_leak.parse_text(c_vlib_path, c_vlib_src)
	a_leak := p_leak.parse_text('leak.v', 'import os\nfn main() {\n\tprintln(leaked_param)\n}\n')
	mut e_leak := new(prefs)
	e_leak.capture_output = true
	mut leak_failed := false
	e_leak.run_files(a_leak) or {
		leak_failed = true
		assert err.msg().contains('unknown variable `leaked_param`'), err.msg()
		assert !err.msg().contains('synthetic_c_global'), err.msg()
		[]Value{}
	}
	assert leak_failed, 'failed vlib C helper parameters must not leak into user scopes'

	// Reading deferred C-backed enum variants must fail with a specific error across
	// qualified reads, bare reads without int(), const collisions, shorthand args,
	// option/alias expected types, struct defaults, and match targets.
	enum_mod_src := 'module probe\npub enum Kind {\n\tunknown = C._IOLBF\n\tfollower\n\tknown = 7\n}\n'
	bad_cases := {
		'qualified_unknown':  'import probe\nfn main() { println(int(probe.Kind.unknown)) }\n'
		'qualified_follower': 'import probe\nfn main() { println(int(probe.Kind.follower)) }\n'
		'unwrapped_unknown':  'import probe\nfn main() { x := probe.Kind.unknown\n\t_ = x }\n'
		'const_clash':        'import probe\nconst unknown = 9\nfn main() { println(probe.Kind.unknown) }\n'
		'shorthand_arg':      'import probe\nfn show(k probe.Kind) { println(int(k)) }\nfn main() { show(.unknown) }\n'
		'option_arg':         'import probe\nfn show(k ?probe.Kind) { _ = k }\nfn main() { show(.unknown) }\n'
		'alias_arg':          'import probe\ntype Alias = probe.Kind\nfn show(k Alias) { println(int(k)) }\nfn main() { show(.unknown) }\n'
		'struct_implicit':    'import probe\nstruct Box { kind probe.Kind }\nfn main() { println(int(Box{}.kind)) }\n'
		'struct_shorthand':   'import probe\nstruct Box { kind probe.Kind = .unknown }\nfn main() { println(int(Box{}.kind)) }\n'
		'struct_qualified':   'import probe\nstruct Box { kind probe.Kind = probe.Kind.unknown }\nfn main() { println(int(Box{}.kind)) }\n'
		'match_var':          "import probe\nfn main() { k := probe.Kind.known\n\tmatch k { .unknown { println('w') } else { println('k') } } }\n"
		'match_call_stmt':    "import probe\nfn kind() probe.Kind { return .known }\nfn main() { match kind() { .unknown { println('w') } else { println('k') } } }\n"
		'match_call_expr':    "import probe\nfn kind() probe.Kind { return .known }\nfn main() { println(match kind() { .unknown { 'w' } else { 'k' } }) }\n"
	}
	for case_name, app_src in bad_cases {
		out, msg := vsh_eval_vlib_sources('probe', enum_mod_src, '${case_name}.v', app_src)
		expected_field := if case_name == 'qualified_follower' {
			'probe.Kind.follower'
		} else {
			'probe.Kind.unknown'
		}
		assert msg.contains(expected_field), '${case_name}: expected `${expected_field}`, got out=`${out}` err=`${msg}`'
		assert msg.contains('not supported'), '${case_name}: ${msg}'
	}

	// Explicit field overrides and nested aliases to known members stay usable.
	for override_body in [
		'println(int(Box{kind: .known}.kind))',
		'println(int(Box{.known}.kind))',
	] {
		out, msg := vsh_eval_vlib_sources('probe', enum_mod_src, 'override.v',
			'import probe\nstruct Box { kind probe.Kind = .unknown }\nfn main() { ${override_body} }\n')
		assert msg == '', msg
		assert out == '7\n', out
	}
	alias_out, alias_msg := vsh_eval_vlib_sources('probe', enum_mod_src, 'nested_alias.v',
		"import probe\ntype Alias = probe.Kind\ntype Nested = Alias\nfn kind() Nested { return .known }\nfn main() {\n\tprintln(int(kind()))\n\tmatch kind() {\n\t\t.known { println('known') }\n\t\telse { println('wrong') }\n\t}\n}\n")
	assert alias_msg == '', alias_msg
	assert alias_out == '7\nknown\n', alias_out

	// Deferred enum member named `name` must not return the type-name string.
	name_out, name_msg := vsh_eval_vlib_sources('probe',
		'module probe\npub enum Kind { name = C._IOLBF\n\tknown = 7 }\n', 'enum_name.v',
		'import probe\nfn main() { println(probe.Kind.name) }\n')
	assert name_msg.contains('probe.Kind.name'), 'got out=`${name_out}` err=`${name_msg}`'

	// Transitive C-backed enum and const initializers remain deferred without breaking known members.
	trans_enum_out, trans_enum_msg := vsh_eval_vlib_sources('probe',
		'module probe\npub enum First { unknown = C._IOLBF }\npub enum Second { unknown = int(First.unknown)\n\tknown = 7 }\n',
		'trans_enum.v', 'import probe\nfn main() { println(int(probe.Second.known)) }\n')
	assert trans_enum_msg == '', trans_enum_msg
	assert trans_enum_out == '7\n', trans_enum_out

	trans_const_out, trans_const_msg := vsh_eval_vlib_sources('probe',
		'module probe\nconst c_flag = C._IOLBF\npub enum FromConst { unknown = c_flag\n\tknown = 7 }\n__global c_global = c_flag\n',
		'trans_const.v', 'import probe\nfn main() { println(int(probe.FromConst.known)) }\n')
	assert trans_const_msg == '', trans_const_msg
	assert trans_const_out == '7\n', trans_const_out

	// Deferred globals (direct, qualified, compound-assigned, or from interrupted fn init())
	// must fail instead of falling through to unrelated constants or partial state.
	for case_name, pair in {
		'unqualified_global': [
			'module probe\nfn make() int { return C._IOLBF }\n__global state = make()\npub fn get() int { return state }\n',
			'import probe\nconst state = 77\nfn main() { println(probe.get()) }\n',
		]
		'qualified_global':   [
			'module probe\n__global state = C._IOLBF\npub fn get() int { return probe.state }\n',
			'import probe\nconst state = 77\nfn main() { println(probe.get()) }\n',
		]
		'compound_global':    [
			'module probe\n__global state = C._IOLBF\npub fn bump() { state += 1 }\n',
			'import probe\nfn main() { probe.bump() }\n',
		]
		'partial_init':       [
			'module probe\n__global state = 1\nfn init() {\n\tstate = 5\n\tC.abs(-9)\n\tstate = 9\n}\npub fn get() int { return state }\n',
			'import probe\nfn main() { println(probe.get()) }\n',
		]
	} {
		out, msg := vsh_eval_vlib_sources('probe', pair[0], '${case_name}.v', pair[1])
		assert msg.contains('C-backed global `probe.state`'), '${case_name}: got out=`${out}` err=`${msg}`'
	}

	// Locals shadowing a deferred global and explicit reassignment still work.
	reinit_out, reinit_msg := vsh_eval_vlib_sources('probe',
		'module probe\n__global state = 1\nfn init() { C.abs(-9) }\npub fn local() int {\n\tstate := 41\n\treturn state\n}\npub fn reset() { state = 9 }\npub fn get() int { return state }\n',
		'reinit_global.v',
		'import probe\nfn main() {\n\tprintln(probe.local())\n\tprobe.reset()\n\tprintln(probe.get())\n}\n')
	assert reinit_msg == '', reinit_msg
	assert reinit_out == '41\n9\n', reinit_out

	// Non-C failures inside vlib .c.v globals and fn init() must still propagate.
	for kind, bad_vlib_src in {
		'global_panic': "module os\nfn boom() int {\n\tpanic('vlib_non_c_global')\n\treturn 0\n}\n__global broken_global = boom()\n"
		'init_panic':   "module os\nfn init() {\n\tpanic('vlib_non_c_init')\n}\n"
	} {
		out, msg := vsh_eval_vlib_sources('os', bad_vlib_src, '${kind}.v',
			'import os\nfn main() {}\n')
		assert msg.contains('vlib_non_c_'), '${kind}: got out=`${out}` err=`${msg}`'
	}

	// Non-os vlib .c.v modules with C-backed fn init() are also deferred cleanly.
	rand_out, rand_msg := vsh_eval_vlib_sources('rand',
		'module rand\nfn init() {\n\tC.missing_rand_init()\n}\npub fn val() int { return 42 }\n',
		'use_rand.v', 'import rand\nfn main() {\n\tprintln(rand.val())\n}\n')
	assert rand_msg == '', rand_msg
	assert rand_out == '42\n', rand_out

	// A non-vlib .c.v module (even under a directory named `vlib` without builtin/builtin.v)
	// must still run its fn init() and fail on bad C enums.
	fake_vlib := os.join_path(os.vtmp_dir(), 'eval_fake_vlib_${os.getpid()}', 'vlib')
	mut prefs_fake := pref.new_preferences()
	prefs_fake.vroot = vroot
	prefs_fake.module_search_paths = [fake_vlib]
	local_c_path := os.join_path(fake_vlib, 'localmod', 'localmod.c.v')
	mut p_local := parser.Parser.new(prefs_fake)
	p_local.parse_text(local_c_path,
		"module localmod\nfn init() {\n\tprintln('local-init')\n}\npub fn val() int { return 9 }\n")
	a_local := p_local.parse_text('use_local.v',
		'import localmod\nfn main() {\n\tprintln(localmod.val())\n}\n')
	mut e_local := new(prefs_fake)
	e_local.capture_output = true
	e_local.run_files(a_local) or { panic(err) }
	assert e_local.stdout() == 'local-init\n9\n'

	mut p_local_bad := parser.Parser.new(prefs_fake)
	p_local_bad.parse_text(local_c_path,
		'module localmod\npub enum BadLocal {\n\ta = C.MISSING_LOCAL\n}\n')
	a_local_bad := p_local_bad.parse_text('use_local_bad.v', 'import localmod\nfn main() {}\n')
	mut e_local_bad := new(prefs_fake)
	e_local_bad.capture_output = true
	mut local_bad_failed := false
	e_local_bad.run_files(a_local_bad) or {
		local_bad_failed = true
		assert err.msg().contains('unknown variable `C`'), err.msg()
		[]Value{}
	}
	assert local_bad_failed, 'non-vlib .c.v enum error must not be deferred'
}
