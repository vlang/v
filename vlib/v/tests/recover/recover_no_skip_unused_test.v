import arrays
import os

const vexe = os.getenv_opt('VEXE') or { @VEXE }

// `-no-skip-unused` keeps every concrete function, `recover` itself among them,
// so the used functions cannot say whether the program calls `recover()`. A
// panic must run the pending deferred blocks when a function the build keeps
// calls the builtin `recover()` or uses it as a value. A name that a function
// binds itself (a parameter, a capture, a `recover :=` local) is not the builtin;
// any other name the checker leaves unresolved counts as the builtin.
struct Case {
	name    string
	files   map[string]string
	cleanup bool // the deferred `println('cleanup')` runs with -nocache
	// ... and with -no-skip-unused, which also keeps the functions nothing calls
	kept_cleanup bool
	code         int    // exit code
	output       string // the exact output, where it matters
}

const panic_tail = "
	defer {
		println('cleanup')
	}
	panic('boom')
}
"

const recover_guard = 'module guard\n\npub fn stop() {\n\tdefer {\n\t\trecover()\n\t}\n}\n\npub fn other() int {\n\treturn 2\n}\n'

const cases = [
	Case{
		name:         'without_recover'
		files:        {
			'main.v': 'fn main() {' + panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'guarded_call'
		files:        {
			'main.v':        'import guard\n\nfn main() {\n\tif arguments().len > 1 {\n\t\tguard.stop()\n\t}' +
				panic_tail
			'guard/guard.v': recover_guard
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'call_inside_if_false'
		files:        {
			'main.v':        'import guard\n\nfn main() {\n\tif false {\n\t\tguard.stop()\n\t}' + panic_tail
			'guard/guard.v': recover_guard
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'markused_function'
		files:        {
			'main.v': '@[markused]\nfn stop() {\n\tdefer {\n\t\trecover()\n\t}\n}\n\nfn main() {' + panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'function_nothing_calls'
		files:        {
			'main.v': 'fn stop() {\n\tdefer {\n\t\trecover()\n\t}\n}\n\nfn main() {' + panic_tail
		}
		cleanup:      false
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'function_value'
		files:        {
			'main.v': 'fn main() {\n\tf := recover\n\t_ = f' + panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'function_value_nothing_calls'
		files:        {
			'main.v': 'fn keep() {\n\tf := recover\n\t_ = f\n}\n\nfn main() {' + panic_tail
		}
		cleanup:      false
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'function_value_in_a_generic_function'
		files:        {
			'main.v': 'fn keep[T](x T) {\n\tf := recover\n\t_ = f\n\t_ = x\n}\n\nfn main() {\n\tkeep(1)' +
				panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'variable_named_recover'
		files:        {
			'main.v': 'fn main() {\n\trecover := 3\n\tprintln(recover)' + panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'module_with_its_own_recover'
		files:        {
			'own/own.v': 'module own\n\npub fn recover() int {\n\treturn 1\n}\n\npub fn use() int {\n\treturn recover()\n}\n'
			'main.v':    'import own\n\nfn main() {\n\tprintln(own.use())' + panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'module_with_its_own_recover_as_a_value'
		files:        {
			'own/own.v': 'module own\n\npub fn recover() int {\n\treturn 1\n}\n\npub fn use() int {\n\tf := recover\n\treturn f()\n}\n'
			'main.v':    'import own\n\nfn main() {\n\tprintln(own.use())' + panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'module_recover_nothing_calls'
		files:        {
			'guard/guard.v': recover_guard
			'main.v':        'import guard\n\nfn main() {\n\tprintln(guard.other())' + panic_tail
		}
		cleanup:      false
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'recovered'
		files:        {
			'main.v': "fn risky() {\n\tdefer {\n\t\tif msg := recover() {\n\t\t\tprintln('cleanup \${msg}')\n\t\t}\n\t}\n\tpanic('boom')\n}\n\nfn main() {\n\trisky()\n\tprintln('after')\n}\n"
		}
		cleanup:      true
		kept_cleanup: true
		code:         0
		output:       'cleanup boom\nafter\n'
	},
	Case{
		name:         'recovered_in_an_imported_module'
		files:        {
			'guard/guard.v': "module guard\n\npub fn attempt(f fn ()) {\n\tdefer {\n\t\tif msg := recover() {\n\t\t\tprintln('recovered \${msg}')\n\t\t}\n\t}\n\tf()\n}\n"
			'main.v':        "import guard\n\nfn inner() {\n\tdefer {\n\t\tprintln('inner cleanup')\n\t}\n\tpanic('boom')\n}\n\nfn outer() {\n\tdefer {\n\t\tprintln('outer cleanup')\n\t}\n\tinner()\n}\n\nfn main() {\n\tguard.attempt(outer)\n\tprintln('after')\n}\n"
		}
		cleanup:      true
		kept_cleanup: true
		code:         0
		output:       'inner cleanup\nouter cleanup\nrecovered boom\nafter\n'
	},
	// Names a function binds itself, which the checker can leave unresolved.
	Case{
		name:         'function_parameter_named_recover'
		files:        {
			'main.v': 'fn invoke(recover fn ()) {\n\trecover()\n}\n\nfn noop() {}\n\nfn main() {\n\tinvoke(noop)' +
				panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'local_closure_named_recover'
		files:        {
			'main.v': 'fn main() {\n\trecover := fn () ?string {\n\t\treturn none\n\t}\n\t_ = recover()' +
				panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'variable_named_recover_in_an_unused_generic'
		files:        {
			'main.v': 'fn unused[T](x T) {\n\trecover := 3\n\tprintln(recover)\n\t_ = x\n}\n\nfn main() {' +
				panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'closure_capturing_a_local_named_recover'
		files:        {
			'main.v': 'fn main() {\n\trecover := fn () ?string {\n\t\treturn none\n\t}\n\tg := fn [recover] () {\n\t\t_ = recover()\n\t}\n\tg()' +
				panic_tail
		}
		cleanup:      false
		kept_cleanup: false
		code:         1
	},
	Case{
		name:         'closure_with_the_builtin_inside_a_generic_that_binds_recover'
		files:        {
			'main.v': 'fn fnval() ?string {\n\treturn none\n}\n\nfn keep[T](x T) {\n\trecover := fnval\n\t_ = recover()\n\tg := fn () {\n\t\tdefer {\n\t\t\trecover()\n\t\t}\n\t}\n\tg()\n\t_ = x\n}\n\nfn main() {\n\tkeep(1)' +
				panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	// A local ends with its block; the builtin is back after it. In a generic
	// function the checker leaves the call unresolved.
	Case{
		name:         'builtin_after_a_block_with_a_local_named_recover'
		files:        {
			'main.v': 'fn keep[T](x T) {\n\tif arguments().len > 0 {\n\t\trecover := x\n\t\t_ = recover\n\t}\n\tdefer {\n\t\trecover()\n\t}\n}\n\nfn main() {\n\tkeep(1)' +
				panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	// An if-guard binds its name in the guarded block only; the builtin is back in the `else`.
	Case{
		name:         'builtin_in_the_else_of_an_if_guard_named_recover'
		files:        {
			'main.v': 'fn opt() ?int {\n\treturn none\n}\n\nfn keep[T](x T) {\n\tdefer {\n\t\tif recover := opt() {\n\t\t\t_ = recover\n\t\t} else {\n\t\t\trecover()\n\t\t}\n\t}\n\t_ = x\n}\n\nfn main() {\n\tkeep(1)' +
				panic_tail
		}
		cleanup:      true
		kept_cleanup: true
		code:         1
	},
	// Known limitation: these two cases assert current conservative behavior, not
	// required behavior. The checker leaves generic bodies unresolved. A scan
	// that follows generic instantiations would turn `kept_cleanup` to false.
	Case{
		name:         'call_in_an_uninstantiated_generic'
		files:        {
			'main.v': 'fn keep[T](x T) {\n\trecover()\n\t_ = x\n}\n\nfn main() {' + panic_tail
		}
		cleanup:      false
		kept_cleanup: true
		code:         1
	},
	Case{
		name:         'module_recover_in_an_uninstantiated_generic'
		files:        {
			'own/own.v': 'module own\n\npub fn recover() int {\n\treturn 1\n}\n\nfn keep[T](x T) int {\n\t_ = x\n\treturn recover()\n}\n\npub fn use() int {\n\treturn recover()\n}\n'
			'main.v':    'import own\n\nfn main() {\n\tprintln(own.use())' + panic_tail
		}
		cleanup:      false
		kept_cleanup: true
		code:         1
	},
]

fn write_project(dir string, c Case) !string {
	project := os.join_path(dir, c.name)
	for name, text in c.files {
		path := os.join_path(project, name)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, text)!
	}
	return project
}

fn build_and_run(dir string, c Case, flags []string) !os.Result {
	project := write_project(dir, c)!
	exe := os.join_path(project, 'app')
	mut args := [vexe]
	args << flags
	args << ['-o', exe, os.join_path(project, 'main.v')]
	compile := os.exec(args)
	if compile.exit_code != 0 {
		return error(compile.output)
	}
	return os.exec([exe])
}

// check_cases builds and runs each case with `flags`, and returns what did not match.
fn check_cases(dir string, part []Case, flags []string) []string {
	mut failures := []string{}
	for c in part {
		want := if flags == ['-no-skip-unused'] { c.kept_cleanup } else { c.cleanup }
		run := build_and_run(dir, c, flags) or {
			failures << '${c.name} ${flags}: ${err}'
			continue
		}
		if run.exit_code != c.code || run.output.contains('cleanup') != want
			|| (c.output != '' && run.output != c.output) {
			failures << '${c.name} ${flags}: exit ${run.exit_code}: ${run.output}'
		}
	}
	return failures
}

fn test_panic_frames_with_and_without_no_skip_unused() {
	dir := os.join_path(os.vtmp_dir(), 'recover_no_skip_unused_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	// -nocache: the module cache turns the frames on for a whole build when
	// a cached module's code calls `recover()`, which is not what this tests.
	// The builds are independent, so they run side by side, a half of the cases each.
	// Each run has a folder of its own, made here: os.mkdir_all fails when another
	// thread makes the same folder at the same time.
	for flags in [['-nocache'], ['-no-skip-unused']] {
		for i in 0 .. 2 {
			os.mkdir_all(os.join_path(dir, '${flags[0].trim_left('-')}_${i}'))!
		}
	}
	mut checks := []thread []string{}
	for flags in [['-nocache'], ['-no-skip-unused']] {
		for i, part in [cases[..cases.len / 2], cases[cases.len / 2..]] {
			part_dir := os.join_path(dir, '${flags[0].trim_left('-')}_${i}')
			checks << spawn check_cases(part_dir, part, flags)
		}
	}
	failures := arrays.flatten(checks.wait())
	assert failures.len == 0, failures.join('\n')
}

fn test_coverage_and_test_runs_keep_the_default_behavior() {
	dir := os.join_path(os.vtmp_dir(), 'recover_no_skip_unused_runs_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	// A test file without recover() panics without running its deferred block,
	// and one that calls it, or uses it as a value, runs it first; the same with
	// coverage, as `v -no-skip-unused -coverage DIR test` runs them.
	// Each run has a file of its own, so they all run side by side.
	mut runs := []thread string{}
	for name, body in {
		'plain':   'fn test_panics() {' + panic_tail
		'guarded': 'fn stop() {\n\tdefer {\n\t\trecover()\n\t}\n}\n\nfn test_panics() {\n\tif arguments().len > 99 {\n\t\tstop()\n\t}' +
			panic_tail
		'value':   'fn test_panics() {\n\tf := recover\n\t_ = f' + panic_tail
	} {
		for kind in ['plain', 'coverage'] {
			test_file := os.join_path(dir, '${name}_${kind}_test.v')
			os.write_file(test_file, body) or {
				runs.wait()
				panic(err)
			}
			mut flags := ['-no-skip-unused']
			if kind == 'coverage' {
				flags << ['-coverage', test_file + '.coverage']
			}
			runs << spawn check_test_file(test_file, flags, name != 'plain')
		}
	}
	coverage := build_and_run(dir, cases[0], ['-no-skip-unused', '-coverage',
		os.join_path(dir, 'coverage')]) or {
		runs.wait()
		panic(err)
	}
	failures := runs.wait().filter(it != '')
	assert coverage.exit_code == 1 && !coverage.output.contains('cleanup'), coverage.output
	assert failures.len == 0, failures.join('\n')
	test_dir := os.join_path(dir, 'tests')
	os.mkdir_all(test_dir)!
	for name, body in {
		'plain':   'fn test_panics() {'
		'guarded': 'fn stop() {\n\tdefer {\n\t\trecover()\n\t}\n}\n\nfn test_panics() {\n\tif arguments().len > 99 {\n\t\tstop()\n\t}'
	} {
		os.write_file(os.join_path(test_dir, '${name}_test.v'), body +
			panic_tail.replace('cleanup', '${name} cleanup'))!
	}
	directory := os.exec([vexe, '-no-skip-unused', '-coverage',
		os.join_path(dir, 'directory_coverage'), 'test', test_dir])
	assert directory.exit_code != 0 && directory.output.contains('guarded cleanup')
		&& !directory.output.contains('plain cleanup'), directory.output
}

// check_test_file runs a test file that panics, and returns what did not match.
fn check_test_file(test_file string, flags []string, cleanup bool) string {
	mut args := [vexe]
	args << flags
	args << ['test', test_file]
	run := os.exec(args)
	if run.exit_code != 1 || !run.output.contains('V panic: boom')
		|| run.output.contains('cleanup') != cleanup {
		return '${test_file} ${flags}: exit ${run.exit_code}: ${run.output}'
	}
	return ''
}

// An object file can export any function, so one that recovers keeps its
// panic frame even when nothing in it calls that function.
fn test_an_object_file_keeps_the_frames_of_an_exported_recover() {
	dir := os.join_path(os.vtmp_dir(), 'recover_no_skip_unused_obj_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	for name, source in {
		'exported': "pub fn parse(s string) int {\n\tdefer {\n\t\tif msg := recover() {\n\t\t\tprintln(msg)\n\t\t}\n\t}\n\tif s.len == 0 {\n\t\tpanic('empty')\n\t}\n\treturn s.int()\n}\n"
		'plain':    "pub fn parse(s string) int {\n\tdefer {\n\t\tprintln('cleanup')\n\t}\n\treturn s.int()\n}\n"
	} {
		src := os.join_path(dir, '${name}.v')
		os.write_file(src, source)!
		c_file := os.join_path(dir, '${name}.c')
		compile := os.exec([vexe, '-no-skip-unused', '-is_o', '-o', c_file, src])
		assert compile.exit_code == 0, compile.output
		// The frame of `parse` is a landing pad in its own body (the exported
		// `main__parse` only calls it), not the v_unwind_setjmp macro of the preamble.
		mut body := []string{}
		mut inside := false
		for line in os.read_lines(c_file)! {
			if !inside {
				inside = line.ends_with(' parse(string s) {') && !line.contains('main__')
			} else if line == '}' {
				break
			} else {
				body << line
			}
		}
		assert body.len > 0, '${name}: no parse() in the C'
		frames := body.any(it.contains('if (v_unwind_setjmp('))
		assert frames == (name == 'exported'), '${name}: landing pad in parse() ${frames}'
	}
}
