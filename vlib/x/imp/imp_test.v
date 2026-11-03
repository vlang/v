module imp

import os
import time

// noop is a flow body that does nothing.
fn noop(_ Flow) ! {}

// sandbox changes to a new empty temporary directory and returns its path.
fn sandbox(name string) string {
	dir := os.join_path(os.vtmp_dir(), 'imp_test_${os.getpid()}_${name}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	os.chdir(dir) or { panic(err) }
	return dir
}

// set_mtime backdates a path, so tests need not sleep across second boundaries.
fn set_mtime(path string, offset i64) {
	stamp := time.now().unix() + offset
	os.utime(path, int(stamp), int(stamp)) or { panic(err) }
}

// test_paths_overlap covers equal, nested and merely similar paths.
fn test_paths_overlap() {
	assert paths_overlap('src', 'src')
	assert paths_overlap('src', './src/')
	assert paths_overlap('src', 'src/gen.v')
	assert paths_overlap('src/gen.v', 'src')
	assert paths_overlap('.', 'app')
	assert !paths_overlap('src', 'srcs/gen.v')
}

// mtime_fixture creates an up to date flow in a sandbox and returns it.
fn mtime_fixture(name string) Flow {
	sandbox(name)
	os.mkdir_all('src/sub') or { panic(err) }
	os.write_file('src/sub/a.v', '') or { panic(err) }
	os.write_file('out', '') or { panic(err) }
	set_mtime('src/sub/a.v', -100)
	set_mtime('src/sub', -100)
	set_mtime('src', -100)
	set_mtime('out', -50)
	return Flow{
		id:    'f'
		takes: ['src']
		gives: ['out']
		flow:  noop
	}
}

// test_up_to_date_when_outputs_are_newer checks the skipping case.
fn test_up_to_date_when_outputs_are_newer() {
	assert is_up_to_date(mtime_fixture('fresh'))
}

// test_stale_on_nested_change checks that directories are walked.
fn test_stale_on_nested_change() {
	f := mtime_fixture('changed')
	set_mtime('src/sub/a.v', -10)
	assert !is_up_to_date(f)
}

// test_stale_on_nested_creation_and_removal checks directory mtimes are used.
fn test_stale_on_nested_creation_and_removal() {
	f := mtime_fixture('created')
	os.write_file('src/sub/b.v', '')!
	assert !is_up_to_date(f)

	g := mtime_fixture('removed')
	os.rm('src/sub/a.v')!
	assert !is_up_to_date(g)
}

// test_stale_on_missing_paths checks that missing paths never skip.
fn test_stale_on_missing_paths() {
	f := mtime_fixture('missing_out')
	os.rm('out')!
	assert !is_up_to_date(f)

	g := mtime_fixture('missing_in')
	os.rmdir_all('src')!
	assert !is_up_to_date(g)
}

// test_stale_on_same_second checks that a tie counts as out of date.
fn test_stale_on_same_second() {
	f := mtime_fixture('tie')
	set_mtime('src', -50)
	assert !is_up_to_date(f)
}

// test_output_inside_input_dir_is_not_an_input checks outputs are excluded.
fn test_output_inside_input_dir_is_not_an_input() {
	f := mtime_fixture('nested_out')
	os.write_file('src/out', '')!
	set_mtime('src/sub', -100)
	set_mtime('src', -100)
	set_mtime('src/out', -50)
	assert is_up_to_date(Flow{ ...f, gives: ['src/out'] })
}

// test_no_outputs_is_never_up_to_date checks a flow without gives always runs.
fn test_no_outputs_is_never_up_to_date() {
	f := mtime_fixture('no_gives')
	assert !is_up_to_date(Flow{ ...f, gives: [] })
}

// test_cycle_is_rejected checks that implicit dependency cycles are errors.
fn test_cycle_is_rejected() {
	mut c := new()
	c.add_flow(id: 'a', takes: ['y'], gives: ['x'], flow: noop)
	c.add_flow(id: 'b', takes: ['x'], gives: ['y'], flow: noop)
	c.execute(['a']) or {
		assert err.msg().contains('cycle')
		return
	}
	assert false
}

// test_unknown_flow_and_hook_are_rejected checks that unknown ids are errors.
fn test_unknown_flow_and_hook_are_rejected() {
	mut c := new()
	c.add_flow(id: 'a', pre: [FlowDep{ id: 'nope' }], check: .none, flow: noop)
	c.execute(['b']) or {
		assert err.msg().contains('`b`')
		c.execute(['a']) or {
			assert err.msg().contains('`nope`')
			return
		}
	}
	assert false
}

// test_missing_output_fails_the_flow checks gives are verified after a run.
fn test_missing_output_fails_the_flow() {
	sandbox('no_output')
	mut c := new()
	c.add_flow(id: 'a', gives: ['out'], flow: noop)
	c.execute(['a']) or {
		assert err.msg().contains('did not produce')
		return
	}
	assert false
}

// test_system_args_reports_failures covers the error messages of system_args.
fn test_system_args_reports_failures() {
	system_args(['true'])!
	system_args(['imp_no_such_binary']) or {
		assert err.msg() == 'could not start `imp_no_such_binary`'
		system_args([]) or {
			assert err.msg() == 'no program to run'
			return
		}
	}
	assert false
}

// test_check_binary covers present and missing binaries.
fn test_check_binary() {
	assert check_binary(['sh'])
	assert !check_binary(['sh', 'imp_no_such_binary'])
}

// test_check_system_c_links covers missing headers and failed links.
fn test_check_system_c_links() {
	assert check_system_c_links(['stdio.h', 'math.h'])
	assert check_system_c_links(['math.h'], flags: ['-lm'])
	assert !check_system_c_links(['stdio.h', 'imp_no_such_header.h'])
	// the header is fine here, so only the failed link can fail the check
	assert !check_system_c_links(['stdio.h'], flags: ['-limp_no_such_lib'])
}

// test_check_v_modules covers vmodules, vlib, dotted names and ./modules.
fn test_check_v_modules() {
	dir := sandbox('vmodules')
	os.mkdir_all('mods/a/b')!
	os.mkdir_all('mods/empty')!
	os.write_file('mods/a/b/b.v', 'module b')!
	old := os.getenv('VMODULES')
	os.setenv('VMODULES', os.join_path(dir, 'mods'), true)
	defer {
		os.setenv('VMODULES', old, true)
	}
	assert check_v_modules(['os', 'a.b'])
	assert !check_v_modules(['empty'])
	assert !check_v_modules(['imp_no_such_module'])

	os.mkdir_all('modules/local')!
	os.write_file('modules/local/local.v', 'module local')!
	assert check_v_modules(['local'])
}

// script is the build script of the end to end test.
const script = "import x.imp

const target = 'app'

mut ctx := imp.new()
ctx.add_target(target)
ctx.add_flow(
	id:    'build'
	help:  'build the app'
	takes: ['src']
	gives: [target]
	flow:  fn (self imp.Flow) ! {
		imp.system_args(['cp', 'src/app.sh', target])!
		chmod(target, 0o755)!
	}
)
ctx.add_flow(
	id:    'fresh'
	help:  'clean, then build'
	takes: []
	check: .none
	pre:   [imp.FlowDep{id: 'clean'}, imp.FlowDep{id: 'build'}]
	post:  [imp.FlowDep{id: 'run'}]
	flow:  fn (self imp.Flow) ! {}
)
ctx.add_flow(id: 'note', check: .none, flow: fn (self imp.Flow) ! {})
ctx.add_flow(id: 'always', check: .none, flow: fn (self imp.Flow) ! {})
ctx.add_flow(
	id:    'stamp'
	takes: ['src']
	gives: ['stamp']
	pre:   [imp.FlowDep{id: 'note'}]
	post:  [imp.FlowDep{id: 'note'}, imp.FlowDep{id: 'always', required: true}]
	flow:  fn (self imp.Flow) ! {
		write_file('stamp', '')!
	}
)
ctx.add_flow(
	id:    'wipe'
	check: .none
	flow:  fn (self imp.Flow) ! {
		rm('forced') or {}
	}
)
ctx.add_flow(
	id:    'forced'
	takes: ['src']
	gives: ['forced']
	pre:   [imp.FlowDep{id: 'wipe', required: true}]
	flow:  fn (self imp.Flow) ! {
		write_file('forced', '')!
	}
)
ctx.add_flow(
	id:    'mode'
	takes: ['src']
	gives: ['mode']
	flags: [imp.Flag{id: 'fast', help: 'go fast'}]
	flow:  fn (self imp.Flow) ! {
		println(self.flag('fast'))
		write_file('mode', '')!
	}
)
ctx.add_flow(
	id:    'usemode'
	takes: ['mode']
	check: .none
	flow:  fn (self imp.Flow) ! {}
)
package := if ctx.flag(id: 'android', help: 'package an apk') { 'pkg.apk' } else { 'pkg' }
ctx.add_flow(
	id:    'pkg'
	gives: [package]
	flow:  fn [package] (self imp.Flow) ! {
		println(self.flag('android'))
		write_file(package, '')!
	}
)
ctx.add_flow(
	id:    'hashed'
	takes: ['data']
	gives: ['hashed']
	check: .hash
	flags: [imp.Flag{id: 'fast'}]
	flow:  fn (self imp.Flow) ! {
		println(self.flag('fast'))
		if exists('poison') {
			return error('poisoned')
		}
		write_file('hashed', '')!
	}
)
ctx.add_flow(
	id:    'fails'
	check: .none
	post:  [imp.FlowDep{id: 'note'}]
	flow:  fn (self imp.Flow) ! {
		imp.system_args(['false'])!
		println('unreachable')
	}
)
ctx.add_flow(
	id:    'need'
	check: .none
	flow:  fn (self imp.Flow) ! {
		println(imp.check_binary(['imp_no_such_binary']))
		imp.check_binary(['imp_no_such_binary'], panic: true)
		println('unreachable')
	}
)
ctx.add_auto_flow(.clean)
ctx.add_auto_flow(.run)
ctx.default(['build'])
ctx.flow()
"

// test_end_to_end replays the session at the bottom of spec.vsh.
fn test_end_to_end() {
	sandbox('e2e')
	os.mkdir_all('src')!
	os.write_file('src/app.sh', '#!/bin/sh\necho running\n')!
	os.write_file('build.vsh', script)!
	// the installed module path, as in the README
	res := os.exec([@VEXE, '-o', 'build', 'build.vsh'])
	assert res.exit_code == 0, res.output
	// compiling a .vsh also runs it, which has built the default flow
	os.rm('app')!

	flows := fn (args string) string {
		mut argv := ['./build']
		argv << args.fields()
		res := os.exec(argv)
		assert res.exit_code == 0, res.output
		return res.output.trim_space()
	}
	assert flows('') == '>> build'
	assert os.exists('app')
	// the output must not share a second with the sources, or build reruns
	set_mtime('src/app.sh', -10)
	set_mtime('src', -10)
	assert flows('run') == '>> build (skip)\n>> run\nrunning'
	assert flows('clean') == '>> clean'
	assert !os.exists('app')
	assert flows('run') == '>> build\n>> run\nrunning'
	assert flows('clean run') == '>> clean\n>> build\n>> run\nrunning'
	// pre hooks run ahead of the check, so the fresh build is not skipped
	assert flows('fresh') == '>> clean\n>> build\n>> fresh\n>> run\nrunning'
	assert flows('help').contains('build the app (default)')

	// the first run works, so all hooks run; the skipped run keeps required ones
	assert flows('stamp') == '>> note\n>> stamp\n>> always'
	assert flows('stamp') == '>> stamp (skip)\n>> always'
	// a required pre hook comes ahead of the check, and defeats the skip
	assert flows('forced') == '>> wipe\n>> forced'
	assert flows('forced') == '>> wipe\n>> forced'

	assert flows('mode') == '>> mode\nfalse'
	assert flows('mode') == '>> mode (skip)'
	// a set flag defeats the skip, whatever its position
	assert flows('mode --fast') == '>> mode\ntrue'
	// and reaches a flow that only runs as a dependency
	assert flows('--fast usemode') == '>> mode\ntrue\n>> usemode'
	assert flows('help').contains('--fast  go fast')
	unknown := os.exec(['./build', 'build', '--nope'])
	assert unknown.exit_code == 1
	assert unknown.output.contains('unknown flag `--nope`')

	// a script-level flag picks the target, so each variant has its own skip
	assert flows('pkg --android') == '>> pkg\ntrue'
	assert os.exists('pkg.apk') && !os.exists('pkg')
	assert flows('pkg') == '>> pkg\nfalse'
	assert flows('pkg --android') == '>> pkg (skip)'
	assert flows('help').contains('--android  package an apk')

	os.mkdir_all('data')!
	os.write_file('data/a', '1')!
	assert flows('hashed') == '>> hashed\nfalse'
	assert flows('hashed') == '>> hashed (skip)'
	// an mtime alone means nothing, even backdated or in the future
	set_mtime('data/a', 100)
	assert flows('hashed') == '>> hashed (skip)'
	// a change within the same second is seen
	os.write_file('data/a', '2')!
	assert flows('hashed') == '>> hashed\nfalse'
	os.write_file('data/b', '')!
	assert flows('hashed') == '>> hashed\nfalse'
	os.rm('data/b')!
	assert flows('hashed') == '>> hashed\nfalse'
	// flags are remembered, in both directions
	assert flows('hashed --fast') == '>> hashed\ntrue'
	assert flows('hashed --fast') == '>> hashed (skip)'
	assert flows('hashed') == '>> hashed\nfalse'
	os.rm('hashed')!
	assert flows('hashed') == '>> hashed\nfalse'
	// a failed run leaves nothing to skip on, even if the inputs are reverted
	os.write_file('poison', '')!
	os.write_file('data/a', '3')!
	assert os.exec(['./build', 'hashed']).exit_code == 1
	os.rm('poison')!
	os.write_file('data/a', '2')!
	assert flows('hashed') == '>> hashed\nfalse'

	fails := os.exec(['./build', 'fails'])
	assert fails.exit_code == 1
	assert fails.output.contains('flow `fails` failed: `false` exited with status 1')
	assert !fails.output.contains('unreachable')
	assert !fails.output.contains('>> note')

	need := os.exec(['./build', 'need', 'build'])
	assert need.exit_code == 1
	assert need.output.contains('false')
	assert need.output.contains('check failed: missing binary: imp_no_such_binary')
	assert !need.output.contains('unreachable')
	assert !need.output.contains('>> build')

	bad := os.exec(['./build', 'nope'])
	assert bad.exit_code == 1
	assert bad.output.contains('unknown flow `nope`')
}

// test_failed_mtime_flow_reruns checks partial outputs of a failure are not
// taken as up to date, although they are newer than the inputs.
fn test_failed_mtime_flow_reruns() {
	sandbox('failed_mtime')
	os.write_file('src', '')!
	set_mtime('src', -100)
	os.write_file('poison', '')!
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['src']
		gives: ['out']
		flow:  fn (_ Flow) ! {
			if os.exists('poison') {
				os.write_file('out', 'partial')!
				return error('poisoned')
			}
			os.write_file('out', 'done')!
		}
	)
	c.execute(['build']) or { assert err.msg().contains('poisoned') }
	assert os.read_file('out')! == 'partial'
	os.rm('poison')!
	c.execute(['build'])!
	assert os.read_file('out')! == 'done'
	// success clears the mark, so the flow can be skipped again
	assert is_up_to_date(c.find('build')!) && !os.exists(dirty_path(c.find('build')!))
}

// test_post_hook_waits_for_active_producer checks that a post hook needing the
// flow that pulled in its owner runs after that flow, not before it.
fn test_post_hook_waits_for_active_producer() {
	sandbox('post_waits')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [FlowDep{
			id: 'audit'
		}]
		flow:  fn (_ Flow) ! {
			os.write_file('gen.txt', 'gen')!
		}
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		flow:  fn (_ Flow) ! {
			os.write_file('app.txt', os.read_file('gen.txt')! + ' app')!
		}
	)
	c.add_flow(
		id:    'audit'
		takes: ['app.txt']
		check: .none
		flow:  fn (_ Flow) ! {
			os.write_file('audit.txt', os.read_file('app.txt')!)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('audit.txt')! == 'gen app'
	os.rm('audit.txt')!
	// requested on its own, the hook's producer is already done
	c.execute(['gen'])!
	assert os.read_file('audit.txt')! == 'gen app'
}

// test_absolute_and_relative_paths_match checks producers are found whatever
// the spelling of a path.
fn test_absolute_and_relative_paths_match() {
	sandbox('abs_paths')
	assert paths_overlap(os.abs_path('gen.txt'), 'gen.txt')
	assert paths_overlap('./', os.abs_path('gen.txt'))
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: [os.abs_path('gen.txt')]
		check: .none
		flow:  fn (_ Flow) ! {
			os.write_file('gen.txt', 'gen')!
		}
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		check: .none
		flow:  fn (_ Flow) ! {
			os.read_file('gen.txt')!
		}
	)
	c.execute(['build'])!
}

// test_hash_flow_over_own_state_dir checks a hash flow taking the directory
// holding `.imp` does not see its own stamp as a change.
fn test_hash_flow_over_own_state_dir() {
	dir := sandbox('hash_root')
	runs := os.join_path(dir, 'runs')
	os.mkdir_all('proj')!
	os.chdir('proj')!
	os.write_file('a', '1')!
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['.']
		gives: ['out']
		check: .hash
		flow:  fn [runs] (_ Flow) ! {
			os.write_file(runs, (os.read_file(runs) or { '' }) + 'x')!
			os.write_file('out', '')!
		}
	)
	for _ in 0 .. 3 {
		c.execute(['build'])!
	}
	assert os.read_file(runs)! == 'x'
	os.write_file('a', '2')!
	c.execute(['build'])!
	assert os.read_file(runs)! == 'xx'
}
