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

// record is a flow body that reads the inputs, creates the outputs and appends
// the flow id to `log`, so that a test can check the order flows ran in.
// Directories are left to the test.
fn record(f Flow) ! {
	for path in f.takes {
		if !os.is_dir(path) {
			os.read_file(path)!
		}
	}
	for path in f.gives {
		if !os.is_dir(path) {
			os.write_file(path, f.id)!
		}
	}
	os.write_file('log', (os.read_file('log') or { '' }) + '${f.id} ')!
}

// hook is a hook on the flow `id`, left out when its owner is skipped.
fn hook(id string) FlowDep {
	return FlowDep{
		id: id
	}
}

// required is a hook on the flow `id` that always runs.
fn required(id string) FlowDep {
	return FlowDep{
		id:       id
		required: true
	}
}

// restore_env sets the environment variable `name` back to `value`, or unsets it
// when it was not set.
fn restore_env(name string, value ?string) {
	if v := value {
		os.setenv(name, v, true)
	} else {
		os.unsetenv(name)
	}
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

// test_hash_skip_needs_outputs_and_inputs checks that, like an mtime flow, a
// hash flow without outputs or with an input missing is never skipped.
fn test_hash_skip_needs_outputs_and_inputs() {
	sandbox('hash_needs')
	os.write_file('src', '')!
	os.write_file('out', '')!
	f := Flow{
		id:    'h'
		takes: ['src']
		gives: ['out']
		check: .hash
		flow:  noop
	}
	c := new()
	digest := c.digest(f)
	os.mkdir_all(os.dir(stamp_path(f)))!
	os.write_file(stamp_path(f), digest)!
	assert is_unchanged(f, digest)
	assert !is_unchanged(Flow{ ...f, gives: [] }, digest)
	os.rm('src')!
	assert !is_unchanged(f, digest)
}

// test_walks_do_not_follow_links checks that a link loop in an input directory
// cannot trap the skip checks, which take a dangling link as an entry like any
// other.
fn test_walks_do_not_follow_links() {
	sandbox('links')
	os.mkdir_all('src')!
	os.write_file('src/a.v', '')!
	os.symlink('.', 'src/loop')!
	os.symlink('nowhere', 'src/dangling')!
	os.write_file('out', '')!
	set_mtime('src/a.v', -100)
	set_mtime('src', -100)
	set_mtime('out', -50)
	f := Flow{
		id:    'f'
		takes: ['src']
		gives: ['out']
		flow:  noop
	}
	assert is_up_to_date(f)
	c := new()
	digest := c.digest(f)
	assert c.digest(f) == digest
	os.rm('src/dangling')!
	assert c.digest(f) != digest
}

// test_remove_deletes_links_not_their_targets checks that removing a target
// never reaches through a link, even one inside a removed directory, and that a
// missing path is not an error.
fn test_remove_deletes_links_not_their_targets() {
	sandbox('remove')
	os.mkdir_all('keep/sub')!
	os.write_file('keep/sub/file', '')!
	os.mkdir_all('dir')!
	os.symlink('../keep', 'dir/link')!
	os.symlink('keep', 'link')!
	os.symlink('nowhere', 'dangling')!
	os.write_file('file', '')!
	for path in ['dir', 'link', 'dangling', 'file', 'missing'] {
		remove(path)!
	}
	assert os.exists('keep/sub/file')
	assert !os.exists('dir') && !os.exists('file')
	assert !os.is_link('link') && !os.is_link('dangling')
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
			assert err.msg() == 'flow `a` has unknown pre hook `nope`'
			return
		}
	}
	assert false
}

// test_pre_hook_cycle_is_rejected checks that cycles running through pre hooks,
// required or not, are errors like those through implicit dependencies.
fn test_pre_hook_cycle_is_rejected() {
	mut c := new()
	c.add_flow(id: 'a', pre: [hook('b')], check: .none, flow: noop)
	c.add_flow(id: 'b', takes: ['x'], check: .none, flow: noop)
	c.add_flow(id: 'c', gives: ['x'], pre: [required('a')], check: .none, flow: noop)
	c.execute(['a']) or {
		assert err.msg().contains('cycle')
		return
	}
	assert false
}

// test_flow_taking_its_own_output_is_not_a_cycle checks that a flow rewriting a
// file in place does not depend on itself.
fn test_flow_taking_its_own_output_is_not_a_cycle() {
	sandbox('in_place')
	os.write_file('main.v', '')!
	mut c := new()
	c.add_flow(id: 'fmt', takes: ['main.v'], gives: ['main.v'], check: .none, flow: record)
	c.execute(['fmt'])!
	assert os.read_file('log')! == 'fmt '
}

// test_mistakes_are_reported_before_any_flow_runs checks that an unknown flow,
// whether requested or named as a hook anywhere in the script, stops the build
// before anything ran, and that the message says where the name is.
fn test_mistakes_are_reported_before_any_flow_runs() {
	sandbox('validate_first')
	mut c := new()
	c.add_flow(id: 'ok', check: .none, flow: record)
	mut msg := ''
	c.execute(['ok', 'nope']) or { msg = err.msg() }
	assert msg == 'unknown flow `nope`'
	c.add_flow(id: 'typo', check: .none, post: [hook('missing')], flow: record)
	msg = ''
	c.execute(['ok']) or { msg = err.msg() }
	assert msg == 'flow `typo` has unknown post hook `missing`'
	assert !os.exists('log')
}

// test_default_flows checks that flags alone run the default flows, and that a
// build with nothing to run is an error, as is an unknown default.
fn test_default_flows() {
	sandbox('defaults')
	mut c := new()
	c.add_flow(
		id:    'b'
		check: .none
		flags: [Flag{
			id: 'fast'
		}]
		flow:  fn (f Flow) ! {
			os.write_file('fast', f.flag('fast').str())!
		}
	)
	mut msg := ''
	c.execute([]) or { msg = err.msg() }
	assert msg == 'no flow given and no default flow set'
	c.default(['nope'])
	msg = ''
	c.execute([]) or { msg = err.msg() }
	assert msg == 'unknown default flow `nope`'
	c.default(['b'])
	c.execute(['--fast'])!
	assert os.read_file('fast')! == 'true'
	c.execute([])!
	assert os.read_file('fast')! == 'false'
}

// test_help_flow_replaces_the_help checks that a flow called help runs instead
// of the generated help, whichever way the help is asked for.
fn test_help_flow_replaces_the_help() {
	sandbox('help_flow')
	mut c := new()
	c.add_flow(id: 'help', check: .none, flow: record)
	for arg in ['help', '-h', '--help'] {
		c.execute([arg])!
	}
	assert os.read_file('log')! == 'help help help '
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

// test_check_system_c_links_uses_cc checks that $CC runs with its arguments,
// and that a missing one fails the check rather than falling back to another.
fn test_check_system_c_links_uses_cc() {
	old := os.getenv_opt('CC')
	defer {
		restore_env('CC', old)
	}
	// stand-ins for a compiler, succeeding or failing whatever they are given
	os.setenv('CC', 'sh -c true', true)
	assert check_system_c_links(['imp_no_such_header.h'])
	os.setenv('CC', 'sh -c false', true)
	assert !check_system_c_links(['stdio.h'])
	os.setenv('CC', 'imp_no_such_cc', true)
	assert !check_system_c_links(['stdio.h'])
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
ctx.add_flow(
	id:    'needcc'
	check: .none
	flow:  fn (self imp.Flow) ! {
		imp.check_system_c_links(['stdio.h'], panic: true)
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

	// a missing compiler is reported by the name $CC gives it
	old_cc := os.getenv_opt('CC')
	os.setenv('CC', 'imp_no_such_cc -O2', true)
	needcc := os.exec(['./build', 'needcc'])
	restore_env('CC', old_cc)
	assert needcc.exit_code == 1
	assert needcc.output.contains('check failed: missing C compiler: imp_no_such_cc')

	bad := os.exec(['./build', 'nope'])
	assert bad.exit_code == 1
	assert bad.output.contains('unknown flow `nope`')
}

// mistakes_script is a build script whose mistakes only show when it runs. The
// mistake to make is picked with the IMP_MISTAKE environment variable.
const mistakes_script = "import x.imp

mut ctx := imp.new()
ctx.add_target('app')
ctx.add_flow(id: 'docs', name: 'Docs', help: 'write the docs', check: .none, flow: fn (self imp.Flow) ! {})
ctx.add_flow(id: 'talk', check: .none, flags: [imp.Flag{id: 'quiet'}], flow: fn (self imp.Flow) ! {})
ctx.add_flow(
	id:    'sneaky'
	check: .none
	flow:  fn (self imp.Flow) ! {
		println(self.flag('quiet'))
	}
)
match getenv('IMP_MISTAKE') {
	'duplicate' { ctx.add_flow(id: 'docs', check: .none, flow: fn (self imp.Flow) ! {}) }
	'targets' { ctx.add_target('lib') }
	else {}
}
ctx.add_auto_flow(.run)
ctx.default(['docs'])
ctx.flow()
"

// test_script_mistakes_are_reported checks the messages a build script gets for
// its mistakes, and that the names and help of its flows reach the user.
fn test_script_mistakes_are_reported() {
	sandbox('mistakes')
	os.write_file('build.vsh', mistakes_script)!
	// compiling a .vsh also runs it, which runs the default flow
	res := os.exec([@VEXE, '-o', 'build', 'build.vsh'])
	assert res.exit_code == 0, res.output
	defer {
		os.unsetenv('IMP_MISTAKE')
	}
	build := fn (mistake string, args ...string) os.Result {
		os.setenv('IMP_MISTAKE', mistake, true)
		mut argv := ['./build']
		argv << args
		return os.exec(argv)
	}
	assert build('').output.trim_space() == '>> Docs'
	for help in ['help', '-h', '--help'] {
		assert build('', help).output.contains('Docs [docs]  write the docs (default)')
	}
	sneaky := build('', 'sneaky')
	assert sneaky.exit_code != 0
	assert sneaky.output.contains('imp: flow `sneaky` did not declare flag `quiet`')
	duplicate := build('duplicate')
	assert duplicate.exit_code != 0
	assert duplicate.output.contains('imp: duplicate flow id `docs`')
	targets := build('targets')
	assert targets.exit_code != 0
	assert targets.output.contains('imp: the run flow needs exactly one target, got 2')
	// the auto flows are made before any flow runs
	assert !targets.output.contains('>> Docs')
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

// test_flag_of_another_flow_does_not_defeat_a_skip checks that a set flag only
// makes the flows declaring it run, whatever their check.
fn test_flag_of_another_flow_does_not_defeat_a_skip() {
	sandbox('other_flag')
	os.write_file('src', '')!
	os.write_file('out', '')!
	set_mtime('src', -100)
	set_mtime('out', -50)
	mut c := new()
	c.add_flow(id: 'timed', takes: ['src'], gives: ['out'], flow: record)
	c.add_flow(id: 'hashed', takes: ['src'], gives: ['hashed.out'], check: .hash, flow: record)
	c.add_flow(
		id:    'other'
		check: .none
		flags: [Flag{
			id: 'fast'
		}]
		flow:  noop
	)
	c.execute(['hashed'])!
	c.execute(['timed', 'hashed', '--fast'])!
	assert os.read_file('log')! == 'hashed '
}

// test_hash_flow_reruns_when_a_script_flag_changes checks that the flags of the
// script are part of the digest, as any flow may read them.
fn test_hash_flow_reruns_when_a_script_flag_changes() {
	sandbox('script_flag_hash')
	os.write_file('src', '')!
	mut c := new()
	// declared as a build script would; the test binary was not given it
	assert !c.flag(id: 'android')
	c.add_flow(id: 'h', takes: ['src'], gives: ['h.out'], check: .hash, flow: record)
	for args in [['h'], ['h'], ['h', '--android'], ['h', '--android'], ['h']] {
		c.execute(args)!
	}
	assert os.read_file('log')! == 'h h h '
}

// test_hash_check_ignores_rewrites_with_the_same_content checks what hash checks
// are for: an input rewritten with the same content lets a hash flow skip, while
// an mtime flow sees a newer input.
fn test_hash_check_ignores_rewrites_with_the_same_content() {
	sandbox('rewrite')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		flow:  fn (_ Flow) ! {
			os.write_file('gen.txt', 'same')!
		}
	)
	c.add_flow(id: 'hashed', takes: ['gen.txt'], gives: ['hashed.out'], check: .hash, flow: record)
	c.add_flow(id: 'timed', takes: ['gen.txt'], gives: ['timed.out'], flow: record)
	for _ in 0 .. 2 {
		c.execute(['hashed', 'timed'])!
	}
	assert os.read_file('log')! == 'hashed timed timed '
}

// test_hash_flow_reruns_when_an_input_is_renamed checks that the digest covers
// the paths of the inputs, not only their contents.
fn test_hash_flow_reruns_when_an_input_is_renamed() {
	sandbox('rename')
	os.mkdir_all('data')!
	os.write_file('data/a', '1')!
	mut c := new()
	c.add_flow(id: 'h', takes: ['data'], gives: ['h.out'], check: .hash, flow: record)
	c.execute(['h'])!
	os.rename('data/a', 'data/b')!
	c.execute(['h'])!
	c.execute(['h'])!
	assert os.read_file('log')! == 'h h '
}

// test_shared_dependency_runs_once_per_pass checks that a flow needed twice runs
// once, while each requested flow is a pass of its own.
fn test_shared_dependency_runs_once_per_pass() {
	sandbox('diamond')
	mut c := new()
	c.add_flow(id: 'gen', gives: ['gen.txt'], check: .none, flow: record)
	c.add_flow(id: 'left', takes: ['gen.txt'], gives: ['l.txt'], check: .none, flow: record)
	c.add_flow(id: 'right', takes: ['gen.txt'], gives: ['r.txt'], check: .none, flow: record)
	c.add_flow(id: 'top', takes: ['l.txt', 'r.txt'], check: .none, flow: record)
	c.execute(['top'])!
	assert os.read_file('log')! == 'gen left right top '
	os.rm('log')!
	c.execute(['left', 'right'])!
	assert os.read_file('log')! == 'gen left gen right '
}

// test_pre_hooks_and_dependencies_run_in_order checks that a flow runs its
// required pre hooks, then its dependencies, then its other pre hooks, each in
// the declared order.
fn test_pre_hooks_and_dependencies_run_in_order() {
	sandbox('pre_order')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['dep.txt']
		check: .none
		pre:   [hook('opt1'), required('req1'), hook('opt2'), required('req2')]
		flow:  record
	)
	c.add_flow(id: 'dep', gives: ['dep.txt'], check: .none, flow: record)
	for id in ['opt1', 'opt2', 'req1', 'req2'] {
		c.add_flow(id: id, check: .none, flow: record)
	}
	c.execute(['top'])!
	assert os.read_file('log')! == 'req1 req2 dep opt1 opt2 top '
}

// test_post_hooks_of_a_dependency_run_before_the_dependent checks that the post
// hooks of a dependency run right after it, when they do not need the flow that
// pulled it in.
fn test_post_hooks_of_a_dependency_run_before_the_dependent() {
	sandbox('post_dep')
	mut c := new()
	c.add_flow(id: 'gen', gives: ['gen.txt'], check: .none, post: [hook('note')], flow: record)
	c.add_flow(id: 'build', takes: ['gen.txt'], check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	c.execute(['build'])!
	assert os.read_file('log')! == 'gen note build '
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

// test_post_hooks_keep_order_when_using_owner_output checks that a post hook
// taking its owner's output runs right after the owner's body, ahead of a later
// hook that removes that output.
fn test_post_hooks_keep_order_when_using_owner_output() {
	sandbox('post_order')
	mut c := new()
	c.add_flow(
		id:    'build'
		gives: ['out']
		check: .none
		post:  [FlowDep{
			id: 'audit'
		}, FlowDep{
			id: 'clean'
		}]
		flow:  fn (_ Flow) ! {
			os.write_file('out', 'built')!
			os.write_file('log', 'build')!
		}
	)
	c.add_flow(
		id:    'audit'
		takes: ['out']
		check: .none
		flow:  fn (_ Flow) ! {
			os.write_file('audit.txt', os.read_file('out')!)!
			os.write_file('log', os.read_file('log')! + ' audit')!
		}
	)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (_ Flow) ! {
			os.rm('out')!
			os.write_file('log', os.read_file('log')! + ' clean')!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'build audit clean'
	assert os.read_file('audit.txt')! == 'built'
	assert !os.exists('out')
}

// test_post_hooks_keep_order_when_waiting_for_active_producer checks that the
// hooks after a post hook waiting for the flow that pulled in its owner wait as
// well. A cleanup overtaking the waiting hook would delete the input of that
// flow.
fn test_post_hooks_keep_order_when_waiting_for_active_producer() {
	sandbox('post_wait_order')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [FlowDep{
			id: 'audit'
		}, FlowDep{
			id: 'clean'
		}]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('gen.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'gen build audit clean '
}

// test_post_hooks_keep_order_when_nested_hooks_wait checks that a post hook
// also comes after the post hooks of the hook before it, when those wait for a
// flow further out. The cleanup would otherwise delete the input of that flow.
fn test_post_hooks_keep_order_when_nested_hooks_wait() {
	sandbox('post_wait_nested')
	mut c := new()
	c.add_flow(id: 'pkg', takes: ['app.txt'], gives: ['pkg.txt'], check: .none, flow: record)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [FlowDep{
			id: 'audit'
		}, FlowDep{
			id: 'clean'
		}]
		flow:  record
	)
	c.add_flow(
		id:    'audit'
		takes: ['app.txt']
		check: .none
		post:  [FlowDep{
			id: 'report'
		}]
		flow:  record
	)
	c.add_flow(id: 'report', takes: ['pkg.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			remove('app.txt')!
			record(f)!
		}
	)
	c.execute(['pkg'])!
	assert os.read_file('log')! == 'gen build audit pkg report clean '
}

// test_post_hook_waits_again_for_a_flow_further_out checks that hooks waiting
// behind another hook are checked again once it ran, as they may need a flow
// further out than it did.
fn test_post_hook_waits_again_for_a_flow_further_out() {
	sandbox('post_wait_again')
	mut c := new()
	c.add_flow(id: 'pkg', takes: ['app.txt'], gives: ['pkg.txt'], check: .none, flow: record)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('audit'), hook('ship')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'ship', takes: ['pkg.txt'], check: .none, flow: record)
	c.execute(['pkg'])!
	assert os.read_file('log')! == 'gen build audit pkg ship '
}

// test_waiting_hooks_run_before_own_post_hooks checks that hooks waiting for a
// flow run in the order they began to wait, ahead of the post hooks of that
// flow, so that its cleanup cannot remove what they read.
fn test_waiting_hooks_run_before_own_post_hooks() {
	sandbox('post_queue')
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['a.txt', 'b.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('tidy')]
		flow:  record
	)
	c.add_flow(id: 'a', gives: ['a.txt'], check: .none, post: [hook('check_a')], flow: record)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('check_b')], flow: record)
	c.add_flow(id: 'check_a', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'check_b', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'tidy'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('app.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'a b build check_a check_b tidy '
}

// test_post_hook_naming_the_pulling_flow_runs_it_once checks that a post hook
// that is the flow which pulled in its owner does not run again, and that the
// hooks after it wait for that flow.
fn test_post_hook_naming_the_pulling_flow_runs_it_once() {
	sandbox('post_ancestor')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('gen.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'gen build clean '
	os.rm('log')!
	// requested on its own, gen runs build as an ordinary post hook
	c.execute(['gen'])!
	assert os.read_file('log')! == 'gen build clean '
}

// test_post_hook_cycle_runs_each_hook_once checks that post hooks naming each
// other run once per pass, whichever of them is requested.
fn test_post_hook_cycle_runs_each_hook_once() {
	sandbox('post_cycle')
	mut c := new()
	c.add_flow(id: 'x', check: .none, post: [hook('y')], flow: record)
	c.add_flow(id: 'y', check: .none, post: [hook('x'), hook('z')], flow: record)
	c.add_flow(id: 'z', check: .none, flow: record)
	c.execute(['x'])!
	assert os.read_file('log')! == 'x y z '
	os.rm('log')!
	c.execute(['y'])!
	assert os.read_file('log')! == 'y x z '
}

// test_reused_hook_keeps_waiting_post_work_first checks that a hook reached a
// second time, while its post hooks still wait for a flow further out, runs
// once, and that the hooks after it wait for those post hooks. The cleanup
// would otherwise delete an input of that flow.
fn test_reused_hook_keeps_waiting_post_work_first() {
	sandbox('post_shared')
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['a.txt', 'b.txt']
		gives: ['app.txt']
		check: .none
		flow:  record
	)
	c.add_flow(id: 'a', gives: ['a.txt'], check: .none, post: [hook('shared')], flow: record)
	c.add_flow(
		id:    'b'
		gives: ['b.txt']
		check: .none
		post:  [hook('shared'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'shared', check: .none, post: [hook('audit')], flow: record)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('a.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'a shared b build audit clean '
}

// test_post_hook_cycle_waiting_on_itself_still_runs checks that post hooks
// waiting for each other's post work, through a cycle of post hooks, all run
// at the end of the pass instead of being dropped.
fn test_post_hook_cycle_waiting_on_itself_still_runs() {
	sandbox('post_cycle_wait')
	mut c := new()
	c.add_flow(id: 'top', takes: ['a.txt', 'b.txt'], gives: ['top.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p'), hook('b'), hook('x')]
		flow:  record
	)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('a'), hook('y')], flow: record)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', check: .none, flow: record)
	c.add_flow(id: 'y', check: .none, flow: record)
	c.execute(['top'])!
	assert os.read_file('log')! == 'a b top p y x '
}

// test_required_post_hooks_of_a_skipped_flow_keep_order checks that a skipped
// flow still runs its required post hooks in order, waiting where they need
// to, and leaves out the others.
fn test_required_post_hooks_of_a_skipped_flow_keep_order() {
	sandbox('post_skipped')
	os.write_file('src', '')!
	mut c := new()
	c.add_flow(
		id:    'gen'
		takes: ['src']
		gives: ['gen.txt']
		check: .hash
		post:  [required('audit'), hook('note'), required('tidy')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	c.add_flow(
		id:    'tidy'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('app.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert os.read_file('log')! == 'gen build audit note tidy '
	os.rm('log')!
	c.execute(['build'])!
	assert os.read_file('log')! == 'build audit tidy '
}

// test_failing_waiting_post_hook_stops_the_rest checks that a post hook failing
// after it waited for a flow fails the build, and the hooks after it never run.
fn test_failing_waiting_post_hook_stops_the_rest() {
	sandbox('post_fails')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('audit'), hook('note')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'audit'
		takes: ['app.txt']
		check: .none
		flow:  fn (f Flow) ! {
			record(f)!
			return error('audit found a problem')
		}
	)
	c.add_flow(id: 'note', check: .none, flow: record)
	mut msg := ''
	c.execute(['build']) or { msg = err.msg() }
	assert msg == 'flow `audit` failed: audit found a problem'
	assert os.read_file('log')! == 'gen build audit '
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

// test_dependencies_follow_directories checks that a flow taking a directory
// depends on the flows writing into it, and that a flow taking a file depends
// on the flow giving a directory around it.
fn test_dependencies_follow_directories() {
	sandbox('dir_deps')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['out/gen.txt']
		check: .none
		flow:  fn (f Flow) ! {
			os.mkdir_all('out')!
			record(f)!
		}
	)
	c.add_flow(id: 'zip', takes: ['out'], gives: ['out.zip'], check: .none, flow: record)
	c.add_flow(
		id:    'site'
		gives: ['site']
		check: .none
		flow:  fn (f Flow) ! {
			os.mkdir_all('site')!
			os.write_file('site/index.html', '')!
			record(f)!
		}
	)
	c.add_flow(id: 'page', takes: ['site/index.html'], check: .none, flow: record)
	c.execute(['zip', 'page'])!
	assert os.read_file('log')! == 'gen zip site page '
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

// test_state_files_never_clash checks that each flow id gets a state file of its
// own, in the directory of its kind, even on a case-insensitive file system,
// while plain ids keep readable names.
fn test_state_files_never_clash() {
	plain := state_file('digest', Flow{ id: 'build-2_x', flow: noop })
	assert plain == os.join_path(stamp_dir, 'digest', 'build-2_x')
	mut seen := map[string]string{}
	for id in ['a/b', 'a_b', 'a\\b', 'a%2Fb', 'A_b', 'a.b', '.', '..', 'a b', 'é'] {
		path := state_file('digest', Flow{ id: id, flow: noop })
		name := os.file_name(path)
		assert os.dir(path) == os.join_path(stamp_dir, 'digest') && name !in ['.', '..'], path
		key := name.to_lower()
		assert key !in seen, '`${id}` and `${seen[key]}` share `${name}`'
		seen[key] = id
	}
}

// test_long_ids_get_bounded_state_files checks that ids whose encoding would
// exceed a file name limit get a short state file, still of their own, and that
// such a flow runs and is skipped like any other.
fn test_long_ids_get_bounded_state_files() {
	long := 'A'.repeat(86)
	mut names := map[string]bool{}
	for id in [long, long + 'B', 'A'.repeat(200), 'é'.repeat(100), 'a'.repeat(128), 'a'.repeat(129)] {
		name := os.file_name(state_file('dirty', Flow{ id: id, flow: noop }))
		assert name.len <= max_state_name, name
		assert name.to_lower() !in names, name
		names[name.to_lower()] = true
	}
	assert os.file_name(state_file('dirty', Flow{ id: 'a'.repeat(128), flow: noop })) == 'a'.repeat(128)
	sandbox('state_long')
	os.write_file('src', '')!
	set_mtime('src', -10)
	mut c := new()
	c.add_flow(id: long, takes: ['src'], gives: ['out'], flow: record)
	c.execute([long])!
	c.execute([long])!
	assert os.read_file('log')! == '${long} '
}

// test_flows_keep_their_state_apart checks that encoding ids for their state
// files cannot make two flows share one, as `a/b` and `a_b` would if `/` became
// `_`.
fn test_flows_keep_their_state_apart() {
	sandbox('state_apart')
	os.write_file('x', 'x')!
	os.write_file('y', 'y')!
	mut c := new()
	c.add_flow(id: 'a/b', takes: ['x'], gives: ['x.out'], check: .hash, flow: record)
	c.add_flow(id: 'a_b', takes: ['y'], gives: ['y.out'], check: .hash, flow: record)
	for id in ['a/b', 'a_b', 'a/b', 'a_b'] {
		c.execute([id])!
	}
	assert os.read_file('log')! == 'a/b a_b '
}

// test_clean_removes_targets_added_later checks that the clean flow covers the
// targets known when the build starts, files and directories alike, and that it
// does not mind targets that are already gone.
fn test_clean_removes_targets_added_later() {
	sandbox('auto_clean')
	os.write_file('bin', '')!
	os.mkdir_all('dist/sub')!
	os.write_file('dist/sub/file', '')!
	os.write_file('keep', '')!
	mut c := new()
	c.add_auto_flow(.clean)
	c.add_target('bin')
	c.add_target('dist')
	c.resolve_auto_flows()
	c.execute(['clean'])!
	assert !os.exists('bin') && !os.exists('dist') && os.exists('keep')
	c.execute(['clean'])!
}
