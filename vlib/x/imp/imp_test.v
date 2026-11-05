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

// contents returns the text of the file `path`, or a marker when it cannot be
// read, so that an assertion on a missing file fails its own test instead of
// stopping the whole test run.
fn contents(path string) string {
	return os.read_file(path) or { '<${err.msg()}>' }
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
	assert contents('log') == 'fmt '
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
	assert contents('fast') == 'true'
	c.execute([])!
	assert contents('fast') == 'false'
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
	assert contents('log') == 'help help help '
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
	assert contents('out') == 'partial'
	os.rm('poison')!
	c.execute(['build'])!
	assert contents('out') == 'done'
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
	assert contents('log') == 'hashed '
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
	assert contents('log') == 'h h h '
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
	assert contents('log') == 'hashed timed timed '
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
	assert contents('log') == 'h h '
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
	assert contents('log') == 'gen left right top '
	os.rm('log')!
	c.execute(['left', 'right'])!
	assert contents('log') == 'gen left gen right '
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
	assert contents('log') == 'req1 req2 dep opt1 opt2 top '
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
	assert contents('log') == 'gen note build '
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
	assert contents('audit.txt') == 'gen app'
	os.rm('audit.txt')!
	// requested on its own, the hook's producer is already done
	c.execute(['gen'])!
	assert contents('audit.txt') == 'gen app'
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
	assert contents('log') == 'build audit clean'
	assert contents('audit.txt') == 'built'
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
	assert contents('log') == 'gen build audit clean '
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
	assert contents('log') == 'gen build audit pkg report clean '
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
	assert contents('log') == 'gen build audit pkg ship '
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
	assert contents('log') == 'a b build check_a check_b tidy '
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
	assert contents('log') == 'gen build clean '
	os.rm('log')!
	// requested on its own, gen runs build as an ordinary post hook
	c.execute(['gen'])!
	assert contents('log') == 'gen build clean '
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
	assert contents('log') == 'x y z '
	os.rm('log')!
	c.execute(['y'])!
	assert contents('log') == 'y x z '
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
	assert contents('log') == 'a shared b build audit clean '
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
	assert contents('log') == 'a b top p y x '
}

// test_post_hook_naming_the_pulling_flow_waits_for_its_post_hooks checks that
// the hooks after a post hook naming the flow which pulled in its owner wait
// for the post hooks of that flow, which are not scheduled yet when the hook
// is reached. The cleanup would otherwise delete an input of those hooks.
fn test_post_hook_naming_the_pulling_flow_waits_for_its_post_hooks() {
	sandbox('post_ancestor_post')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('app.txt')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert contents('log') == 'gen build audit clean '
	os.rm('log')!
	c.execute(['gen'])!
	assert contents('log') == 'gen build audit clean '
}

// test_post_hook_cycle_keeps_waits_outside_it checks that breaking a cycle of
// post hooks waiting for each other leaves the waits outside the cycle in
// place, even for a flow waited for before the flows of the cycle. The cleanup
// would otherwise delete an input of a hook still part of that flow's post work.
fn test_post_hook_cycle_keeps_waits_outside_it() {
	sandbox('post_cycle_outside')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['w.txt', 'q.txt', 'b.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(id: 'w', gives: ['w.txt'], check: .none, post: [hook('a')], flow: record)
	c.add_flow(
		id:    'q'
		gives: ['q.txt']
		check: .none
		post:  [hook('w'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('a'), hook('y')], flow: record)
	c.add_flow(id: 'a', check: .none, post: [hook('p'), hook('b'), hook('x')], flow: record)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'y', check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('top.txt')!
			record(f)!
		}
	)
	c.execute(['top'])!
	assert contents('log') == 'w a q b top p y x clean '
}

// direct_cycle_waiter builds a cycle between a and b, with an outside flow
// waiting directly on `wait_on`. The outside flow is registered before b when
// it waits on a, and after b when it waits on b.
fn direct_cycle_waiter(wait_on string, y_body fn (Flow) !) Context {
	outside := if wait_on == 'a' { 'q' } else { 'r' }
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: if wait_on == 'a' {
			['a.txt', 'q.txt', 'b.txt']
		} else {
			['a.txt', 'b.txt', 'r.txt']
		}
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p'), hook('b'), hook('x')]
		flow:  record
	)
	b := Flow{
		id:    'b'
		gives: ['b.txt']
		check: .none
		post:  [hook('a'), hook('y')]
		flow:  record
	}
	waiter := Flow{
		id:    outside
		gives: ['${outside}.txt']
		check: .none
		post:  [hook(wait_on), hook('clean')]
		flow:  record
	}
	if wait_on == 'a' {
		c.add_flow(waiter)
		c.add_flow(b)
	} else {
		c.add_flow(b)
		c.add_flow(waiter)
	}
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'y', check: .none, flow: y_body)
	c.add_flow(id: 'clean', check: .none, flow: remove_top)
	return c
}

// test_post_hook_cycle_keeps_direct_waiter_after_post_work checks that waits
// outside a cycle remain pending when they name either cycle member directly.
// Releasing clean with a cycle wait would delete the input that x reads.
fn test_post_hook_cycle_keeps_direct_waiter_after_post_work() {
	for wait_on in ['a', 'b'] {
		sandbox('post_cycle_direct_wait_${wait_on}')
		mut c := direct_cycle_waiter(wait_on, record)
		c.execute(['top']) or { assert false, err.msg() }
		want := if wait_on == 'a' { 'a q b top p y x clean ' } else { 'a b r top p y x clean ' }
		assert contents('log') == want, wait_on
		assert !os.exists('top.txt')
	}
}

// test_failing_post_cycle_hook_keeps_direct_waiter_blocked checks that a hook
// failing while the cycle is released stops the outside cleanup as well as
// the remaining work within the cycle.
fn test_failing_post_cycle_hook_keeps_direct_waiter_blocked() {
	sandbox('post_cycle_direct_wait_fails')
	mut c := direct_cycle_waiter('a', fail)
	mut msg := ''
	c.execute(['top']) or { msg = err.msg() }
	assert msg == 'flow `y` failed: y found a problem'
	assert contents('log') == 'a q b top p y '
	assert contents('top.txt') == 'top'
}

// test_inline_post_hook_cycle_keeps_outside_waiter_after_post_work checks that
// a cycle reached while running its hooks keeps an outside cleanup behind all
// its post work. The producer q queues the cleanup while r needs its output.
fn test_inline_post_hook_cycle_keeps_outside_waiter_after_post_work() {
	sandbox('post_cycle_inline_wait')
	mut c := new()
	c.add_flow(
		id:    'a'
		gives: ['app.txt']
		check: .none
		post:  [hook('b'), hook('x')]
		flow:  record
	)
	c.add_flow(id: 'b', check: .none, post: [hook('r'), hook('y')], flow: record)
	c.add_flow(id: 'r', takes: ['q.txt'], check: .none, post: [hook('a')], flow: record)
	c.add_flow(
		id:    'q'
		gives: ['q.txt']
		check: .none
		post:  [hook('b'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'x', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'y', check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_app)
	c.execute(['a']) or { assert false, err.msg() }
	assert contents('log') == 'a b q r y x clean '
	assert !os.exists('app.txt')
}

// remove_top is a flow body that deletes `top.txt` if it is still there, then
// records the flow.
fn remove_top(f Flow) ! {
	remove('top.txt')!
	record(f)!
}

// test_skipped_flow_reached_by_a_released_hook_waits_for_its_post_hooks checks
// that the wait for post work not scheduled yet holds for a skipped flow too,
// whose required post hooks run in place of the others.
fn test_skipped_flow_reached_by_a_released_hook_waits_for_its_post_hooks() {
	sandbox('post_ancestor_skipped')
	os.write_file('src', '')!
	mut c := new()
	c.add_flow(
		id:    'gen'
		takes: ['src']
		gives: ['gen.txt']
		check: .hash
		post:  [required('build'), required('clean')]
		flow:  record
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .hash
		post:  [required('audit'), hook('note')]
		flow:  record
	)
	c.add_flow(
		id:    'audit'
		check: .none
		flow:  fn (f Flow) ! {
			if os.exists('cleaned') {
				return error('ran after clean')
			}
			record(f)!
		}
	)
	c.add_flow(id: 'note', check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.write_file('cleaned', '')!
			record(f)!
		}
	)
	c.execute(['build'])!
	assert contents('log') == 'gen build audit note clean '
	os.rm('log')!
	os.rm('cleaned')!
	c.execute(['build'])!
	assert contents('log') == 'audit clean '
}

// test_post_hook_cycles_are_broken_one_at_a_time checks that separate cycles of
// post hooks waiting for each other are each broken on their own, and every
// hook still runs once.
fn test_post_hook_cycles_are_broken_one_at_a_time() {
	sandbox('post_cycles')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['a.txt', 'b.txt', 'c.txt', 'd.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p'), hook('b'), hook('x')]
		flow:  record
	)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('a'), hook('y')], flow: record)
	c.add_flow(
		id:    'c'
		gives: ['c.txt']
		check: .none
		post:  [hook('r'), hook('d'), hook('z')]
		flow:  record
	)
	c.add_flow(id: 'd', gives: ['d.txt'], check: .none, post: [hook('c'), hook('u')], flow: record)
	for id in ['p', 'x', 'r', 'z'] {
		c.add_flow(id: id, takes: ['top.txt'], check: .none, flow: record)
	}
	for id in ['y', 'u'] {
		c.add_flow(id: id, check: .none, flow: record)
	}
	c.execute(['top'])!
	assert contents('log') == 'a b c d top p r y x u z '
}

// test_chain_into_post_hook_cycle_keeps_its_order checks that a chain of
// several waits leading into a cycle of post hooks is released in order, after
// the cycle is broken. Both cleanups would otherwise delete an input of `x`.
fn test_chain_into_post_hook_cycle_keeps_its_order() {
	sandbox('post_cycle_chain')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['w.txt', 'q.txt', 'r.txt', 'b.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(id: 'w', gives: ['w.txt'], check: .none, post: [hook('a')], flow: record)
	c.add_flow(
		id:    'q'
		gives: ['q.txt']
		check: .none
		post:  [hook('w'), hook('clean')]
		flow:  record
	)
	c.add_flow(
		id:    'r'
		gives: ['r.txt']
		check: .none
		post:  [hook('q'), hook('clean2')]
		flow:  record
	)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('a'), hook('y')], flow: record)
	c.add_flow(id: 'a', check: .none, post: [hook('p'), hook('b'), hook('x')], flow: record)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'y', check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_top)
	c.add_flow(id: 'clean2', check: .none, flow: remove_top)
	c.execute(['top'])!
	assert contents('log') == 'w a q r b top p y x clean clean2 '
}

// test_post_hook_cycle_through_the_pulling_flow_runs_each_once checks that post
// hooks of a flow and of the flow that pulled it in, naming each other, run
// once each and end the pass, though their post work can only wait for each
// other. The cycle is broken at the flow waited for first, `build`.
fn test_post_hook_cycle_through_the_pulling_flow_runs_each_once() {
	sandbox('post_ancestor_cycle')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		check: .none
		post:  [hook('gen'), hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'audit', check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: record)
	c.execute(['build'])!
	assert contents('log') == 'gen build clean audit '
}

// test_failing_hook_released_by_drain_stops_the_rest checks that a hook failing
// once a cycle of post hooks is broken fails the build, and the hooks waiting
// after it never run.
fn test_failing_hook_released_by_drain_stops_the_rest() {
	sandbox('post_cycle_fails')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['w.txt', 'q.txt', 'b.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(id: 'w', gives: ['w.txt'], check: .none, post: [hook('a')], flow: record)
	c.add_flow(
		id:    'q'
		gives: ['q.txt']
		check: .none
		post:  [hook('w'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'b', gives: ['b.txt'], check: .none, post: [hook('a'), hook('y')], flow: record)
	c.add_flow(id: 'a', check: .none, post: [hook('p'), hook('b'), hook('x')], flow: record)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'y'
		check: .none
		flow:  fn (f Flow) ! {
			record(f)!
			return error('y found a problem')
		}
	)
	c.add_flow(id: 'x', check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_top)
	mut msg := ''
	c.execute(['top']) or { msg = err.msg() }
	assert msg == 'flow `y` failed: y found a problem'
	assert contents('log') == 'w a q b top p y '
}

// test_post_hook_cycles_do_not_leak_into_the_next_pass checks that a pass ending
// with a cycle of post hooks broken leaves nothing waiting for the next one.
fn test_post_hook_cycles_do_not_leak_into_the_next_pass() {
	sandbox('post_cycle_passes')
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
	c.execute(['top', 'b'])!
	assert contents('log') == 'a b top p y x b a top p x y '
}

// remove_app is a flow body that deletes `app.txt` if it is still there, then
// records the flow.
fn remove_app(f Flow) ! {
	remove('app.txt')!
	record(f)!
}

// fail is a flow body that records the flow, then fails.
fn fail(f Flow) ! {
	record(f)!
	return error('${f.id} found a problem')
}

// test_flow_naming_itself_as_post_hook_runs_once checks that a flow among its
// own post hooks is not run again, and does not wait for itself.
fn test_flow_naming_itself_as_post_hook_runs_once() {
	sandbox('post_self')
	mut c := new()
	c.add_flow(id: 'a', check: .none, post: [hook('a'), hook('x')], flow: record)
	c.add_flow(id: 'x', check: .none, flow: record)
	c.execute(['a'])!
	assert contents('log') == 'a x '
}

// test_flow_naming_itself_after_a_waiting_hook_runs_once checks that a flow
// among its own post hooks does not wait for itself either once its post hooks
// waited for the flow that pulled it in.
fn test_flow_naming_itself_after_a_waiting_hook_runs_once() {
	sandbox('post_self_wait')
	mut c := new()
	c.add_flow(id: 'top', takes: ['a.txt'], gives: ['top.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p'), hook('a'), hook('x')]
		flow:  record
	)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', check: .none, flow: record)
	c.execute(['top'])!
	assert contents('log') == 'a top p x '
}

// test_repeated_post_hook_runs_once checks that a post hook listed twice runs
// once, in its first place, even while it waits for the flow that pulled in
// its owner.
fn test_repeated_post_hook_runs_once() {
	sandbox('post_repeated')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('audit'), hook('note'), hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	c.execute(['build'])!
	assert contents('log') == 'gen build audit note '
}

// test_hooks_waiting_for_the_same_post_work_keep_their_order checks that hooks
// of several dependencies, all waiting for the post work of the flow that
// pulled them in, run after it in the order they began to wait.
fn test_hooks_waiting_for_the_same_post_work_keep_their_order() {
	sandbox('post_ancestor_many')
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['g1.txt', 'g2.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(
		id:    'gen1'
		gives: ['g1.txt']
		check: .none
		post:  [hook('build'), hook('clean1')]
		flow:  record
	)
	c.add_flow(
		id:    'gen2'
		gives: ['g2.txt']
		check: .none
		post:  [hook('build'), hook('clean2')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean1', check: .none, flow: remove_app)
	c.add_flow(id: 'clean2', check: .none, flow: remove_app)
	c.execute(['build'])!
	assert contents('log') == 'gen1 gen2 build audit clean1 clean2 '
}

// test_nested_hook_reaching_the_releasing_flow_waits_for_its_post_hooks checks
// that a hook run while the hooks waiting for a body are released, and naming
// that flow, waits for its post hooks, which are not scheduled yet.
fn test_nested_hook_reaching_the_releasing_flow_waits_for_its_post_hooks() {
	sandbox('post_nested_ancestor')
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'gen', gives: ['gen.txt'], check: .none, post: [hook('lint')], flow: record)
	c.add_flow(
		id:    'lint'
		takes: ['app.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_app)
	c.execute(['build'])!
	assert contents('log') == 'gen build lint audit clean '
}

// test_wait_for_post_work_ends_at_the_pulling_flow checks that hooks waiting
// for the post work of the flow that pulled in their owner do not also wait for
// the flows further out, which need none of them.
fn test_wait_for_post_work_ends_at_the_pulling_flow() {
	sandbox('post_ancestor_outer')
	mut c := new()
	c.add_flow(id: 'pkg', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('note')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	c.execute(['pkg'])!
	assert contents('log') == 'gen build audit note pkg '
}

// test_released_hook_waits_again_for_an_active_flow checks that hooks released
// by the end of some post work wait again when they need a flow still running
// further out.
fn test_released_hook_waits_again_for_an_active_flow() {
	sandbox('post_wait_then_defer')
	mut c := new()
	c.add_flow(id: 'top', takes: ['app.txt'], gives: ['top.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('p'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'p', takes: ['top.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_app)
	c.execute(['top'])!
	assert contents('log') == 'gen build audit top p clean '
}

// test_failing_post_hook_of_the_pulling_flow_stops_the_waiting_hooks checks
// that hooks waiting for the post work of the flow that pulled in their owner
// never run when that post work fails.
fn test_failing_post_hook_of_the_pulling_flow_stops_the_waiting_hooks() {
	sandbox('post_ancestor_fails')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], check: .none, post: [hook('audit')], flow: record)
	c.add_flow(id: 'audit', check: .none, flow: fail)
	c.add_flow(id: 'clean', check: .none, flow: record)
	mut msg := ''
	c.execute(['build']) or { msg = err.msg() }
	assert msg == 'flow `audit` failed: audit found a problem'
	assert contents('log') == 'gen build audit '
}

// test_failing_body_drops_the_hooks_waiting_for_it checks that hooks waiting for
// the body of a flow never run when that body fails.
fn test_failing_body_drops_the_hooks_waiting_for_it() {
	sandbox('post_body_fails')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('audit'), hook('note')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], gives: ['app.txt'], check: .none, flow: fail)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	mut msg := ''
	c.execute(['build']) or { msg = err.msg() }
	assert msg == 'flow `build` failed: build found a problem'
	assert contents('log') == 'gen build '
}

// test_post_hook_cycle_of_three_runs_each_once checks that three flows whose
// post work waits for each other in a ring all finish, the ring broken at the
// flow waited for first.
fn test_post_hook_cycle_of_three_runs_each_once() {
	sandbox('post_cycle_three')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['a.txt', 'b.txt', 'c.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p1'), hook('b'), hook('x')]
		flow:  record
	)
	c.add_flow(
		id:    'b'
		gives: ['b.txt']
		check: .none
		post:  [hook('p2'), hook('c'), hook('y')]
		flow:  record
	)
	c.add_flow(
		id:    'c'
		gives: ['c.txt']
		check: .none
		post:  [hook('p3'), hook('a'), hook('z')]
		flow:  record
	)
	for id in ['p1', 'p2', 'p3'] {
		c.add_flow(id: id, takes: ['top.txt'], check: .none, flow: record)
	}
	for id in ['x', 'y', 'z'] {
		c.add_flow(id: id, check: .none, flow: record)
	}
	c.execute(['top'])!
	assert contents('log') == 'a b c top p1 p2 p3 x z y '
}

// test_post_hook_that_is_a_pre_hook_of_the_pulling_flow_runs_once checks that a
// pre hook of a flow, also a post hook of its dependency, runs once, wherever it
// comes first.
fn test_post_hook_that_is_a_pre_hook_of_the_pulling_flow_runs_once() {
	sandbox('post_is_pre')
	mut c := new()
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		check: .none
		pre:   [required('prep'), hook('lint')]
		flow:  record
	)
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('prep'), hook('lint')]
		flow:  record
	)
	c.add_flow(id: 'prep', check: .none, flow: record)
	c.add_flow(id: 'lint', check: .none, flow: record)
	c.execute(['build'])!
	assert contents('log') == 'prep gen lint build '
}

// test_skipped_producer_leaves_out_hooks_naming_the_pulling_flow checks that a
// skipped flow leaves out its optional post hooks, including the flow that
// pulled it in and the hooks that would have waited for it.
fn test_skipped_producer_leaves_out_hooks_naming_the_pulling_flow() {
	sandbox('post_ancestor_left_out')
	os.write_file('src', '')!
	mut c := new()
	c.add_flow(
		id:    'gen'
		takes: ['src']
		gives: ['gen.txt']
		check: .hash
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'build', takes: ['gen.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: record)
	c.execute(['build'])!
	assert contents('log') == 'gen build clean '
	os.rm('log')!
	c.execute(['build'])!
	assert contents('log') == 'build '
}

// remove_c is a flow body that deletes `c.txt` if it is still there, then
// records the flow.
fn remove_c(f Flow) ! {
	remove('c.txt')!
	record(f)!
}

// test_nested_pulling_flows_finish_their_post_work_inside_out checks that along
// a chain of flows, each with post hooks naming the flow that pulled it in, the
// cleanups wait for the post work further in, whichever flow is requested.
fn test_nested_pulling_flows_finish_their_post_work_inside_out() {
	sandbox('post_ancestor_chain')
	mut c := new()
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('b'), hook('clean_a')]
		flow:  record
	)
	c.add_flow(
		id:    'b'
		takes: ['a.txt']
		gives: ['b.txt']
		check: .none
		post:  [hook('c'), hook('clean_b')]
		flow:  record
	)
	c.add_flow(
		id:    'c'
		takes: ['b.txt']
		gives: ['c.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['c.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean_a', check: .none, flow: remove_c)
	c.add_flow(id: 'clean_b', check: .none, flow: remove_c)
	for id in ['c', 'b', 'a'] {
		c.execute([id])!
		assert contents('log') == 'a b c audit clean_b clean_a ', id
		os.rm('log')!
	}
}

// test_pre_hook_naming_the_flow_it_runs_ahead_of_waits_for_its_post_hooks
// checks that the hooks after a post hook of a pre hook, naming the flow the pre
// hook runs ahead of, wait for the post hooks of that flow, whether or not the
// pre hook is required.
fn test_pre_hook_naming_the_flow_it_runs_ahead_of_waits_for_its_post_hooks() {
	for dep in [hook('lint'), required('lint')] {
		sandbox('post_pre_ancestor_${dep.required}')
		mut c := new()
		c.add_flow(
			id:    'build'
			gives: ['app.txt']
			check: .none
			pre:   [dep]
			post:  [hook('audit')]
			flow:  record
		)
		c.add_flow(
			id:    'lint'
			check: .none
			post:  [hook('build'), hook('note')]
			flow:  record
		)
		c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
		c.add_flow(id: 'note', check: .none, flow: remove_app)
		c.execute(['build'])!
		assert contents('log') == 'lint build audit note ', dep.str()
	}
}

// test_post_hook_naming_a_finished_producer_does_not_wait checks that a post
// hook naming a dependency whose post work is already complete, after waiting
// for the flow that names it, lets the hooks after it run at once.
fn test_post_hook_naming_a_finished_producer_does_not_wait() {
	sandbox('post_done_producer')
	mut c := new()
	c.add_flow(id: 'gen', gives: ['gen.txt'], check: .none, post: [hook('audit')], flow: record)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('gen'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_app)
	c.execute(['build'])!
	assert contents('log') == 'gen build audit clean '
}

// test_repeated_request_waits_for_post_work_each_pass checks that a flow
// requested twice starts its second pass afresh, waiting as it did in the first.
fn test_repeated_request_waits_for_post_work_each_pass() {
	sandbox('post_ancestor_twice')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('build'), hook('clean')]
		flow:  record
	)
	c.add_flow(
		id:    'build'
		takes: ['gen.txt']
		gives: ['app.txt']
		check: .none
		post:  [hook('audit')]
		flow:  record
	)
	c.add_flow(id: 'audit', takes: ['app.txt'], check: .none, flow: record)
	c.add_flow(id: 'clean', check: .none, flow: remove_app)
	c.execute(['build', 'build'])!
	assert contents('log') == 'gen build audit clean gen build audit clean '
}

// test_failing_pass_stops_the_requested_flows_after_it checks that a requested
// flow failing ends the build, so the flows requested after it never run.
fn test_failing_pass_stops_the_requested_flows_after_it() {
	sandbox('pass_fails')
	mut c := new()
	c.add_flow(id: 'bad', check: .none, post: [hook('note')], flow: fail)
	c.add_flow(id: 'ok', check: .none, flow: record)
	c.add_flow(id: 'note', check: .none, flow: record)
	mut msg := ''
	c.execute(['bad', 'ok']) or { msg = err.msg() }
	assert msg == 'flow `bad` failed: bad found a problem'
	assert contents('log') == 'bad '
}

// test_post_hook_cycle_of_a_dependency_finishes_before_the_dependent checks
// that post hooks of a dependency naming each other carry on at once, rather
// than wait for the end of the pass, so they still run ahead of the dependent.
fn test_post_hook_cycle_of_a_dependency_finishes_before_the_dependent() {
	sandbox('post_cycle_dep')
	mut c := new()
	c.add_flow(id: 'top', takes: ['x.txt'], check: .none, flow: record)
	c.add_flow(id: 'x', gives: ['x.txt'], check: .none, post: [hook('y'), hook('w')], flow: record)
	c.add_flow(id: 'y', check: .none, post: [hook('x'), hook('z')], flow: record)
	c.add_flow(id: 'z', check: .none, flow: record)
	c.add_flow(id: 'w', check: .none, flow: record)
	c.execute(['top'])!
	assert contents('log') == 'x y z w top '
}

// test_post_hook_ring_is_broken_at_the_flow_in_it_waited_for_first checks that
// a ring of post work entered from a flow waited for earlier is broken at the
// flow in the ring waited for first, `b`, not where the wait leads into it,
// `a`, nor at the flow outside, `w`, whose cleanup would delete an input of `x`.
fn test_post_hook_ring_is_broken_at_the_flow_in_it_waited_for_first() {
	sandbox('post_ring_entry')
	mut c := new()
	c.add_flow(
		id:    'top'
		takes: ['a.txt', 'b.txt', 'c.txt', 'w.txt', 'q.txt']
		gives: ['top.txt']
		check: .none
		flow:  record
	)
	c.add_flow(
		id:    'a'
		gives: ['a.txt']
		check: .none
		post:  [hook('p1'), hook('b'), hook('x')]
		flow:  record
	)
	c.add_flow(
		id:    'b'
		gives: ['b.txt']
		check: .none
		post:  [hook('p2'), hook('c'), hook('y')]
		flow:  record
	)
	c.add_flow(
		id:    'c'
		gives: ['c.txt']
		check: .none
		post:  [hook('p3'), hook('a'), hook('z')]
		flow:  record
	)
	c.add_flow(
		id:    'w'
		gives: ['w.txt']
		check: .none
		post:  [hook('p0'), hook('a'), hook('x0')]
		flow:  record
	)
	c.add_flow(
		id:    'q'
		gives: ['q.txt']
		check: .none
		post:  [hook('w'), hook('clean')]
		flow:  record
	)
	for id in ['p0', 'p1', 'p2', 'p3', 'x'] {
		c.add_flow(id: id, takes: ['top.txt'], check: .none, flow: record)
	}
	for id in ['x0', 'y', 'z'] {
		c.add_flow(id: id, check: .none, flow: record)
	}
	c.add_flow(id: 'clean', check: .none, flow: remove_top)
	c.execute(['top'])!
	assert contents('log') == 'a b c w q top p1 p2 p3 p0 x z y x0 clean '
}

// finish_step is the step at which the post work of the flow `id` is complete.
fn finish_step(id string) Step {
	return Step{
		id:     id
		finish: true
	}
}

// test_cycle_start_picks_the_ring_member_waited_for_first checks the choice of
// `cycle_start` directly: a ring entered from outside, at a member other than
// the one waited for first, and a flow holding its own finish step.
fn test_cycle_start_picks_the_ring_member_waited_for_first() {
	// w waits on a, and the ring a -> b -> c -> a, where b was waited for first
	ring := Pass{
		order:   ['w', 'b', 'c', 'a']
		waiting: {
			'w': [[Step{
				id: 'clean'
			}]]
			'a': [[finish_step('c')], [finish_step('w')]]
			'b': [[finish_step('a')]]
			'c': [[finish_step('b')]]
		}
	}
	assert ring.cycle_start() == Cycle{
		start:   'b'
		members: ['b', 'c', 'a']
	}
	// flows no longer waited for are passed over
	mut finished := ring
	finished.order = ['gone', 'w', 'b', 'c', 'a']
	assert finished.cycle_start().start == 'b'
	looped := Pass{
		order:   ['x']
		waiting: {
			'x': [[Step{
				id: 'z'
			}, finish_step('x')]]
		}
	}
	assert looped.cycle_start() == Cycle{
		start:   'x'
		members: ['x']
	}
}

// test_drain_completes_a_flow_waiting_on_nothing checks that a flow waited for
// whose own completion is held nowhere, so in no cycle, is still completed at
// the end of a pass, and what waits for it runs. A pass never leaves such a
// flow, so the state is made by hand.
fn test_drain_completes_a_flow_waiting_on_nothing() {
	sandbox('drain_unheld')
	mut c := new()
	c.add_flow(id: 'note', check: .none, flow: record)
	mut pass := Pass{
		state:   {
			'k': FlowState.ran
		}
		order:   ['k']
		waiting: {
			'k': [[Step{
				id: 'note'
			}]]
		}
	}
	assert pass.cycle_start() == Cycle{
		start: 'k'
	}
	c.drain(mut pass)!
	assert pass.waiting.len == 0
	assert pass.state['k'] == .done
	assert contents('log') == 'note '
}

// test_reaching_a_running_flow_is_an_internal_error checks that a flow reached
// again while it runs is reported, rather than run twice or waited for. Passes
// never do this, so the state is made by hand.
fn test_reaching_a_running_flow_is_an_internal_error() {
	sandbox('visit_active')
	mut c := new()
	c.add_flow(id: 'a', check: .none, flow: record)
	mut pass := Pass{
		state: {
			'a': FlowState.active
		}
		stack: ['a']
	}
	mut msg := ''
	c.visit('a', [], mut pass) or { msg = err.msg() }
	assert msg == 'flow `a` was reached while still running'
	assert !os.exists('log')
}

// Lcg is a small seeded random generator, so that a failing graph of
// `test_random_graphs_run_each_reachable_flow_once` can be reproduced.
struct Lcg {
mut:
	state u64
}

// next returns a number in `[0, n)`.
fn (mut r Lcg) next(n int) int {
	r.state = r.state * 6364136223846793005 + 1442695040888963407
	return int((r.state >> 33) % u64(n))
}

// test_random_graphs_run_each_reachable_flow_once runs many random graphs of
// pre hooks, dependencies and post hooks, optional or required. Each graph is
// requested twice. The first time nothing can be skipped; the second time every
// flow with outputs is skipped, so only its required hooks run. Bodies read
// their inputs, so a flow running ahead of a dependency fails the pass.
//
// Half of the graphs only point forward, from a flow to flows with a higher
// index. Nothing ever waits in them, so the order must be exactly that of a
// depth first walk. In the others, post hooks may name any flow; see
// `check_random_pass` for what their order must satisfy.
fn test_random_graphs_run_each_reachable_flow_once() {
	dir := sandbox('random_graphs')
	mut r := Lcg{
		state: 20261104
	}
	for round in 0 .. 1200 {
		for entry in os.ls(dir)! {
			remove(entry)!
		}
		forward := round % 2 == 0
		n := 2 + r.next(7)
		mut flows := []Flow{}
		for i in 0 .. n {
			// dependencies and pre hooks only point to flows with a higher index,
			// so that validate accepts every graph
			mut takes := []string{}
			mut pre := []FlowDep{}
			for j in i + 1 .. n {
				if j % 3 != 0 && r.next(3) == 0 {
					takes << 'f${j}.txt'
				}
				if r.next(6) == 0 {
					pre << FlowDep{
						id:       'f${j}'
						required: r.next(2) == 0
					}
				}
			}
			mut post := []FlowDep{}
			for _ in 0 .. r.next(4) {
				if forward && i == n - 1 {
					break
				}
				// half the hooks of other graphs name an earlier flow, like one that
				// pulled the flow in, so that hooks often wait for each other
				target := if forward {
					i + 1 + r.next(n - i - 1)
				} else if i > 0 && r.next(2) == 0 {
					r.next(i)
				} else {
					r.next(n)
				}
				post << FlowDep{
					id:       'f${target}'
					required: r.next(2) == 0
				}
			}
			flows << Flow{
				id:    'f${i}'
				takes: takes
				gives: if i % 3 != 0 { ['f${i}.txt'] } else { []string{} }
				check: .hash
				pre:   pre
				post:  post
				flow:  record
			}
		}
		mut c := new()
		for f in flows {
			c.add_flow(f)
		}
		requested := 'f${r.next(n)}'
		graph := flows.map('${it.id} takes ${it.takes} pre ${it.pre} post ${it.post}').join('; ')
		for skipping in [false, true] {
			what := 'round ${round}, ${requested} requested, skipping ${skipping}; ${graph}'
			os.rm('log') or {}
			c.execute([requested]) or { assert false, '${err.msg()}; ${what}' }
			ran := (os.read_file('log') or { '' }).fields()
			check_random_pass(flows, requested, skipping, ran, what)
			if forward {
				mut seen := map[string]bool{}
				mut want := []string{}
				walk_forward(flows, requested, skipping, mut seen, mut want)
				assert ran == want, 'ran ${ran}, want ${want}; ${what}'
			}
		}
	}
}

// walk_forward appends to `out` the flows a pass over a graph that only points
// forward runs, in order: a plain depth first walk, as nothing waits.
fn walk_forward(flows []Flow, id string, skipping bool, mut seen map[string]bool, mut out []string) {
	if seen[id] {
		return
	}
	seen[id] = true
	f := flows.filter(it.id == id)[0]
	skip := skipping && f.gives.len > 0
	for dep in f.pre.filter(it.required) {
		walk_forward(flows, dep.id, skipping, mut seen, mut out)
	}
	for path in f.takes {
		walk_forward(flows, path.all_before('.'), skipping, mut seen, mut out)
	}
	if !skip {
		for dep in f.pre.filter(!it.required) {
			walk_forward(flows, dep.id, skipping, mut seen, mut out)
		}
		out << id
	}
	for hook in f.post.filter(!skip || it.required) {
		walk_forward(flows, hook.id, skipping, mut seen, mut out)
	}
}

// reach returns the flows reachable from `from` along `edges`, leaving out the
// edges from `cut_from` to `cut_to`.
fn reach(from []string, edges map[string][]string, cut_from string, cut_to string) map[string]bool {
	mut found := map[string]bool{}
	mut todo := from.clone()
	for todo.len > 0 {
		id := todo.pop()
		if !found[id] {
			found[id] = true
			for next in edges[id] {
				if !(id == cut_from && next == cut_to) {
					todo << next
				}
			}
		}
	}
	return found
}

// check_random_pass checks the flows `ran` by a pass of a random graph against
// what the pass reaches. With `skipping` set, every flow with outputs is
// skipped: it does not run, and leaves out its optional pre and post hooks.
fn check_random_pass(flows []Flow, requested string, skipping bool, ran []string, what string) {
	mut needs := map[string][]string{} // what runs ahead of a flow, whether or not it is skipped
	mut hooks := map[string][]string{} // the post hooks a flow runs
	mut edges := map[string][]string{}
	mut skips := map[string]bool{}
	for f in flows {
		skips[f.id] = skipping && f.gives.len > 0
		mut ahead := f.pre.filter(it.required).map(it.id)
		ahead << f.takes.map(it.all_before('.'))
		if !skips[f.id] {
			ahead << f.pre.filter(!it.required).map(it.id)
		}
		needs[f.id] = ahead
		hooks[f.id] = f.post.filter(!skips[f.id] || it.required).map(it.id)
		edges[f.id] = arrays_concat(ahead, hooks[f.id])
	}
	reachable := reach([requested], edges, '', '')
	mut seen := map[string]bool{}
	for i, id in ran {
		assert !seen[id], '${id} ran twice in ${ran}; ${what}'
		seen[id] = true
		assert reachable[id] && !skips[id], '${id} should not have run in ${ran}; ${what}'
		for dep in needs[id] {
			assert skips[dep] || dep in ran[..i], '${id} ran before ${dep} in ${ran}; ${what}'
		}
	}
	for id, _ in reachable {
		assert skips[id] || seen[id], '${id} never ran in ${ran}; ${what}'
	}
	// A post hook `later` of a flow `x`, reached only through `x`, comes after
	// the post work of each hook `h` before it: the flows its post hooks reach.
	// That holds unless the post work of `h` leads back to `x`, a cycle in which
	// one of them has to go first, or contains a cycle of its own, which the
	// pass breaks somewhere.
	for x, _ in reachable {
		for j, later in hooks[x] {
			if !seen[later] || later in hooks[x][..j] || later == x || later in needs[x]
				|| reach([requested], edges, x, later)[later] {
				continue
			}
			for h in hooks[x][..j] {
				work := reach([h], hooks, '', '')
				if work[x] || work[later] || has_cycle(work, hooks) {
					continue
				}
				for y, _ in work {
					assert !seen[y] || ran.index(y) < ran.index(later), '${later}, a post hook of ${x}, ran before ${y} from the post work of ${h} in ${ran}; ${what}'
				}
			}
		}
	}
}

// has_cycle reports whether the flows in `within` hold a cycle of `hooks`.
fn has_cycle(within map[string]bool, hooks map[string][]string) bool {
	for id, _ in within {
		mut from := []string{}
		for next in hooks[id] {
			from << next
		}
		if reach(from, hooks, '', '')[id] {
			return true
		}
	}
	return false
}

// test_hook_after_post_work_running_further_up_waits_for_it checks that the
// hooks after a post hook wait for its post work even when that post work is
// already running further up, if what runs it needs the flow they belong to as
// a dependency. That post work goes on once the dependency is done, so waiting
// for it ends, unlike in a cycle of post hooks. The cleanup would otherwise
// delete the input of `build` before it runs.
fn test_hook_after_post_work_running_further_up_waits_for_it() {
	sandbox('post_running_above')
	mut c := new()
	c.add_flow(
		id:    'gen'
		gives: ['gen.txt']
		check: .none
		post:  [hook('lint'), hook('clean')]
		flow:  record
	)
	c.add_flow(id: 'lint', check: .none, post: [hook('build')], flow: record)
	c.add_flow(id: 'build', takes: ['gen.txt'], check: .none, flow: record)
	c.add_flow(
		id:    'clean'
		check: .none
		flow:  fn (f Flow) ! {
			os.rm('gen.txt')!
			record(f)!
		}
	)
	c.execute(['lint'])!
	assert contents('log') == 'lint gen build clean '
	os.rm('log')!
	c.execute(['gen'])!
	assert contents('log') == 'gen lint build clean '
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
	assert contents('log') == 'gen build audit note tidy '
	os.rm('log')!
	c.execute(['build'])!
	assert contents('log') == 'build audit tidy '
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
	assert contents('log') == 'gen build audit '
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
	assert contents('log') == 'gen zip site page '
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
	assert contents(runs) == 'x'
	os.write_file('a', '2')!
	c.execute(['build'])!
	assert contents(runs) == 'xx'
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
	assert contents('log') == '${long} '
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
	assert contents('log') == 'a/b a_b '
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
