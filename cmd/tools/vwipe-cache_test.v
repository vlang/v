import os
import os.filelock
import time

const vexe = @VEXE
const vroot = os.dir(vexe)
const lock_holder_env = 'VTEST_WIPE_CACHE_LOCK_HOLDER'
const lock_release_env = 'VTEST_WIPE_CACHE_LOCK_RELEASE'

// The test executable doubles as a compiler in the middle of a tool build: started with
// `lock_holder_env` set, it takes that build lock and keeps it until it is told to let go.
fn testsuite_begin() {
	if lock_path := os.getenv_opt(lock_holder_env) {
		hold_lock_until_released(lock_path, os.getenv(lock_release_env))
	}
}

// hold_lock_until_released takes `lock_path`, reports that, and keeps the lock until
// `release_path` appears.
@[noreturn]
fn hold_lock_until_released(lock_path string, release_path string) {
	mut build_lock := filelock.new_file(lock_path, mode: .exclusive)
	build_lock.acquire() or {
		eprintln(err.msg())
		exit(2)
	}
	println('locked')
	os.flush()
	deadline := time.now().add(30 * time.second)
	for time.now() < deadline && !os.exists(release_path) {
		time.sleep(10 * time.millisecond)
	}
	build_lock.release()
	exit(0)
}

// start_lock_holder runs the lock holder above in a child process and returns once the
// child reports that it holds `lock_path`. The child lets go when `release_path` appears;
// keep that outside any directory the tool wipes.
fn start_lock_holder(lock_path string, release_path string) !&os.Process {
	mut p := os.new_process(os.executable())
	mut env := os.environ()
	env[lock_holder_env] = lock_path
	env[lock_release_env] = release_path
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	mut output := ''
	deadline := time.now().add(10 * time.second)
	for time.now() < deadline {
		output += p.stdout_read()
		if output.contains('locked') {
			return p
		}
		if !p.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	p.signal_kill()
	p.wait()
	p.close()
	return error('the lock holder did not take ${lock_path}: ${output}')
}

// prepare_wipe_tool builds the tool into `root` and returns its path with an environment that
// points every tool cache candidate below `root`, so nothing of the real caches is touched.
fn prepare_wipe_tool(root string) !(string, map[string]string) {
	tool := os.join_path(root, 'vwipe-cache' + exe_suffix())
	build := os.exec([vexe, '-o', tool, os.join_path(vroot, 'cmd', 'tools', 'vwipe-cache.v')])
	assert build.exit_code == 0, build.output
	tools := os.join_path(root, 'tools')
	home := os.join_path(root, 'home')
	xdg := os.join_path(root, 'xdg')
	vtmp := os.join_path(root, 'vtmp')
	for dir in [tools, home, xdg, vtmp] {
		os.mkdir_all(dir)!
	}
	mut env := os.environ()
	env['VTOOLS_CACHE_DIR'] = tools
	env['XDG_CACHE_HOME'] = xdg
	env['HOME'] = home
	env['USERPROFILE'] = home
	env['VTMP'] = vtmp
	// The default wipe also clears the V3 and module caches. Point them below `root`, or an
	// inherited value (the test runner's own temp folder) gets wiped.
	env['V3CACHE'] = os.join_path(root, 'v3')
	env['VCACHE'] = os.join_path(root, 'vcache')
	env.delete('VTOOLS_NO_CACHE')
	return tool, env
}

// run_wipe_tool runs the built tool with `args` and the cache environment `env`, and
// returns its exit code and combined output.
fn run_wipe_tool(tool string, args []string, env map[string]string) (int, string) {
	return run_wipe_tool_in(tool, args, env, '')
}

// run_wipe_tool_in is run_wipe_tool with the tool started in `work_folder`, unless that is ''.
fn run_wipe_tool_in(tool string, args []string, env map[string]string, work_folder string) (int, string) {
	mut p := os.new_process(tool)
	p.set_args(args)
	p.set_environment(env)
	if work_folder != '' {
		p.set_work_folder(work_folder)
	}
	p.set_redirect_stdio()
	p.run()
	mut output := ''
	for p.is_alive() {
		output += p.stdout_read()
		output += p.stderr_read()
		time.sleep(10 * time.millisecond)
	}
	output += p.stdout_slurp()
	output += p.stderr_slurp()
	p.wait()
	code := p.code
	p.close()
	return code, output
}

fn exe_suffix() string {
	$if windows {
		return '.exe'
	}
	return ''
}

fn test_wipe_cache_tool_leaves_the_tools_being_built_alone() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_busy_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')

	lock_suffix := '.${os.getuid()}.toolcache.lock'
	busy_entry := os.join_path(tools, 'vtest-' + 'a'.repeat(64) + '.0')
	stage := os.join_path(busy_entry, '.v-toolcache-stage-0-1-2-0')
	staged_binary := os.join_path(stage, 'vtest' + exe_suffix())
	idle_entry := os.join_path(tools, 'vfmt-' + 'b'.repeat(64) + '.0')
	idle_binary := os.join_path(idle_entry, 'vfmt' + exe_suffix())
	// `v test-all` is a different tool whose name starts with the busy one.
	prefixed_entry := os.join_path(tools, 'vtest-all-' + 'c'.repeat(64) + '.0')
	busy_lock := os.join_path(tools, '.vtest${lock_suffix}')
	idle_lock := os.join_path(tools, '.vfmt${lock_suffix}')
	// A directory that merely looks like a lock file is an ordinary entry.
	lock_shaped_dir := os.join_path(tools, '.vfoo${lock_suffix}')
	os.mkdir_all(stage)!
	os.write_file(staged_binary, 'half built')!
	os.mkdir_all(idle_entry)!
	os.write_file(idle_binary, 'bin')!
	os.mkdir_all(prefixed_entry)!
	os.mkdir_all(lock_shaped_dir)!
	os.write_file(busy_lock, '')!
	os.write_file(idle_lock, '')!

	release_path := os.join_path(root, 'lock.release')
	mut holder := start_lock_holder(busy_lock, release_path)!
	defer {
		os.write_file(release_path, '') or {}
		holder.wait()
		holder.close()
	}
	dry_code, dry := run_wipe_tool(tool, ['--tools', '--dry-run', '--verbose'], env)
	assert dry_code == 0, dry
	assert dry.contains('Would wipe V tool cache folder ${tools}, except for the tools another process has locked right now: vtest.'), dry
	assert dry.contains('  would keep ${busy_entry}'), dry
	assert dry.contains('  would remove ${idle_entry}'), dry
	assert os.is_file(idle_binary)

	code, output := run_wipe_tool(tool, ['--tools', '--verbose'], env)
	assert code == 0, output
	assert output.contains('V tool cache folder ${tools} was wiped, except for the tools another process has locked right now: vtest.'), output
	assert output.contains('  keeping ${busy_entry}'), output
	assert output.contains('  keeping ${busy_lock}'), output
	assert os.is_file(staged_binary)
	assert !os.exists(idle_entry)
	assert !os.exists(prefixed_entry)
	assert !os.exists(lock_shaped_dir)
	// Lock files are persistent mutexes and stay, whether held or not.
	assert os.is_file(busy_lock)
	assert os.is_file(idle_lock)

	// Once the build is over, nothing holds the lock and the entry goes too; the lock files
	// keep the root in place.
	os.write_file(release_path, '')!
	holder.wait()
	released_code, released_output := run_wipe_tool(tool, ['--tools'], env)
	assert released_code == 0, released_output
	assert released_output.contains('V tool cache folder ${tools} was wiped.'), released_output
	assert !os.exists(busy_entry)
	assert os.is_file(busy_lock)
	assert os.is_file(idle_lock)
	assert os.is_dir(tools)
}

fn test_wipe_cache_tool_reports_a_tool_root_it_cannot_remove() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_stuck_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')
	// A plain file is not a tool entry, so no lock is taken for it and the root cannot be kept.
	entry := os.join_path(tools, 'scratch.txt')
	os.write_file(entry, 'scratch')!
	// Nothing is kept, so the wipe ends by removing the root itself. Windows refuses that
	// while the tool has it as its working folder; POSIX refuses it while the parent cannot
	// be written to.
	mut work_folder := ''
	$if windows {
		work_folder = tools
	} $else {
		if os.geteuid() == 0 {
			// root removes the folder anyway
			return
		}
		os.chmod(root, 0o500)!
	}
	// Restored here, at the end of the test: a defer inside the comptime block would run as soon
	// as that block ends, before the tool has been started.
	defer {
		os.chmod(root, 0o700) or {}
	}
	code, output := run_wipe_tool_in(tool, ['--tools'], env, work_folder)
	assert code != 0, output
	assert output.contains('could not remove V tool cache folder ${tools}'), output
	assert !os.exists(entry), output
}

fn test_wipe_cache_tool_removes_a_directory_link_without_following_it() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_link_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')
	target := os.join_path(root, 'target')
	os.mkdir_all(target)!
	os.write_file(os.join_path(target, 'keep.txt'), 'keep')!
	link := os.join_path(tools, 'vlink')
	// Creating a directory link can need a privilege the host does not grant; skip then.
	os.symlink(target, link) or { return }
	assert os.is_link(link)
	code, output := run_wipe_tool(tool, ['--tools'], env)
	assert code == 0, output
	assert !os.is_link(link), output
	assert os.is_file(os.join_path(target, 'keep.txt')), output
}

// The default wipe also cleans the V tmp folder, which holds the `tools` cache folder. Removing
// that folder with its parent would take the entry and lock file of a running build.
fn test_wipe_cache_default_run_keeps_a_live_build_under_vtmp_tools() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_vtmp_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	vtmp_tools := os.join_path(vtmp, 'tools')
	lock_suffix := '.${os.getuid()}.toolcache.lock'
	busy_entry := os.join_path(vtmp_tools, 'vtest-' + 'a'.repeat(64) + '.0')
	staged_binary := os.join_path(busy_entry, '.v-toolcache-stage-0-1-2-0', 'vtest' + exe_suffix())
	busy_lock := os.join_path(vtmp_tools, '.vtest${lock_suffix}')
	stray := os.join_path(vtmp, 'stray.c')
	os.mkdir_all(os.dir(staged_binary))!
	os.write_file(staged_binary, 'half built')!
	os.write_file(busy_lock, '')!
	os.write_file(stray, 'tmp')!
	release_path := os.join_path(root, 'lock.release')
	mut holder := start_lock_holder(busy_lock, release_path)!
	defer {
		os.write_file(release_path, '') or {}
		holder.wait()
		holder.close()
	}
	code, output := run_wipe_tool(tool, []string{}, env)
	assert code == 0, output
	assert os.is_file(staged_binary), output
	assert os.is_file(busy_lock), output
	assert os.is_dir(vtmp_tools), output
	assert !os.exists(stray), output
	// `--vtmp` alone skips the tool pass, so the V tmp wipe must protect the same entry itself.
	vtmp_code, vtmp_output := run_wipe_tool(tool, ['--vtmp'], env)
	assert vtmp_code == 0, vtmp_output
	assert os.is_file(staged_binary), vtmp_output
	assert os.is_file(busy_lock), vtmp_output
}

fn test_wipe_cache_tool_claims_an_entry_by_creating_its_lock_file() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_claim_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')
	entry := os.join_path(tools, 'vfmt-' + 'b'.repeat(64) + '.0')
	os.mkdir_all(entry)!
	os.write_file(os.join_path(entry, 'vfmt' + exe_suffix()), 'bin')!
	lock_path := os.join_path(tools, '.vfmt.${os.getuid()}.toolcache.lock')
	// A dry run must not create the lock file.
	dry_code, dry := run_wipe_tool(tool, ['--tools', '--dry-run'], env)
	assert dry_code == 0, dry
	assert !os.exists(lock_path), dry
	code, output := run_wipe_tool(tool, ['--tools'], env)
	assert code == 0, output
	assert !os.exists(entry), output
	// The lock file is created before the entry is removed and is kept afterwards, so the root stays.
	assert os.is_file(lock_path), output
	assert os.is_dir(tools), output
}
