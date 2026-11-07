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

struct WipeRootCase {
	root    string // VTOOLS_CACHE_DIR
	work    string // where the build's files are, when that is not `root`
	link    string // `root` is a link to this folder
	vtmp    string // VTMP, when it is spelled differently
	args    []string
	again   []string // a second wipe, during the same build
	sibling string   // a folder the wipe clears, next to the root or holding it
	no_v3   bool     // leave V3CACHE unset, so the V3 caches are the ones inside VTMP
	no_env  bool     // the wipe does not have the VTOOLS_CACHE_DIR that the build has
	outer   string   // the wipe's VTOOLS_CACHE_DIR, a root that holds this one
}

// A tool cache root can be configured anywhere, so it can sit inside any folder that a wipe
// clears, or inside another tool cache root, and the wipe's environment need not name it. Whichever
// pass reaches it, a live build keeps its entry and lock, and everything else goes.
fn test_wipe_cache_keeps_a_live_build_wherever_the_tool_root_is() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_roots_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	vcache := os.join_path(root, 'vcache')
	v3 := os.join_path(root, 'v3')
	custom := os.join_path(vtmp, 'custom-tools')
	lock_suffix := '.${os.getuid()}.toolcache.lock'
	cases := [
		WipeRootCase{
			root:    custom
			args:    []
			sibling: vtmp
		},
		WipeRootCase{
			root:    custom
			args:    ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'a', 'b', 'custom-tools')
			args:    ['--vtmp']
			sibling: os.join_path(vtmp, 'a', 'b')
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'a', '..', 'custom-tools')
			work:    custom
			args:    ['--vtmp']
			again:   ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    custom
			no_env:  true
			args:    ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    custom
			work:    custom + '.untrusted-1-1'
			args:    ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    custom
			vtmp:    os.join_path(root, 'y', '..', 'vtmp')
			args:    ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'lnk')
			link:    custom
			args:    ['--vtmp']
			again:   ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    os.join_path(root, 'lnk_out')
			link:    custom
			args:    []
			sibling: vtmp
		},
		WipeRootCase{
			root:    os.join_path(root, 'lnk_out')
			link:    custom
			args:    ['--tools']
			again:   ['--vtmp']
			sibling: vtmp
		},
		WipeRootCase{
			root:    vtmp
			args:    ['--vtmp']
			sibling: ''
		},
		WipeRootCase{
			root:    vtmp
			args:    []
			sibling: ''
		},
		WipeRootCase{
			root:    os.join_path(vcache, 'tools')
			args:    []
			sibling: vcache
		},
		WipeRootCase{
			root:    os.join_path(vcache, 'tools')
			args:    ['--vcache']
			sibling: vcache
		},
		WipeRootCase{
			root:    os.join_path(v3, 'tools')
			args:    []
			sibling: v3
		},
		WipeRootCase{
			root:    os.join_path(v3, 'tools')
			args:    ['--v3cache']
			sibling: v3
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'v3_thirdparty_objs', 'tools')
			args:    ['--v3cache']
			sibling: os.join_path(vtmp, 'v3_thirdparty_objs')
			no_v3:   true
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'tools', 'custom')
			args:    ['--tools']
			sibling: os.join_path(vtmp, 'tools')
		},
		WipeRootCase{
			root:    os.join_path(vtmp, 'tools', 'a', 'custom')
			args:    ['--tools']
			sibling: os.join_path(vtmp, 'tools', 'a')
		},
		// A folder keyed by a hash is not an entry when a root is inside it.
		WipeRootCase{
			root:    os.join_path(vtmp, 'tools', 'job-' + 'c'.repeat(64), 'custom')
			outer:   os.join_path(vtmp, 'tools')
			args:    ['--tools']
			sibling: os.join_path(vtmp, 'tools', 'job-' + 'c'.repeat(64))
		},
	]
	for i, c in cases {
		what := '${c.root} ${c.args} ${c.again}'
		mut env := base_env.clone()
		env['VTOOLS_CACHE_DIR'] = c.root
		if c.no_env {
			env['VTOOLS_CACHE_DIR'] = os.join_path(root, 'tools')
		}
		if c.outer != '' {
			env['VTOOLS_CACHE_DIR'] = c.outer
		}
		if c.no_v3 {
			env.delete('V3CACHE')
		}
		if c.vtmp != '' {
			os.mkdir_all(os.join_path(root, 'y'))!
			env['VTMP'] = c.vtmp
		}
		work := if c.work != '' {
			c.work
		} else if c.link != '' {
			c.link
		} else {
			c.root
		}
		os.mkdir_all(work)!
		if c.link != '' {
			os.mkdir_all(os.dir(c.root))!
			// Creating a directory link can need a privilege the host does not grant; skip then.
			os.symlink(c.link, c.root) or {
				os.rmdir_all(work) or {}
				continue
			}
		} else {
			// A spelling through a folder, or a root that a moved-aside folder sits beside.
			os.mkdir_all(c.root)!
		}
		staged_binary := os.join_path(work, 'vtest-' + 'a'.repeat(64) + '.0',
			'.v-toolcache-stage-0-1-2-0', 'vtest' + exe_suffix())
		busy_lock := os.join_path(work, '.vtest${lock_suffix}')
		idle_entry := os.join_path(work, 'vfmt-' + 'b'.repeat(64) + '.0')
		stray := if c.sibling != '' { os.join_path(c.sibling, 'stray_${i}.c') } else { '' }
		stray_dir := if c.sibling != '' { os.join_path(c.sibling, 'stray_dir_${i}') } else { '' }
		os.mkdir_all(os.dir(staged_binary))!
		os.write_file(staged_binary, 'half built')!
		os.write_file(busy_lock, '')!
		os.mkdir_all(idle_entry)!
		// Its name is the start of the root's name, which does not make it a folder leading there.
		prefix_dir := if os.dir(work) == c.sibling { work[..work.len - 1] } else { '' }
		if stray != '' {
			os.write_file(stray, 'tmp')!
			os.mkdir_all(stray_dir)!
			os.write_file(os.join_path(stray_dir, 'x'), 'tmp')!
		}
		if prefix_dir != '' {
			os.mkdir_all(prefix_dir)!
			os.write_file(os.join_path(prefix_dir, 'x'), 'tmp')!
		}
		release_path := os.join_path(root, 'lock_${i}.release')
		mut holder := start_lock_holder(busy_lock, release_path)!
		mut code, mut output := run_wipe_tool(tool, c.args, env)
		if code == 0 && c.again.len > 0 {
			code, output = run_wipe_tool(tool, c.again, env)
		}
		survived := os.is_file(staged_binary) && os.is_file(busy_lock)
		idle_gone := !os.exists(idle_entry)
		stray_gone := (stray == '' || (!os.exists(stray) && !os.exists(stray_dir)))
			&& (prefix_dir == '' || !os.exists(prefix_dir))
		os.write_file(release_path, '')!
		holder.wait()
		holder.close()
		assert code == 0, '${what}: ${output}'
		assert survived, '${what}: ${output}'
		assert idle_gone, '${what}: ${output}'
		assert stray_gone, '${what}: ${output}'
		if os.is_link(c.root) {
			os.rm(c.root) or { os.rmdir(c.root) or {} }
		}
		for dir in [vtmp, vcache, v3] {
			os.rmdir_all(dir) or {}
			os.mkdir_all(dir)!
		}
		os.rmdir_all(os.join_path(root, 'tools')) or {}
	}
}

// new_live_build makes the staged output and the lock file of a build in `folder`.
fn new_live_build(folder string) !(string, string) {
	binary := os.join_path(folder, 'vtest-' + 'a'.repeat(64) + '.0', '.v-toolcache-stage-0-1-2-0',
		'vtest' + exe_suffix())
	os.mkdir_all(os.dir(binary))!
	os.write_file(binary, 'half built')!
	build_lock := os.join_path(folder, '.vtest.${os.getuid()}.toolcache.lock')
	os.write_file(build_lock, '')!
	return binary, build_lock
}

fn test_wipe_cache_keeps_every_live_root_in_one_folder() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_two_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	first := os.join_path(vtmp, 'custom-tools')
	second := os.join_path(vtmp, 'tools')
	first_binary, first_lock := new_live_build(first)!
	second_binary, second_lock := new_live_build(second)!
	first_release := os.join_path(root, 'first.release')
	second_release := os.join_path(root, 'second.release')
	mut first_holder := start_lock_holder(first_lock, first_release)!
	mut second_holder := start_lock_holder(second_lock, second_release)!
	defer {
		os.write_file(first_release, '') or {}
		os.write_file(second_release, '') or {}
		first_holder.wait()
		first_holder.close()
		second_holder.wait()
		second_holder.close()
	}
	mut env := base_env.clone()
	env['VTOOLS_CACHE_DIR'] = first
	code, output := run_wipe_tool(tool, ['--vtmp'], env)
	assert code == 0, output
	assert os.is_file(first_binary), output
	assert os.is_file(first_lock), output
	assert os.is_file(second_binary), output
	assert os.is_file(second_lock), output
}

fn test_wipe_cache_dry_run_changes_nothing_around_a_root() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_dry_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	for i, args in [['--dry-run'], ['--vtmp', '--dry-run'], ['--tools', '--dry-run']] {
		folder := os.join_path(vtmp, 'tools', 'a', 'custom')
		binary, _ := new_live_build(folder)!
		idle := os.join_path(folder, 'vfmt-' + 'b'.repeat(64) + '.0')
		os.mkdir_all(idle)!
		beside := os.join_path(os.dir(folder), 'junk_${i}')
		stray := os.join_path(vtmp, 'stray_${i}.c')
		os.write_file(beside, 'junk')!
		os.write_file(stray, 'tmp')!
		mut env := base_env.clone()
		env['VTOOLS_CACHE_DIR'] = folder
		code, output := run_wipe_tool(tool, args, env)
		assert code == 0, output
		assert os.is_file(binary), '${args}: ${output}'
		assert os.is_dir(idle), '${args}: ${output}'
		assert os.is_file(beside), '${args}: ${output}'
		assert os.is_file(stray), '${args}: ${output}'
	}
}

// A root that is not inside the selected folder is not selected, even when its name starts with
// that folder's name.
fn test_wipe_cache_leaves_a_root_beside_the_selected_folder() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_beside_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vcache := os.join_path(root, 'vcache')
	beside := vcache + '-tools'
	idle := os.join_path(beside, 'vfmt-' + 'b'.repeat(64) + '.0')
	os.mkdir_all(idle)!
	mut inner_idle := os.join_path(vcache, 'tools', 'vfmt-' + 'b'.repeat(64) + '.0')
	os.mkdir_all(inner_idle)!
	stray := os.join_path(vcache, 'stray.c')
	os.write_file(stray, 'tmp')!
	mut env := base_env.clone()
	env['VTOOLS_CACHE_DIR'] = beside
	code, output := run_wipe_tool(tool, ['--vcache'], env)
	assert code == 0, output
	assert os.is_dir(idle), output
	assert !os.exists(inner_idle), output
	assert !os.exists(stray), output
}

// A lock file makes a root: every build holds one before it touches an entry, and a build can hold
// it before it has an entry. An entry-shaped name does not, and does not hide a root inside it. A
// link is not followed out of the folder being wiped.
fn test_wipe_cache_knows_a_root_by_its_lock_file() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_marks_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	lock_suffix := '.${os.getuid()}.toolcache.lock'
	hex := 'b'.repeat(64)
	lock_only := os.join_path(vtmp, 'lock-only', '.vtest${lock_suffix}')
	os.mkdir_all(os.dir(lock_only))!
	os.write_file(lock_only, '')!
	entry_shaped := os.join_path(vtmp, 'job-${hex}')
	os.mkdir_all(entry_shaped)!
	file_shaped := os.join_path(vtmp, 'foo', 'vfmt-${hex}')
	os.mkdir_all(os.dir(file_shaped))!
	os.write_file(file_shaped, 'file')!
	// A folder keyed by a hash, as a CI job's can be, holds a live root.
	hashed_root := os.join_path(vtmp, 'job2-${hex}', 'tools')
	hashed_binary, hashed_lock := new_live_build(hashed_root)!
	outside := os.join_path(root, 'outside')
	// One level down, since a link straight to a root is removed as a link in any case.
	outside_entry := os.join_path(outside, 'sub', 'vfmt-${hex}.0')
	outside_lock := os.join_path(outside, 'sub', '.vfmt${lock_suffix}')
	outside_file := os.join_path(outside, 'keep.txt')
	os.mkdir_all(outside_entry)!
	os.write_file(outside_lock, '')!
	os.write_file(outside_file, 'keep')!
	link := os.join_path(vtmp, 'lnk')
	linked := if _ := os.symlink(outside, link) { true } else { false }
	release_path := os.join_path(root, 'lock.release')
	hashed_release := os.join_path(root, 'hashed.release')
	mut holder := start_lock_holder(lock_only, release_path)!
	mut hashed_holder := start_lock_holder(hashed_lock, hashed_release)!
	defer {
		os.write_file(release_path, '') or {}
		os.write_file(hashed_release, '') or {}
		holder.wait()
		holder.close()
		hashed_holder.wait()
		hashed_holder.close()
	}
	code, output := run_wipe_tool(tool, ['--vtmp'], base_env)
	assert code == 0, output
	assert os.is_file(lock_only), output
	assert os.is_file(hashed_binary), output
	assert os.is_file(hashed_lock), output
	assert !os.exists(entry_shaped), output
	assert !os.exists(os.join_path(vtmp, '.job${lock_suffix}')), output
	assert !os.exists(os.dir(file_shaped)), output
	assert output.contains('another process has locked right now: vtest'), output
	if linked {
		assert !os.is_link(link), output
		assert os.is_dir(outside_entry), output
		assert os.is_file(outside_file), output
	}
}

// A tool's name can contain dashes, which its entry's name must still be told by.
fn test_wipe_cache_keeps_a_live_build_of_a_tool_with_a_dash_in_its_name() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_dash_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')
	binary := os.join_path(tools, 'vcheck-md-' + 'a'.repeat(64) + '.0', 'stage', 'vcheck-md')
	os.mkdir_all(os.dir(binary))!
	os.write_file(binary, 'half built')!
	build_lock := os.join_path(tools, '.vcheck-md.${os.getuid()}.toolcache.lock')
	os.write_file(build_lock, '')!
	release_path := os.join_path(root, 'lock.release')
	mut holder := start_lock_holder(build_lock, release_path)!
	defer {
		os.write_file(release_path, '') or {}
		holder.wait()
		holder.close()
	}
	code, output := run_wipe_tool(tool, ['--tools'], env)
	assert code == 0, output
	assert os.is_file(binary), output
	assert os.is_file(build_lock), output
}

// A link is removed as a link, whichever folder it stands for or sits in.
fn test_wipe_cache_does_not_follow_a_link_to_a_folder_with_a_root_inside() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_links_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	outside := os.join_path(root, 'outside')
	entry := os.join_path(outside, 'sub', 'vfmt-' + 'b'.repeat(64) + '.0')
	keep := os.join_path(outside, 'keep.txt')
	os.mkdir_all(entry)!
	os.write_file(os.join_path(outside, 'sub', '.vfmt.${os.getuid()}.toolcache.lock'), '')!
	os.write_file(keep, 'keep')!
	// In a tool cache root, with the tool pass.
	tools_link := os.join_path(root, 'tools', 'vlink')
	if _ := os.symlink(outside, tools_link) {
	} else {
		// Creating a directory link can need a privilege the host does not grant; skip then.
		return
	}
	code, output := run_wipe_tool(tool, ['--tools'], base_env)
	assert code == 0, output
	assert !os.is_link(tools_link), output
	assert os.is_dir(entry), output
	assert os.is_file(keep), output
	// As the folder being wiped.
	vcache := os.join_path(root, 'vcache')
	os.rmdir_all(vcache) or {}
	os.symlink(outside, vcache)!
	vcache_code, vcache_output := run_wipe_tool(tool, ['--vcache'], base_env)
	assert vcache_code == 0, vcache_output
	assert !os.is_link(vcache), vcache_output
	assert os.is_dir(vcache), vcache_output
	assert os.is_dir(entry), vcache_output
	assert os.is_file(keep), vcache_output
}

// With no root left inside it, a folder goes as a whole and comes back empty.
fn test_wipe_cache_recreates_a_folder_with_no_root_left_in_it() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_empty_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	mut env := base_env.clone()
	stray := os.join_path(vtmp, 'stray.c')
	os.write_file(stray, 'tmp')!
	code, output := run_wipe_tool(tool, ['--vtmp'], env)
	assert code == 0, output
	assert os.is_dir(vtmp), output
	assert !os.exists(stray), output
	// An empty folder named as the tool cache holds nothing of a build, so it is no root.
	env['VTOOLS_CACHE_DIR'] = vtmp
	os.write_file(stray, 'tmp')!
	empty_code, empty_output := run_wipe_tool(tool, ['--vtmp'], env)
	assert empty_code == 0, empty_output
	assert os.is_dir(vtmp), empty_output
	assert !os.exists(stray), empty_output
}

// A failed cache wipe stops the run before the tool caches, and a failure while removing the
// rest of a folder around a root is reported, not swallowed.
fn test_wipe_cache_reports_a_failure_around_a_root() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_fail_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.chmod(os.join_path(root, 'vtmp', 'stuck'), 0o700) or {}
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	tools := os.join_path(root, 'tools')
	idle := os.join_path(tools, 'vfmt-' + 'b'.repeat(64) + '.0')
	os.mkdir_all(idle)!
	blocker := os.join_path(root, 'not_a_directory')
	os.write_file(blocker, 'file')!
	mut env := base_env.clone()
	env['VCACHE'] = os.join_path(blocker, 'cache')
	code, output := run_wipe_tool(tool, []string{}, env)
	assert code != 0, output
	assert output.contains('could not create V cache folder'), output
	assert os.is_dir(idle), output

	vtmp := os.join_path(root, 'vtmp')
	folder := os.join_path(vtmp, 'custom-tools')
	new_live_build(folder)!
	stuck := os.join_path(vtmp, 'stuck')
	stuck_file := os.join_path(stuck, 'file')
	os.mkdir_all(stuck)!
	os.write_file(stuck_file, 'held')!
	mut held := os.File{}
	$if windows {
		held = os.open_file(stuck_file, 'r')!
	} $else {
		if os.geteuid() == 0 {
			// root removes the file anyway
			return
		}
		os.chmod(stuck, 0o500)!
	}
	// Here, at the end of the test: a defer inside the comptime block would run as soon as that
	// block ends.
	defer {
		held.close()
	}
	mut around_env := base_env.clone()
	around_env['VTOOLS_CACHE_DIR'] = folder
	around_code, around_output := run_wipe_tool(tool, ['--vtmp'], around_env)
	assert around_code != 0, around_output
	assert around_output.contains('could not remove'), around_output
}

// hold_a_file_in makes a file in `folder` that cannot be removed for as long as the returned file
// stays open. It returns false where nothing stops the removal (root on POSIX).
fn hold_a_file_in(folder string, mut held []os.File, mut locked_dirs []string) !bool {
	os.mkdir_all(folder)!
	path := os.join_path(folder, 'held')
	os.write_file(path, 'held')!
	$if windows {
		held << os.open_file(path, 'r')!
		return true
	} $else {
		if os.geteuid() == 0 {
			return false
		}
		os.chmod(folder, 0o500)!
		locked_dirs << folder
		return true
	}
}

struct WipeFailureCase {
	root  string // VTOOLS_CACHE_DIR, with a live build in it
	outer string // the wipe's VTOOLS_CACHE_DIR, when that is a root holding this one
	stuck string // a folder holding what cannot be removed
	args  []string
	free  string // something else selected, removed although the failure came first
}

// A failure inside a root or around it is reported, whichever step hit it, it never costs the
// live build, and everything else selected is still attempted.
fn test_wipe_cache_reports_a_failure_inside_or_around_a_root() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_stuck_roots_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	idle := 'vfmt-' + 'b'.repeat(64) + '.0'
	nested := os.join_path(vtmp, 'tools', 'a', 'custom')
	custom := os.join_path(vtmp, 'custom')
	deep := os.join_path(vtmp, 'a', 'custom')
	cases := [
		WipeFailureCase{
			root:  nested
			stuck: os.join_path(nested, idle)
			args:  ['--tools']
		},
		WipeFailureCase{
			root:  nested
			outer: os.join_path(vtmp, 'tools')
			stuck: os.join_path(nested, idle)
			args:  ['--tools']
		},
		WipeFailureCase{
			root:  custom
			stuck: os.join_path(custom, idle)
			args:  ['--vtmp']
		},
		WipeFailureCase{
			root:  nested
			stuck: os.join_path(vtmp, 'tools', 'a', 'stuck')
			args:  ['--tools']
		},
		WipeFailureCase{
			root:  deep
			stuck: os.join_path(vtmp, 'a', 'stuck')
			args:  ['--vtmp']
			free:  os.join_path(vtmp, 'z_free.txt')
		},
		WipeFailureCase{
			root:  custom
			stuck: os.join_path(vtmp, 'a_stuck')
			args:  ['--vtmp']
			free:  os.join_path(vtmp, 'z_free.txt')
		},
	]
	for i, c in cases {
		what := '${c.root} ${c.stuck} ${c.args}'
		binary, build_lock := new_live_build(c.root)!
		if c.free != '' {
			os.write_file(c.free, 'tmp')!
		}
		mut held := []os.File{}
		mut locked_dirs := []string{}
		if !hold_a_file_in(c.stuck, mut held, mut locked_dirs)! {
			// root removes the file anyway
			return
		}
		release_path := os.join_path(root, 'lock_${i}.release')
		mut holder := start_lock_holder(build_lock, release_path)!
		mut env := base_env.clone()
		env['VTOOLS_CACHE_DIR'] = if c.outer != '' { c.outer } else { c.root }
		code, output := run_wipe_tool(tool, c.args, env)
		survived := os.is_file(binary) && os.is_file(build_lock)
		free_gone := c.free == '' || !os.exists(c.free)
		os.write_file(release_path, '')!
		holder.wait()
		holder.close()
		for mut file in held {
			file.close()
		}
		for dir in locked_dirs {
			os.chmod(dir, 0o700) or {}
		}
		assert code != 0, '${what}: ${output}'
		assert survived, '${what}: ${output}'
		assert free_gone, '${what}: ${output}'
		os.rmdir_all(vtmp) or {}
		os.mkdir_all(vtmp)!
	}
}

// Roots inside roots are wiped once each, and a line says a thing once.
fn test_wipe_cache_wipes_a_nested_root_once() ! {
	root := os.join_path(os.vtmp_dir(), 'vwipe_cache_once_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	tool, base_env := prepare_wipe_tool(root)!
	vtmp := os.join_path(root, 'vtmp')
	lock_name := '.vtest.${os.getuid()}.toolcache.lock'
	mut folder := vtmp
	for name in ['a', 'b', 'c', 'd'] {
		folder = os.join_path(folder, name)
		os.mkdir_all(folder)!
		os.write_file(os.join_path(folder, lock_name), '')!
	}
	code, output := run_wipe_tool(tool, ['--vtmp'], base_env)
	assert code == 0, output
	deepest := 'V tool cache folder ${folder} was wiped.'
	assert output.count(deepest) == 1, output
	assert output.count('\n') <= 6, output
	// The tool pass and the V tmp pass reach the same root, and it is reported once.
	tools := os.join_path(vtmp, 'tools')
	new_live_build(tools)!
	mut env := base_env.clone()
	env['VTOOLS_CACHE_DIR'] = tools
	default_code, default_output := run_wipe_tool(tool, []string{}, env)
	assert default_code == 0, default_output
	assert default_output.count('V tool cache folder ${tools} was wiped.') == 1, default_output
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
