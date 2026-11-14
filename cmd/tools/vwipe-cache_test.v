import os

const vexe = @VEXE
const vroot = os.dir(vexe)
const test_folder = os.join_path(os.vtmp_dir(), 'vwipe_cache_tool_test_${os.getpid()}')
const wipe_tool = os.join_path(test_folder, 'vwipe-cache' + $if windows {
	'.exe'
} $else {
	''
})

// The standalone tool hands its arguments to the compiler's own `v wipe-cache`, which
// cmd/v/toolcache_test.v tests. These tests build the tool once, and run it with every cache
// folder inside a folder of their own.
fn testsuite_begin() {
	os.rmdir_all(test_folder) or {}
	os.mkdir_all(test_folder)!
	os.chmod(test_folder, 0o700)!
	source := os.join_path(vroot, 'cmd', 'tools', 'vwipe-cache.v')
	build := os.exec([vexe, '-o', wipe_tool, source])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(test_folder) or {}
}

// WipeToolRun is what one run of the tool printed, and its exit status.
struct WipeToolRun {
	output    string
	exit_code int
}

// wipe_tool_folders makes a set of cache folders for one test and returns the environment that
// points `v wipe-cache` at them.
fn wipe_tool_folders(name string) !map[string]string {
	folder := os.join_path(test_folder, name)
	mut environment := os.environ()
	for variable, relative in {
		'HOME':             'home'
		'USERPROFILE':      'home'
		'XDG_CACHE_HOME':   os.join_path('home', '.cache')
		'VTMP':             'vtmp'
		'VCACHE':           'vcache'
		'V3CACHE':          'v3cache'
		'VTOOLS_CACHE_DIR': 'tools'
	} {
		path := os.join_path(folder, relative)
		os.mkdir_all(path)!
		environment[variable] = path
	}
	environment.delete('V_WIPE_CACHE_FORWARDED')
	return environment
}

// run_wipe_tool runs the built tool with `arguments` in `environment`.
fn run_wipe_tool(environment map[string]string, arguments ...string) WipeToolRun {
	mut process := os.new_process(wipe_tool)
	process.set_args(arguments)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.wait()
	code := process.code
	process.close()
	return WipeToolRun{
		output:    output
		exit_code: code
	}
}

fn test_the_tool_runs_the_command_of_the_compiler() ! {
	environment := wipe_tool_folders('runs')!
	vcache := environment['VCACHE']
	tools := environment['VTOOLS_CACHE_DIR']
	object := os.join_path(vcache, 'object.o')
	stale := os.join_path(tools, 'stale')
	os.write_file(object, 'x')!
	os.write_file(stale, 'x')!

	dry := run_wipe_tool(environment, '--dry-run')
	assert dry.exit_code == 0, dry.output
	assert dry.output.contains('Would wipe V cache folder ${vcache}.'), dry.output
	assert dry.output.contains('Would wipe V tool cache folder ${tools}.'), dry.output
	assert os.is_file(object)
	assert os.is_file(stale)

	// Only what the options select goes, as with the command itself.
	selected := run_wipe_tool(environment, '--tools')
	assert selected.exit_code == 0, selected.output
	assert selected.output.contains('V tool cache folder ${tools} was wiped.'), selected.output
	assert !os.exists(stale)
	assert os.is_file(object)

	// `v up` and its tests start the tool with the name of the command first.
	everything := run_wipe_tool(environment, 'wipe-cache')
	assert everything.exit_code == 0, everything.output
	assert everything.output.contains('V cache folder ${vcache} was wiped.'), everything.output
	assert !os.exists(object)
}

fn test_the_tool_passes_the_failure_of_the_command_on() ! {
	environment := wipe_tool_folders('fails')!
	unknown := run_wipe_tool(environment, '--no-such-option')
	assert unknown.exit_code == 1, unknown.output
	assert unknown.output.contains('v wipe-cache: unknown option `--no-such-option`'), unknown.output
	assert unknown.output.contains('Usage:'), unknown.output

	help := run_wipe_tool(environment, '--help')
	assert help.exit_code == 0, help.output
	assert help.output.contains('Usage:'), help.output
	assert help.output.contains('--dry-run'), help.output
}

// A V executable that does not run the command itself hands it back to the tool. The tool must
// say so and stop, not start that executable again.
fn test_the_tool_does_not_run_in_a_circle() ! {
	mut environment := wipe_tool_folders('circle')!
	environment['V_WIPE_CACHE_FORWARDED'] = '1'
	object := os.join_path(environment['VCACHE'], 'object.o')
	os.write_file(object, 'x')!
	handed_back := run_wipe_tool(environment)
	assert handed_back.exit_code == 1, handed_back.output
	assert handed_back.output.contains('does not implement `wipe-cache` itself'), handed_back.output
	assert os.is_file(object)
}
