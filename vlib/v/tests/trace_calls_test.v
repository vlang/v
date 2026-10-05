// vtest build: !(os_id_ubuntu? && musl?) && !sanitized_job?
import os
import strings
import time

const vexe = @VEXE
const gcc_path = os.find_abs_path_of_executable('gcc') or { '' }
const cdefs_h_32bit_exists = os.exists('/usr/include/i386-linux-gnu/sys/cdefs.h')

fn separator() {
	eprintln('-'.repeat(30))
}

fn test_trace_fns() {
	os.chdir(@VEXEROOT)!
	folder := os.join_path('vlib', 'v', 'tests', 'testdata', 'trace_calls')
	fpath := os.join_path(folder, 'single_println.vv')
	should_match_fpath := os.join_path(folder, 'single_println.vv.must_match.simple')
	assert run_trace_fns(fpath, 'println') == 1
	assert run_trace_fns(fpath, '_vinit') == 1
	assert run_trace_fns(fpath, 'println,main.main') == 2
	assert run_trace_fns(fpath, 'builtin') > 3
	assert run_trace_fns(fpath, 'C.main') == 1
	assert run_trace_fns(fpath, '_vcleanup') == 1
	assert run_trace_fns(fpath, 'main.*') == 1
	assert run_trace_fns(fpath, 'missing_function') == 0
	eprintln('> `-trace-calls -trace-fns PATTERNS` works')
	separator()
}

fn test_tracing() {
	os.chdir(@VEXEROOT)!
	folder := os.join_path('vlib', 'v', 'tests', 'testdata', 'trace_calls')
	files := os.walk_ext(folder, '.vv')
	for fpath in files {
		should_match_fpath := '${fpath}.must_match'
		if !os.exists(should_match_fpath) {
			eprintln('> skipping ${fpath}, because ${should_match_fpath} does not exist.')
			continue
		}
		run_single_program(fpath, should_match_fpath, '', '64bit')
		if cdefs_h_32bit_exists && gcc_path != '' {
			// try running the same programs, compiled in 32bit mode too, if gcc is available:
			run_single_program(fpath, should_match_fpath, '-cc gcc -m32 -gc none', '32bit')
		} else {
			eprintln('> skipping -m32 compilation since either 32bit headers are not installed, or you do not have gcc installed')
		}
		separator()
	}
}

struct CmdOutput {
	cmd    string
	output string
}

fn run(fpath string, compiler_opts string, label string) CmdOutput {
	cmd := '${os.quoted_path(vexe)} -new-compiler ${compiler_opts} -no-skip-unused -trace-calls run ${os.quoted_path(fpath)}'
	res := os.exec([vexe, '-new-compiler', ...(os.split_args(compiler_opts) or { panic(err) }),
		'-no-skip-unused', '-trace-calls', 'run', fpath])
	if res.exit_code != 0 {
		eprintln('> ${label} compilation output:\n${res.output}')
		assert res.exit_code == 0, 'compilation of ${fpath} failed'
	}
	return CmdOutput{cmd, res.output}
}

fn run_trace_fns(fpath string, patterns string) int {
	// ignore the header line and the final output `hi` line:
	return run(fpath, '-trace-fns ${patterns}', 'trace_fns_only_println').output.split_into_lines().len - 2
}

fn run_single_program(fpath string, should_match_fpath string, compiler_opts string, label string) {
	c := run(fpath, compiler_opts, label)
	lines := os.read_lines(should_match_fpath) or {
		assert false, '${should_match_fpath} should be readable'
		return
	}
	if lines.len == 0 {
		assert false, '${should_match_fpath} should contain at least one line/glob match pattern'
	}
	mut matched := false
	for line in lines {
		if c.output.match_glob(line) {
			matched = true
			println('> ${label} trace output of ${fpath} matches line pattern: ${line}')
			continue
		} else {
			eprintln('-----------------------------------')
			eprintln(c.output)
			eprintln('-----------------------------------')
			assert false, '> trace output of ${fpath} DID NOT match the line pattern: `${line}`, run cmd:\n${c.cmd}'
		}
	}
}

// Exercise the normal mark-used path, not just the compatibility fixture mode
// above. -new-compiler prevents a fallback from hiding missing instrumentation.
fn test_new_compiler_trace_calls_preserves_output_and_stack_base() {
	os.chdir(@VEXEROOT)!
	dir := os.join_path(os.temp_dir(), 'v_trace_calls_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'program.v')
	binary := os.join_path(dir, 'program' + $if windows { '.exe' } $else { '' })
	stdout_file := os.join_path(dir, 'stdout.txt')
	stderr_file := os.join_path(dir, 'stderr.txt')
	profile_file := os.join_path(dir, 'profile.txt')
	os.write_file(source, 'fn visit(n int) { if n > 0 { visit(n - 1) } }
fn main() { visit(2) println("traced output") }
')!
	for flags in ['', '-no-parallel', '-prod', '-profile ${os.quoted_path(profile_file)}'] {
		build := os.exec([vexe, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-trace-calls', '-o', binary, source])
		assert build.exit_code == 0, build.output
		result := trace_capture_files([binary], stdout_file, stderr_file, map[string]string{})
		assert result.exit_code == 0, result.output
		assert os.read_file(stdout_file)!.trim_space() == 'traced output'
		trace := os.read_file(stderr_file)!
		assert trace.count('#          tid       ns      ssize name') == 1
		assert trace.contains('C.main')
		assert trace.contains('_vinit')
		assert trace.contains('main main.main/0')
		assert trace.count('main main.visit/1') == 3
		assert trace.contains('builtin println/1')
		assert trace.contains('_vcleanup')
		assert !trace.contains('trace_calls.on_call')
		assert !trace.contains('trace_calls.current_time')
		mut previous_ns := u64(0)
		for line in trace.split_into_lines() {
			if !line.starts_with('> trace ') {
				continue
			}
			fields := line.fields()
			assert fields.len >= 6, line
			assert fields[2].u64() > 0, line
			ns := fields[3].u64()
			assert ns >= previous_ns, line
			previous_ns = ns
			// A reset or truncated base produces an address-sized delta, not a
			// stack-sized one. Optimizers may place the anchor on either side.
			stack_size := fields[4].i64()
			assert stack_size > -8 * 1024 * 1024 && stack_size < 8 * 1024 * 1024, line
		}
	}
	assert os.read_file(profile_file)!.contains('visit')
	plain := os.exec([vexe, '-new-compiler', 'run', source])
	assert plain.exit_code == 0, plain.output
	assert plain.output.trim_space() == 'traced output'
}

fn test_trace_fns_requires_an_argument() {
	result := os.exec([vexe, '-new-compiler', '-trace-calls', '-trace-fns'])
	assert result.exit_code != 0
	assert result.output.contains('option `-trace-fns` requires a value')
}

fn test_trace_calls_rejects_unsupported_backends() {
	for backend in ['arm64', 'wasm', 'eval', 'fastc'] {
		result := os.exec([vexe, '-new-compiler', '-b', '${backend}', '-trace-calls',
			'examples/hello_world.v'])
		assert result.exit_code != 0
		assert result.output.contains('option `-trace-calls` is only supported by the C backend')
	}
}

fn test_trace_calls_preserves_implicit_embed_imports() {
	dir := os.join_path(os.temp_dir(), 'v_trace_embed_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'embed.v')
	os.write_file(os.join_path(dir, 'message.txt'), 'embedded output')!
	os.write_file(source, 'fn main() { println(\$embed_file("message.txt").to_string()) }')!
	for flags in ['', '-no-parallel'] {
		result := os.exec([vexe, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-trace-calls', '-trace-fns', 'main.main', 'run', source])
		assert result.exit_code == 0, result.output
		assert result.output.contains('main main.main/0')
		assert result.output.contains('embedded output')
	}
}

// Drain both pipes while the child runs, so a full stderr pipe cannot block stdout.
fn trace_capture_files(arguments []string, stdout_file string, stderr_file string, variables map[string]string) os.Result {
	mut process := os.new_process(arguments[0])
	process.set_args(arguments[1..])
	if variables.len > 0 {
		mut environment := os.environ()
		for key, value in variables { environment[key] = value }
		process.set_environment(environment)
	}
	process.set_redirect_stdio()
	process.set_stdin_path(if os.user_os() == 'windows' { 'NUL' } else { '/dev/null' })
	process.run()
	mut stdout := strings.new_builder(1024)
	mut stderr := strings.new_builder(1024)
	for process.is_alive() {
		out := process.stdout_read()
		err := process.stderr_read()
		stdout.write_string(out)
		stderr.write_string(err)
		if out.len == 0 && err.len == 0 { time.sleep(time.millisecond) }
	}
	stdout.write_string(process.stdout_slurp())
	stderr.write_string(process.stderr_slurp())
	process.wait()
	stdout_text := stdout.str()
	stderr_text := stderr.str()
	os.write_file(stdout_file, stdout_text) or { panic(err) }
	if stderr_file != '' { os.write_file(stderr_file, stderr_text) or { panic(err) } }
	result := os.Result{
		exit_code: process.code
		output:    if stderr_file == '' {
			stderr_text
		} else {
			process.err
		}
	}
	process.close()
	return result
}
