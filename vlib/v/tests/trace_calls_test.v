// vtest build: !(os_id_ubuntu? && musl?)
import os

const vexe = @VEXE
const gcc_path = os.find_abs_path_of_executable('gcc') or { '' }
const cdefs_h_32bit_exists = os.exists('/usr/include/i386-linux-gnu/sys/cdefs.h')

fn separator() {
	eprintln('-'.repeat(30))
}

fn test_trace_fns() {
	os.chdir(@VROOT)!
	folder := os.join_path('vlib', 'v', 'tests', 'testdata', 'trace_calls')
	fpath := os.join_path(folder, 'single_println.vv')
	should_match_fpath := os.join_path(folder, 'single_println.vv.must_match.simple')
	assert run_trace_fns(fpath, 'println') == 1
	assert run_trace_fns(fpath, '_vinit') == 1
	assert run_trace_fns(fpath, 'println,main.main') == 2
	assert run_trace_fns(fpath, 'builtin') > 3
	eprintln('> `-trace-calls -trace-fns PATTERNS` works')
	separator()
}

fn test_tracing() {
	os.chdir(@VROOT)!
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
	cmd := '${os.quoted_path(vexe)} ${compiler_opts} -no-skip-unused -trace-calls run ${os.quoted_path(fpath)}'
	res := os.execute(cmd)
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
