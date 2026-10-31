import os

// Reading the test runner's own stdin would block whenever the suite is
// started from a terminal, and would return whatever the runner happened to
// pipe in. Every `os.get_*` input helper is therefore exercised through a small
// child binary, which this test compiles and then feeds a file through
// `Process.set_stdin_path`, so the input is fully determined.
const stdin_probe_source = r"import os

fn main() {
	mode := if os.args.len > 1 { os.args[1] } else { '' }
	match mode {
		'get_line' {
			println(os.get_line())
		}
		'get_lines' {
			println(os.get_lines().str())
		}
		'get_lines_joined' {
			println(os.get_lines_joined())
		}
		'get_raw_lines' {
			println(os.get_raw_lines().str())
		}
		'get_raw_lines_joined' {
			println(os.get_raw_lines_joined())
		}
		'get_trimmed_lines' {
			println(os.get_trimmed_lines().str())
		}
		'input_opt' {
			res := os.input_opt('PROMPT> ') or { '<none>' }
			println(res)
		}
		'input' {
			println(os.input('PROMPT> '))
		}
		else {
			println('<bad mode>')
		}
	}
	println('END-MARKER')
}
"

const stdin_marker = '\nEND-MARKER\n'

struct StdinProbe {
	exe string
	dir string
}

fn stdin_probe_dir() string {
	root := os.join_path(os.vtmp_dir(), 'os_stdin_input_tests_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn new_stdin_probe() StdinProbe {
	dir := stdin_probe_dir()
	source := os.join_path(dir, 'stdin_probe.v')
	os.write_file(source, stdin_probe_source) or { panic(err) }
	exe := os.join_path(dir, $if windows {
		'stdin_probe.exe'
	} $else {
		'stdin_probe'
	})
	build := os.exec([@VEXE, '-o', exe, source])
	assert build.exit_code == 0, build.output
	return StdinProbe{
		exe: exe
		dir: dir
	}
}

fn (p StdinProbe) close() {
	os.rmdir_all(p.dir) or {}
}

// feed returns everything the probe wrote to stdout for `mode` when its stdin
// was the file holding `payload`. The probe ends its output with a marker
// line, so a value that itself ends in a newline stays distinguishable.
fn (p StdinProbe) feed(mode string, payload string) string {
	input := os.join_path(p.dir, 'stdin.txt')
	os.write_file(input, payload) or { panic(err) }
	mut child := os.new_process(p.exe)
	child.set_args([mode])
	child.set_redirect_stdio()
	child.set_stdin_path(input)
	child.wait()
	out := child.stdout_slurp()
	errs := child.stderr_slurp()
	code := child.code
	child.close()
	assert code == 0, errs
	assert out.ends_with(stdin_marker), out
	return out[..out.len - stdin_marker.len]
}

fn test_get_line_returns_one_line_without_its_newline() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('get_line', 'a\nb\nc') == 'a'
	assert p.feed('get_line', 'only') == 'only'
	assert p.feed('get_line', 'only\n') == 'only'
	// An empty stdin gives an empty line rather than an error.
	assert p.feed('get_line', '') == ''
	// NOTE: `get_line()` trims `\r\n` on Windows but only `\n` elsewhere
	// (os.v:459), so a CRLF line keeps a trailing `\r` off Windows. No CRLF
	// payload is asserted here, because the two platforms disagree.
}

fn test_get_lines_and_get_lines_joined_stop_at_a_blank_line() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('get_lines', 'a\nb\nc') == "['a', 'b', 'c']"
	// Reading stops on the first empty line, so the lines after it are lost.
	assert p.feed('get_lines', 'a\nb\n\nc\n') == "['a', 'b']"
	// Each line is trimmed on both sides.
	assert p.feed('get_lines', '  padded  \n x \n') == "['padded', 'x']"
	assert p.feed('get_lines', '') == '[]'

	assert p.feed('get_lines_joined', 'a\nb\nc') == 'abc'
	assert p.feed('get_lines_joined', 'a\nb\n\nc\n') == 'ab'
	assert p.feed('get_lines_joined', '  padded  \n x \n') == 'paddedx'
	assert p.feed('get_lines_joined', '') == ''
}

fn test_get_raw_lines_keep_every_line_and_its_newline() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('get_raw_lines', 'a\nb\nc') == "['a\n', 'b\n', 'c']"
	assert p.feed('get_raw_lines', 'a\nb\n\nc\n') == "['a\n', 'b\n', '\n', 'c\n']"
	assert p.feed('get_raw_lines', 'a\r\nb\r\nc\n') == "['a\r\n', 'b\r\n', 'c\n']"
	assert p.feed('get_raw_lines', 'only') == "['only']"
	assert p.feed('get_raw_lines', 'only\n') == "['only\n']"
	assert p.feed('get_raw_lines', '') == '[]'
}

fn test_get_raw_lines_joined_concatenates_the_raw_lines() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('get_raw_lines_joined', 'a\nb\nc') == 'a\nb\nc'
	assert p.feed('get_raw_lines_joined', 'a\nb\n\nc\n') == 'a\nb\n\nc\n'
	assert p.feed('get_raw_lines_joined', 'a\r\nb\r\nc\n') == 'a\r\nb\r\nc\n'
	assert p.feed('get_raw_lines_joined', 'only') == 'only'
	assert p.feed('get_raw_lines_joined', '') == ''
}

fn test_get_trimmed_lines_keeps_empty_lines_and_strips_only_line_endings() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('get_trimmed_lines', 'a\nb\nc') == "['a', 'b', 'c']"
	// Unlike `get_lines`, an empty line survives as an empty string.
	assert p.feed('get_trimmed_lines', 'a\nb\n\nc\n') == "['a', 'b', '', 'c']"
	assert p.feed('get_trimmed_lines', 'a\r\nb\r\nc\n') == "['a', 'b', 'c']"
	// Only `\r` and `\n` at the end are removed; other spacing is kept.
	assert p.feed('get_trimmed_lines', '  padded  \n x \n') == "['  padded  ', ' x ']"
	assert p.feed('get_trimmed_lines', 'only') == "['only']"
	assert p.feed('get_trimmed_lines', '') == '[]'
}

fn test_input_and_input_opt_print_the_prompt_and_return_the_first_line() {
	mut p := new_stdin_probe()
	defer {
		p.close()
	}
	assert p.feed('input_opt', 'a\nb\nc') == 'PROMPT> a'
	assert p.feed('input_opt', '  padded  \n x \n') == 'PROMPT>   padded  '
	assert p.feed('input_opt', 'only') == 'PROMPT> only'
	// End of input is reported as `none`.
	assert p.feed('input_opt', '') == 'PROMPT> <none>'

	assert p.feed('input', 'a\nb\nc') == 'PROMPT> a'
	assert p.feed('input', '  padded  \n x \n') == 'PROMPT>   padded  '
	assert p.feed('input', 'only') == 'PROMPT> only'
	// End of input is reported as the literal `<EOF>`.
	assert p.feed('input', '') == 'PROMPT> <EOF>'
}
