import os
import encoding.hex

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
		'get_raw_stdin' {
			println(os.get_raw_stdin().hex())
		}
		'get_raw_line_then_stdin' {
			os.get_raw_line()
			println(os.get_raw_stdin().hex())
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
const stdin_probe_dir = os.join_path(os.vtmp_dir(), 'os_stdin_input_tests_${os.getpid()}')
const stdin_probe = StdinProbe{
	dir: stdin_probe_dir
	exe: os.join_path(stdin_probe_dir, $if windows { 'stdin_probe.exe' } $else { 'stdin_probe' })
}

struct StdinProbe {
	exe string
	dir string
}

fn testsuite_begin() {
	os.rmdir_all(stdin_probe.dir) or {}
	os.mkdir_all(stdin_probe.dir)!
	source := os.join_path(stdin_probe.dir, 'stdin_probe.v')
	os.write_file(source, stdin_probe_source)!
	build := os.exec([@VEXE, '-o', stdin_probe.exe, source])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(stdin_probe.dir) or {}
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
	child.run()
	out := child.stdout_slurp()
	errs := child.stderr_slurp()
	child.wait()
	code := child.code
	child.close()
	assert code == 0, errs
	assert out.ends_with(stdin_marker), out
	value := out[..out.len - stdin_marker.len]
	if mode in ['get_raw_stdin', 'get_raw_line_then_stdin'] {
		// Keep invalid UTF-8 and embedded NUL bytes out of Windows text capture.
		return (hex.decode(value) or { panic(err) }).bytestr()
	}
	return value
}

fn test_get_line_returns_one_line_without_its_newline() {
	assert stdin_probe.feed('get_line', 'a\nb\nc') == 'a'
	assert stdin_probe.feed('get_line', 'only') == 'only'
	assert stdin_probe.feed('get_line', 'only\n') == 'only'
	// An empty stdin gives an empty line rather than an error.
	assert stdin_probe.feed('get_line', '') == ''
	// NOTE: `get_line()` trims `\r\n` on Windows but only `\n` elsewhere
	// (os.v:459), so a CRLF line keeps a trailing `\r` off Windows. No CRLF
	// payload is asserted here, because the two platforms disagree.
}

fn test_get_lines_and_get_lines_joined_stop_at_a_blank_line() {
	assert stdin_probe.feed('get_lines', 'a\nb\nc') == "['a', 'b', 'c']"
	// Reading stops on the first empty line, so the lines after it are lost.
	assert stdin_probe.feed('get_lines', 'a\nb\n\nc\n') == "['a', 'b']"
	// Each line is trimmed on both sides.
	assert stdin_probe.feed('get_lines', '  padded  \n x \n') == "['padded', 'x']"
	assert stdin_probe.feed('get_lines', '') == '[]'

	assert stdin_probe.feed('get_lines_joined', 'a\nb\nc') == 'abc'
	assert stdin_probe.feed('get_lines_joined', 'a\nb\n\nc\n') == 'ab'
	assert stdin_probe.feed('get_lines_joined', '  padded  \n x \n') == 'paddedx'
	assert stdin_probe.feed('get_lines_joined', '') == ''
}

fn test_get_raw_lines_keep_every_line_and_its_newline() {
	assert stdin_probe.feed('get_raw_lines', 'a\nb\nc') == "['a\n', 'b\n', 'c']"
	assert stdin_probe.feed('get_raw_lines', 'a\nb\n\nc\n') == "['a\n', 'b\n', '\n', 'c\n']"
	assert stdin_probe.feed('get_raw_lines', 'a\r\nb\r\nc\n') == "['a\r\n', 'b\r\n', 'c\n']"
	assert stdin_probe.feed('get_raw_lines', 'only') == "['only']"
	assert stdin_probe.feed('get_raw_lines', 'only\n') == "['only\n']"
	assert stdin_probe.feed('get_raw_lines', '') == '[]'
}

fn test_get_raw_lines_joined_concatenates_the_raw_lines() {
	assert stdin_probe.feed('get_raw_lines_joined', 'a\nb\nc') == 'a\nb\nc'
	assert stdin_probe.feed('get_raw_lines_joined', 'a\nb\n\nc\n') == 'a\nb\n\nc\n'
	assert stdin_probe.feed('get_raw_lines_joined', 'a\r\nb\r\nc\n') == 'a\r\nb\r\nc\n'
	assert stdin_probe.feed('get_raw_lines_joined', 'only') == 'only'
	assert stdin_probe.feed('get_raw_lines_joined', '') == ''
}

fn test_get_trimmed_lines_keeps_empty_lines_and_strips_only_line_endings() {
	assert stdin_probe.feed('get_trimmed_lines', 'a\nb\nc') == "['a', 'b', 'c']"
	// Unlike `get_lines`, an empty line survives as an empty string.
	assert stdin_probe.feed('get_trimmed_lines', 'a\nb\n\nc\n') == "['a', 'b', '', 'c']"
	assert stdin_probe.feed('get_trimmed_lines', 'a\r\nb\r\nc\n') == "['a', 'b', 'c']"
	// Only `\r` and `\n` at the end are removed; other spacing is kept.
	assert stdin_probe.feed('get_trimmed_lines', '  padded  \n x \n') == "['  padded  ', ' x ']"
	assert stdin_probe.feed('get_trimmed_lines', 'only') == "['only']"
	assert stdin_probe.feed('get_trimmed_lines', '') == '[]'
}

fn test_input_and_input_opt_print_the_prompt_and_return_the_first_line() {
	assert stdin_probe.feed('input_opt', 'a\nb\nc') == 'PROMPT> a'
	assert stdin_probe.feed('input_opt', '  padded  \n x \n') == 'PROMPT>   padded  '
	assert stdin_probe.feed('input_opt', 'only') == 'PROMPT> only'
	// End of input is reported as `none`.
	assert stdin_probe.feed('input_opt', '') == 'PROMPT> <none>'

	assert stdin_probe.feed('input', 'a\nb\nc') == 'PROMPT> a'
	assert stdin_probe.feed('input', '  padded  \n x \n') == 'PROMPT>   padded  '
	assert stdin_probe.feed('input', 'only') == 'PROMPT> only'
	// End of input is reported as the literal `<EOF>`.
	assert stdin_probe.feed('input', '') == 'PROMPT> <EOF>'
}

fn test_get_raw_stdin_reads_every_byte_until_eof() {
	assert stdin_probe.feed('get_raw_stdin', 'one\ntwo\nthree\n') == 'one\ntwo\nthree\n'
	assert stdin_probe.feed('get_raw_stdin', 'a\r\n\n\x00b\xff') == 'a\r\n\n\x00b\xff'
	assert stdin_probe.feed('get_raw_stdin', '') == ''
	assert stdin_probe.feed('get_raw_stdin', 'unterminated') == 'unterminated'
	// The encoded output exceeds common pipe capacities and must drain before wait.
	payload := '0123456789'.repeat(20_000) + '\x00tail'
	assert stdin_probe.feed('get_raw_stdin', payload) == payload
}

fn test_get_raw_stdin_keeps_bytes_buffered_by_a_previous_line_read() {
	$if windows {
		// Windows line input uses a different console encoding path.
		return
	}
	assert stdin_probe.feed('get_raw_line_then_stdin', 'first\nsecond\nthird') == 'second\nthird'
}
