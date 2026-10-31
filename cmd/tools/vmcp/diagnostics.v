// Running the compiler and turning its output into records an agent can act on.
//
// The type checker collects its diagnostics internally and exposes them only as
// printed text, so `v_check` runs the compiler as a child process and reads that
// text back. The parser is deliberately strict about the `path:line:column: kind:
// message` shape `v.errors.format` produces, and keeps everything it does not
// recognise in a `raw` field rather than dropping it: a diagnostic that failed
// to parse must still reach the agent, even unparsed.
module main

import os
import v.astjson

// Diagnostic is one compiler message with the position it points at.
pub struct Diagnostic {
pub mut:
	// path is the file, as the compiler reported it.
	path string
	// line and column are 1-based. A diagnostic without a usable position has
	// both at zero.
	line   int
	column int
	// kind is `error`, `warning` or `notice`.
	kind string
	// message is the text after the kind, with the source context stripped.
	message string
	// raw is the original line, for a diagnostic that could not be parsed.
	raw string
}

// parsed reports whether the diagnostic carries a usable position.
pub fn (d Diagnostic) parsed() bool {
	return d.path != '' && d.line > 0
}

// to_json renders one diagnostic as a JSON object.
pub fn (d Diagnostic) to_json() string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(d.path)
	w.key('line')
	w.number(d.line)
	w.key('column')
	w.number(d.column)
	w.key('kind')
	w.string(d.kind)
	w.key('message')
	w.string(d.message)
	if d.raw != '' {
		w.key('raw')
		w.string(d.raw)
	}
	w.end_object()
	return w.str()
}

// max_diagnostics_default caps how many diagnostics one tool response carries
// before it truncates. Counts stay totals; only the array is paged.
const max_diagnostics_default = 100

// diagnostics_json renders a list of diagnostics as a JSON array.
pub fn diagnostics_json(items []Diagnostic) string {
	mut w := astjson.Writer{}
	w.begin_array()
	for item in items {
		w.array_raw(item.to_json())
	}
	w.end_array()
	return w.str()
}

// page_diagnostics keeps the head of `items` and reports how many were omitted.
// The first error is usually the cause and the rest its cascade, so the head is
// what an agent reads first. `max < 1` means the default. Counts computed
// elsewhere stay totals; only the rendered array is paged.
fn page_diagnostics(items []Diagnostic, max int) ([]Diagnostic, int) {
	limit := if max < 1 { max_diagnostics_default } else { max }
	if items.len <= limit {
		return items, 0
	}
	return items[..limit], items.len - limit
}

// parse_diagnostics reads the compiler's output into records.
//
// Each diagnostic starts a new record; every following line up to the next one is
// that diagnostic's source context, which is folded into `raw` instead of being
// reported as a separate message.
pub fn parse_diagnostics(output string) []Diagnostic {
	mut out := []Diagnostic{}
	for line in output.split_into_lines() {
		if diagnostic := parse_diagnostic_line(line) {
			out << diagnostic
		} else if out.len > 0 && line.trim_space() != '' {
			// Source context for the diagnostic above; keep it verbatim.
			mut previous := out[out.len - 1]
			previous.raw += '\n' + line
			out[out.len - 1] = previous
		}
	}
	return out
}

// parse_diagnostic_line reads one `<path>:<line>:<column>: <kind>: <message>`
// line, or returns none when the line is source context or a note.
//
// The colon that ends the path is found by trying each colon in turn, because a
// Windows path carries its own colon (`C:/src/main.v`) long before the position
// starts. Scanning forward is what keeps a drive letter from being read as the
// line number.
fn parse_diagnostic_line(line string) ?Diagnostic {
	mut offset := 0
	for offset < line.len {
		relative := line[offset..].index(':') or { return none }
		colon := offset + relative
		position := position_of(line[colon + 1..]) or {
			offset = colon + 1
			continue
		}
		return Diagnostic{
			path:    line[..colon]
			line:    position.line
			column:  position.column
			kind:    kind_of(position.body)
			message: message_of(position.body)
		}
	}
	return none
}

// Position is the `<line>:<column>: <body>` head of a diagnostic, with the path
// already split off.
struct Position {
	line   int
	column int
	body   string
}

// position_of reads a `<line>:<column>: <rest>` head, or returns none when the
// text does not start with two numbers.
fn position_of(text string) ?Position {
	first_colon := text.index(':') or { return none }
	line_no := text[..first_colon].int()
	if line_no <= 0 {
		return none
	}
	rest := text[first_colon + 1..]
	second_colon := rest.index(':') or { return none }
	column := rest[..second_colon].int()
	if column <= 0 {
		return none
	}
	return Position{
		line:   line_no
		column: column
		body:   rest[second_colon + 1..].trim_space()
	}
}

// kind_of reads the leading `error:`, `warning:` or `notice:` of a diagnostic
// body, or an empty string when the body carries no kind.
fn kind_of(body string) string {
	for candidate in diagnostic_kinds {
		if body.starts_with(candidate + ':') {
			return candidate
		}
	}
	return ''
}

// message_of returns the text of a diagnostic body after its kind.
fn message_of(body string) string {
	kind := kind_of(body)
	if kind == '' {
		return body
	}
	return body[kind.len + 1..].trim_space()
}

// diagnostic_kinds are the kinds `v.errors.format` prints.
const diagnostic_kinds = ['error', 'warning', 'notice']

// compiler_run is the outcome of one compiler invocation.
pub struct CompilerRun {
pub:
	// exit_code is the compiler's own exit code.
	exit_code int
	// output is everything the compiler printed, standard streams merged.
	output string
	// command is the command line that ran, for the error message when the
	// compiler could not be started at all.
	command string
	// launch_error is why the compiler could not be started, or an empty string
	// when it did start.
	//
	// This is not the same as a non-zero exit code: a failed launch produces
	// `exec failed (...)` as the whole output and never ran the compiler, so a
	// caller that read it as diagnostics would report a broken sandbox as a
	// compilation error.
	launch_error string
}

// started reports whether the compiler actually ran.
pub fn (r CompilerRun) started() bool {
	return r.launch_error == ''
}

// run_compiler runs the workspace compiler with `args`.
//
// `args` is a real argument array, and `os.Process` starts the process with it
// rather than with a command string, so an argument carrying a space arrives as
// one argument instead of being split by a shell. That also means callers pass
// raw paths and raw flags: quoting here would double-quote them.
//
// The environment is left alone so the compiler sees the same flags, module
// search path and environment the user's shell would give it. Only the child
// changes its working directory, so relative flags and program file accesses
// use the workspace without changing the MCP server's own directory.
pub fn run_compiler(ws &Workspace, args []string) CompilerRun {
	command := compiler_command_line(ws.compiler, args)
	executable := os.find_abs_path_of_executable(ws.compiler) or {
		return CompilerRun{
			exit_code:    -1
			command:      command
			launch_error: err.msg()
		}
	}
	if !os.is_executable(executable) {
		return CompilerRun{
			exit_code:    -1
			command:      command
			launch_error: '`${ws.compiler}` is not executable'
		}
	}
	if !os.is_dir(ws.root) {
		return CompilerRun{
			exit_code:    -1
			command:      command
			launch_error: 'workspace `${ws.root}` is not a directory'
		}
	}
	$if !windows {
		if !os.is_executable(ws.root) {
			return CompilerRun{
				exit_code:    -1
				command:      command
				launch_error: 'workspace `${ws.root}` is not searchable'
			}
		}
	}
	$if windows {
		result := run_compiler_windows(executable, args, ws.root)
		return CompilerRun{
			exit_code:    result.exit_code
			output:       result.output
			command:      command
			launch_error: result.launch_error
		}
	}
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(ws.root)
	process.set_redirect_stdio_merged()
	// MCP owns stdin. Programs receive EOF instead of an unwritten input pipe.
	process.set_stdin_path(os.path_devnull)
	process.run()
	output := process.stdout_slurp()
	process.wait()
	exit_code := process.code
	process.close()
	trimmed := output.trim_space()
	return CompilerRun{
		exit_code:    exit_code
		output:       output
		command:      command
		launch_error: if exit_code != 0 && launch_failure(trimmed) { trimmed } else { '' }
	}
}

// compiler_command_line renders `compiler` and `args` as a command a person can
// read, and copy and paste into a shell.
//
// It is for reporting only: nothing is run through it. `os.Process` takes the
// array directly, so a value that needed quoting here still reaches the compiler
// as exactly one argument.
fn compiler_command_line(compiler string, args []string) string {
	mut parts := [os.quoted_path(compiler)]
	for arg in args {
		parts << os.quoted_path(arg)
	}
	return parts.join(' ')
}

// launch_failure reports whether output is a process-launch failure message.
//
// Process launch failures can arrive on stderr rather than through a flag,
// so the messages are matched by their shape: a real compiler always
// speaks in diagnostics of the form `path:line:column: kind: message`.
fn launch_failure(output string) bool {
	if output == '' {
		return false
	}
	if parse_diagnostic_line(output) != none {
		return false
	}
	return output.starts_with('os: failed to execute "')
		|| output.starts_with('exec failed (')
		|| output.starts_with('exec requires at least one argument')
		|| output.starts_with('exec("') && output.ends_with('") failed')
}

// count_errors returns how many of the diagnostics are errors.
pub fn count_errors(items []Diagnostic) int {
	mut n := 0
	for item in items {
		if item.kind == 'error' {
			n++
		}
	}
	return n
}

// unparsed returns the diagnostics that carried no usable position, which is
// where a compiler message about the project as a whole ends up.
pub fn unparsed(items []Diagnostic) []string {
	mut out := []string{}
	for item in items {
		if !item.parsed() {
			out << item.raw.trim_space()
		}
	}
	return out
}

// trim_output keeps the tail of `output`, which is where a compiler explains what
// went wrong, and marks that it was trimmed.
pub fn trim_output(output string, max_lines int) string {
	lines := output.trim_space().split_into_lines()
	if lines.len <= max_lines {
		return output.trim_space()
	}
	kept := lines[lines.len - max_lines..]
	return '(earlier output omitted)\n' + kept.join('\n')
}
