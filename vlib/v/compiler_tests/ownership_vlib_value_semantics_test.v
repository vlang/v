import os

// Standard library bodies keep V's value semantics under `-ownership`: an owned value that
// a program passes to them, or one that they create themselves, is not checked as a move
// inside the library. The program's own code keeps its move checks.

fn ownership_value_semantics_source(name string, code string) !string {
	root := os.join_path(os.vtmp_dir(), 'ownership_vlib_value_semantics_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, code)!
	return source
}

fn ownership_value_semantics_cleanup(source string) {
	os.rmdir_all(os.dir(source)) or {}
}

fn ownership_value_semantics_v(mode string, args []string) os.Result {
	mut cmd := [@VEXE, '-new-compiler', '-nocache', '-ownership']
	if mode != '' {
		cmd << mode
	}
	cmd << args
	return os.exec(cmd)
}

fn test_owned_arguments_are_not_moved_inside_stdlib_bodies() {
	source := ownership_value_semantics_source('stdlib', 'import os
import net.http
import time

fn main() {
	path := "/tmp/example/file.txt".to_owned()
	assert os.base(path.clone()) == "file.txt"
	assert os.dir(path.clone()) == "/tmp/example"
	assert os.file_name(path) == "file.txt"
	mut header := http.new_header()
	key := "X-Request-Id".to_owned()
	header.add_custom(key, "abc".to_owned())!
	assert header.custom_values("X-Request-Id") == ["abc"]
	request := http.parse_request_head_str("GET /path?q=1 HTTP/1.1\\r\\nHost: example.com\\r\\n\\r\\n".to_owned())!
	assert request.url == "/path?q=1"
	assert request.header.get(.host) or { "" } == "example.com"
	stamp := time.parse("2020-01-02 03:04:05".to_owned())!
	assert stamp.format() == "2020-01-02 03:04"
	println("ok")
}
')!
	defer { ownership_value_semantics_cleanup(source) }
	for mode in ['-no-parallel', ''] {
		check := ownership_value_semantics_v(mode, ['-check', source])
		assert check.exit_code == 0, '${mode}: ${check.output}'
		assert !check.output.contains('error:'), '${mode}: ${check.output}'
		out := ownership_value_semantics_v(mode, ['-cc', 'clang', 'run', source])
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', '${mode}: ${out.output}'
	}
}

fn test_stdlib_owned_values_do_not_report_moves_in_veb() {
	source := ownership_value_semantics_source('veb', 'import veb

struct App {}

fn (mut app App) index() veb.Result {
	return veb.no_result()
}

fn main() {
	mut app := &App{}
	veb.run[App, veb.Context](mut app, 8080)
}
')!
	defer { ownership_value_semantics_cleanup(source) }
	out := ownership_value_semantics_v('', ['-check', source])
	assert out.exit_code == 0, out.output
	assert !out.output.contains('error:'), out.output
}

fn test_program_code_keeps_move_checks() {
	source := ownership_value_semantics_source('moves', 'import os

struct Named {
	name string
}

fn consume(value string) int {
	return value.len
}

fn show(named Named) int {
	return named.name.len
}

fn main() {
	value := "owned".to_owned()
	println(consume(value))
	println(value)
	path := "/tmp/example".to_owned()
	println(os.base(path))
	println(path)
	named := Named{name: "x"}
	println(show(named))
	println(show(named))
}
')!
	defer { ownership_value_semantics_cleanup(source) }
	for mode in ['-no-parallel', ''] {
		out := ownership_value_semantics_v(mode, ['-check', source])
		assert out.exit_code != 0, '${mode}: ${out.output}'
		assert out.output.contains('main.v:18:10: error: use of moved value: `value`'), '${mode}: ${out.output}'
		assert out.output.contains('main.v:21:10: error: use of moved value: `path`'), '${mode}: ${out.output}'
		assert out.output.contains('main.v:24:15: error: use of moved value: `named`'), '${mode}: ${out.output}'
		assert out.output.count('error:') == 3, '${mode}: ${out.output}'
	}
}
