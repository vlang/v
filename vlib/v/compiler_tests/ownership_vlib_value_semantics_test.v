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

// ownership_value_semantics_c builds `source` into C with `-gc none` and returns the C code.
fn ownership_value_semantics_c(mode string, source string) !string {
	c_file := os.join_path(os.dir(source), 'main${mode}.c')
	out := ownership_value_semantics_v(mode, ['-gc', 'none', '-o', c_file, source])
	assert out.exit_code == 0, '${mode}: ${out.output}'
	return os.read_file(c_file)!
}

// ownership_value_semantics_c_fn returns the definition of the C function `name`.
fn ownership_value_semantics_c_fn(c string, name string) string {
	mut lines := []string{}
	for line in c.split_into_lines() {
		if lines.len == 0 {
			if !line.starts_with('\t') && line.contains(' ${name}(') && line.ends_with(') {') {
				lines << line
			}
			continue
		}
		lines << line
		if line == '}' {
			break
		}
	}
	assert lines.len > 0, 'missing C function `${name}`'
	return lines.join('\n')
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

fn test_stdlib_bodies_do_not_destroy_values_they_replace_or_remove() {
	source := ownership_value_semantics_source('lowering', 'import net.http
import net.urllib

fn main() {
	mut header := http.new_header()
	header.add_custom("X-Over", "one".to_owned())!
	header.add_custom("X-Keep", "two".to_owned())!
	header.delete_custom("X-Over")
	assert header.get_custom("X-Keep") or { "" } == "two"
	response := http.parse_response("HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\nX-Folded: a\\r\\n b\\r\\nX-Last: c\\r\\n\\r\\nbody".to_owned())!
	assert response.header.get(.content_type) or { "" } == "text/plain"
	assert response.header.get_custom("X-Folded") or { "" } == "a b"
	assert response.body == "body"
	mut values := urllib.parse_query("a=1&b=2&a=3".to_owned())!
	values.del("a")
	assert values.get("b") or { "" } == "2"
	request := http.parse_request_head_str("GET /path HTTP/1.1\\r\\nHost: example.com\\r\\n\\r\\n".to_owned())!
	assert request.url == "/path"
	println("ok")
}
')!
	defer { ownership_value_semantics_cleanup(source) }
	for mode in ['-no-parallel', ''] {
		c := ownership_value_semantics_c(mode, source)!
		// `h.data[i] = h.data[i + 1]` replaces an entry whose strings the next slot still shares.
		delete_at := ownership_value_semantics_c_fn(c, 'http__Header__delete_at')
		assert !delete_at.contains('__drop_assign'), '${mode}: ${delete_at}'
		assert !delete_at.contains('string__free('), '${mode}: ${delete_at}'
		// `last_key, last_value = parse_header(line)!` follows `h.add_custom(last_key, last_value)!`.
		parse_headers := ownership_value_semantics_c_fn(c, 'http__parse_headers').replace_each([
			'\t',
			'',
			'\n',
			'',
			' ',
			'',
		])
		assert !parse_headers.contains('string__free(&(last_key))'), '${mode}: ${parse_headers}'
		assert !parse_headers.contains('string__free(&(last_value))'), '${mode}: ${parse_headers}'
		// `v.data.delete(idx)`
		values_del := ownership_value_semantics_c_fn(c, 'urllib__Values__del')
		assert !values_del.contains('string__free('), '${mode}: ${values_del}'
		// `request_cookies[cookie.name] = cookie.value`
		request_head := ownership_value_semantics_c_fn(c, 'http__parse_request_head_str')
		assert !request_head.contains('map_old_value'), '${mode}: ${request_head}'
	}
	out := ownership_value_semantics_v('', ['-gc', 'none', '-cc', 'clang', 'run', source])
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output
}

fn test_stdlib_bodies_copy_values_for_owning_builtin_parameters() {
	// `fail` moves an owned message into `error`, so the parameter of `error` takes ownership.
	// The literal message that `strconv` passes to `error` is then copied for it.
	source := ownership_value_semantics_source('copies', 'import strconv

fn fail(message string) ! {
	return error(message)
}

fn main() {
	fail("custom failure".to_owned()) or { println(err.msg()) }
	strconv.atoi("") or { println(err.msg()) }
}
')!
	defer { ownership_value_semantics_cleanup(source) }
	for mode in ['-no-parallel', ''] {
		c := ownership_value_semantics_c(mode, source)!
		atoi_check := ownership_value_semantics_c_fn(c, 'strconv__atoi_common_check')
		assert atoi_check.contains('string__clone('), '${mode}: ${atoi_check}'
	}
	out := ownership_value_semantics_v('', ['-gc', 'none', '-cc', 'clang', 'run', source])
	assert out.exit_code == 0, out.output
	assert out.output.split_into_lines() == ['custom failure', 'strconv.atoi: parsing "": empty string'], out.output
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
