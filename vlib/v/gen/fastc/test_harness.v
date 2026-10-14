module fastc

import os
import strings
import v.pref

// fastc_is_test_harness_function reports the functions of a test file its main calls.
fn fastc_is_test_harness_function(name string) bool {
	return name.starts_with('test_')
		|| name in ['testsuite_begin', 'testsuite_end', 'before_each', 'after_each']
}

// fastc_is_test_source reports whether `path` is a test file of this build, by the driver's
// rule: a test file for this backend or the C backend, backend- and arch-qualified ones included.
fn fastc_is_test_source(path string, prefs &pref.Preferences) bool {
	return pref.is_test_file_for_platform(path, prefs.backend, prefs.target)
		|| pref.is_test_file_for_platform(path, 'c', prefs.target)
}

// fastc_source_line is the 1-based line of byte `offset` in the file at `path`.
fn fastc_source_line(path string, offset int) int {
	text := os.read_file(path) or { return 0 }
	return fastc_text_line(text, offset)
}

// fastc_text_line is the 1-based line of byte `offset` in `text`.
fn fastc_text_line(text string, offset int) int {
	mut line := 1
	end := if offset < text.len { offset } else { text.len }
	for i in 0 .. end {
		if text[i] == `\n` {
			line++
		}
	}
	return line
}

// fastc_test_failure_line is the line a test that returns an error reports, chosen as the C
// harness chooses it: the first line of its body that can return or propagate one, else the
// line of its `fn`.
fn fastc_test_failure_line(text string, position int) int {
	fn_line := fastc_text_line(text, position)
	lines := text.split_into_lines()
	for index in fn_line .. lines.len {
		trimmed := lines[index].trim_space()
		if trimmed.starts_with('fn ') {
			break
		}
		if trimmed == 'return' || trimmed.starts_with('return ') || trimmed.contains(' or {')
			|| trimmed.contains(')!') || trimmed.contains(']!') || trimmed.contains(')?')
			|| trimmed.contains(']?') || trimmed.ends_with('!') || trimmed.ends_with('?') {
			return index + 1
		}
	}
	return fn_line
}

struct FastcTestFunction {
	name         string
	c_name       string
	path         string
	position     int
	failure_line int
	return_type  string
}

// fastc_synthesized_test_main is the main of a test build: it runs every test of the test
// file in source order between testsuite_begin and testsuite_end, each between before_each and
// after_each. A failed assert reports itself and ends its test (v_fastc_test_assert_fail
// longjmps back here): a failed before_each skips the test, after_each runs either way. A test
// returning an error fails with its message; the exit status is 1 when any test failed.
fn fastc_synthesized_test_main(functions map[string]FastcFunctionSignature, function_c_names map[string]string, prefs &pref.Preferences, has_startup_inits bool, has_cleanup_hooks bool) string {
	selfhost := prefs.building_v
	mut texts := map[string]string{}
	mut tests := []FastcTestFunction{}
	mut hooks := map[string]string{}
	for key, signature in functions {
		name := key.all_after_last('.')
		if !fastc_is_test_harness_function(name) || signature.parameter_types.len > 0
			|| key.starts_with('C.') || !fastc_is_test_source(signature.path, prefs) {
			continue
		}
		c_name := function_c_names[key] or { fastc_c_function_name(signature.module_name, name) }
		if !name.starts_with('test_') {
			hooks[name] = c_name
			continue
		}
		if signature.path !in texts {
			texts[signature.path] = os.read_file(signature.path) or { '' }
		}
		text := texts[signature.path]
		position := text.index('fn ${name}(') or { text.len }
		tests << FastcTestFunction{
			name:         name
			c_name:       c_name
			path:         signature.path
			position:     position
			failure_line: fastc_test_failure_line(text, position)
			return_type:  signature.return_type
		}
	}
	tests.sort_with_compare(fn (a &FastcTestFunction, b &FastcTestFunction) int {
		if a.path != b.path {
			return if a.path < b.path { -1 } else { 1 }
		}
		return a.position - b.position
	})
	before_each := hooks['before_each'] or { '' }
	after_each := hooks['after_each'] or { '' }
	mut out := strings.new_builder(1024)
	out.writeln('#include <setjmp.h>')
	out.writeln('static jmp_buf v_fastc_test_jump;')
	out.writeln('static int v_fastc_test_active;')
	out.writeln('static int v_fastc_test_failures;')
	out.writeln('void v_fastc_test_assert_fail(string where, string message) {')
	out.writeln('\tfprintf(stderr, "%.*s: %.*s\\n", (int)where.len, (char *)where.str, (int)message.len, (char *)message.str);')
	out.writeln('\tv_fastc_test_failures++;')
	out.writeln('\tif (v_fastc_test_active) longjmp(v_fastc_test_jump, 1);')
	out.writeln('\texit(1);')
	out.writeln('}')
	parameters := if selfhost { 'int argc, char **argv' } else { 'void' }
	out.writeln('int main(${parameters}) {')
	out.writeln('\tsetvbuf(stdout, NULL, _IONBF, 0);')
	if selfhost {
		out.writeln('\tg_main_argc = argc;')
		out.writeln('\tg_main_argv = argv;')
	}
	if has_startup_inits {
		out.writeln('\tv_fastc_init_globals();')
	}
	if has_cleanup_hooks {
		out.writeln('\tatexit(v_fastc_cleanup_modules);')
	}
	out.writeln('\tint v_fastc_failed_tests = 0;')
	if begin := hooks['testsuite_begin'] {
		out.writeln('\t${begin}();')
	}
	for test in tests {
		out.writeln('\t{')
		out.writeln('\t\tint before = v_fastc_test_failures;')
		out.writeln('\t\tv_fastc_test_active = 1;')
		out.writeln('\t\tif (setjmp(v_fastc_test_jump) == 0) {')
		if before_each != '' {
			out.writeln('\t\t\t${before_each}();')
		}
		out.writeln('\t\t\tif (v_fastc_test_failures == before) {')
		if test.return_type in ['__v_result', 'Option'] {
			// A test returning an error reports it as the C harness does.
			message := if test.return_type == '__v_result' {
				'builtin__IError_msg(v_fastc_test_result.err)'
			} else {
				'_S("none")'
			}
			path_literal := test.path.replace('\\', '\\\\').replace('"', '\\"')
			out.writeln('\t\t\t\t${test.return_type} v_fastc_test_result = ${test.c_name}();')
			out.writeln('\t\t\t\tif (v_fastc_test_result.state != 0) {')
			out.writeln('\t\t\t\t\tstring v_fastc_test_error = ${message};')
			out.writeln('\t\t\t\t\tfprintf(stderr, "%s:%d: fn %s failed propagation with error: %.*s\\n", "${path_literal}", ${test.failure_line}, "${test.name}", (int)v_fastc_test_error.len, (char *)v_fastc_test_error.str);')
			out.writeln('\t\t\t\t\tv_fastc_test_failures++;')
			out.writeln('\t\t\t\t}')
		} else {
			out.writeln('\t\t\t\t${test.c_name}();')
		}
		out.writeln('\t\t\t}')
		out.writeln('\t\t}')
		if after_each != '' {
			out.writeln('\t\tif (setjmp(v_fastc_test_jump) == 0) {')
			out.writeln('\t\t\t${after_each}();')
			out.writeln('\t\t}')
		}
		out.writeln('\t\tv_fastc_test_active = 0;')
		out.writeln('\t\tif (v_fastc_test_failures != before) {')
		out.writeln('\t\t\tv_fastc_failed_tests++;')
		out.writeln('\t\t\tfprintf(stderr, "FAIL: ${test.name}\\n");')
		out.writeln('\t\t}')
		out.writeln('\t}')
	}
	if end := hooks['testsuite_end'] {
		out.writeln('\t${end}();')
	}
	out.writeln('\tif (v_fastc_failed_tests > 0) {')
	out.writeln('\t\tfprintf(stderr, "%d of ${tests.len} tests failed\\n", v_fastc_failed_tests);')
	out.writeln('\t\treturn 1;')
	out.writeln('\t}')
	out.writeln('\treturn 0;')
	out.writeln('}')
	out.writeln('')
	return out.str()
}

// test_propagation_failure is what a failed `?`/`!` does in a test function, which returns
// nothing: the test fails with the error (V's "failed propagation with error").
fn (g &Parser) test_propagation_failure(temporary string, is_result bool) string {
	mut w := unsafe { &Parser(g) }
	w.protos.writeln('void v_fastc_test_assert_fail(string where, string message);')
	where := '${g.path}: ${g.current_function}: failed propagation'
	where_literal := '"' + where.replace('\\', '\\\\').replace('"', '\\"') + '"'
	message := if is_result { 'builtin__IError_msg(${temporary}.err)' } else { '_S("none")' }
	return 'v_fastc_test_assert_fail(_S(${where_literal}), ${message});'
}
