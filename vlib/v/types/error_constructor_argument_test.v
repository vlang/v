module types

import os
import v.parser
import v.pref

fn error_constructor_check(source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'error_constructor_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.interface_names['IError'] = true
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_error_constructor_rejects_ierror_message() {
	tc := error_constructor_check('fn fail() !string { return error("failed") }
fn main() {
	fail() or { _ = error(err) }
}
')
	assert tc.errors.any(it.msg == 'cannot use `IError` as `string` in argument 1 to `error`'), tc.errors.str()
	assert tc.notices.any(it.msg == '`error(err)` can be shortened to just `err`'), tc.notices.str()
}

fn test_error_with_code_rejects_ierror_message() {
	tc := error_constructor_check('fn fail() !string { return error("failed") }
fn main() {
	fail() or { _ = error_with_code(err, 7) }
}
')
	assert tc.errors.any(it.msg == 'cannot use `IError` as `string` in argument 1 to `error_with_code`'), tc.errors.str()
}

fn test_error_constructor_accepts_strings_and_propagation() {
	tc := error_constructor_check('fn fail() !string { return error("failed") }
fn propagate() !string { return fail() or { return err } }
fn wrap() !string { return fail() or { return error("prefix: \${err}") } }
fn main() { _ = error_with_code("message", 7) }
')
	assert tc.errors.len == 0, tc.errors.str()
}
