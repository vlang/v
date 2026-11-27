import os
import v.tempname

fn test_ierror_patterns_use_type_ids_and_file_local_imports() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_match_codegen_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'myerrs')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'myerrs.v'), "module myerrs

pub struct FirstError {
	Error
pub:
	input string
}

pub fn (err FirstError) msg() string {
	return 'first ' + err.input
}

pub fn (err FirstError) code() int {
	return 11
}

pub struct SecondError {
	Error
}

pub fn (err SecondError) msg() string {
	return 'second'
}

pub fn (err SecondError) code() int {
	return 22
}

pub fn fail(code int) !int {
	if code == 1 {
		return &FirstError{
			input: 'alpha'
		}
	}
	if code == 2 {
		return &SecondError{}
	}
	return 7
}

pub fn fail_from_match(code int) !int {
	return match code {
		1 {
			&FirstError{
				input: 'match'
			}
		}
		2 {
			&SecondError{}
		}
		else {
			7
		}
	}
}

pub fn fail_with_defer(code int) !int {
	defer {
		_ := code
	}
	if code == 1 {
		return &FirstError{
			input: 'defer'
		}
	}
	return 7
}
 ") or {
		panic(err)
	}

	alias_dir := os.join_path(root, 'nested', 'aliaserrs')
	selective_dir := os.join_path(root, 'nested', 'selecterrs')
	os.mkdir_all(alias_dir) or { panic(err) }
	os.mkdir_all(selective_dir) or { panic(err) }
	os.write_file(os.join_path(alias_dir, 'aliaserrs.v'), "module aliaserrs

pub struct AliasError {
	Error
pub:
	label string
}

pub fn (err AliasError) msg() string {
	return 'alias ' + err.label
}

pub fn (err AliasError) code() int {
	return 31
}

pub fn fail_alias() !int {
	return &AliasError{
		label: 'nested'
	}
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(selective_dir, 'selecterrs.v'), "module selecterrs

pub struct SelectiveError {
	Error
pub:
	label string
}

pub fn (err SelectiveError) msg() string {
	return 'selective ' + err.label
}

pub fn (err SelectiveError) code() int {
	return 41
}

pub fn fail_selective() !int {
	return &SelectiveError{
		label: 'nested'
	}
}
") or {
		panic(err)
	}

	fake_dir := os.join_path(root, 'fake')
	os.mkdir_all(fake_dir) or { panic(err) }
	os.write_file(os.join_path(fake_dir, 'fake.v'), "module fake

pub struct Error {
	MessageError
pub:
	label string
}

pub fn fail() !int {
	return Error{
		label: 'selective'
	}
}
") or {
		panic(err)
	}

	// Keep each import scope in the file that uses it.
	os.write_file(os.join_path(root, 'nested_patterns.v'), "module main

import nested.aliaserrs as aliased
import nested.selecterrs { SelectiveError, fail_selective }

fn classify_alias(err IError) string {
	return match err {
		aliased.AliasError {
			'alias'
		}
		else {
			'other'
		}
	}
}

fn classify_selective(err IError) string {
	return match err {
		SelectiveError {
			'selective'
		}
		else {
			'other'
		}
	}
}

fn check_nested_patterns() {
	mut matched := 0
	aliased.fail_alias() or {
		assert err.msg() == 'alias nested'
		assert classify_alias(err) == 'alias'
		assert classify_selective(err) == 'other'
		assert err is aliased.AliasError
		matched++
	}

	fail_selective() or {
		assert err.msg() == 'selective nested'
		assert classify_alias(err) == 'other'
		assert classify_selective(err) == 'selective'
		assert err is SelectiveError
		matched++
	}
	assert matched == 2
}
") or {
		panic(err)
	}

	os.write_file(os.join_path(root, 'selective_error_pattern.v'), "module main

import fake { Error, fail }

fn matches_fake(err IError) bool {
	if err is Error {
		assert err.label == 'selective'
		return true
	}
	return false
}

fn classify_scoped_error(err IError) string {
	return match err {
		Error {
			assert err.label == 'selective'
			err.label
		}
		else {
			'other'
		}
	}
}

fn check_scoped_error_pattern() {
	fail() or {
		assert matches_fake(err)
		assert classify_scoped_error(err) == 'selective'
		return
	}
	assert false
}
") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import myerrs

fn classify(err IError) string {
	match err {
		myerrs.FirstError {
			return 'first'
		}
		myerrs.SecondError {
			return 'second'
		}
		else {
			return 'unknown'
		}
	}
}

fn is_first(err IError) bool {
	return err is myerrs.FirstError
}

fn main() {
	myerrs.fail(1) or {
		assert err.msg() == 'first alpha'
		assert err.code() == 11
		assert classify(err) == 'first'
		assert is_first(err)
	}
	myerrs.fail(2) or {
		assert err.msg() == 'second'
		assert err.code() == 22
		assert classify(err) == 'second'
		assert !is_first(err)
	}
	assert myerrs.fail(3)! == 7
	myerrs.fail_from_match(1) or {
		assert err.msg() == 'first match'
		assert classify(err) == 'first'
	}
	myerrs.fail_with_defer(1) or {
		assert err.msg() == 'first defer'
		assert classify(err) == 'first'
	}
	println('ok')

	check_nested_patterns()
	check_scoped_error_pattern()
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_match_codegen_input')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	classify_body := c_code.all_after('string classify(IError err) {').all_before('\n}')
	assert classify_body.contains('err._typ == '), classify_body
	// The interface method still receives the address of the typed receiver.
	first_error_body := c_code.all_after('myerrs__fail(1);').all_before('\n\t}')
	assert first_error_body.contains('IError__msg(&err)'), first_error_body
	assert !c_code.contains('err == myerrs__FirstError'), c_code
	assert !c_code.contains('err == myerrs__SecondError'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok\nok'
}

fn test_ierror_rejects_incompatible_imported_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_pattern_rejections_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), "module fake

pub struct Error {}

pub struct MessageError {}

pub struct FalseError {
	Error
}

pub fn (err FalseError) msg() string {
	return 'false error'
}

pub fn (err FalseError) code() int {
	return 91
}

pub struct FalseMessageError {
	MessageError
}

pub fn (err FalseMessageError) msg() string {
	return 'false message error'
}

pub fn (err FalseMessageError) code() int {
	return 92
}

pub fn fail() !int {
	return Error{}
}
") or {
		panic(err)
	}

	fakebox_dir := os.join_path(root, 'fakebox')
	os.mkdir_all(fakebox_dir) or { panic(err) }
	os.write_file(os.join_path(fakebox_dir, 'fakebox.v'), "module fakebox

pub struct Error {}

pub struct MessageError {}

pub struct FalseError {
	Error
}

pub fn (err FalseError) msg() string {
	return 'false error'
}

pub fn (err FalseError) code() int {
	return 91
}

pub struct FalseMessageError {
	MessageError
}

pub fn (err FalseMessageError) msg() string {
	return 'false message error'
}

pub fn (err FalseMessageError) code() int {
	return 92
}

pub fn fail_error() !int {
	return &FalseError{}
}

pub fn fail_message_error() !int {
	return &FalseMessageError{}
}
") or {
		panic(err)
	}

	noterrs_dir := os.join_path(root, 'noterrs')
	os.mkdir_all(noterrs_dir) or { panic(err) }
	os.write_file(os.join_path(noterrs_dir, 'noterrs.v'), "module noterrs

pub struct NotError {}

pub fn (err NotError) msg() string {
	return 'not actually an error'
}
") or {
		panic(err)
	}

	// Keep the selective Error import scoped to its fixture.
	os.write_file(os.join_path(root, 'scoped_error.v'), 'module main

import fake { Error, fail }

fn is_fake(err IError) bool {
	return err is Error
}

fn check_scoped_error() {
	fail() or {
		_ := is_fake(err)
		return
	}
}
') or { panic(err) }

	os.write_file(os.join_path(root, 'msg_only.v'), "module main

import noterrs

fn classify_msg_only(err IError) string {
	match err {
		noterrs.NotError {
			return 'not-error'
		}
		else {
			return 'unknown'
		}
	}
}

fn is_not_error(err IError) bool {
	return err is noterrs.NotError
}

fn check_msg_only() {
	err := error('plain')
	assert classify_msg_only(err) == 'unknown'
	assert !is_not_error(err)
}
") or { panic(err) }

	os.write_file(os.join_path(root, 'fake_embeds.v'), "module main

import fake

fn classify_fake_embeds(err IError) string {
	match err {
		fake.FalseError {
			return 'false-error'
		}
		fake.FalseMessageError {
			return 'false-message-error'
		}
		else {
			return 'unknown'
		}
	}
}

fn is_false_error(err IError) bool {
	return err is fake.FalseError || err is fake.FalseMessageError
}

fn check_fake_embeds() {
	err := error('plain')
	assert classify_fake_embeds(err) == 'unknown'
	assert !is_false_error(err)
}
") or { panic(err) }

	os.write_file(os.join_path(root, 'main.v'), 'import fakebox

fn main() {
	check_scoped_error()
	check_msg_only()
	check_fake_embeds()
	a := fakebox.fail_error() or { 0 }
	b := fakebox.fail_message_error() or { 0 }
	_ = a + b
}
') or { panic(err) }

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, root, '-b', 'c', '-nocolor', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut diagnostics := map[string][]string{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		file := location.all_before_last(':').all_before_last(':')
		diagnostics[os.file_name(file)] << message
	}
	for file, expected in {
		'fakebox.v':      [
			'cannot use `&fakebox.FalseError` as type `!int` in return argument',
			'cannot use `&fakebox.FalseMessageError` as type `!int` in return argument',
		]
		'fake.v':         ['fake.Error` as type `!int` in return argument']
		'scoped_error.v': ["`Error` doesn't implement interface `IError`"]
		'msg_only.v':     [
			'`noterrs.NotError` is not compatible with `IError`',
			"`noterrs.NotError` doesn't implement interface `IError`",
		]
		'fake_embeds.v':  [
			'`fake.FalseError` is not compatible with `IError`',
			'`fake.FalseMessageError` is not compatible with `IError`',
			"`fake.FalseError` doesn't implement interface `IError`",
			"`fake.FalseMessageError` doesn't implement interface `IError`",
		]
	} {
		for message in expected {
			assert diagnostics[file].any(it.contains(message)), 'expected ${message} in ${file}\n${compile.output}'
		}
	}
}

fn test_local_error_pattern_does_not_fallback_to_builtin_error() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_local_error_no_fallback_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'struct Error {}

fn is_local(err IError) bool {
	return err is Error
}

fn main() {
	err := error("plain")
	_ := is_local(err)
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_local_error_no_fallback_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('Error') && compile.output.contains('IError'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}
