import os
import v.tempname

fn ierror_payload_rejection(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_ierror_payload_${name}_out')
	compile := os.exec([@VEXE, src, '-b', 'c', '-nocolor', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	return compile.output
}

fn test_ierror_payloads_are_preserved_for_typed_and_void_results() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_compat_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "import os

struct CustomError {
	text string
}

type AliasError = CustomError
type AliasChainError = AliasError

fn (err CustomError) msg() string {
	return err.text
}

fn (err CustomError) code() int {
	return 77
}

fn os_error() !int {
	return os.NotExpected{}
}

fn custom_error() !int {
	return CustomError{
		text: 'custom payload'
	}
}

fn alias_error() !int {
	return AliasError(CustomError{
		text: 'alias payload'
	})
}

fn alias_chain_error() !int {
	return AliasChainError(AliasError(CustomError{
		text: 'alias chain payload'
	}))
}

fn direct() !int {
	return error('direct boom')
}

struct VoidError {
	text string
}

fn (err VoidError) msg() string {
	return err.text
}

fn (err VoidError) code() int {
	return 77
}

fn void_custom_error() ! {
	return VoidError{
		text: 'void payload'
	}
}

fn void_custom_error_with_defer() ! {
	defer {
		_ := 1
	}
	return VoidError{
		text: 'void defer payload'
	}
}

fn builtin_error() ! {
	return error('builtin void')
}

fn main() {
	mut payloads := 0
	os_error() or {
		assert err.msg() == ''
		assert err.code() == 0
		payloads++
	}
	custom_error() or {
		assert err.msg() == 'custom payload'
		assert err.code() == 77
		payloads++
	}
	alias_error() or {
		assert err.msg() == 'alias payload'
		assert err.code() == 77
		payloads++
	}
	alias_chain_error() or {
		assert err.msg() == 'alias chain payload'
		assert err.code() == 77
		payloads++
	}
	direct() or {
		assert err.msg() == 'direct boom'
		payloads++
	}
	void_custom_error() or {
		assert err.msg() == 'void payload'
		assert err.code() == 77
		payloads++
	}
	void_custom_error_with_defer() or {
		assert err.msg() == 'void defer payload'
		assert err.code() == 77
		payloads++
	}
	builtin_error() or {
		assert err.msg() == 'builtin void'
		assert err.code() == 0
		payloads++
	}
	assert payloads == 8
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_compat_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	for name in ['void_custom_error', 'void_custom_error_with_defer'] {
		signature := '__v_result ${name}(void) {'
		assert c_code.contains(signature), c_code
		body := c_code.all_after(signature).all_before('\n}')
		assert body.contains('.err = (IError){._typ = '), body
		assert !body.contains('return (__v_result){.ok = false};'), body
	}
}

fn test_ierror_rejects_invalid_payload_uses() {
	output := ierror_payload_rejection('invalid_payload_uses', "struct CustomError {
	text string
}

fn (err CustomError) msg() string {
	return err.text
}

fn (err CustomError) code() int {
	return 77
}

fn takes(x int) {
	_ = x
}

fn check_call_args() {
	takes(CustomError{
		text: 'payload'
	})
	err := error('boom')
	takes(err)
}

fn ok() !int {
	return 1
}

fn check_assignment() {
	mut value := ok() or { return }
	value = CustomError{
		text: 'payload'
	}
}

struct Plain {
	text string
}

type AliasPlain = Plain
type AliasChainPlain = AliasPlain

fn bad_alias() !int {
	return AliasChainPlain(AliasPlain(Plain{
		text: 'nope'
	}))
}

struct MatchError {}

fn (err MatchError) msg() string {
	return 'custom'
}

fn (err MatchError) code() int {
	return 77
}

struct Other {}

fn other() Other {
	return Other{}
}

fn fail_match(x int) !int {
	return match x {
		0 { MatchError{} }
		else { other() }
	}
}

struct NotError {}

fn (err NotError) msg() string {
	return 'msg only'
}

fn bad_message_only() !int {
	return NotError{}
}

fn main() {
	check_call_args()
	check_assignment()
	bad_alias() or {}
	fail_match(1) or {}
	_ := bad_message_only() or { 0 }
}
")
	errors := output.split_into_lines().filter(it.contains(': error: '))
	for expected in [
		['cannot use', '`CustomError`'],
		['cannot use', '`IError`'],
		['cannot assign', '`CustomError`'],
		['in return argument', '`AliasChainPlain`'],
		['return type mismatch', '`Other`'],
		['in return argument', '`NotError`'],
	] {
		assert errors.any(it.contains(expected[0]) && it.contains(expected[1])), 'expected `${expected[0]}` for ${expected[1]}\n${output}'
	}
}

fn test_imported_ierror_payload_resolution() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_imports_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	errmod_dir := os.join_path(root, 'errmod')
	os.mkdir_all(errmod_dir) or { panic(err) }
	os.write_file(os.join_path(errmod_dir, 'errmod.v'), 'module errmod

pub struct CustomError {
	text string
}

pub fn (err CustomError) msg() string {
	return err.text
}

pub fn (err CustomError) code() int {
	return 77
}
	') or {
		panic(err)
	}

	// Keep the selective import scoped to the file that uses it.
	selective_path := os.join_path(root, 'selective_payload.v')
	os.write_file(selective_path, "module main

import errmod { CustomError }

fn fail() !int {
	return CustomError{
		text: 'imported'
	}
}

fn check_selective_imported_payload() {
	fail() or {
		assert err.msg() == 'imported'
		assert err.code() == 77
		println('ok')
		return
	}
	println('bad')
}
	") or {
		panic(err)
	}

	payload_dir := os.join_path(root, 'payloadmod')
	os.mkdir_all(payload_dir) or { panic(err) }
	os.write_file(os.join_path(payload_dir, 'payloadmod.v'), "module payloadmod

pub struct EmbeddedBuiltinError {
	Error
}

pub fn (err EmbeddedBuiltinError) msg() string {
	return 'embedded builtin error'
}

pub fn (err EmbeddedBuiltinError) code() int {
	return 33
}

pub fn builtin_error() !int {
	return Error{}
}

pub fn embedded_error() !int {
	return EmbeddedBuiltinError{}
}

// This wrapper inherits msg and code from the builtin Error.
pub struct InheritedBuiltinError {
	Error
}

pub fn payload() !IError {
	return InheritedBuiltinError{}
}
") or {
		panic(err)
	}

	fake_dir := os.join_path(root, 'fake')
	os.mkdir_all(fake_dir) or { panic(err) }
	os.write_file(os.join_path(fake_dir, 'fake.v'), 'module fake

pub struct Error {}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake
import payloadmod

fn fail_with_normal_import() !int {
	_ := fake.Error{}
	return Error{}
}

fn main() {
	check_selective_imported_payload()

	payloadmod.builtin_error() or {
		assert err.msg() == ''
		assert err.code() == 0
		println('module builtin error')
	}
	payloadmod.embedded_error() or {
		assert err.msg() == 'embedded builtin error'
		assert err.code() == 33
		println('module embedded error')
	}
	fail_with_normal_import() or {
		assert err.msg() == ''
		assert err.code() == 0
		println('builtin error with normal import')
	}
	value := payloadmod.payload() or { panic(err) }
	assert value.msg() == ''
	assert value.code() == 0
	println('embedded IError value')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == [
		'ok',
		'module builtin error',
		'module embedded error',
		'builtin error with normal import',
		'embedded IError value',
	], run.output
}

fn test_imported_error_embeds_are_not_result_payloads() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_imported_embeds_${tempname.unique_token()}')
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

pub fn fail_error() !int {
	return FalseError{}
}

pub fn fail_message_error() !int {
	return FalseMessageError{}
}
") or {
		panic(err)
	}

	// Keep each selective import in the file that declares its wrapper.
	for embed, function in {
		'Error':        'selective_error'
		'MessageError': 'selective_message_error'
	} {
		os.write_file(os.join_path(root, '${function}.v'), "module main

import fake { ${embed} }

struct Selective${embed} {
	${embed}
}

fn (err Selective${embed}) msg() string {
	return 'bad'
}

fn (err Selective${embed}) code() int {
	return 7
}

fn ${function}() !int {
	return Selective${embed}{}
}
") or { panic(err) }
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import fake

fn main() {
	a := fake.fail_error() or { 0 }
	b := fake.fail_message_error() or { 0 }
	_ = a + b
	_ := selective_error() or { 0 }
	_ := selective_message_error() or { 0 }
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'out')
	compile := os.exec([@VEXE, root, '-b', 'c', '-nocolor', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	errors := compile.output.split_into_lines().filter(it.contains(': error: '))
	for typ, file in {
		'fake.FalseError':        'fake.v'
		'fake.FalseMessageError': 'fake.v'
		'SelectiveError':         'selective_error.v'
		'SelectiveMessageError':  'selective_message_error.v'
	} {
		assert errors.any(it.contains('${file}:') && it.contains('${typ}`')
			&& it.contains('in return argument')), 'expected rejection of `${typ}` in ${file}\n${compile.output}'
	}
}

// The builtin Error and MessageError types reserve their names in the main module.
fn test_main_cannot_redeclare_builtin_error_types() {
	names := ['Error', 'MessageError']
	mut declarations := []string{cap: names.len}
	mut calls := []string{cap: names.len}
	for name in names {
		fail := 'fail_${name.camel_to_snake()}'
		declarations << "struct ${name} {}

struct Local${name} {
	${name}
}

fn (err Local${name}) msg() string {
	return 'bad'
}

fn (err Local${name}) code() int {
	return 7
}

fn ${fail}() !int {
	return Local${name}{}
}
"
		calls << '_ := ${fail}() or { 0 }'
	}
	source := '${declarations.join('\n')}\nfn main() {\n\t${calls.join('\n\t')}\n}\n'
	output := ierror_payload_rejection('main_local_error_types', source)
	for name in names {
		assert output.contains(': error: cannot register struct `${name}`, another type with this name exists'), output
	}
}
