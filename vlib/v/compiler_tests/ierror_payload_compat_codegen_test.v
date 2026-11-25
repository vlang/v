import os
import v.tempname

fn run_bad_ierror_payload(name string, source string, expected string, detail string) {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_ierror_payload_${name}_out')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert compile.output.contains(detail), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
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

fn test_selective_imported_ierror_payload_is_result_error() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_selective_custom_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'errmod')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'errmod.v'), 'module errmod

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

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import errmod { CustomError }

fn fail() !int {
	return CustomError{
		text: 'imported'
	}
}

fn main() {
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

	bin := os.join_path(root, 'v3_ierror_payload_selective_custom_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_bare_ierror_payload_is_not_call_argument_value() {
	run_bad_ierror_payload('bad_call_arg', "struct CustomError {
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

fn main() {
	takes(CustomError{
		text: 'payload'
	})
}
",
		'cannot use', 'CustomError')
	run_bad_ierror_payload('bad_ierror_call_arg', "fn takes(x int) {
	_ = x
}

fn main() {
	err := error('boom')
	takes(err)
}
",
		'cannot use', 'IError')
}

fn test_bare_ierror_payload_is_not_assigned_to_unwrapped_result_value() {
	run_bad_ierror_payload('bad_assignment', "struct CustomError {
	text string
}

fn (err CustomError) msg() string {
	return err.text
}

fn (err CustomError) code() int {
	return 77
}

fn ok() !int {
	return 1
}

fn main() {
	mut value := ok() or { return }
	value = CustomError{
		text: 'payload'
	}
}
",
		'cannot assign', 'CustomError')
}

fn test_alias_of_non_ierror_payload_is_rejected() {
	run_bad_ierror_payload('bad_alias_payload', "struct Plain {
	text string
}

type AliasPlain = Plain
type AliasChainPlain = AliasPlain

fn fail() !int {
	return AliasChainPlain(AliasPlain(Plain{
		text: 'nope'
	}))
}

fn main() {
	fail() or { return }
}
",
		'in return argument', '`AliasChainPlain`')
}

fn test_result_error_match_rejects_non_ierror_branch() {
	run_bad_ierror_payload('bad_match_branch', "struct CustomError {}

fn (err CustomError) msg() string {
	return 'custom'
}

fn (err CustomError) code() int {
	return 77
}

struct Other {}

fn other() Other {
	return Other{}
}

fn fail_match(x int) !int {
	return match x {
		0 { CustomError{} }
		else { other() }
	}
}

fn main() {
	fail_match(1) or { return }
}
",
		'return type mismatch', '`Other`')
}

fn test_unqualified_builtin_error_in_imported_module_is_result_payload() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_builtin_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'payloadmod')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'payloadmod.v'), "module payloadmod

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
	") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import payloadmod

fn main() {
	payloadmod.builtin_error() or {
		assert err.msg() == ''
		assert err.code() == 0
	}
	payloadmod.embedded_error() or {
		assert err.msg() == 'embedded builtin error'
		assert err.code() == 33
	}
	println('ok')
}
	") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_builtin_error_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_msg_only_struct_is_not_result_payload() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_msg_only_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct NotError {}

fn (err NotError) msg() string {
	return 'msg only'
}

fn fail() !int {
	return NotError{}
}

fn main() {
	_ := fail() or { 0 }
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_msg_only_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('in return argument'), compile.output
	assert compile.output.contains('NotError'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_module_local_error_embeds_are_not_result_payloads() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_fake_embed_${tempname.unique_token()}')
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

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import fake

fn main() {
	a := fake.fail_error() or { 0 }
	b := fake.fail_message_error() or { 0 }
	_ = a + b
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_fake_embed_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('in return argument'), compile.output
	assert compile.output.contains('fake.FalseError')
		|| compile.output.contains('fake.FalseMessageError'), compile.output

	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_selective_imported_error_embed_is_not_result_payload() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_selective_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), 'module fake

pub struct Error {}
	') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake { Error }

struct MyErr {
	Error
}

fn (err MyErr) msg() string {
	return 'bad'
}

fn (err MyErr) code() int {
	return 7
}

fn fail() !int {
	return MyErr{}
}

fn main() {
	_ := fail() or { 0 }
}
	") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_selective_error_input')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('in return argument'), compile.output
	assert compile.output.contains('MyErr'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_selective_imported_message_error_embed_is_not_result_payload() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_selective_message_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), 'module fake

pub struct MessageError {}
	') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake { MessageError }

struct MyErr {
	MessageError
}

fn (err MyErr) msg() string {
	return 'bad'
}

fn (err MyErr) code() int {
	return 7
}

fn fail() !int {
	return MyErr{}
}

fn main() {
	_ := fail() or { 0 }
}
	") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_selective_message_error_input')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('in return argument'), compile.output
	assert compile.output.contains('MyErr'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_normal_imported_error_does_not_shadow_builtin_error_payload() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_normal_import_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), 'module fake

pub struct Error {}
	') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake

fn fail() !int {
	_ := fake.Error{}
	return Error{}
}

fn main() {
	fail() or {
		assert err.msg() == ''
		assert err.code() == 0
	}
	println('ok')
}
	") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_normal_import_error_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// The main module cannot declare its own `Error` any more (see
// test_main_cannot_redeclare_builtin_error_types), so an imported module's
// `Error` embed always names the builtin one.
fn test_imported_builtin_error_embed_is_ierror_value() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_payload_imported_builtin_embed_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod_dir := os.join_path(root, 'payloadmod')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'payloadmod.v'), 'module payloadmod

pub struct EmbeddedBuiltinError {
	Error
}

pub fn payload() !IError {
	return EmbeddedBuiltinError{}
}
	') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import payloadmod

fn main() {
	value := payloadmod.payload() or {
		println('bad error: ' + err.msg())
		return
	}
	assert value.msg() == ''
	assert value.code() == 0
	println('ok')
}
	") or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_ierror_payload_imported_builtin_embed_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// Like V, the main module cannot declare a type named like the builtin `Error`
// or `MessageError`, so a main-local error embed cannot shadow them.
fn test_main_cannot_redeclare_builtin_error_types() {
	for name in ['Error', 'MessageError'] {
		run_bad_ierror_payload('main_local_${name}', "struct ${name} {}

struct MyErr {
	${name}
}

fn (err MyErr) msg() string {
	return 'bad'
}

fn (err MyErr) code() int {
	return 7
}

fn fail() !int {
	return MyErr{}
}

fn main() {
	_ := fail() or { 0 }
}
",
			'cannot register struct `${name}`', 'another type with this name exists')
	}
}
