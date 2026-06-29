import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_ierror_concrete_match_uses_type_ids() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_${os.getpid()}')
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
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('._typ == '), c_code
	assert c_code.contains('IError__msg(&err)'), c_code
	assert !c_code.contains('err == myerrs__FirstError'), c_code
	assert !c_code.contains('err == myerrs__SecondError'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_ierror_patterns_resolve_nested_alias_and_selective_imports() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_alias_match_codegen_${os.getpid()}')
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

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import nested.aliaserrs as aliased
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

fn main() {
	aliased.fail_alias() or {
		assert err.msg() == 'alias nested'
		assert classify_alias(err) == 'alias'
		assert classify_selective(err) == 'other'
		assert err is aliased.AliasError
	}

	fail_selective() or {
		assert err.msg() == 'selective nested'
		assert classify_alias(err) == 'other'
		assert classify_selective(err) == 'selective'
		assert err is SelectiveError
	}
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_alias_match_codegen_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_selective_imported_error_pattern_prefers_scoped_error() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_selective_error_pattern_${os.getpid()}')
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), "module fake

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

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake { Error, fail }

fn matches_fake(err IError) bool {
	if err is Error {
		assert err.label == 'selective'
		return true
	}
	return false
}

fn classify(err IError) string {
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

fn main() {
	fail() or {
		assert matches_fake(err)
		assert classify(err) == 'selective'
		println('ok')
		return
	}
	assert false
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_selective_error_pattern_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_scoped_error_pattern_does_not_fallback_to_builtin_error() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_scoped_error_no_fallback_${os.getpid()}')
	mod_dir := os.join_path(root, 'fake')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'fake.v'), 'module fake

pub struct Error {}

pub fn fail() !int {
	return Error{}
}
') or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import fake { Error, fail }

fn is_fake(err IError) bool {
	return err is Error
}

fn main() {
	fail() or {
		_ := is_fake(err)
		return
	}
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_scoped_error_no_fallback_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('fake.Error') || compile.output.contains('Error'), compile.output
	assert compile.output.contains('IError') || compile.output.contains('cannot return'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_local_error_pattern_does_not_fallback_to_builtin_error() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_local_error_no_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
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

	bin := os.join_path(os.temp_dir(), 'v3_ierror_local_error_no_fallback_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('Error') && compile.output.contains('IError'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_msg_only_struct_is_not_ierror_pattern() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_not_error_${os.getpid()}')
	mod_dir := os.join_path(root, 'noterrs')
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'noterrs.v'), "module noterrs

pub struct NotError {}

pub fn (err NotError) msg() string {
	return 'not actually an error'
}
") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import noterrs

fn classify(err IError) string {
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

fn main() {
	err := error('plain')
	assert classify(err) == 'unknown'
	assert !is_not_error(err)
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_not_error_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('noterrs.NotError') && compile.output.contains('IError'), compile.output

	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_module_local_error_embeds_are_not_ierror_patterns() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_fake_error_pattern_${os.getpid()}')
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
") or {
		panic(err)
	}

	src := os.join_path(root, 'main.v')
	os.write_file(src, "import fake

fn classify(err IError) string {
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

fn main() {
	err := error('plain')
	assert classify(err) == 'unknown'
	assert !is_false_error(err)
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_fake_error_pattern_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('fake.FalseError') && compile.output.contains('IError'), compile.output

	assert compile.output.contains('fake.FalseMessageError') && compile.output.contains('IError'), compile.output

	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_module_local_error_embeds_are_not_boxed_as_ierror() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_match_codegen_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	root := os.join_path(os.temp_dir(), 'v3_ierror_fake_error_box_${os.getpid()}')
	mod_dir := os.join_path(root, 'fakebox')
	os.mkdir_all(mod_dir) or { panic(err) }
	mod_src := os.join_path(mod_dir, 'fakebox.v')
	os.write_file(mod_src, "module fakebox

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

	src := os.join_path(root, 'main.v')
	os.write_file(src, 'import fakebox

fn main() {
	a := fakebox.fail_error() or { 0 }
	b := fakebox.fail_message_error() or { 0 }
	_ = a + b
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_fake_error_box_input')
	compile := os.execute('${v3_bin} ${mod_src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot return'), compile.output
	assert compile.output.contains('fakebox.FalseError')
		|| compile.output.contains('fakebox.FalseMessageError'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}
