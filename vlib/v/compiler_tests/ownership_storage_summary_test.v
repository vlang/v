import os

fn test_ownership_return_does_not_recheck_earlier_chained_receiver_after_move() {
	root := os.join_path(os.vtmp_dir(), 'ownership_chained_receiver_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, 'fn parse_array_key(key string) (string, int) {
	mut index := -1
	mut k := key
	if k.contains("[") {
		index = k.all_after("[").all_before("]").int()
		if k.starts_with("[") { k = "" } else { k = k.all_before("[") }
	}
	return k, index
}
fn main() {
	key, index := parse_array_key("entry[0]")
	assert key == "entry" && index == 0
}
')!
	for mode in ['-no-parallel', ''] {
		result := run_owned_storage_summary(source_path, mode, '-check')
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn test_ownership_value_returns_survive_repeated_storage_queries() {
	root := os.join_path(os.vtmp_dir(), 'ownership_value_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	mut source := 'struct Holder { mut: values map[string]string }\n'
	source += 'fn store0(mut holder Holder, value string) { holder.values["key"] = value }\n'
	for i in 1 .. 19 {
		source += 'fn store${i}(mut holder Holder, value string) { store${i - 1}(mut holder, value); store${i - 1}(mut holder, value) }\n'
	}
	source += 'fn value() string {
	mut holder := Holder{}
	store18(mut holder, "value")
	if text := holder.values["key"] { return text }
	return ""
}
fn main() { assert value() == "value" }
'
	os.write_file(source_path, source)!
	result := run_owned_storage_summary(source_path, '', '-check')
	assert result.exit_code == 0, result.output
}

fn test_ownership_copied_fields_do_not_escape_local_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_copied_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, 'struct Value {
	number int
	text string
	values [2]int
}
struct Container {
	number int
	text string
	value Value
	values [2]int
}
fn number() int {
	local := Container{number: 42}
	return local.number
}
fn text() string {
	local := Container{text: "copied".to_owned()}
	return local.text
}
fn value() Value {
	local := Container{value: Value{number: 43, text: "nested".to_owned(), values: [4, 5]!}}
	return local.value
}
fn fixed() [2]int {
	local := Container{values: [6, 7]!}
	return local.values
}
fn optional() ?int {
	local := Container{number: 44}
	return local.number
}
fn result() !int {
	local := Container{number: 45}
	return local.number
}
fn main() {
	assert number() == 42
	assert text() == "copied"
	returned := value()
	assert returned.number == 43
	assert returned.text == "nested"
	assert returned.values == [4, 5]!
	assert fixed() == [6, 7]!
	assert optional() or { panic("none") } == 44
	assert result() or { panic(err) } == 45
}
')!
	for mode in ['-no-parallel', ''] {
		result := run_owned_storage_summary(source_path, mode, 'run')
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn test_ownership_returned_fields_still_reject_local_references() {
	root := os.join_path(os.vtmp_dir(), 'ownership_reference_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	for return_type, expression in {
		'Holder':   'ptr.holder'
		'&Builder': 'ptr.holder.target'
	} {
		os.write_file(source_path, 'struct Builder { mut: number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Holder { target &Builder }
struct Container { holder Holder }
fn escaped() ${return_type} {
	mut local := Builder{}
	container := Container{holder: Holder{target: local.reference()}}
	ptr := &container
	return ${expression}
}
fn main() {}
')!
		for mode in ['-no-parallel', ''] {
			// Returned references would dangle, so only check these fixtures.
			result := run_owned_storage_summary(source_path, mode, '-check')
			assert result.exit_code != 0, '${return_type}: ${mode}: ${result.output}'
			assert result.output.contains('cannot return a reference to local storage')
				|| result.output.contains('cannot move `ptr.holder.target` because it borrows'), '${return_type}: ${mode}: ${result.output}'
		}
	}
}

fn test_ownership_result_errors_still_reject_local_references() {
	root := os.join_path(os.vtmp_dir(), 'ownership_result_reference_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	for return_type in ['!int', '!string', '!'] {
		os.write_file(source_path, 'struct Builder { mut: number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Fault { target &Builder }
fn (fault Fault) msg() string { return "fault" }
fn (fault Fault) code() int { return fault.target.number }
fn boxed(mut builder Builder) ${return_type} {
	return Fault{target: builder.reference()}
}
fn escaped() ${return_type} {
	mut local := Builder{}
	return boxed(mut local)
}
fn main() {}
')!
		for mode in ['-no-parallel', ''] {
			// A Result can hold a borrowed error payload even when its success value is a scalar.
			result := run_owned_storage_summary(source_path, mode, '-check')
			assert result.exit_code != 0, '${return_type}: ${mode}: ${result.output}'
			assert result.output.contains('cannot return a reference to local storage `local`'), '${return_type}: ${mode}: ${result.output}'
		}
	}
}

fn test_ownership_storage_queries_preserve_overwrites() {
	root := os.join_path(os.vtmp_dir(), 'ownership_storage_proof_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	common := 'struct Builder { mut: number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Holder { mut: target &Builder }
fn store_number(mut holder Holder, number int) { holder.target = &Builder{number: number} }
fn no_op(mut holder Holder, number int) { _ = holder; _ = number }
fn store_borrowed(mut holder Holder, builder &Builder) { holder.target = builder }
'
	for setter, accepted in {
		'store_number(mut holder, 42)':                  true
		'no_op(mut holder, 42)':                         false
		'store_borrowed(mut holder, local.reference())': false
	} {
		initial := if accepted || setter.starts_with('no_op') {
			'Holder{target: local.reference()}'
		} else {
			'Holder{target: &Builder{number: 0}}'
		}
		os.write_file(source_path, common + 'fn escaped() Holder {
	mut local := Builder{}
	mut holder := ${initial}
	${setter}
	return holder
}
fn main() { assert escaped().target.number == 42 }
')!
		for mode in ['-no-parallel', ''] {
			command := if accepted { 'run' } else { '-check' }
			result := run_owned_storage_summary(source_path, mode, command)
			if accepted {
				assert result.exit_code == 0, '${setter}: ${mode}: ${result.output}'
			} else {
				assert result.exit_code != 0, '${setter}: ${mode}: ${result.output}'
				assert result.output.contains('cannot return a reference to local storage `local`'), '${setter}: ${mode}: ${result.output}'
			}
		}
	}
}

fn test_ownership_storage_queries_handle_recursive_setters() {
	root := os.join_path(os.vtmp_dir(), 'ownership_storage_cycle_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	mut source := 'struct Builder { number int }
struct Holder { mut: target &Builder }
'
	for i in 0 .. 18 {
		next := (i + 1) % 18
		source += 'fn store${i}(mut holder Holder, number int) {
	if number > 0 { store${next}(mut holder, number - 1); store${next}(mut holder, number - 1) }
	holder.target = &Builder{number: number}
}
'
	}
	source += 'fn escaped() Holder {
	mut holder := Holder{target: &Builder{number: 0}}
	store0(mut holder, 0)
	return holder
}
fn main() {}
'
	os.write_file(source_path, source)!
	result := run_owned_storage_summary(source_path, '', '-check')
	assert result.exit_code == 0, result.output
}

fn test_ownership_pointer_slot_arguments_still_reject_local_references() {
	root := os.join_path(os.vtmp_dir(), 'ownership_pointer_slot_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, 'struct Builder { number int }
struct Holder { mut: target &&Builder = unsafe { nil } }
fn store_borrowed(mut holder Holder, builder &&Builder) { holder.target = builder }
fn escaped() Holder {
	local := Builder{}
	ptr := &local
	mut holder := Holder{}
	store_borrowed(mut holder, ptr)
	return holder
}
fn main() {}
')!
	for mode in ['-no-parallel', ''] {
		result := run_owned_storage_summary(source_path, mode, '-check')
		assert result.exit_code != 0, '${mode}: ${result.output}'
		assert result.output.contains('cannot return a reference to local storage `local`'), '${mode}: ${result.output}'
	}
}

fn run_owned_storage_summary(source string, mode string, command string) os.Result {
	mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-ownership', '-d',
		'ownership']
	if mode != '' { args << mode }
	args << [command, source]
	return os.exec(args)
}
