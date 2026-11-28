import os
import json2

const local_storage_return_error = 'cannot return a reference to local storage `local`'

struct OwnedStorageRejection {
	fixture  string
	expected []string
}

struct OwnedStorageDiagnostic {
	file     string
	severity string
	message  string
}

fn owned_storage_compile_check_fixtures() (string, string) {
	chained_receiver := 'fn parse_array_key(key string) (string, int) {
	mut index := -1
	mut k := key
	if k.contains("[") {
		index = k.all_after("[").all_before("]").int()
		if k.starts_with("[") { k = "" } else { k = k.all_before("[") }
	}
	return k, index
}
fn check_chained_receiver() {
	key, index := parse_array_key("entry[0]")
	assert key == "entry" && index == 0
}
'
	mut recursive_setters := '
struct Builder { number int }
struct Holder { mut: target &Builder }
'
	for i in 0 .. 18 {
		next := (i + 1) % 18
		recursive_setters += 'fn store${i}(mut holder Holder, number int) {
	if number > 0 { store${next}(mut holder, number - 1); store${next}(mut holder, number - 1) }
	holder.target = &Builder{number: number}
}
'
	}
	recursive_setters += 'fn escaped() Holder {
	mut holder := Holder{target: &Builder{number: 0}}
	store0(mut holder, 0)
	return holder
}
'
	return chained_receiver, recursive_setters
}

fn test_ownership_storage_preserves_map_returns_overwrites_and_projections() {
	root := os.join_path(os.vtmp_dir(), 'ownership_map_returns_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut stores := 'struct Holder { mut: values map[string]string }\n'
	stores += 'fn store0(mut holder Holder, value string) { holder.values["key"] = value }\n'
	for i in 1 .. 19 {
		stores += 'fn store${i}(mut holder Holder, value string) { store${i - 1}(mut holder, value); store${i - 1}(mut holder, value) }\n'
	}
	local_return := stores + 'fn value(flag bool) string {
	mut holder := Holder{}
	if flag {
		input := "value"
		store18(mut holder, input)
	}
	if text := holder.values["key"] { return text }
	return ""
}
pub fn check() { assert value(true) == "value"; assert value(false) == "" }
'
	mutable_return := stores + 'fn value(mut holder Holder, number int) string {
	store18(mut holder, "\${number}")
	if text := holder.values["key"] { return text }
	return ""
}
pub fn check() {
	mut holder := Holder{}
	assert value(mut holder, 42) == "42"
}
'
	chained_return := 'struct Holder { mut: values map[string]string }
struct Item { text string }
fn make_item(text string) Item { return Item{text: text} }
fn (item Item) view() string { return item.text }
fn no_op(mut holder Holder, text string) { _ = holder; _ = text }
fn value(mut holder Holder) string {
	text := "consumed".to_owned()
	no_op(mut holder, make_item(text.clone()).view())
	holder.values["key"] = "value"
	if result := holder.values["key"] { return result }
	return ""
}
pub fn check() {
	mut holder := Holder{}
	assert value(mut holder) == "value"
}
'
	owned_source := owned_storage_map_source_fixture(false) +
		'pub fn check() { assert escaped().value["key"] == "value" }\n'
	overwritten := owned_storage_overwrite_fixture('Holder{target: local.reference()}',
		'store_number(mut holder, 42)', 'pub fn check()')
	run_owned_storage_checks(root, {
		'field_projections': owned_storage_field_projections_fixture()
		'local_return':      local_return
		'mutable_return':    mutable_return
		'chained_return':    chained_return
		'owned_source':      owned_source
		'overwritten':       overwritten
	})!
}

fn run_owned_storage_checks(root string, fixtures map[string]string) ! {
	mut imports := []string{cap: fixtures.len}
	mut calls := []string{cap: fixtures.len}
	for name, fixture in fixtures {
		dir := os.join_path(root, name)
		os.mkdir(dir)!
		os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n${fixture}')!
		imports << 'import ${name}'
		calls << '\t${name}.check()'
	}
	chained_receiver, recursive_setters := owned_storage_compile_check_fixtures()
	source := 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n\n' +
		chained_receiver
	main := os.join_path(root, 'main.v')
	for mode in ['-no-parallel', ''] {
		// The recursive setter cycle exercises the default checker.
		os.write_file(main, if mode == '' { source + recursive_setters } else { source })!
		result := run_owned_storage_summary(root, mode, 'run')
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn test_ownership_storage_rejects_escaping_local_references() {
	root := os.join_path(os.vtmp_dir(), 'ownership_storage_rejections_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnedStorageRejection{}
	for borrowed in ['local.reference()', 'ptr'] {
		pointer_type := if borrowed == 'ptr' { '&&Builder' } else { '&Builder' }
		pointer_binding := if borrowed == 'ptr' { 'ptr := &local' } else { '' }
		cases << OwnedStorageRejection{'struct Builder { number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Holder { mut: target ${pointer_type} = unsafe { nil } }
fn store(mut holder Holder, builder ${pointer_type}) { holder.target = builder }
fn no_op(mut holder Holder, number int) { _ = holder; _ = number }
fn escaped(mut holder Holder) Holder {
	mut local := Builder{number: 42}
	${pointer_binding}
	store(mut holder, ${borrowed})
	no_op(mut holder, 0)
	return holder
}
fn main() {
	mut holder := Holder{}
	_ = escaped(mut holder)
}
', [local_storage_return_error]}
	}

	field_errors := [
		'cannot return a reference to local storage',
		'cannot move `ptr.holder.target` because it borrows',
	]
	for return_type, expression in {
		'Holder':   'ptr.holder'
		'&Builder': 'ptr.holder.target'
	} {
		cases << OwnedStorageRejection{'struct Builder { mut: number int }
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
', field_errors}
	}

	// A Result can hold a borrowed error payload even when its success value is a scalar.
	for return_type in ['!int', '!string', '!'] {
		cases << OwnedStorageRejection{'struct Builder { mut: number int }
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
', [local_storage_return_error]}
	}

	for binding in [
		'mut holder := Holder{target: &local}',
		'original := Holder{target: local.reference()}; mut holder := original',
		'mut holder := Holder{target: &Builder{}}; holder = Holder{target: local.reference()}',
	] {
		cases << OwnedStorageRejection{'struct Builder { mut: number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Holder { mut: target &Builder }
fn no_op(mut holder Holder, number int) { _ = holder; _ = number }
fn escaped() Holder {
	mut local := Builder{}
	${binding}
	no_op(mut holder, 0)
	return holder
}
fn main() { _ = escaped() }
', [local_storage_return_error]}
	}

	cases << OwnedStorageRejection{'struct Builder { number int }
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
', [local_storage_return_error]}
	cases << OwnedStorageRejection{owned_storage_map_source_fixture(true) + 'fn main() { _ = escaped() }\n', [
		'cannot return a reference to local storage `source`',
	]}
	cases << OwnedStorageRejection{owned_storage_overwrite_fixture('Holder{target: local.reference()}',
		'no_op(mut holder, 42)', 'fn main()'), [local_storage_return_error]}
	cases << OwnedStorageRejection{owned_storage_overwrite_fixture('Holder{target: &Builder{number: 0}}',
		'store_borrowed(mut holder, local.reference())', 'fn main()'), [local_storage_return_error]}
	check_owned_storage_rejections(root, cases)!
}

fn owned_storage_map_source_fixture(borrowed_header bool) string {
	field_type := if borrowed_header {
		'&map[string]string = unsafe { nil }'
	} else {
		'map[string]string'
	}
	params := if borrowed_header { 'number int' } else { 'mut source map[string]string' }
	store_body := if borrowed_header { '_ = holder; _ = number' } else { 'holder.value = source' }
	initial := if borrowed_header { 'Holder{value: &source}' } else { 'Holder{}' }
	argument := if borrowed_header { '0' } else { 'mut source' }
	return 'struct Holder { mut: value ${field_type} }
fn store(mut holder Holder, ${params}) { ${store_body} }
fn escaped() Holder {
	mut source := {"key": "value"}
	mut holder := ${initial}
	store(mut holder, ${argument})
	return holder
}
'
}

fn owned_storage_field_projections_fixture() string {
	return 'struct Value {
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
	ptr := &local
	return ptr.number
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
	ptr := &local
	return ptr.number
}
fn result() !int {
	local := Container{number: 45}
	ptr := &local
	return ptr.number
}
pub fn check() {
	assert number() == 42
	assert text() == "copied"
	returned := value()
	assert returned.number == 43
	assert returned.text == "nested"
	assert returned.values == [4, 5]!
	assert fixed() == [6, 7]!
	assert optional() or { panic("none") } == 44
	assert result() or { panic(err) } == 45
	check_scalar_projections()
}

type Number = int
enum State { ready }
struct Scalars {
	flag bool
	fraction f64
	letter rune
	signed int
	unsigned uint
	number Number
	state State
	values [2]int
}
fn flag() bool {
	local := Scalars{flag: true}
	ptr := &local
	return ptr.flag
}
fn fraction() f64 {
	local := Scalars{fraction: 1.5}
	ptr := &local
	return ptr.fraction
}
fn letter() rune {
	local := Scalars{letter: rune(65)}
	ptr := &local
	return ptr.letter
}
fn signed() int {
	local := Scalars{signed: -2}
	ptr := &local
	return ptr.signed
}
fn unsigned() uint {
	local := Scalars{unsigned: 3}
	ptr := &local
	return ptr.unsigned
}
fn aliased_number() Number {
	local := Scalars{number: Number(4)}
	ptr := &local
	return ptr.number
}
fn state() State {
	local := Scalars{state: .ready}
	ptr := &local
	return ptr.state
}
fn indexed() int {
	local := Scalars{values: [5, 6]!}
	ptr := &local
	return ptr.values[1]
}
fn check_scalar_projections() {
	assert flag()
	assert fraction() == 1.5
	assert letter() == rune(65)
	assert signed() == -2
	assert unsigned() == 3
	assert aliased_number() == Number(4)
	assert state() == .ready
	assert indexed() == 6
}
'
}

fn owned_storage_overwrite_fixture(initial string, setter string, entry_point string) string {
	return 'struct Builder { mut: number int }
fn (mut builder Builder) reference() &Builder { return builder }
struct Holder { mut: target &Builder }
fn store_number(mut holder Holder, number int) { holder.target = &Builder{number: number} }
fn no_op(mut holder Holder, number int) { _ = holder; _ = number }
fn store_borrowed(mut holder Holder, builder &Builder) { holder.target = builder }
fn escaped() Holder {
	mut local := Builder{}
	mut holder := ${initial}
	${setter}
	return holder
}
${entry_point} { assert escaped().target.number == 42 }
'
}

fn check_owned_storage_rejections(root string, cases []OwnedStorageRejection) ! {
	// Custom flags can change import resolution or limit diagnostics.
	if os.getenv('VFLAGS').trim_space() !in ['', '-gc none'] {
		source := os.join_path(root, 'main.v')
		for case in cases {
			os.write_file(source, case.fixture)!
			for mode in ['-no-parallel', ''] {
				result := run_owned_storage_summary(source, mode, '-check')
				assert result.exit_code != 0, '${mode}: ${case.fixture}\n${result.output}'
				assert case.expected.any(result.output.contains(it)), '${mode}: ${case.fixture}\n${result.output}'
			}
		}
		return
	}
	batch := os.join_path(root, 'rejections')
	os.mkdir(batch)!
	mut imports := []string{cap: cases.len}
	for i, case in cases {
		name := 'storage_case_${i}'
		dir := os.join_path(batch, name)
		os.mkdir(dir)!
		os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n' + case.fixture)!
		imports << 'import ${name} as _'
	}
	os.write_file(os.join_path(batch, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')!
	for mode in ['-no-parallel', ''] {
		result := run_owned_storage_summary(batch, mode, '-check', '-json-errors')
		assert result.exit_code != 0, '${mode}: ${result.output}'
		mut errors := map[string]string{}
		for line in result.output.split_into_lines() {
			if !line.starts_with('{') {
				continue
			}
			diagnostic := json2.decode[OwnedStorageDiagnostic](line)!
			if diagnostic.severity == 'error' {
				filename := os.file_name(diagnostic.file)
				errors[filename] += diagnostic.message + '\n'
			}
		}
		for i, case in cases {
			messages := errors['storage_case_${i}.v']
			assert case.expected.any(messages.contains(it)), '${mode}: ${case.fixture}\n${result.output}'
		}
	}
}

fn run_owned_storage_summary(source string, mode string, command string, flags ...string) os.Result {
	mut args := [@VEXE, '-no-retry-compilation', '-nocache', '-ownership', '-d', 'ownership']
	if mode != '' { args << mode }
	args << [...flags, command, source]
	return os.exec(args)
}
