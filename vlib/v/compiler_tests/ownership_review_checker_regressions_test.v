import os
import json2

const ownership_review_default_flags = os.getenv('VFLAGS').trim_space() in ['', '-gc none']
const ownership_review_compiler = find_ownership_review_compiler()
const ownership_review_local_receiver_error = 'cannot return a reference to local storage `local`'

fn find_ownership_review_compiler() string {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	source := os.join_path(os.dir(@FILE), '../v.v')
	// The unit runner adds -gc none, which each fixture already passes.
	if ownership_review_default_flags && os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(source) {
		return os.real_path(shared)
	}
	return @VEXE
}

fn run_ownership_review_fixture(source string, flags []string) os.Result {
	return os.exec([ownership_review_compiler, '-nocache', '-ownership', '-gc', 'none', '-cc',
		'clang', ...flags, source])
}

struct OwnershipReviewRejection {
	source   string
	expected string
}

struct OwnershipReviewDiagnostic {
	file     string
	severity string
	message  string
}

fn check_ownership_review_rejections(root string, cases []OwnershipReviewRejection, forbidden ...string) ! {
	// Custom flags can change import resolution or limit diagnostics.
	if !ownership_review_default_flags {
		source := os.join_path(root, 'main.v')
		for case in cases {
			os.write_file(source, case.source)!
			out := run_ownership_review_fixture(source, ['-d', 'ownership', '-check'])
			assert out.exit_code != 0, '${case.source}\n${out.output}'
			assert out.output.contains(case.expected), '${case.source}\n${out.output}'
			assert forbidden.all(!out.output.contains(it)), '${case.source}\n${out.output}'
		}
		return
	}
	batch := os.join_path(root, 'rejections')
	os.mkdir(batch)!
	mut imports := []string{cap: cases.len}
	for i, case in cases {
		name := 'review_case_${i}'
		dir := os.join_path(batch, name)
		os.mkdir(dir)!
		os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n' + case.source)!
		imports << 'import ${name} as _'
	}
	os.write_file(os.join_path(batch, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')!
	out := run_ownership_review_fixture(batch, ['-d', 'ownership', '-json-errors', '-check'])
	assert out.exit_code != 0, out.output
	assert forbidden.all(!out.output.contains(it)), out.output
	mut diagnostics := []OwnershipReviewDiagnostic{}
	for line in out.output.split_into_lines() {
		if line.starts_with('{') {
			diagnostics << json2.decode[OwnershipReviewDiagnostic](line)!
		}
	}
	for i, case in cases {
		filename := 'review_case_${i}.v'
		assert diagnostics.any(it.severity == 'error' && os.file_name(it.file) == filename
			&& it.message.contains(case.expected)), '${case.source}\n${out.output}'
	}
}

fn test_ownership_arc_and_chained_calls_preserve_owned_values() {
	root := os.join_path(os.vtmp_dir(), 'ownership_arc_and_chained_calls_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import sync.arc

struct Value implements IClone {
	text string
}

struct Pair implements IClone {
	left Value
	right Value
}

struct PairHolder implements IClone {
	inner arc.Arc[Pair]
}

fn hold(left Value, right Value) PairHolder {
	return PairHolder{
		inner: arc.new(Pair{left: left, right: right})
	}
}

fn check_arc_literal() {
	holder := hold(Value{"left".to_owned()}, Value{"right".to_owned()})
	assert holder.inner.get().left.text == "left"
	assert holder.inner.get().right.text == "right"
}

struct Holder {
mut:
	inner arc.Arc[Value]
}

fn (mut holder Holder) replace(value Value) &Holder {
	holder.inner = arc.new(value)
	return holder
}

fn check_arc_receiver() {
	mut holder := Holder{arc.new(Value{"old".to_owned()})}
	updated := holder.replace(Value{"new".to_owned()})
	assert updated.inner.get().text == "new"
}

fn normalize(value string) string {
	return value.trim_space()
}

fn split(value string) []string {
	return normalize(value).split("/")
}

fn check_chained_call() {
	assert split(" a/b ".to_owned()) == ["a", "b"]
}

fn main() {
	check_arc_literal()
	check_arc_receiver()
	check_chained_call()
}
')!
	for mode in [['-no-parallel'], []string{}] {
		result := run_ownership_review_fixture(source, [...mode, 'run'])
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
}

fn test_ownership_local_receiver_cannot_escape_in_returns_or_aliases() {
	root := os.join_path(os.vtmp_dir(), 'ownership_receiver_escape_review_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnershipReviewRejection{}
	for return_type, expression in {
		'?&Builder':     'local.set(42)'
		'!&Builder':     'local.set(42)'
		'Holder':        'Holder{target: local.set(42)}'
		'?Holder':       'Holder{target: local.set(42)}'
		'!Holder':       'Holder{target: local.set(42)}'
		'[]&Builder':    '[local.set(42)]'
		'[]Holder':      '[Holder{target: local.set(42)}]'
		'(int, Holder)': '0, Holder{target: local.set(42)}'
	} {
		fixture := 'struct Builder implements Drop {
mut:
	value int
}
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
fn (mut builder Builder) drop() {}
struct Holder { target &Builder }
fn escaped() ${return_type} {
	mut local := Builder{}
	return ${expression}
}
fn main() {}
'
		cases << OwnershipReviewRejection{fixture, ownership_review_local_receiver_error}
	}
	cases << ownership_review_aggregate_alias_rejections()
	check_ownership_review_rejections(root, cases)!
}

fn ownership_review_wrapped_receiver_and_pointer_checks() string {
	return 'struct Builder { mut: value int }
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
struct Holder { target &Builder }
fn optional(mut builder Builder) ?&Builder { return builder.set(42) }
fn result(mut builder Builder) !&Builder { return builder.set(42) }
fn aggregate(mut builder Builder) Holder { return Holder{target: builder.set(42)} }
fn aggregate_forwarded(mut builder Builder) Holder { return aggregate(mut builder) }
fn tuple_forwarded(mut builder Builder) (int, Holder) {
	return 0, Holder{target: builder.set(42)}
}
fn heap() ?Holder {
	mut builder := &Builder{}
	return Holder{target: builder.set(42)}
}
fn named_caller(mut builder Builder) Holder {
	held := Holder{target: builder.set(42)}
	return held
}
fn named_heap() Holder {
	mut builder := &Builder{}
	held := Holder{target: builder.set(42)}
	return held
}
fn rewrap(held Holder) Holder { return Holder{target: held.target} }
fn forwarded_caller(mut builder Builder) Holder {
	held := Holder{target: builder.set(42)}
	return rewrap(held)
}
fn owned_value() Builder {
	return Builder{value: 42}
}
fn identity(value Builder) Builder { return value }
fn forwarded_owned_value() Builder {
	local := Builder{value: 42}
	return identity(local)
}
fn check_wrapped_receivers() {
	mut builder := Builder{}
	assert (optional(mut builder) or { panic("none") }).value == 42
	assert (result(mut builder) or { panic(err) }).value == 42
	assert aggregate(mut builder).target.value == 42
	assert aggregate_forwarded(mut builder).target.value == 42
	_, held := tuple_forwarded(mut builder)
	assert held.target.value == 42
	assert (heap() or { panic("none") }).target.value == 42
	assert named_caller(mut builder).target.value == 42
	assert named_heap().target.value == 42
	assert forwarded_caller(mut builder).target.value == 42
	assert owned_value().value == 42
	assert forwarded_owned_value().value == 42
}

fn check_pointer_identity() {
	owner := "original".to_owned()
	view := *pointer(&owner)
	assert view == owner
	assert unsafe { view.str == owner.str }
}
'
}

fn test_ownership_string_pointer_loans_and_owned_method_returns() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_loans_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut cases := []OwnershipReviewRejection{}
	for view in ['*pointer(&owner)', '*forward(pointer(&owner))',
		'*(if owner.len > 0 { pointer(&owner) } else { pointer(&owner) })',
		'*(maybe_pointer(&owner) or { &owner })', '*(result_pointer(&owner) or { &owner })',
		'*unwrap(wrap(&owner))'] {
		for action, expected in {
			'owner = "replacement".to_owned()': 'cannot assign to `owner` because it is borrowed'
			'consume(owner)':                   'cannot move `owner` because it is borrowed'
		} {
			fixture := 'fn pointer(value &string) &string { return value }
fn forward(value &string) &string { return pointer(value) }
fn maybe_pointer(value &string) ?&string { return value }
fn result_pointer(value &string) !&string { return value }
struct Box { value &string }
fn wrap(value &string) Box { return Box{value: value} }
fn unwrap(box Box) &string { return box.value }
fn consume(value string) { _ = value }
fn main() {
	mut owner := "original".to_owned()
	view := ${view}
	${action}
	println(view)
}
'
			cases << OwnershipReviewRejection{fixture, expected}
		}
	}
	for method in ['substr_unsafe', 'owned_text'] {
		fixture := 'struct Source {}
fn (source Source) ${method}() string { return "independent".to_owned() }
fn consume(value string) { _ = value }
fn main() {
	source := Source{}
	value := source.${method}()
	consume(value)
	println(value)
}
'
		cases << OwnershipReviewRejection{fixture, 'use of moved value: `value`'}
	}
	check_ownership_review_rejections(root, cases)!
}

fn ownership_review_aggregate_alias_rejections() []OwnershipReviewRejection {
	mut cases := []OwnershipReviewRejection{}
	for name, body in {
		'named':       'held := Holder{target: local.set(42)}; return held'
		'forwarded':   'return wrap(mut local)'
		'tuple_call':  'return tuple_wrap(mut local)'
		'nested':      'held := Holder{target: local.set(42)}; return nest(held)'
		'result':      'return local.reference()!'
		'optional':    'return local.optional() or { return none }'
		'conditional': 'return if local.value == 0 { Holder{target: local.set(42)} } else { Holder{target: local.set(41)} }'
	} {
		return_type := match name {
			'tuple_call' { '(int, Holder)' }
			'nested' { 'Outer' }
			'result' { '!&Builder' }
			'optional' { '?&Builder' }
			else { 'Holder' }
		}
		fixture := 'struct Builder implements Drop { mut: value int }
fn (mut builder Builder) set(value int) &Builder {
	builder.value = value
	return builder
}
fn (mut builder Builder) drop() {}
struct Holder { target &Builder }
struct Outer { held Holder }
fn nest(held Holder) Outer { return Outer{held: held} }
fn (mut builder Builder) reference() !&Builder { return builder }
fn (mut builder Builder) optional() ?&Builder { return builder }
fn wrap(mut target Builder) Holder { return Holder{target: target.set(42)} }
fn tuple_wrap(mut target Builder) (int, Holder) { return 0, Holder{target: target.set(42)} }
fn escaped() ${return_type} {
	mut local := Builder{}
	${body}
}
fn main() {}
'
		cases << OwnershipReviewRejection{fixture, ownership_review_local_receiver_error}
	}
	return cases
}

fn test_ownership_runtime_storage_and_loans() {
	root := os.join_path(os.vtmp_dir(), 'ownership_runtime_storage_and_loans_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	prelude := 'fn consume(value string) { _ = value }
fn pointer(value &string) &string { return value }
'
	mut accepted := prelude
	mut calls := []string{}
	mut rejections := []OwnershipReviewRejection{}
	for creation, text in {
		'*pointer(&original)':                     'fallback'
		'unsafe { original.substr_unsafe(1, 4) }': 'all'
	} {
		body := '	mut original := "fallback".to_owned()
	mut view := ${creation}
	result := "abc".substr_or(0, 4, view)
	assert view == "${text}"
	view = ""
	original = "replacement".to_owned()'
		accepted += '\nfn check_${text}_copy() {\n${body}
	assert result == "${text}"
}
'
		calls << '\tcheck_${text}_copy()'
		rejections << OwnershipReviewRejection{prelude + 'fn main() {\n${body}
	consume(result); println(result)
}
', 'use of moved value: `result`'}
	}
	os.write_file(source, accepted + '\nfn main() {\n${calls.join('\n')}\n}\n')!
	checked := run_ownership_review_fixture(source, ['-d', 'ownership', '-check'])
	assert checked.exit_code == 0, checked.output

	calls << ['\tcheck_wrapped_receivers()', '\tcheck_pointer_identity()']
	os.write_file(source, accepted + '\n' + ownership_review_wrapped_receiver_and_pointer_checks() +
		'\nfn main() {\n${calls.join('\n')}\n}\n')!
	out := run_ownership_review_fixture(source, ['-d', 'ownership', 'run'])
	assert out.exit_code == 0, out.output
	check_ownership_review_rejections(root, rejections, 'cannot assign to `original`')!
}
