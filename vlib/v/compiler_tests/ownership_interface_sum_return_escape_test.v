import arrays
import os

// Each fixture already passes the unit runner's -gc none.
const interface_sum_escape_default_vflags = os.getenv('VFLAGS').trim_space() in ['', '-gc none']
const interface_sum_escape_compiler = find_interface_sum_escape_compiler()

fn find_interface_sum_escape_compiler() string {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	source := os.join_path(os.dir(@FILE), '../v.v')
	if interface_sum_escape_default_vflags && os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(source) {
		return os.real_path(shared)
	}
	return @VEXE
}

fn run_interface_sum_escape_fixture(source string, flags []string) os.Result {
	return os.exec([interface_sum_escape_compiler, '-nocache', '-ownership', '-d', 'ownership',
		'-gc', 'none', '-cc', 'clang', ...flags, source])
}

struct InterfaceSumEscapeCase {
	typ        string
	expression string
	binding    string
}

const interface_sum_escape_declarations = '
struct Builder { mut: value int }
fn (mut builder Builder) set(value int) &Builder { builder.value = value; return builder }
struct Holder { target &Builder }
fn (holder Holder) read() int { return holder.target.value }
struct Twin { target &Builder }
struct Other { number int }
struct ValueTarget { target int }
interface Target { target &Builder }
interface InheritedTarget { Target }
interface Opaque { read() int }
interface Sized { size() int }
struct StringBox { text string }
fn (box StringBox) size() int { return box.text.len }
struct NumberBox { number int }
fn (box NumberBox) size() int { return box.number }
struct Link { nested Opaque }
fn (link Link) read() int { return link.nested.read() }
type AliasedTarget = Target
type Shared = Holder | Twin
type Unique = Holder | Other
type Mixed = Holder | ValueTarget
type Nested = Unique | int
fn wrap_target(mut builder Builder) Target { return Holder{target: builder.set(42)} }
fn wrap_inherited(mut builder Builder) InheritedTarget { return Holder{target: builder.set(42)} }
fn wrap_opaque(mut builder Builder) Opaque { return Holder{target: builder.set(42)} }
fn wrap_shared(mut builder Builder) Shared { return Holder{target: builder.set(42)} }
fn wrap_unique(mut builder Builder) Unique { return Holder{target: builder.set(42)} }
fn wrap_mixed(mut builder Builder) Mixed { return Holder{target: builder.set(42)} }
fn wrap_nested(mut builder Builder) Nested { return Holder{target: builder.set(42)} }
fn forward_target(mut builder Builder) Target { return wrap_target(mut builder) }
fn forward_unique(mut builder Builder) Unique { return wrap_unique(mut builder) }
fn identity_target(value Target) Target { return value }
fn identity_opaque(value Opaque) Opaque { return value }
fn identity_unique(value Unique) Unique { return value }
fn boxed(text string) Sized { return StringBox{text: text} }
fn boxed_number(number int) Sized { return NumberBox{number: number} }
fn recursive_wrap(mut builder Builder, depth int) Opaque {
	if depth == 0 { return Holder{target: builder.set(42)} }
	return Link{nested: recursive_wrap(mut builder, depth - 1)}
}
'

fn test_ownership_interface_and_sum_payloads_cannot_return_local_references() {
	root := os.join_path(os.vtmp_dir(), 'ownership_interface_sum_escape_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cases := [
		InterfaceSumEscapeCase{'Target', 'wrap_target(mut local)', ''},
		InterfaceSumEscapeCase{'InheritedTarget', 'wrap_inherited(mut local)', ''},
		InterfaceSumEscapeCase{'Opaque', 'wrap_opaque(mut local)', ''},
		InterfaceSumEscapeCase{'AliasedTarget', 'wrap_target(mut local)', ''},
		InterfaceSumEscapeCase{'?Target', 'wrap_target(mut local)', ''},
		InterfaceSumEscapeCase{'!Unique', 'wrap_unique(mut local)', ''},
		InterfaceSumEscapeCase{'Shared', 'wrap_shared(mut local)', ''},
		InterfaceSumEscapeCase{'Unique', 'wrap_unique(mut local)', ''},
		InterfaceSumEscapeCase{'Mixed', 'wrap_mixed(mut local)', ''},
		InterfaceSumEscapeCase{'Nested', 'wrap_nested(mut local)', ''},
		InterfaceSumEscapeCase{'Target', 'forward_target(mut local)', ''},
		InterfaceSumEscapeCase{'Unique', 'forward_unique(mut local)', ''},
		InterfaceSumEscapeCase{'Target', 'identity_target(held)', 'held := wrap_target(mut local)'},
		InterfaceSumEscapeCase{'Opaque', 'identity_opaque(held)', 'held := wrap_opaque(mut local)'},
		InterfaceSumEscapeCase{'Unique', 'identity_unique(held)', 'held := wrap_unique(mut local)'},
		InterfaceSumEscapeCase{'Opaque', 'recursive_wrap(mut local, 2)', ''},
		InterfaceSumEscapeCase{'Holder', 'wrap_unique(mut local) as Holder', ''},
		InterfaceSumEscapeCase{'Holder', 'wrap_opaque(mut local) as Holder', ''},
	]
	// Custom flags can limit the diagnostics emitted by a single compilation.
	batch_size := if interface_sum_escape_default_vflags { cases.len } else { 1 }
	for i, batch in arrays.chunk(cases, batch_size) {
		check_interface_sum_escape_batch(os.join_path(root, 'batch_${i}'), batch)!
	}
}

fn check_interface_sum_escape_batch(root string, cases []InterfaceSumEscapeCase) ! {
	os.mkdir(root)!
	os.write_file(os.join_path(root, 'main.v'), interface_sum_escape_declarations + '\nfn main() {}\n')!
	for i, case in cases {
		source := os.join_path(root, 'escape_case_${i}.v')
		os.write_file(source, 'module main

fn escaped_${i}() ${case.typ} {
	mut local := Builder{}
	${case.binding}
	return ${case.expression}
}
')!
	}
	for mode in [['-no-parallel'], []string{}] {
		// Each file must report its own rejection in the shared compilation.
		out := run_interface_sum_escape_fixture(root, [...mode, '-check'])
		assert out.exit_code != 0, '${mode}: ${out.output}'
		diagnostics := out.output.split_into_lines().filter(it.contains('cannot return a reference to local storage `local`'))
		for i, case in cases {
			filename := 'escape_case_${i}.v'
			assert diagnostics.any(it.contains('${filename}:') || it.contains('${filename}"')), '${mode}: ${case.typ}: ${case.expression}: ${out.output}'
		}
	}
}

fn test_ownership_interface_and_sum_payloads_keep_caller_heap_and_value_storage() {
	root := os.join_path(os.vtmp_dir(), 'ownership_interface_sum_safe_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, interface_sum_escape_declarations + '
fn caller_target(mut builder Builder) Target { return forward_target(mut builder) }
fn caller_opaque(mut builder Builder) Opaque { return wrap_opaque(mut builder) }
fn caller_shared(mut builder Builder) Shared { return wrap_shared(mut builder) }
fn caller_unique(mut builder Builder) Unique { return forward_unique(mut builder) }
fn caller_mixed(mut builder Builder) Mixed { return wrap_mixed(mut builder) }
fn heap_target() Target { mut heap := &Builder{}; return wrap_target(mut heap) }
fn heap_inherited() InheritedTarget { mut heap := &Builder{}; return wrap_inherited(mut heap) }
fn heap_opaque() Opaque { mut heap := &Builder{}; held := wrap_opaque(mut heap); return identity_opaque(held) }
fn heap_unique() Unique { mut heap := &Builder{}; held := wrap_unique(mut heap); return identity_unique(held) }
fn heap_nested() Nested { mut heap := &Builder{}; return wrap_nested(mut heap) }
fn heap_option() ?Target { mut heap := &Builder{}; return wrap_target(mut heap) }
fn heap_result() !Unique { mut heap := &Builder{}; return wrap_unique(mut heap) }
fn value_unique() Unique { return Other{number: 51} }
fn value_mixed() Mixed { return ValueTarget{target: 52} }
fn caller_recursive(mut builder Builder) Opaque { return recursive_wrap(mut builder, 2) }
fn heap_recursive() Opaque { mut heap := &Builder{}; return recursive_wrap(mut heap, 2) }
fn local_owned_box() Sized { local := "safe".to_owned(); return boxed(local) }
fn local_number_box() Sized { local := 53; return boxed_number(local) }
fn main() {
	mut builder := Builder{}
	assert voidptr(caller_target(mut builder).target) == voidptr(&builder)
	assert caller_opaque(mut builder).read() == 42
	shared := caller_shared(mut builder)
	if shared is Holder { assert voidptr(shared.target) == voidptr(&builder) } else { panic("wrong shared variant") }
	unique := caller_unique(mut builder)
	if unique is Holder { assert voidptr(unique.target) == voidptr(&builder) } else { panic("wrong unique variant") }
	mixed := caller_mixed(mut builder)
	if mixed is Holder { assert voidptr(mixed.target) == voidptr(&builder) } else { panic("wrong mixed variant") }
	assert heap_target().target.value == 42
	assert heap_inherited().target.value == 42
	assert heap_opaque().read() == 42
	assert (heap_opaque() as Holder).target.value == 42
	assert (heap_unique() as Holder).target.value == 42
	assert caller_recursive(mut builder).read() == 42
	assert heap_recursive().read() == 42
	assert local_owned_box().size() == 4
	assert local_number_box().size() == 53
	heap := heap_unique()
	if heap is Holder { assert heap.target.value == 42 } else { panic("wrong heap variant") }
	nested := heap_nested()
	if nested is Holder { assert nested.target.value == 42 } else { panic("wrong nested variant") }
	assert (heap_option() or { panic("none") }).target.value == 42
	result := heap_result() or { panic(err) }
	if result is Holder { assert result.target.value == 42 } else { panic("wrong result variant") }
	value := value_unique()
	if value is Other { assert value.number == 51 } else { panic("wrong value variant") }
	number := value_mixed()
	if number is ValueTarget { assert number.target == 52 } else { panic("wrong number variant") }
}
')!
	for mode in [['-no-parallel'], []string{}] {
		out := run_interface_sum_escape_fixture(source, [...mode, 'run'])
		assert out.exit_code == 0, '${mode}: ${out.output}'
	}
}
