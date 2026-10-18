module fastc

import os
import v.cmdexec
import v.flat

// Programs FastC compiled without an error but to code that computes something else than
// the C backend; each was found running vlib's or blobly_emb's tests under FastC.

// fastc_build_program builds `source` with FastC into `output`
// in a fresh directory named after `name`, which the caller removes.
fn fastc_build_program(name string, source string, output string) (string, string) {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	program := os.join_path(root, output)
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root],
		os.real_path(os.join_path(@DIR, '../../../..')), {
			'VEXE':                   @VEXE
			'VFLAGS':                 ''
			'VOSARGS':                ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
	assert build.exit_code == 0, build.output
	return root, program
}

// An interface method in a `mut:` section may be implemented with an immutable receiver
// (hash.Hash.size() on a crypto Digest); the dispatcher found no case and returned 0.
const mut_section_interface_source = 'module main

interface Sizer {
mut:
	write(p []u8) !int
	size() int
}

struct Digest {
	n int
}

fn (mut d Digest) write(p []u8) !int {
	return p.len
}

fn (d &Digest) size() int {
	return d.n
}

fn new_digest() &Digest {
	return &Digest{
		n: 20
	}
}

fn main() {
	get := fn () Sizer {
		return new_digest()
	}
	mut sizer := get()
	assert sizer.size() == 20
	assert sizer.write([u8(1), 2])! == 2
	println("mut section interfaces work")
}
'

fn test_fastc_mut_section_interface_dispatch_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_mut_section_interface', mut_section_interface_source,
		'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'mut section interfaces work', run.output
}

// An interface method call that matches no implementation must stop the program,
// never return a zero value as if a method had answered.
fn test_fastc_interface_dispatch_without_a_match_panics() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_dispatch_default', mut_section_interface_source,
		'program.c')
	defer { os.rmdir_all(root) or {} }
	source := os.read_file(program) or { panic(err) }
	dispatch := source.all_after('int Sizer_size(Sizer value) {').all_before('\n}')
	assert dispatch.contains('case __v_typeid_Digest:'), dispatch
	assert dispatch.contains('builtin__panic('), dispatch
}

// A struct implementing IError, returned from a `!T` function, is that function's error
// (csv's EndOfFileError: read as a success, every reader looped until memory ran out),
// and an interface method an error gets from its embedded `Error` is dispatched to it.
const custom_error_source = 'module main

struct EofError {
	Error
}

fn (e EofError) msg() string {
	return "eof"
}

struct QuoteError {
	Error
}

struct Point {
	Error
	x int
}

fn read(n int) !int {
	if n == 1 {
		return &EofError{}
	}
	if n == 2 {
		return QuoteError{}
	}
	return n * 10
}

fn make_point() !Point {
	return Point{
		x: 3
	}
}

fn main() {
	mut seen := []string{}
	for n in [0, 1, 2, 3] {
		v := read(n) or {
			kind := if err is EofError { "eof" } else if err is QuoteError { "quote" } else { "other" }
			seen << "\${kind}:\${err.msg()}"
			continue
		}
		seen << v.str()
	}
	assert seen == ["0", "eof:eof", "quote:", "30"]
	p := make_point() or { panic(err) }
	assert p.x == 3
	println("custom errors work")
}
'

fn test_fastc_custom_error_returns_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_custom_error', custom_error_source, 'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'custom errors work', run.output
}

// An IError implementer can also be a successful interface payload. Check every
// required method and field so unrelated interfaces still receive an error.
const result_interface_payload_source = 'module main

struct PayloadError {
	Error
	message string
}

fn (e PayloadError) msg() string {
	return e.message
}

interface MessagePayload {
	msg() string
}

interface FieldPayload {
	message string
}

interface OtherPayload {
	code() string
}

interface WrongFieldPayload {
	message int
}

fn ierror_payload() !IError {
	return PayloadError{ message: "ierror" }
}

fn message_payload() !MessagePayload {
	return PayloadError{ message: "message" }
}

fn pointer_message_payload() !MessagePayload {
	return &PayloadError{ message: "pointer" }
}

fn field_payload() !FieldPayload {
	return PayloadError{ message: "field" }
}

fn generic_payload[T](value T) !MessagePayload {
	return value
}

fn other_payload() !OtherPayload {
	return PayloadError{ message: "failure" }
}

fn wrong_field_payload() !WrongFieldPayload {
	return PayloadError{ message: "field failure" }
}

fn main() {
	ierror_value := ierror_payload() or { panic(err) }
	assert ierror_value.msg() == "ierror"
	message_value := message_payload() or { panic(err) }
	assert message_value.msg() == "message"
	pointer_value := pointer_message_payload() or { panic(err) }
	assert pointer_value.msg() == "pointer"
	_ := field_payload() or { panic(err) }
	generic_value := generic_payload(PayloadError{ message: "generic" }) or { panic(err) }
	assert generic_value.msg() == "generic"
	mut saw_error := false
	other_payload() or {
		assert err.msg() == "failure"
		saw_error = true
	}
	assert saw_error
	mut saw_field_error := false
	wrong_field_payload() or {
		assert err.msg() == "field failure"
		saw_field_error = true
	}
	assert saw_field_error
	println("result interface payloads work")
}
'

fn test_fastc_result_interface_payload_returns_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_result_interface_payload', result_interface_payload_source,
		'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'result interface payloads work', run.output
}

// A one-line definition (every interface's) ends at its own `};`. Ending it at the next
// definition's `\n};` made a move carry the following struct along without updating its
// index, and a later move then spliced `struct os__File` into the middle of another struct.
fn test_fastc_one_line_composite_definition_ends_at_its_own_brace() {
	one_line := 'struct io__Writer { void *_object; uintptr_t _typ; };'
	source := one_line + '\n\nstruct io__Impl {\n\tio__Writer w;\n};\n'
	end := fastc_c_composite_definition_end(source, 0) or { panic('no end') }
	assert source[..end] == one_line
	nested := 'struct Outer {\n\tunion {\n\t\tint a;\n\t} u;\n};\nstruct Next {\n\tint b;\n};\n'
	nested_end := fastc_c_composite_definition_end(nested, 0) or { panic('no end') }
	assert nested[..nested_end] == 'struct Outer {\n\tunion {\n\t\tint a;\n\t} u;\n};'
	ordered := fastc_order_c_composite_definitions('struct A { void *_object; uintptr_t _typ; };\n\nstruct B {\n\tA a;\n\tF f;\n};\n\nstruct F {\n\tint x;\n};\n',
		{
			'B': flat.TypeTextBindings.from_names({
				'a': 'A'
				'f': 'F'
			})
		}, {
			'A': true
			'F': true
		}, flat.TypeTextBindings{})
	assert ordered.index('struct F {') or { -1 } < ordered.index('struct B {') or { -1 }, ordered
	assert ordered.count('};') == 3, ordered
}

// V groups `& << >>` with `*` and `| ^` with `+`, and binds all of them tighter than a
// comparison; C binds `+ -` tighter than shifts and `&`, and comparisons tighter than
// `& ^ |`. An integer literal is a V `int` of the platform's width, so `1 << 51` does not
// overflow. ed25519's field arithmetic (`r0.lo & mask_low_51_bits + c4 * 19`, where
// `mask_low_51_bits = u64((1 << 51) - 1)`) depends on both.
const integer_operators_source = 'module main

const mask = u64((1 << 51) - 1)
const big = 1 << 40

fn main() {
	a := u64(0xff)
	b := u64(0x0f)
	c := u64(0x100)
	assert a & b + c == 271
	assert a | b + c == 511
	assert a ^ b + c == 496
	assert a + b & c == 255
	assert a & b == u64(15)
	assert a << 2 + 1 == 1021
	assert a >> 1 + 1 == 128
	assert 1 + a << 1 == 511
	assert a & b * 2 == 30
	x := 5
	y := 3
	assert x & y + 1 == 2
	assert x | y - 1 == 6
	assert x & -y == 5
	assert mask == 2251799813685247
	assert big == 1099511627776
	n := 45
	assert 1 << n == 35184372088832
	assert 0x1e << 40 == 0x1e0000000000
	println("integer operators work")
}
'

fn test_fastc_integer_operator_grouping_and_literal_width_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_integer_operators', integer_operators_source, 'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'integer operators work', run.output
}

// A fixed array passed to a `mut []T` parameter (`copy(mut fixed, src)`) is a view of its own
// storage, whether it is a local, a struct field, or a `mut [N]T` parameter of the caller. A copy
// of it was passed instead, and ed25519's Point.bytes() encoded every point as zeros.
const fixed_array_mut_source = 'module main

struct Holder {
mut:
	s [4]u8
}

fn fill(mut buf [4]u8, src []u8) []u8 {
	copy(mut buf, src)
	return buf[..]
}

fn main() {
	src := [u8(1), 2, 3, 4]
	mut holder := Holder{}
	assert copy(mut holder.s, src) == 4
	assert holder.s[0] == 1 && holder.s[3] == 4
	mut local := [4]u8{}
	copy(mut local, src)
	assert local[0] == 1 && local[3] == 4
	mut param := [4]u8{}
	out := fill(mut param, [u8(5), 6, 7, 8])
	assert out == [u8(5), 6, 7, 8]
	assert param[0] == 5 && param[3] == 8
	println("fixed arrays work")
}
'

fn test_fastc_fixed_array_mut_argument_is_its_storage_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_fixed_array_mut', fixed_array_mut_source, 'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'fixed arrays work', run.output
}

// A fixed-array view passed to a mut parameter must remain alive during the call.
// Clang's scope instrumentation catches an address escaping a statement-expression block.
fn test_fastc_fixed_array_mut_view_lifetime() {
	$if !macos && !linux {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_view_lifetime_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	flags := ['-O1', '-fno-omit-frame-pointer', '-fsanitize=address',
		'-fsanitize-address-use-after-scope', source_path, '-o', program]
	// Some Clang installations lack the sanitizer runtime. Probe it independently.
	os.write_file(source_path, 'int main(void) { return 0; }') or { panic(err) }
	available := cmdexec.run(clang, flags)
	if available.exit_code != 0 {
		return
	}
	probe := cmdexec.run(program, [])
	if probe.exit_code != 0 {
		return
	}
	view := fastc_fixed_array_view('Array', '2', 'sizeof(int)', 'data')
	os.write_file(source_path, '#include <assert.h>
typedef struct { void *data; int len; int cap; int element_size; } Array;
__attribute__((noinline)) static void fill(Array *a) {
	assert(a->len == 2 && a->element_size == sizeof(int));
	((int *)a->data)[0] = 42;
}
int main(void) {
	int data[2] = {1, 2};
	fill(${view});
	assert(data[0] == 42 && data[1] == 2);
	return 0;
}
') or { panic(err) }
	build := cmdexec.run(clang, flags)
	assert build.exit_code == 0, build.output
	run := cmdexec.run_in_merged_with_env(program, [], root, {
		'ASAN_OPTIONS': 'detect_leaks=0:halt_on_error=1'
	})
	assert run.exit_code == 0, run.output
}

// `@[_pack: '1']` lays a struct out without padding, as net.Ip6 needs; fastc ignored it, so
// net.Addr's address sat two bytes late and `127.0.0.1:81` read back as `0.1.0.0:32512`.
// TinyCC honours only `#pragma pack`, so that is what is emitted.
const packed_struct_source = 'module main

@[_pack: \'1\']
struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]u8
	scope_id  u32
}

@[packed]
struct Pair {
	a u8
	b u32
}

struct Plain {
	a u8
	b u32
}

union Data {
	Pair
	Ip6
}

struct Addr {
	f    u16
	addr Data
}

fn main() {
	assert sizeof(Ip6) == 26
	assert sizeof(Pair) == 5
	assert sizeof(Plain) == 8
	a := Addr{}
	assert u64(voidptr(&a.addr)) - u64(voidptr(&a)) == 2
	println("packed structs work")
}
'

fn test_fastc_packed_struct_layout_runtime() {
	$if !macos && !linux {
		return
	}
	root, program := fastc_build_program('fastc_packed_struct', packed_struct_source, 'program')
	defer { os.rmdir_all(root) or {} }
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'packed structs work', run.output
}

// Reordering definitions moves a packed definition together with its `#pragma pack` lines,
// so the pragmas keep surrounding the struct they belong to.
fn test_fastc_ordering_moves_a_packed_definition_with_its_pragmas() {
	source := 'struct B {\n\tP p;\n};\n\n' + fastc_pack_push_line + 'struct P {\n\tu8 a;\n\tu32 b;\n};\n' +
		fastc_pack_pop_line + '\nstruct C {\n\tint c;\n};\n'
	ordered := fastc_order_c_composite_definitions(source, {
		'B': flat.TypeTextBindings.from_names({
			'p': 'P'
		})
	}, {
		'P': true
	}, flat.TypeTextBindings{})
	packed := fastc_pack_push_line + 'struct P {\n\tu8 a;\n\tu32 b;\n};\n' + fastc_pack_pop_line
	assert ordered.contains(packed), ordered
	assert ordered.index(packed) or { -1 } < ordered.index('struct B {') or { -1 }, ordered
	assert ordered.count('#pragma pack(push, 1)') == 1 && ordered.count('#pragma pack(pop)') == 1, ordered
}
