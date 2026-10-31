module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_target_small_options_do_not_reenter_the_allocator() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_option_allocator_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), '#include <stddef.h>\n#include <stdint.h>\nvoid enter_allocator(void);\nvoid leave_allocator(void);\nvoid *next_block(size_t size);\nint64_t allocation_count(void);\nuint64_t next_wide(void);\nint64_t wide_calls(void);\nint native_option_copy(void);\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#include "native.h"
struct Block {
mut:
	value u64
}
type BlockRef = &Block
type Choose = fn (uint) ?BlockRef
struct Large {
	a u64
	b u64
	c u64
}
struct Holder {
mut:
	value ?u64
}
struct Small {
mut:
	value u32
}
struct Counter {
mut:
	left int
}
fn (mut counter Counter) next() ?u64 {
	if counter.left == 0 { return none }
	counter.left--
	return C.next_wide()
}
enum Mode { ready }
fn C.enter_allocator()
fn C.leave_allocator()
fn C.next_block(size uint) voidptr
fn C.allocation_count() i64
fn C.next_wide() u64
fn C.wide_calls() i64
fn C.native_option_copy() int
fn C._vinit(argc int, argv voidptr)
fn choose(size uint) ?BlockRef {
	if size > 4096 { return none }
	return unsafe { BlockRef(C.next_block(size)) }
}
fn optional_copy(value ?BlockRef) ?BlockRef { return value }
fn result_copy(value BlockRef) !BlockRef { return value }
fn result_forward(value BlockRef) !BlockRef { return result_copy(value)! }
@[export: "test_allocator"]
fn allocator(size uint) voidptr {
	C.enter_allocator()
	callback := Choose(choose)
	selected := callback(size)
	copied := selected
	returned := optional_copy(copied)
	if block := returned {
		result := result_forward(block) or { C.leave_allocator(); return unsafe { nil } }
		C.leave_allocator()
		return voidptr(result)
	}
	C.leave_allocator()
	return unsafe { nil }
}
fn wide() ?u64 { return C.next_wide() }
fn zero() ?u64 { return u64(0) }
fn missing() ?u64 { return none }
fn null_pointer() ?voidptr { return unsafe { nil } }
fn result_wide() !u64 { return C.next_wide() }
fn result_missing() !u64 { return error_sentinel }
fn large(value u64) ?Large { return Large{a: value, b: value + 1, c: value + 2} }
fn large_result(value u64) !Large { return Large{a: value, b: value + 1, c: value + 2} }
fn small() ?Small { return Small{value: 7} }
fn branch(take bool) ?u64 { return if take { u64(17) } else { u64(19) } }
fn matched(value int) ?u64 { return match value { 0 { u64(23) } else { u64(29) } } }
fn optional_callback(value u64) ?u64 { return value + 1 }
fn main() {
	C._vinit(0, unsafe { nil })
	before := C.allocation_count()
	first := choose(32) or { panic("missing block") }
	first.value = 41
	original := optional_copy(first)
	copied := original
	if found := copied { assert voidptr(found) == voidptr(first) && found.value == 41 } else { assert false }
	assert voidptr(result_forward(first) or { panic("missing result") }) == voidptr(first)
	wide_option := wide()
	wide_copy := wide_option
	assert (wide_copy or { u64(0) }) == 0xf123456789abcdef
	if value := wide_copy { assert value == 0xf123456789abcdef } else { assert false }
	assert C.wide_calls() == 1
	assert (result_wide() or { u64(0) }) == 0xf123456789abcdef
	assert C.wide_calls() == 2
	mut iterator := Counter{left: 2}
	mut iterations := 0
	for value in iterator { assert value == 0xf123456789abcdef; iterations++ }
	assert iterations == 2 && C.wide_calls() == 4
	assert (zero() or { u64(99) }) == 0
	assert (missing() or { u64(99) }) == 99
	if p := null_pointer() { assert p == unsafe { nil } } else { assert false }
	assert (result_missing() or { u64(77) }) == 77
	assert (branch(true) or { u64(0) }) == 17
	assert (matched(1) or { u64(0) }) == 29
	assert (Mode.from_string("ready") or { Mode.ready }) == .ready
	mut holder := Holder{value: u64(31)}
	holder.value = u64(37)
	assert (holder.value or { u64(0) }) == 37
	mut small_option := small()
	if small_option != none { small_option.value = 11 }
	assert (small_option or { Small{} }).value == 11
	callback := optional_callback
	assert (callback(u64(4)) or { u64(0) }) == 5
	assert C.native_option_copy() == 1
	// Only the explicit block request above can allocate in this section.
	assert C.allocation_count() == before + 1
	before_large := C.allocation_count()
	large_option := large(101)
	large_copy := large_option
	assert (large_copy or { Large{} }).c == 103
	assert (large(201) or { Large{} }).b == 202
	assert (large_option or { Large{} }).a == 101
	assert (large_result(301) or { Large{} }).c == 303
	assert C.allocation_count() == before_large + 3
	mut values := map[int]u64{}
	values[7] = 0xf123456789abcdef
	before_lookup := C.allocation_count()
	assert (values[7] or { u64(0) }) == 0xf123456789abcdef
	if value := values[7] { assert value == 0xf123456789abcdef } else { assert false }
	assert (values[9] or { u64(83) }) == 83
	assert C.allocation_count() == before_lookup
	elements := [u64(0xfedcba9876543210)]
	before_index := C.allocation_count()
	assert (elements[0] or { u64(0) }) == 0xfedcba9876543210
	assert (elements[2] or { u64(89) }) == 89
	assert C.allocation_count() == before_index
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	assert source.contains('uintptr_t inline_bits;'), source
	assert source.contains('V_FASTC_OPTION_SUCCESS(Option,'), source
	prefs.target_libc_headers = false
	hosted_source := generate_files([root], prefs) or { panic(err) }
	assert !hosted_source.contains('inline_bits'), hosted_source
	assert !hosted_source.contains('V_FASTC_OPTION_'), hosted_source
	assert hosted_source.contains('(Option){.data='), hosted_source
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source + r'
static union { max_align_t alignment; unsigned char bytes[1048576]; } pool;
static size_t pool_used;
static int allocator_depth;
static bool allocator_ready;
static i64 allocations;
static i64 wide_evaluations;
void enter_allocator(void) { if (!allocator_ready || ++allocator_depth != 1) abort(); }
void leave_allocator(void) { if (allocator_depth-- != 1) abort(); }
void *next_block(size_t size) {
	if (allocator_depth != 1 && allocator_depth != 0) abort();
	size = (size + 15) & ~(size_t)15;
	if (pool_used + size > sizeof(pool.bytes)) abort();
	void *result = pool.bytes + pool_used;
	pool_used += size;
	allocations++;
	return result;
}
i64 allocation_count(void) { return allocations; }
u64 next_wide(void) { wide_evaluations++; return 0xf123456789abcdefULL; }
i64 wide_calls(void) { return wide_evaluations; }
static int native_evaluations;
static Option native_once(void) { native_evaluations++; return V_FASTC_OPTION_SUCCESS(Option, u64, 0xfedcba9876543210ULL); }
int native_option_copy(void) {
	Option original = native_once();
	Option copied = original;
	u64 first = V_FASTC_OPTION_VALUE(u64, copied);
	u64 second = V_FASTC_OPTION_VALUE(u64, native_once());
	return first == 0xfedcba9876543210ULL && second == first && native_evaluations == 2;
}
__attribute__((constructor)) static void native_entry(void) {
	allocator_ready = true;
	void *block = malloc(16);
	if (block == NULL || allocator_depth != 0 || allocations != 1) abort();
}
') or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-Dmalloc=test_allocator', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
