import os

// platform_c_names_header is ordinary C code that a project shares with the
// generated translation unit. It uses only the names V defines before it: `__v_int`
// and `__v_uint` for V's `int`/`uint`, and the legacy `isize`/`usize` typedefs.
const platform_c_names_header = 'typedef struct {
	__v_int value;
	__v_uint length;
} c_names_pair;

static __v_int c_names_scale(__v_int x) {
	return x * 3;
}

static __v_uint c_names_next(__v_uint n) {
	return n + 1;
}

static c_names_pair c_names_make_pair(__v_int value, __v_uint length) {
	c_names_pair pair;
	pair.value = value;
	pair.length = length;
	return pair;
}

static __v_int c_names_pair_sum(c_names_pair* pair) {
	return pair->value + (__v_int)pair->length;
}

static i64 c_names_legacy_isize(void) {
	isize value = 64;
	return value - 2;
}

static i64 c_names_legacy_usize(void) {
	usize value = sizeof(usize);
	return (i64)value;
}
'

fn platform_c_names_root() string {
	return os.join_path(os.vtmp_dir(), 'platform_int_c_names_${os.getpid()}')
}

// platform_c_names_write_fixture writes a program whose C code spells V's platform
// types with the generated names, and returns its main file.
fn platform_c_names_write_fixture(root string) !string {
	os.mkdir_all(root)!
	header := os.join_path(root, 'names.h')
	os.write_file(header, platform_c_names_header)!
	header_c := header.replace('\\', '/')
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

#include "${header_c}"

@[typedef]
struct C.c_names_pair {
	value  int
	length uint
}

fn C.c_names_scale(x i64) i64
fn C.c_names_next(n uint) uint
fn C.c_names_make_pair(value i64, length uint) C.c_names_pair
fn C.c_names_pair_sum(pair &C.c_names_pair) i64
fn C.c_names_legacy_isize() i64
fn C.c_names_legacy_usize() i64

fn add(a int, b int) int {
	return a + b
}

fn main() {
	big := i64(1) << 40
	assert C.c_names_scale(big) == big * 3
	assert C.c_names_next(uint(big)) == uint(big) + 1
	pair := C.c_names_make_pair(big, uint(big) * 2)
	assert pair.value == big
	assert pair.length == uint(big) * 2
	assert C.c_names_pair_sum(&pair) == big * 3
	assert C.c_names_legacy_isize() == 62
	assert C.c_names_legacy_usize() == i64(sizeof(voidptr))
	assert add(40, 2) == 42
	println("ok")
}
')!
	return source
}

fn test_c_code_uses_the_generated_platform_int_names() {
	root := platform_c_names_root()
	defer {
		os.rmdir_all(root) or {}
	}
	source := platform_c_names_write_fixture(root)!
	exe := os.join_path(root, 'main') + $if windows { '.exe' } $else { '' }
	compile := os.exec([@VEXE, '-o', exe, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_native_c_output_defines_the_names_before_user_headers() {
	root := platform_c_names_root()
	defer {
		os.rmdir_all(root) or {}
	}
	source := platform_c_names_write_fixture(root)!
	header_c := os.join_path(root, 'names.h').replace('\\', '/')
	c_file := os.join_path(root, 'main.c')
	compile := os.exec([@VEXE, '-o', c_file, source])
	assert compile.exit_code == 0, compile.output
	code := os.read_file(c_file)!
	int_ct := if sizeof(voidptr) == 8 { 'i64' } else { 'i32' }
	int_typedef := 'typedef ${int_ct} __v_int;\n'
	uint_typedef := 'typedef size_t __v_uint;\n'
	int_index := code.index(int_typedef) or { -1 }
	uint_index := code.index(uint_typedef) or { -1 }
	include_index := code.index('#include "${header_c}"') or { -1 }
	assert int_index >= 0, 'native C output has no __v_int typedef'
	assert uint_index >= 0, 'native C output has no __v_uint typedef'
	assert include_index >= 0, 'the user header is not included'
	assert int_index < include_index, '__v_int is defined after the user header'
	assert uint_index < include_index, '__v_uint is defined after the user header'
	// The C typedefs of the deprecated V names outlive the V types themselves.
	assert code.contains('typedef ptrdiff_t isize;\n')
	assert code.contains('typedef size_t usize;\n')
	// The typedef is an alias, not a new spelling: generated signatures keep the
	// native `i64`/`i32`. That is what makes `__v_int` ABI-neutral.
	assert code.contains('${int_ct} add(${int_ct} a, ${int_ct} b)')
}

// A program with no `#include` at all still gets the names: the typedefs are not
// part of the system-libc preamble any more.
fn test_c_code_without_includes_gets_the_names() {
	root := os.join_path(os.vtmp_dir(), 'platform_int_c_names_insert_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'names.h')
	os.write_file(header, 'static i64 c_names_inserted_int(void) {
	__v_int value = 60;
	return value + 2;
}

static i64 c_names_inserted_isize(void) {
	isize value = 50;
	return value + 3;
}

static i64 c_names_inserted_usize(void) {
	usize value = sizeof(usize);
	return (i64)value;
}
')!
	header_c := header.replace('\\', '/')
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

#insert "${header_c}"

fn C.c_names_inserted_int() i64
fn C.c_names_inserted_isize() i64
fn C.c_names_inserted_usize() i64

fn main() {
	assert C.c_names_inserted_int() == 62
	assert C.c_names_inserted_isize() == 53
	assert C.c_names_inserted_usize() == i64(sizeof(voidptr))
	println("ok")
}
')!
	exe := os.join_path(root, 'main') + $if windows { '.exe' } $else { '' }
	compile := os.exec([@VEXE, '-o', exe, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
