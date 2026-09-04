import os

const header_owned_vexe = @VEXE
const header_owned_tests_dir = os.dir(@FILE)
const header_owned_v3_dir = os.dir(header_owned_tests_dir)
const header_owned_vlib_dir = os.dir(header_owned_v3_dir)
const header_owned_v3_src = os.join_path(header_owned_v3_dir, 'v3.v')

fn header_owned_v3_bin_path() string {
	return os.join_path(os.vtmp_dir(), 'v3_header_owned_test_${os.getpid()}')
}

fn header_owned_build_v3() string {
	v3_bin := header_owned_v3_bin_path()
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build := os.execute('${os.quoted_path(header_owned_vexe)} -gc none -path "${header_owned_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(header_owned_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn testsuite_begin() {
	os.rm(header_owned_v3_bin_path()) or {}
	_ = header_owned_build_v3()
}

fn testsuite_end() {
	os.rm(header_owned_v3_bin_path()) or {}
}

fn test_plain_v_header_owned_c_structs_are_not_redeclared() {
	root := os.join_path(os.vtmp_dir(), 'v3_header_owned_struct_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := header_owned_build_v3()

	os.write_file(os.join_path(root, 'types.h'), '#ifndef V3_HEADER_OWNED_TYPES_H
#define V3_HEADER_OWNED_TYPES_H
typedef struct V3HeaderOwnedImpl_ {
	long long value;
} V3HeaderOwnedAlias;
typedef struct V3HeaderOwnedTag {
	long long value;
} V3HeaderOwnedTag;
#endif
')!
	os.write_file(os.join_path(root, 'wrapper.h'), '#include "types.h"\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#include "wrapper.h"

@[typedef]
struct C.V3HeaderOwnedAlias {
	value int
}

struct C.V3HeaderOwnedTag {
	value int
}

fn main() {
	alias := C.V3HeaderOwnedAlias{
		value: 40
	}
	tag := C.V3HeaderOwnedTag{
		value: 2
	}
	println(alias.value + tag.value)
}
')!
	out := os.join_path(root, 'out')
	compile := os.execute('${os.quoted_path(v3_bin)} --no-parallel ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
	generated := os.read_file(out + '.c')!
	assert !generated.contains('typedef struct V3HeaderOwnedAlias V3HeaderOwnedAlias;'), generated
	assert !generated.contains('struct V3HeaderOwnedAlias {'), generated
	assert !generated.contains('struct V3HeaderOwnedTag {'), generated
}

fn test_repeated_header_after_define_preserves_preprocessor_semantics() {
	root := os.join_path(os.vtmp_dir(), 'v3_repeated_header_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'library.h'), '#ifndef V3_SINGLE_HEADER_DECLARATION
#define V3_SINGLE_HEADER_DECLARATION
int v3_single_header_value(void);
#endif

#ifdef V3_SINGLE_HEADER_IMPLEMENTATION
int v3_single_header_value(void) {
	return 42;
}
#endif
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#include "library.h"
#define V3_SINGLE_HEADER_IMPLEMENTATION
#include "library.h"

fn C.v3_single_header_value() int

fn main() {
	println(C.v3_single_header_value())
}
')!
	v3_bin := header_owned_build_v3()
	out := os.join_path(root, 'out')
	compile := os.execute('${os.quoted_path(v3_bin)} --no-parallel ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
	generated := os.read_file(out + '.c')!
	assert generated.count('#include "library.h"') == 2, generated
	first_include := generated.index('#include "library.h"') or { -1 }
	define := generated.index('#define V3_SINGLE_HEADER_IMPLEMENTATION') or { -1 }
	assert first_include >= 0, generated
	assert define > first_include, generated
	assert generated[define..].contains('#include "library.h"'), generated
}
