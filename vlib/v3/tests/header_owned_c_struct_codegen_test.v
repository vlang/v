import os
import rand

const header_owned_vexe = @VEXE
const header_owned_tests_dir = os.dir(@FILE)
const header_owned_v3_dir = os.dir(header_owned_tests_dir)
const header_owned_vlib_dir = os.dir(header_owned_v3_dir)
const header_owned_v3_src = os.join_path(header_owned_v3_dir, 'v3.v')

fn header_owned_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_header_owned_test_${os.getpid()}_${rand.ulid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${os.quoted_path(header_owned_vexe)} -gc none -path "${header_owned_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(header_owned_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_header_owned_c_typedef_with_indirect_include_is_not_redeclared() {
	v3_bin := header_owned_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_header_owned_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	out := os.join_path(root, 'out')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'types.h'), '#ifndef V3_HEADER_OWNED_TYPES_H
#define V3_HEADER_OWNED_TYPES_H
typedef struct V3HeaderOwnedImpl_ {
	int value;
} V3HeaderOwnedAlias;
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'wrapper.h'), '#ifndef V3_HEADER_OWNED_WRAPPER_H
#define V3_HEADER_OWNED_WRAPPER_H
#define V3_HEADER_OWNED_TYPES_HEADER "types.h"
#include V3_HEADER_OWNED_TYPES_HEADER
#if 0
typedef struct V3InactiveImpl_ {
	int ignored;
} V3InactiveAlias;
#include "inactive_nested.h"
#endif
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'inactive_nested.h'), 'typedef struct V3InactiveNestedImpl_ {
	int ignored;
} V3InactiveNestedAlias;
') or { panic(err) }
	os.write_file(os.join_path(root, 'source_feature.h'), '#ifdef V3_SOURCE_FEATURE
typedef struct V3SourceFeatureImpl_ {
	int value;
} V3SourceFeatureAlias;
#define V3_FROM_PREVIOUS_HEADER
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'previous_header_feature.h'), '#ifdef V3_FROM_PREVIOUS_HEADER
typedef struct V3PreviousHeaderImpl_ {
	int value;
} V3PreviousHeaderAlias;
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'source_undef.h'), '#ifdef V3_SOURCE_DISABLED
typedef struct V3SourceUndefImpl_ {
	int value;
} V3SourceUndefAlias;
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#flag -DV3_SOURCE_DISABLED
#include "wrapper.h"
#define V3_SOURCE_FEATURE
#include "source_feature.h"
#include "previous_header_feature.h"
#undef V3_SOURCE_DISABLED
#include "source_undef.h"

@[typedef]
struct C.V3HeaderOwnedAlias {
	value int
}

@[typedef]
struct C.V3HeaderlessLocal {
	value int
}

@[typedef]
struct C.V3InactiveAlias {
	value int
}

@[typedef]
struct C.V3InactiveNestedAlias {
	value int
}

@[typedef]
struct C.V3SourceFeatureAlias {
	value int
}

@[typedef]
struct C.V3PreviousHeaderAlias {
	value int
}

@[typedef]
struct C.V3SourceUndefAlias {
	value int
}

fn main() {
	item := C.V3HeaderOwnedAlias{
		value: 42
	}
	local := C.V3HeaderlessLocal{
		value: 7
	}
	inactive := C.V3InactiveAlias{
		value: 5
	}
	inactive_nested := C.V3InactiveNestedAlias{
		value: 6
	}
	source_feature := C.V3SourceFeatureAlias{
		value: 8
	}
	previous_header := C.V3PreviousHeaderAlias{
		value: 10
	}
	source_undef := C.V3SourceUndefAlias{
		value: 9
	}
	println(item.value + local.value + inactive.value + inactive_nested.value + source_feature.value +
		previous_header.value + source_undef.value)
}
') or { panic(err) }

	compile := os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '87', run.output
	generated := os.read_file(out + '.c') or { panic(err) }
	assert !generated.contains('typedef struct V3HeaderOwnedAlias V3HeaderOwnedAlias;'), generated
	assert !generated.contains('struct V3HeaderOwnedAlias {'), generated
	assert generated.contains('typedef struct V3HeaderlessLocal V3HeaderlessLocal;'), generated
	assert generated.contains('struct V3HeaderlessLocal {'), generated
	assert generated.contains('typedef struct V3InactiveAlias V3InactiveAlias;'), generated
	assert generated.contains('struct V3InactiveAlias {'), generated
	assert generated.contains('typedef struct V3InactiveNestedAlias V3InactiveNestedAlias;'), generated
	assert generated.contains('struct V3InactiveNestedAlias {'), generated
	assert !generated.contains('typedef struct V3SourceFeatureAlias V3SourceFeatureAlias;'), generated
	assert !generated.contains('struct V3SourceFeatureAlias {'), generated
	assert !generated.contains('typedef struct V3PreviousHeaderAlias V3PreviousHeaderAlias;'), generated
	assert !generated.contains('struct V3PreviousHeaderAlias {'), generated
	assert generated.contains('typedef struct V3SourceUndefAlias V3SourceUndefAlias;'), generated
	assert generated.contains('struct V3SourceUndefAlias {'), generated
}

fn test_postinclude_c_typedef_is_declared_before_generated_bodies() {
	v3_bin := header_owned_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_postinclude_typedef_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	out := os.join_path(root, 'out')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'late.h'), 'typedef struct V3PostincludeAlias V3PostincludeAlias;\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#postinclude "late.h"

@[typedef]
struct C.V3PostincludeAlias {
	value int
}

fn read_postinclude_value(item C.V3PostincludeAlias) int {
	return item.value
}

fn main() {
	item := C.V3PostincludeAlias{
		value: 43
	}
	println(read_postinclude_value(item))
}
') or { panic(err) }

	compile := os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '43', run.output
	generated := os.read_file(out + '.c') or { panic(err) }
	forward_pos := generated.index('typedef struct V3PostincludeAlias V3PostincludeAlias;') or {
		-1
	}
	body_pos := generated.index('struct V3PostincludeAlias {') or { -1 }
	postinclude_pos := generated.index('#include "late.h"') or { -1 }
	assert forward_pos >= 0, generated
	assert body_pos > forward_pos, generated
	assert postinclude_pos > body_pos, generated
}
