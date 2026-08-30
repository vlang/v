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

fn header_owned_large_header(text string) string {
	return text + '/*' + 'x'.repeat(262_145) + '*/\n'
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

fn test_shadowed_system_header_does_not_claim_missing_typedef() {
	v3_bin := header_owned_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_header_owned_shadow_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(os.join_path(root, 'X11')) or { panic(err) }
	out := os.join_path(root, 'out')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'X11', 'Xlib.h'), '#pragma once\ntypedef int LocalX11Marker;\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#include <X11/Xlib.h>

@[typedef]
struct C.XEvent { value int }

fn main() {
	println(C.XEvent{ value: 42 }.value)
}
')!
	compile := os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
	generated := os.read_file(out + '.c')!
	assert generated.contains('struct XEvent {'), generated
}

fn test_header_owned_typedef_preprocessor_state_matches_emitted_c() {
	v3_bin := header_owned_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_header_owned_review_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root) or { panic(err) }
	out := os.join_path(root, 'out')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'valued.h'), '#if FEATURE_VALUE == 7\ntypedef struct { int value; } ValuedAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'compiler.h'), '#if defined(__clang__) || defined(__GNUC__)\ntypedef struct { int value; } CompilerAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'compiler_version.h'), '#if __GNUC__ >= 4\ntypedef struct { int value; } CompilerVersionAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'invoked_macro.h'), '#define UNUSED_ALIAS typedef struct Wrong UnusedMacroAlias;\n#define DECLARE_INVOKED_ALIAS typedef struct { int value; } InvokedMacroAlias;\nDECLARE_INVOKED_ALIAS\n')!
	os.write_file(os.join_path(root, 'function_macro.h'), '#define UNUSED_FUNCTION(name) typedef struct Wrong name;\n#define DECLARE_FUNCTION(name) typedef struct FunctionImpl name;\nDECLARE_FUNCTION(FunctionMacroAlias)\nstruct FunctionImpl { int value; };\n')!
	os.write_file(os.join_path(root, 'child.h'), '#ifdef CHILD_FEATURE\ntypedef struct { int value; } RevisitedAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'child_wrapper.h'), '#include "child.h"\n#define CHILD_FEATURE 1\n#include "child.h"\n')!
	os.write_file(os.join_path(root, 'lazy_one.h'), 'typedef struct { int value; } LazyAlias;\n')!
	os.write_file(os.join_path(root, 'lazy_two.h'), '/* the current alias target intentionally has no typedef */\n')!
	os.write_file(os.join_path(root, 'lazy_wrapper.h'), '#define B "lazy_one.h"\n#define A B\n#undef B\n#define B "lazy_two.h"\n#include A\n')!
	os.write_file(os.join_path(root, 'once.h'), header_owned_large_header('#pragma once\n#ifdef ONCE_FEATURE\ntypedef struct { int value; } OnceAlias;\n#endif\n'))!
	os.write_file(os.join_path(root, 'attributes.h'), 'typedef struct AttributeImpl AttributeAlias __attribute__((deprecated));\nstruct AttributeImpl { int value; };\n')!
	os.write_file(os.join_path(root, 'false_tokens.h'), '// typedef struct Wrong CommentAlias;\n#define MAKE_ALIAS typedef struct Wrong MacroAlias;\nstatic const char *example = "typedef struct Wrong StringAlias;";\n')!
	os.write_file(os.join_path(root, 'preinclude.h'), '#ifdef PREINCLUDE_FEATURE\ntypedef struct { int value; } PreincludeAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'repeat_angle.h'), header_owned_large_header('#ifdef REPEAT_FEATURE\ntypedef struct { int value; } RepeatedAlias;\n#endif\n'))!
	os.write_file(os.join_path(root, 'common.h'), '#ifdef HAS_COMMON_ALIAS\ntypedef struct { int value; } CommonAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#flag -DFEATURE_VALUE=7
#define PREINCLUDE_FEATURE
#preinclude "child_wrapper.h"
#preinclude "preinclude.h"
#include "valued.h"
#include "compiler.h"
#include "compiler_version.h"
#include "invoked_macro.h"
#include "function_macro.h"
#include "lazy_wrapper.h"
#include <once.h>
#define ONCE_FEATURE
#include <./once.h>
#include "attributes.h"
#include "false_tokens.h"
#include <repeat_angle.h>
#define REPEAT_FEATURE
#include <repeat_angle.h>
#if UNKNOWN_BRANCH
#define HAS_COMMON_ALIAS
#else
#define HAS_COMMON_ALIAS
#endif
#include "common.h"

@[typedef]
struct C.ValuedAlias { value int }
@[typedef]
struct C.CompilerAlias { value int }
@[typedef]
struct C.CompilerVersionAlias { value int }
@[typedef]
struct C.InvokedMacroAlias { value int }
@[typedef]
struct C.FunctionMacroAlias { value int }
@[typedef]
struct C.RevisitedAlias { value int }
@[typedef]
struct C.LazyAlias { value int }
@[typedef]
struct C.OnceAlias { value int }
@[typedef]
struct C.AttributeAlias { value int }
@[typedef]
struct C.MacroAlias { value int }
@[typedef]
struct C.PreincludeAlias { value int }
@[typedef]
struct C.RepeatedAlias { value int }
@[typedef]
struct C.CommonAlias { value int }

fn main() {
	values := [C.ValuedAlias{ value: 1 }.value, C.CompilerAlias{ value: 2 }.value,
		C.CompilerVersionAlias{ value: 11 }.value, C.InvokedMacroAlias{ value: 12 }.value,
		C.FunctionMacroAlias{ value: 13 }.value, C.RevisitedAlias{ value: 3 }.value,
		C.LazyAlias{ value: 4 }.value,
		C.OnceAlias{ value: 5 }.value, C.AttributeAlias{ value: 6 }.value,
		C.MacroAlias{ value: 7 }.value, C.PreincludeAlias{ value: 8 }.value,
		C.RepeatedAlias{ value: 9 }.value, C.CommonAlias{ value: 10 }.value]
	mut total := 0
	for value in values {
		total += value
	}
	println(total)
}
')!
	compile := os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '91', run.output
	generated := os.read_file(out + '.c')!
	for owned in ['ValuedAlias', 'CompilerAlias', 'CompilerVersionAlias', 'InvokedMacroAlias',
		'FunctionMacroAlias', 'RevisitedAlias', 'AttributeAlias', 'RepeatedAlias', 'CommonAlias'] {
		assert !generated.contains('struct ${owned} {'), generated
	}
	for fallback in ['LazyAlias', 'OnceAlias', 'MacroAlias', 'PreincludeAlias'] {
		assert generated.contains('struct ${fallback} {'), generated
	}
}

fn test_header_owned_typedefs_follow_import_emission_order() {
	v3_bin := header_owned_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_header_owned_module_order_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	out := os.join_path(root, 'out')
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'header_order' }\n")!
	os.write_file(os.join_path(root, 'order.h'), '#ifdef ORDER_FEATURE\ntypedef struct { int value; } OrderedAlias;\n#endif\n')!
	os.write_file(os.join_path(root, 'dep', 'dep.v'), 'module dep\n#undef ORDER_FEATURE\npub fn keep() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#define ORDER_FEATURE
import dep
#include "order.h"

@[typedef]
struct C.OrderedAlias { value int }

fn main() {
	dep.keep()
	println(C.OrderedAlias{ value: 42 }.value)
}
')!
	compile := os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(root)} -b c -o ${os.quoted_path(out)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
	generated := os.read_file(out + '.c')!
	assert generated.contains('struct OrderedAlias {'), generated
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
