import os
import v.tempname

fn test_header_owned_c_structs_and_repeated_includes() {
	root := os.join_path(os.vtmp_dir(), 'v3_header_owned_and_repeated_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	os.write_file(os.join_path(root, 'types.h'), '#ifndef V3_HEADER_OWNED_TYPES_H
#define V3_HEADER_OWNED_TYPES_H
typedef struct V3HeaderOwnedImpl_ {
	long long value;
} V3HeaderOwnedAlias;
typedef struct {
	long long value;
} _V3HeaderOwnedAlias;
typedef struct V3HeaderOwnedTag {
	long long value;
} V3HeaderOwnedTag;
#endif
')!
	os.write_file(os.join_path(root, 'wrapper.h'), '#include "types.h"\n')!
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
#include "wrapper.h"
#include "library.h"
#define V3_SINGLE_HEADER_IMPLEMENTATION
#include "library.h"

@[typedef]
struct C.V3HeaderOwnedAlias {
	value int
}

@[typedef]
struct C._V3HeaderOwnedAlias {
	value int
}

struct C.V3HeaderOwnedTag {
	value int
}

fn C.v3_single_header_value() int

fn main() {
	alias := C.V3HeaderOwnedAlias{
		value: 20
	}
	private_alias := C._V3HeaderOwnedAlias{
		value: 20
	}
	tag := C.V3HeaderOwnedTag{
		value: 2
	}
	println(alias.value + private_alias.value + tag.value)
	println(C.v3_single_header_value())
}
')!
	out := os.join_path(root, 'out')
	compile := os.exec([@VEXE, '--no-parallel', os.join_path(root, 'main.v'), '-b', 'c', '-o',
		'${out}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n42', run.output
	generated := os.read_file(out + '.c')!
	assert !generated.contains('typedef struct V3HeaderOwnedAlias V3HeaderOwnedAlias;'), generated
	assert !generated.contains('struct V3HeaderOwnedAlias {'), generated
	assert !generated.contains('struct _V3HeaderOwnedAlias'), generated
	assert !generated.contains('struct V3HeaderOwnedTag {'), generated

	// library.h sits beside main.v, and the generated C is compiled elsewhere,
	// so the include names the header by its resolved path.
	header := os.real_path(os.join_path(root, 'library.h')).replace('\\', '/')
	include := '#include "${header}"'
	assert generated.count(include) == 2, generated
	first_include := generated.index(include) or { -1 }
	define := generated.index('#define V3_SINGLE_HEADER_IMPLEMENTATION') or { -1 }
	assert first_include >= 0, generated
	assert define > first_include, generated
	assert generated[define..].contains(include), generated
}

fn test_macos_header_only_objective_c_uses_objective_c_translation_unit() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'v3_header_only_objective_c_${tempname.unique_token()}')
		os.mkdir(root)!
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(root, 'objective_c_api.h'), '#ifndef __OBJC__
#error this header requires an Objective-C translation unit
#endif
@interface V3HeaderOnlyObject
@end
')!
		os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -I @DIR
#include "objective_c_api.h"

fn main() {
	println(42)
}
')!
		out := os.join_path(root, 'out')
		compile := os.exec([@VEXE, '-nocache', '--no-parallel', '-cc', 'clang',
			os.join_path(root, 'main.v'), '-b', 'c', '-o', '${out}'])
		assert compile.exit_code == 0, compile.output
		run := os.exec([out])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '42', run.output
	}
}
