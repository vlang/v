import os

const directive_order_vexe = @VEXE
const directive_order_tests_dir = os.dir(@FILE)
const directive_order_v3_dir = os.dir(directive_order_tests_dir)
const directive_order_vlib_dir = os.dir(directive_order_v3_dir)
const directive_order_v3_src = os.join_path(directive_order_v3_dir, 'v3.v')

fn directive_order_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_directive_order_test')
	build :=
		os.execute('${directive_order_vexe} -gc none -path "${directive_order_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${directive_order_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn directive_order_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn directive_order_gen_c(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import sokol.f as _

fn main() {}
')
	directive_order_write_file(root, 'sokol/c/c.v', 'module c

#define SOKOL_GFX_IMPL
#include "sokol_gfx.h"
')
	directive_order_write_file(root, 'sokol/c/sokol_gfx.h', '')
	directive_order_write_file(root, 'sokol/f/f.v', 'module f

import sokol.c as _

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"
#include "sokol_gfx.h"
#ifdef KEEP_DUPLICATE_INCLUDE
#include "sokol_gfx.h"
#endif
')
	directive_order_write_file(root, 'sokol/f/util/sokol_fontstash.h', '')
	directive_order_write_file(root, 'sokol/f/sokol_gfx.h', '')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_dotted_collision(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_dotted_collision_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_dotted_collision' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import foo.user as _
import bar as _

fn main() {}
')
	directive_order_write_file(root, 'bar/bar.v', 'module bar

#define SHORT_BAR
')
	directive_order_write_file(root, 'foo/bar/bar.v', 'module bar

#define FOO_BAR
')
	directive_order_write_file(root, 'foo/user/user.v', 'module user

import foo.bar as _

#define FOO_USER
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_dotted_collision.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_importer_macro(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_importer_macro_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_importer_macro' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#define USE_FOO

import wrapper

fn main() {
	println(wrapper.value().str())
}
')
	directive_order_write_file(root, 'wrapper/wrapper.v', 'module wrapper

#insert "foo.h"

fn C.foo_value() int

pub fn value() int {
	return C.foo_value()
}
')
	directive_order_write_file(root, 'wrapper/foo.h', '#ifndef USE_FOO
#error "USE_FOO must be defined before foo.h"
#endif

static inline int foo_value(void) {
	return 42;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_importer_macro')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_dir_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_dir_header_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_dir_header' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(wrapper.value().str())
}
')
	directive_order_write_file(root, 'wrapper/wrapper.v', 'module wrapper

#insert "@DIR/dir_value.h"

fn C.dir_value() int

pub fn value() int {
	return C.dir_value()
}
')
	directive_order_write_file(root, 'wrapper/dir_value.h', 'static inline int dir_value(void) {
	return 24;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_dir_header')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '24', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_flag_include_dir_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_flag_include_dir_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_flag_include_dir' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#flag -I @DIR/include
#insert "flag_value.c"

fn C.flag_value() int

fn main() {
	println(C.flag_value().str())
}
')
	directive_order_write_file(root, 'include/flag_value.c', '#include <flag_value.h>

static inline int flag_value(void) {
	return flag_value_inner();
}
')
	directive_order_write_file(root, 'include/flag_value.h', 'static inline int flag_value_inner(void) {
	return 52;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_flag_include_dir')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '52', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_late_flag_include_dir_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_late_flag_include_dir_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_late_flag_include_dir' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "late_flag_value.c"
#flag -I @DIR/include

fn C.late_flag_value() int

fn main() {
	println(C.late_flag_value().str())
}
')
	directive_order_write_file(root, 'include/late_flag_value.c', '#include <late_flag_value.h>

static inline int late_flag_value(void) {
	return late_flag_value_inner();
}
')
	directive_order_write_file(root, 'include/late_flag_value.h', 'static inline int late_flag_value_inner(void) {
	return 63;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_late_flag_include_dir')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '63', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_multiline_static_inline(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_multiline_static_inline_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_multiline_static_inline' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "multiline_value.h"

fn C.multiline_value() int

fn main() {
	println(C.multiline_value().str())
}
')
	directive_order_write_file(root, 'multiline_value.h', 'static inline
int multiline_value(void) {
	return 33;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_multiline_static_inline')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '33', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_extern_after_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_extern_after_header_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_extern_after_header' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "native_thing.h"

@[typedef]
struct C.NativeThing {
	value int
}

fn C.native_use(item &C.NativeThing) int

fn main() {
	mut item := C.NativeThing{
		value: 7
	}
	_ := C.native_use(&item)
}
')
	directive_order_write_file(root, 'native_thing.h', 'typedef struct NativeThing {
	int value;
} NativeThing;
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_extern_after_header.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_header_declared_prototype(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_header_declared_proto_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_header_declared_proto' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "declared_proto.h"

fn C.declared_text() &char

fn main() {
	_ := C.declared_text()
}
')
	directive_order_write_file(root, 'declared_proto.h', 'const char* declared_text(void);
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_header_declared_proto.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_struct_field_after_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_struct_field_after_header_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_struct_field_after_header' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "field_thing.h"

@[typedef]
struct C.FieldThing {
	value int
}

struct Wrap {
	item C.FieldThing
}

fn main() {
	_ := Wrap{}
}
')
	directive_order_write_file(root, 'field_thing.h', 'typedef struct {
	int value;
} FieldThing;
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_struct_field_after_header.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_anonymous_typedef(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_anonymous_typedef_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_anonymous_typedef' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "anonymous_thing.h"

@[typedef]
struct C.AnonymousThing {
	value int
}

fn C.anonymous_value(item &C.AnonymousThing) int

fn main() {
	mut item := C.AnonymousThing{
		value: 31
	}
	println(C.anonymous_value(&item).str())
}
')
	directive_order_write_file(root, 'anonymous_thing.h', 'typedef struct {
	int value;
} AnonymousThing;

static inline int anonymous_value(AnonymousThing* item) {
	return item->value;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_anonymous_typedef')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '31', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_tagged_typedef_alias(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_tagged_typedef_alias_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_tagged_typedef_alias' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "tagged_thing.h"

@[typedef]
struct C.TaggedAlias {
	value int
}

fn C.tagged_value(item &C.TaggedAlias) int

fn main() {
	mut item := C.TaggedAlias{
		value: 43
	}
	println(C.tagged_value(&item).str())
}
')
	directive_order_write_file(root, 'tagged_thing.h', 'typedef struct TaggedImpl {
	int value;
} TaggedAlias;

static inline int tagged_value(TaggedAlias* item) {
	return item->value;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_tagged_typedef_alias')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '43', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_union_typedef_aliases(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_union_typedef_aliases_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_union_typedef_aliases' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "union_thing.h"

@[typedef]
union C.AnonymousUnion {
	value int
}

@[typedef]
union C.TaggedUnionAlias {
	value int
}

struct WrapUnion {
	anonymous C.AnonymousUnion
	tagged    C.TaggedUnionAlias
}

fn main() {
	_ := WrapUnion{}
}
')
	directive_order_write_file(root, 'union_thing.h', 'typedef union {
	int value;
} AnonymousUnion;

typedef union TaggedUnionImpl {
	int value;
} TaggedUnionAlias;
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_union_typedef_aliases.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_nested_include(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_nested_include_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_nested_include' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "outer.h"

@[typedef]
struct C.NestedThing {
	value int
}

fn C.nested_value(item &C.NestedThing) int

fn main() {
	mut item := C.NestedThing{
		value: 11
	}
	println(C.nested_value(&item).str())
}
')
	directive_order_write_file(root, 'outer.h', '#include "types.h"

static inline int nested_value(NestedThing* item) {
	return NESTED_BIAS + item->value;
}
')
	directive_order_write_file(root, 'types.h', '#include <stdint.h>

#define NESTED_BIAS 5

typedef struct {
	uint32_t value;
} NestedThing;
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_nested_include')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '16', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_unresolved_system_include(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_unresolved_system_include_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_unresolved_system_include' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <platform_user_header.h>
#define USE_DLFCN
#define PLATFORM_API_COMPAT 7
#ifdef USE_DLFCN
#include <dlfcn.h>
#endif
#include <stdint.h>

fn C.dlopen(filename &char, flags i32) voidptr

fn main() {
	nil_cstr := &char(unsafe { nil })
	_ = C.dlopen(nil_cstr, 0)
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_unresolved_system_include.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_unresolved_quoted_include(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_unresolved_quoted_include_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_unresolved_quoted_include' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include "external_missing.h"
#define AFTER_QUOTED_INCLUDE 1

fn main() {}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_unresolved_quoted_include.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_nested_system_include(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_nested_system_include_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_nested_system_include' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "nested_preserved.h"

fn main() {}
')
	directive_order_write_file(root, 'nested_preserved.h', '#if defined(__linux__)
#define NESTED_PLATFORM_HEADER 1
#include <nested_platform_header.h>
#endif

#include <stdint.h>

typedef uint64_t NestedWord;
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_nested_system_include.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_headerless_x11_aggregates(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_x11_aggregates_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_x11_aggregates' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <X11/Xlib.h>

@[typedef]
union C.XClientMessageData {
mut:
	l [5]i64
}

@[typedef]
union C.XEvent {
mut:
	@type   int
	xclient C.XClientMessageData
}

struct Wrap {
	event C.XEvent
}

fn main() {
	_ := Wrap{}
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_x11_aggregates.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_headerless_bcrypt_fn(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_bcrypt_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_bcrypt' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <bcrypt.h>

fn C.BCryptGenRandom(alg int, buffer voidptr, len int, flags int) int

fn main() {
	_ := C.BCryptGenRandom(0, voidptr(0), 0, 0)
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_bcrypt.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_headerless_mach_headers(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_mach_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_mach' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <mach/mach.h>
#include <mach/mach_time.h>

@[typedef]
struct C.mach_timebase_info_data_t {
	numer u32
	denom u32
}

fn C.mach_timebase_info(&C.mach_timebase_info_data_t)

fn main() {
	mut tb := C.mach_timebase_info_data_t{}
	C.mach_timebase_info(&tb)
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_mach.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_task_info_reference(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_task_info_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_task_info' }\n")
	directive_order_write_file(root, 'main.v', 'module main

fn C.task_info() int

fn main() {
	_ := C.task_info()
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_task_info.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_headerless_timerfd_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_timerfd_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_timerfd' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <sys/timerfd.h>

pub struct C.timespec {
	tv_sec  i64
	tv_nsec i64
}

pub struct C.itimerspec {
	it_interval C.timespec
	it_value    C.timespec
}

fn C.clock_gettime(clock_id i32, ts &C.timespec) i32
fn C.nanosleep(req &C.timespec, rem &C.timespec) i32
fn C.timerfd_create(clockid int, flags int) int
fn C.timerfd_settime(fd int, flags int, new_value &C.itimerspec, old_value &C.itimerspec) int
fn C.timerfd_gettime(fd int, curr_value &C.itimerspec) int

fn main() {
	req := C.timespec{}
	C.clock_gettime(C.CLOCK_MONOTONIC, &req)
	C.nanosleep(&req, unsafe { nil })
	mut spec := C.itimerspec{}
	fd := C.timerfd_create(C.CLOCK_MONOTONIC, C.TFD_CLOEXEC | C.TFD_NONBLOCK)
	C.timerfd_settime(fd, 0, &spec, unsafe { nil })
	C.timerfd_gettime(fd, &spec)
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_timerfd.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_stdarg_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_stdarg_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_stdarg' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include "stdarg_user.h"

fn C.stdarg_sum(count int, ...int) int

fn main() {
	println(C.stdarg_sum(3, 1, 2, 3).str())
}
')
	directive_order_write_file(root, 'stdarg_user.h', '#include <stdarg.h>
#include <stddef.h>

struct StdargThing {
	char tag;
	int value;
};

static inline int stdarg_sum(int count, ...) {
	va_list ap;
	va_start(ap, count);
	int total = (int)offsetof(struct StdargThing, value);
	total -= (int)offsetof(struct StdargThing, value);
	for (int i = 0; i < count; i++) {
		total += va_arg(ap, int);
	}
	va_end(ap);
	return total;
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_stdarg')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '6', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_inttypes_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_inttypes_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_inttypes' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include <stdint.h>
#include "inttypes_user.h"

fn C.inttypes_macro_widths() int

fn main() {
	println(C.inttypes_macro_widths().str())
}
')
	directive_order_write_file(root, 'inttypes_macros.h', '#include <inttypes.h>
')
	directive_order_write_file(root, 'inttypes_user.h', '#include "inttypes_macros.h"

static inline int inttypes_macro_widths(void) {
	return (int) (sizeof(PRId64) + sizeof(PRIuPTR) + sizeof(SCNi64));
}
')
	bin_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_inttypes')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_out}')
	assert result.exit_code == 0, result.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().int() > 0, run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_nested_poll_header(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_poll_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_poll' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "poll_user.h"

struct C.pollfd {
	fd int
	events i16
	revents i16
}

fn C.poll_user_fd(item &C.pollfd) int

fn main() {}
')
	directive_order_write_file(root, 'poll_user.h', '#include <poll.h>

static inline int poll_user_fd(struct pollfd* item) {
	return item->fd;
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_poll.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_rwmutex(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_rwmutex_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_rwmutex' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import sync

fn main() {
	mut m := sync.new_rwmutex()
	m.lock()
	m.unlock()
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_rwmutex.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_shared_runtime(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_shared_runtime_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_shared_runtime' }\n")
	directive_order_write_file(root, 'main.v', 'module main

struct SharedOnly {
	a shared int
}

fn main() {
	_ := SharedOnly{}
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_shared_runtime.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_request_extern(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_request_extern_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_request_extern' }\n")
	directive_order_write_file(root, 'main.v', 'module main

fn C.request(code int) int

fn main() {
	_ := C.request(7)
}
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order_request_extern.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_index(c_code string, needle string) int {
	return c_code.index(needle) or { -1 }
}

fn directive_order_count(c_code string, needle string) int {
	mut count := 0
	mut rest := c_code
	for {
		idx := rest.index(needle) or { break }
		count++
		rest = rest[idx + needle.len..]
	}
	return count
}

fn directive_order_has_include_directive(c_code string) bool {
	for line in c_code.split_into_lines() {
		clean := line.trim_space()
		if clean.starts_with('#include') {
			return true
		}
	}
	return false
}

fn test_c_directives_follow_import_dependency_order() {
	c_code := directive_order_gen_c(directive_order_build_v3())
	gfx_define := directive_order_index(c_code, '#define SOKOL_GFX_IMPL')
	fontstash_define := directive_order_index(c_code, '#define SOKOL_FONTSTASH_IMPL')
	block_start := directive_order_index(c_code, '#ifdef KEEP_DUPLICATE_INCLUDE')
	assert gfx_define >= 0, c_code
	assert fontstash_define >= 0, c_code
	assert block_start >= 0, c_code
	assert gfx_define < fontstash_define, c_code
	assert !directive_order_has_include_directive(c_code), c_code
	block_tail := c_code[block_start..]
	block_end := directive_order_index(block_tail, '#endif')
	assert block_end >= 0, c_code
}

fn test_c_directives_preserve_dotted_import_module_identity() {
	c_code := directive_order_gen_c_dotted_collision(directive_order_build_v3())
	short_bar := directive_order_index(c_code, '#define SHORT_BAR')
	foo_bar := directive_order_index(c_code, '#define FOO_BAR')
	foo_user := directive_order_index(c_code, '#define FOO_USER')
	assert short_bar >= 0, c_code
	assert foo_bar >= 0, c_code
	assert foo_user >= 0, c_code
	assert foo_bar < foo_user, c_code
}

fn test_importer_macro_is_emitted_before_dependency_include() {
	c_code := directive_order_gen_and_run_importer_macro(directive_order_build_v3())
	use_foo := directive_order_index(c_code, '#define USE_FOO')
	foo_header := directive_order_index(c_code, 'static inline int foo_value')
	assert use_foo >= 0, c_code
	assert foo_header >= 0, c_code
	assert use_foo < foo_header, c_code
	assert !c_code.contains('int foo_value(void);'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_dir_include_is_expanded_before_header_probe() {
	c_code := directive_order_gen_and_run_dir_header(directive_order_build_v3())
	dir_header := directive_order_index(c_code, 'static inline int dir_value')
	assert dir_header >= 0, c_code
	assert !c_code.contains('int dir_value(void);'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_quoted_include_uses_flag_include_dirs() {
	c_code := directive_order_gen_and_run_flag_include_dir_header(directive_order_build_v3())
	flag_header := directive_order_index(c_code, 'static inline int flag_value')
	assert flag_header >= 0, c_code
	assert directive_order_index(c_code, 'static inline int flag_value_inner') >= 0, c_code
	assert !c_code.contains('int flag_value(void);'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_quoted_include_uses_later_flag_include_dirs() {
	c_code := directive_order_gen_and_run_late_flag_include_dir_header(directive_order_build_v3())
	flag_header := directive_order_index(c_code, 'static inline int late_flag_value')
	assert flag_header >= 0, c_code
	assert directive_order_index(c_code, 'static inline int late_flag_value_inner') >= 0, c_code

	assert !c_code.contains('int late_flag_value(void);'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_multiline_static_inline_header_is_not_redeclared() {
	c_code := directive_order_gen_and_run_multiline_static_inline(directive_order_build_v3())
	header_idx := directive_order_index(c_code, 'static inline\nint multiline_value')
	assert header_idx >= 0, c_code
	assert !c_code.contains('int multiline_value(void);'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_inlined_headers_are_emitted_before_extern_prototypes() {
	c_code := directive_order_gen_c_extern_after_header(directive_order_build_v3())
	header_idx := directive_order_index(c_code, 'typedef struct NativeThing')
	proto_idx := directive_order_index(c_code, 'int native_use(NativeThing*')
	assert header_idx >= 0, c_code
	assert proto_idx >= 0, c_code
	assert header_idx < proto_idx, c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_header_declared_prototypes_are_not_redeclared() {
	c_code := directive_order_gen_c_header_declared_prototype(directive_order_build_v3())
	header_idx := directive_order_index(c_code, 'const char* declared_text(void);')
	assert header_idx >= 0, c_code
	assert directive_order_count(c_code, 'declared_text(void);') == 1, c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_inlined_headers_are_emitted_before_type_declarations() {
	c_code := directive_order_gen_c_struct_field_after_header(directive_order_build_v3())
	header_idx := directive_order_index(c_code, '} FieldThing;')
	wrap_idx := directive_order_index(c_code, 'struct Wrap {')
	assert header_idx >= 0, c_code
	assert wrap_idx >= 0, c_code
	assert header_idx < wrap_idx, c_code
	assert !c_code.contains('typedef struct FieldThing'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_anonymous_typedef_struct_header_is_not_duplicated() {
	c_code := directive_order_gen_and_run_anonymous_typedef(directive_order_build_v3())
	assert c_code.contains('} AnonymousThing;'), c_code
	assert c_code.contains('static inline int anonymous_value'), c_code
	assert !c_code.contains('typedef struct AnonymousThing'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_tagged_typedef_struct_alias_header_is_not_duplicated() {
	c_code := directive_order_gen_and_run_tagged_typedef_alias(directive_order_build_v3())
	assert c_code.contains('} TaggedAlias;'), c_code
	assert c_code.contains('static inline int tagged_value'), c_code
	assert !c_code.contains('typedef struct TaggedAlias'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_inlined_typedef_union_headers_are_not_duplicated() {
	c_code := directive_order_gen_c_union_typedef_aliases(directive_order_build_v3())
	assert c_code.contains('} AnonymousUnion;'), c_code
	assert c_code.contains('} TaggedUnionAlias;'), c_code
	assert !c_code.contains('typedef union AnonymousUnion'), c_code
	assert !c_code.contains('union AnonymousUnion {\n'), c_code
	assert !c_code.contains('typedef union TaggedUnionAlias'), c_code
	assert !c_code.contains('union TaggedUnionAlias {\n'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_nested_local_header_includes_are_inlined_recursively() {
	c_code := directive_order_gen_and_run_nested_include(directive_order_build_v3())
	bias_idx := directive_order_index(c_code, '#define NESTED_BIAS 5')
	stdint_idx := directive_order_index(c_code, 'typedef unsigned int uint32_t;')
	typedef_idx := directive_order_index(c_code, '} NestedThing;')
	outer_fn_idx := directive_order_index(c_code, 'static inline int nested_value')
	assert bias_idx >= 0, c_code
	assert stdint_idx >= 0, c_code
	assert typedef_idx >= 0, c_code
	assert outer_fn_idx >= 0, c_code
	assert stdint_idx < typedef_idx, c_code
	assert bias_idx < outer_fn_idx, c_code
	assert typedef_idx < outer_fn_idx, c_code
	assert !c_code.contains('typedef struct NestedThing'), c_code
	assert !directive_order_has_include_directive(c_code), c_code
}

fn test_unresolved_system_include_is_dropped() {
	c_code := directive_order_gen_c_unresolved_system_include(directive_order_build_v3())
	assert !directive_order_has_include_directive(c_code), c_code
	assert !c_code.contains('#include <platform_user_header.h>'), c_code
	assert !c_code.contains('#include <dlfcn.h>'), c_code
	assert !c_code.contains('#include <stdint.h>'), c_code
	assert c_code.contains('typedef unsigned int uint32_t;'), c_code
	assert c_code.contains('void* dlopen('), c_code
	api_compat_idx := directive_order_index(c_code, '#define PLATFORM_API_COMPAT 7')
	guard_idx := directive_order_index(c_code, '#ifdef USE_DLFCN')
	time_t_idx := directive_order_index(c_code, 'typedef long long time_t;')
	off_t_idx := directive_order_index(c_code, 'typedef long long off_t;')
	wchar_idx := directive_order_index(c_code, 'typedef unsigned int wchar_t;')
	fd_set_idx := directive_order_index(c_code, '#ifndef FD_SET')
	assert api_compat_idx >= 0, c_code
	assert guard_idx >= 0, c_code
	assert time_t_idx >= 0, c_code
	assert off_t_idx >= 0, c_code
	assert wchar_idx >= 0, c_code
	assert fd_set_idx >= 0, c_code
	assert c_code.contains('#if !defined(_TIME_T) && !defined(_TIME_T_DEFINED) && !defined(__time_t_defined)'), c_code
	assert c_code.contains('#if !defined(_OFF_T) && !defined(_OFF_T_DEFINED) && !defined(__off_t_defined)'), c_code
}

fn test_unresolved_quoted_include_is_preserved() {
	c_code := directive_order_gen_c_unresolved_quoted_include(directive_order_build_v3())
	include_idx := directive_order_index(c_code, '#include "external_missing.h"')
	define_idx := directive_order_index(c_code, '#define AFTER_QUOTED_INCLUDE 1')
	assert include_idx >= 0, c_code
	assert define_idx >= 0, c_code
	assert include_idx < define_idx, c_code
}

fn test_nested_system_include_is_dropped_from_inlined_header() {
	c_code := directive_order_gen_c_nested_system_include(directive_order_build_v3())
	include_idx := directive_order_index(c_code, '#include <nested_platform_header.h>')
	preamble_idx := directive_order_index(c_code, 'typedef signed char i8;')
	stdint_guard_idx := directive_order_index(c_code,
		'#if !defined(__V_HEADERLESS_STDINT_H) && !defined(_STDINT_H)')
	nested_define_idx := directive_order_index(c_code, '#define NESTED_PLATFORM_HEADER 1')
	nested_word_idx := directive_order_index(c_code, 'typedef uint64_t NestedWord;')
	assert include_idx == -1, c_code
	assert !directive_order_has_include_directive(c_code), c_code
	assert preamble_idx >= 0, c_code
	assert stdint_guard_idx >= 0, c_code
	assert nested_define_idx >= 0, c_code
	assert nested_word_idx >= 0, c_code
	assert preamble_idx < stdint_guard_idx, c_code
	assert stdint_guard_idx < nested_word_idx, c_code
	assert nested_define_idx < stdint_guard_idx, c_code
}

fn test_system_header_aggregates_are_emitted_headerlessly() {
	c_code := directive_order_gen_c_headerless_x11_aggregates(directive_order_build_v3())
	assert !directive_order_has_include_directive(c_code), c_code
	assert !c_code.contains('#include <X11/Xlib.h>'), c_code
	assert c_code.contains('typedef union XEvent XEvent;'), c_code
	assert c_code.contains('union XEvent {\n'), c_code
	assert c_code.contains('typedef union XClientMessageData XClientMessageData;'), c_code
	assert c_code.contains('union XClientMessageData {\n'), c_code
}

fn test_system_header_functions_are_emitted_headerlessly() {
	c_code := directive_order_gen_c_headerless_bcrypt_fn(directive_order_build_v3())
	assert !directive_order_has_include_directive(c_code), c_code
	assert !c_code.contains('#include <bcrypt.h>'), c_code
	assert directive_order_count(c_code, 'BCryptGenRandom(') == 2, c_code
}

fn test_mach_headers_are_emitted_headerlessly() {
	c_code := directive_order_gen_c_headerless_mach_headers(directive_order_build_v3())
	preamble_idx := directive_order_index(c_code, 'typedef signed char i8;')
	assert preamble_idx >= 0, c_code
	assert !directive_order_has_include_directive(c_code), c_code
	assert !c_code.contains('#include <mach/mach.h>'), c_code
	assert !c_code.contains('#include <mach/mach_time.h>'), c_code
	assert !c_code.contains('#define panic mach_panic'), c_code
	assert c_code.contains('typedef unsigned int task_t;'), c_code
	assert c_code.contains('struct task_basic_info {'), c_code
	assert c_code.contains('#define KERN_SUCCESS 0'), c_code
	assert c_code.contains('#define MACH_TASK_BASIC_INFO_COUNT 12'), c_code
	assert c_code.contains('#define TASK_BASIC_INFO 18'), c_code
	assert c_code.contains('typedef struct mach_timebase_info_data_t mach_timebase_info_data_t;'), c_code
	assert c_code.contains('struct mach_timebase_info_data_t {'), c_code
	assert c_code.contains('void mach_timebase_info('), c_code
}

fn test_inferred_mach_headers_are_target_guarded() {
	c_code := directive_order_gen_c_task_info_reference(directive_order_build_v3())
	assert c_code.contains('#ifdef __APPLE__\n#define panic mach_panic\n#include <mach/mach.h>\n#undef panic\n#include <mach/task.h>\n#endif'), c_code
	assert c_code.contains('#ifdef __APPLE__\n#ifndef _MACH_TASK_INFO_H_\ntypedef unsigned int task_t;'), c_code
	assert c_code.contains('#ifndef __APPLE__\nint task_info(void);\n#endif'), c_code
}

fn test_timerfd_header_uses_headerless_decls() {
	c_code := directive_order_gen_c_headerless_timerfd_header(directive_order_build_v3())
	assert !c_code.contains('#include'), c_code
	assert c_code.contains('struct itimerspec {'), c_code
	assert c_code.contains('struct tm {'), c_code
	assert c_code.contains('i32 clock_gettime('), c_code
	assert c_code.contains('i32 nanosleep('), c_code
	assert c_code.contains('int timerfd_create('), c_code
	assert c_code.contains('#define TFD_CLOEXEC O_CLOEXEC'), c_code
	assert c_code.contains('#define TFD_NONBLOCK O_NONBLOCK'), c_code
}

fn test_stdarg_in_inlined_header_uses_headerless_va_defs() {
	c_code := directive_order_gen_and_run_stdarg_header(directive_order_build_v3())
	assert !c_code.contains('#include <stdarg.h>'), c_code
	assert !c_code.contains('#include <stddef.h>'), c_code
	assert c_code.contains('typedef __builtin_va_list va_list;'), c_code
	assert c_code.contains('#define va_start(ap, last) __builtin_va_start(ap, last)'), c_code
	assert c_code.contains('#define va_arg(ap, type) __builtin_va_arg(ap, type)'), c_code
	assert c_code.contains('#define offsetof(type, member) __builtin_offsetof(type, member)'), c_code
	assert c_code.contains('offsetof(struct StdargThing, value)'), c_code
	assert c_code.contains('static inline int stdarg_sum(int count, ...)'), c_code
}

fn test_inttypes_in_inlined_header_keeps_format_macros() {
	c_code := directive_order_gen_and_run_inttypes_header(directive_order_build_v3())
	assert c_code.contains('#include <stdint.h>'), c_code
	assert c_code.contains('#include <inttypes.h>'), c_code
	assert !c_code.contains('__V_HEADERLESS_STDINT_H'), c_code
	assert c_code.contains('sizeof(PRId64) + sizeof(PRIuPTR) + sizeof(SCNi64)'), c_code
}

fn test_poll_in_inlined_header_uses_headerless_struct() {
	c_code := directive_order_gen_c_nested_poll_header(directive_order_build_v3())
	assert !directive_order_has_include_directive(c_code), c_code
	assert !c_code.contains('#include <poll.h>'), c_code
	assert c_code.contains('static inline int poll_user_fd(struct pollfd* item)'), c_code
	assert c_code.contains('struct pollfd {\n'), c_code
}

fn test_rwmutex_keeps_linux_rwlockattr_prototype() {
	c_code := directive_order_gen_c_rwmutex(directive_order_build_v3())
	assert c_code.contains('int pthread_rwlockattr_setkind_np('), c_code
	assert c_code.contains('int pthread_rwlockattr_init('), c_code
	assert c_code.contains('int pthread_rwlockattr_destroy('), c_code
	assert c_code.contains('int pthread_rwlock_init('), c_code
	assert c_code.contains('int pthread_rwlock_wrlock('), c_code
	assert c_code.contains('int pthread_rwlock_unlock('), c_code
	assert !c_code.contains('i32 pthread_rwlock'), c_code
}

fn test_shared_runtime_keeps_rwmutex_init_prototypes() {
	c_code := directive_order_gen_c_shared_runtime(directive_order_build_v3())
	$if !windows {
		assert c_code.contains('pthread_rwlockattr_init(void*)'), c_code
		assert c_code.contains('pthread_rwlockattr_setkind_np(void*, i32)'), c_code
		assert c_code.contains('pthread_rwlock_init(void*, void*)'), c_code
	}
}

fn test_request_named_c_function_keeps_extern_prototype() {
	c_code := directive_order_gen_c_request_extern(directive_order_build_v3())
	assert c_code.contains('int request(int'), c_code
}
