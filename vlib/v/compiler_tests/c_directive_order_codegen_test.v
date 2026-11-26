import os
import v.tempname

fn directive_order_fixture_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), '${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn directive_order_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn directive_order_gen_c(root string, portable bool) string {
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

import sokol.gfx as _

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"
#include "sokol_gfx.h"
#ifdef KEEP_DUPLICATE_INCLUDE
#include "sokol_gfx.h"
#endif
')
	directive_order_write_file(root, 'sokol/gfx/gfx.v', 'module gfx

import sokol.c as _
')
	directive_order_write_file(root, 'sokol/f/util/sokol_fontstash.h', '')
	directive_order_write_file(root, 'sokol/f/sokol_gfx.h', '')
	c_out := os.join_path(root, 'v3_c_directive_order.c')
	mut args := [@VEXE, '-b', 'c', '-o', c_out]
	if portable {
		args << '-cross'
	}
	args << os.join_path(root, 'main.v')
	result := os.exec(args)
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_dotted_collision() string {
	root := directive_order_fixture_dir('v3_c_directive_order_dotted_collision_project')
	defer { os.rmdir_all(root) or {} }
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
	c_out := os.join_path(root, 'v3_c_directive_order_dotted_collision.c')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_local_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_local_headers_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_local_headers' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#define USE_FOO

import wrapper
import dir_wrapper

#insert "multiline_value.h"

fn C.multiline_value() int

fn main() {
	println(wrapper.value().str())
	println(dir_wrapper.value().str())
	println(C.multiline_value().str())
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
	directive_order_write_file(root, 'dir_wrapper/dir_wrapper.v', 'module dir_wrapper

#insert "@DIR/dir_value.h"

fn C.dir_value() int

pub fn value() int {
	return C.dir_value()
}
')
	directive_order_write_file(root, 'dir_wrapper/dir_value.h', 'static inline int dir_value(void) {
	return 24;
}
')
	directive_order_write_file(root, 'multiline_value.h', 'static inline
int multiline_value(void) {
	return 33;
}
')
	bin_out := os.join_path(root, 'v3_c_directive_order_local_headers')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n24\n33', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_and_run_flag_include_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_flag_include_headers_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_flag_include_headers' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#flag -I @DIR/early_include
#insert "flag_value.c"

fn C.flag_value() int

#insert "late_flag_value.c"
#flag -I @DIR/late_include

fn C.late_flag_value() int

fn main() {
	println(C.flag_value().str())
	println(C.late_flag_value().str())
}
')
	directive_order_write_file(root, 'early_include/flag_value.c', '#include <flag_value.h>

static inline int flag_value(void) {
	return flag_value_inner();
}
')
	directive_order_write_file(root, 'early_include/flag_value.h', 'static inline int flag_value_inner(void) {
	return 52;
}
')
	directive_order_write_file(root, 'late_include/late_flag_value.c', '#include <late_flag_value.h>

static inline int late_flag_value(void) {
	return late_flag_value_inner();
}
')
	directive_order_write_file(root, 'late_include/late_flag_value.h', 'static inline int late_flag_value_inner(void) {
	return 63;
}
')
	bin_out := os.join_path(root, 'v3_c_directive_order_flag_include_headers')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '52\n63', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_extern_after_header() string {
	root := directive_order_fixture_dir('v3_c_directive_order_extern_after_header_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_extern_after_header' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "native_thing.h"
#flag @VMODROOT/native_use.c

@[typedef]
struct C.NativeThing {
	value int
}

@[c_extern]
fn C.native_use(item &C.NativeThing) int

fn main() {
	mut item := C.NativeThing{
		value: 7
	}
	println(C.native_use(&item).str())
}
')
	directive_order_write_file(root, 'native_thing.h', 'typedef struct NativeThing {
	int value;
} NativeThing;
')
	directive_order_write_file(root, 'native_use.c', '#include "native_thing.h"

int native_use(NativeThing* item) { return item->value; }
')
	bin_out := os.join_path(root, 'v3_c_directive_order_extern_after_header')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '7', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_header_declarations() string {
	root := directive_order_fixture_dir('v3_c_directive_order_header_declarations_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod',
		"Module { name: 'directive_order_header_declarations' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "declared_proto.h"

fn C.declared_text() &char

#insert "field_thing.h"

@[typedef]
struct C.FieldThing {
	value int
}

struct Wrap {
	item C.FieldThing
}

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
	_ := C.declared_text()
	_ := Wrap{}
	_ := WrapUnion{}
}
')
	directive_order_write_file(root, 'declared_proto.h', 'const char* declared_text(void);\n')
	directive_order_write_file(root, 'field_thing.h', 'typedef struct {
	int value;
} FieldThing;
')
	directive_order_write_file(root, 'union_thing.h', 'typedef union {
	int value;
} AnonymousUnion;

typedef union TaggedUnionImpl {
	int value;
} TaggedUnionAlias;
')
	c_out := os.join_path(root, 'v3_c_directive_order_header_declarations.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', c_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_header_typedefs() string {
	root := directive_order_fixture_dir('v3_c_directive_order_header_typedefs_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_header_typedefs' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#insert "anonymous_thing.h"

@[typedef]
struct C.AnonymousThing {
	value int
}

fn C.anonymous_value(item &C.AnonymousThing) int

#insert "tagged_thing.h"

@[typedef]
struct C.TaggedAlias {
	value int
}

fn C.tagged_value(item &C.TaggedAlias) int

#insert "outer.h"

@[typedef]
struct C.NestedThing {
	value int
}

fn C.nested_value(item &C.NestedThing) int

fn main() {
	mut anonymous_item := C.AnonymousThing{
		value: 31
	}
	println(C.anonymous_value(&anonymous_item).str())

	mut tagged_item := C.TaggedAlias{
		value: 43
	}
	println(C.tagged_value(&tagged_item).str())

	mut nested_item := C.NestedThing{
		value: 11
	}
	println(C.nested_value(&nested_item).str())
}
')
	directive_order_write_file(root, 'anonymous_thing.h', 'typedef struct {
	int value;
} AnonymousThing;

static inline int anonymous_value(AnonymousThing* item) {
	return item->value;
}
')
	directive_order_write_file(root, 'tagged_thing.h', 'typedef struct TaggedImpl {
	int value;
} TaggedAlias;

static inline int tagged_value(TaggedAlias* item) {
	return item->value;
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
	bin_out := os.join_path(root, 'v3_c_directive_order_header_typedefs')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '31\n43\n16', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_unresolved_system_include() string {
	root := directive_order_fixture_dir('v3_c_directive_order_unresolved_system_include_project')
	defer { os.rmdir_all(root) or {} }
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
	c_out := os.join_path(root, 'v3_c_directive_order_unresolved_system_include.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_opaque_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_opaque_headers_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_opaque_headers' }\n")
	directive_order_write_file(root, 'main.v', 'module main

#include "external_missing.h"
#define AFTER_QUOTED_INCLUDE 1

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
	c_out := os.join_path(root, 'v3_c_directive_order_opaque_headers.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', c_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_sdk_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_sdk_headers_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_sdk_headers' }\n")
	directive_order_write_file(root, 'main.v', 'module main

fn main() {
	check_x11()
	check_bcrypt()
}
')
	directive_order_write_file(root, 'x11.v', 'module main

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

fn check_x11() {
	_ := Wrap{}
}
')
	directive_order_write_file(root, 'bcrypt.v', 'module main

#include <bcrypt.h>

fn C.BCryptGenRandom(alg int, buffer voidptr, len int, flags int) int

fn check_bcrypt() {
	_ := C.BCryptGenRandom(0, voidptr(0), 0, 0)
}
')
	c_out := os.join_path(root, 'v3_c_directive_order_sdk_headers.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', c_out, root])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_sdk_mach_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_mach_project')
	defer { os.rmdir_all(root) or {} }
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
	c_out := os.join_path(root, 'v3_c_directive_order_mach.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_task_info_reference() string {
	root := directive_order_fixture_dir('v3_c_directive_order_task_info_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_task_info' }\n")
	directive_order_write_file(root, 'main.v', 'module main

fn C.task_info() int

fn main() {
	_ := C.task_info()
}
')
	c_out := os.join_path(root, 'v3_c_directive_order_task_info.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_sdk_timerfd_header() string {
	root := directive_order_fixture_dir('v3_c_directive_order_timerfd_project')
	defer { os.rmdir_all(root) or {} }
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
	c_out := os.join_path(root, 'v3_c_directive_order_timerfd.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_and_run_standard_headers() string {
	root := directive_order_fixture_dir('v3_c_directive_order_standard_headers_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_standard_headers' }\n")
	directive_order_write_file(root, 'main.v', 'module main

fn main() {
	println(C.stdarg_sum(3, 1, 2, 3).str())
	println(C.inttypes_macro_widths().str())
}
')
	directive_order_write_file(root, 'stdarg.v', 'module main

#include "stdarg_user.h"

fn C.stdarg_sum(count int, ...int) int
')
	directive_order_write_file(root, 'inttypes.v', 'module main

#include <stdint.h>
#include "inttypes_user.h"

fn C.inttypes_macro_widths() int
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
	directive_order_write_file(root, 'inttypes_macros.h', '#include <inttypes.h>\n')
	directive_order_write_file(root, 'inttypes_user.h', '#include "inttypes_macros.h"

static inline int inttypes_macro_widths(void) {
	return (int) (sizeof(PRId64) + sizeof(PRIuPTR) + sizeof(SCNi64));
}
')
	bin_out := os.join_path(root, 'v3_c_directive_order_standard_headers')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, root])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	values := run.output.trim_space().split_into_lines()
	assert values.len == 2, run.output
	assert values[0] == '6', run.output
	assert values[1].int() > 0, run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_nested_poll_header() string {
	root := directive_order_fixture_dir('v3_c_directive_order_poll_project')
	defer { os.rmdir_all(root) or {} }
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
	c_out := os.join_path(root, 'v3_c_directive_order_poll.c')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_rwmutex() string {
	root := directive_order_fixture_dir('v3_c_directive_order_rwmutex_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_rwmutex' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import sync

fn main() {
	mut m := sync.new_rwmutex()
	m.lock()
	m.unlock()
	println("OK")
}
')
	bin_out := os.join_path(root, 'v3_c_directive_order_rwmutex')
	result := os.exec([@VEXE, '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
	return os.read_file(bin_out + '.c') or { panic(err) }
}

fn directive_order_gen_c_shared_runtime() string {
	root := directive_order_fixture_dir('v3_c_directive_order_shared_runtime_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_shared_runtime' }\n")
	directive_order_write_file(root, 'main.v', 'module main

struct SharedOnly {
	a shared int
}

fn main() {
	_ := SharedOnly{}
}
')
	c_out := os.join_path(root, 'v3_c_directive_order_shared_runtime.c')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_request_extern() string {
	root := directive_order_fixture_dir('v3_c_directive_order_request_extern_project')
	defer { os.rmdir_all(root) or {} }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_request_extern' }\n")
	directive_order_write_file(root, 'request.c', 'int request(int code) { return code + 1; }\n')
	directive_order_write_file(root, 'main.v', 'module main

#flag @VMODROOT/request.c

fn C.request(code int) int

fn main() {
	_ := C.request(7)
}
')
	c_out := os.join_path(root, 'v3_c_directive_order_request_extern.c')
	result := os.exec([@VEXE, '-b', 'c', '-o', '${c_out}', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_gen_c_request_macro(root string) string {
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order_request_macro' }\n")
	directive_order_write_file(root, 'request.h', '#define request(code) ((code) + 1)\n')
	directive_order_write_file(root, 'main.v', 'module main

#flag -I@VMODROOT
#include "request.h"

fn C.request(code int) int

fn main() {
	_ := C.request(7)
}
')
	c_out := os.join_path(root, 'v3_c_directive_order_request_macro.c')
	result := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', '${c_out}'])
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

fn test_c_directives_follow_import_dependency_order() {
	root := directive_order_fixture_dir('v3_c_directive_order_project')
	defer { os.rmdir_all(root) or {} }
	c_code := directive_order_gen_c(root, true)
	gfx_define := directive_order_index(c_code, '#define SOKOL_GFX_IMPL')
	fontstash_define := directive_order_index(c_code, '#define SOKOL_FONTSTASH_IMPL')
	block_start := directive_order_index(c_code, '#ifdef KEEP_DUPLICATE_INCLUDE')
	assert gfx_define >= 0, c_code
	assert fontstash_define >= 0, c_code
	assert block_start >= 0, c_code
	assert gfx_define < fontstash_define, c_code
	assert gfx_define < directive_order_index(c_code, 'sokol_gfx.h"'), c_code
	assert fontstash_define < directive_order_index(c_code, 'sokol_fontstash.h"'), c_code
	block_tail := c_code[block_start..]
	block_end := directive_order_index(block_tail, '#endif')
	assert block_end >= 0, c_code
}

fn test_native_c_directives_preserve_resolved_includes_in_dependency_order() {
	root := directive_order_fixture_dir('v3_c_directive_order_project')
	defer { os.rmdir_all(root) or {} }
	c_code := directive_order_gen_c(root, false)
	project_path := os.real_path(root)
	gfx_header := os.join_path(project_path, 'sokol/c/sokol_gfx.h').replace('\\', '/')
	fontstash_header := os.join_path(project_path, 'sokol/f/util/sokol_fontstash.h').replace('\\', '/')
	gfx_define := directive_order_index(c_code, '#define SOKOL_GFX_IMPL')
	gfx_include := directive_order_index(c_code, '#include "${gfx_header}"')
	fontstash_define := directive_order_index(c_code, '#define SOKOL_FONTSTASH_IMPL')
	fontstash_include := directive_order_index(c_code, '#include "${fontstash_header}"')
	assert gfx_define >= 0, c_code
	assert gfx_define < gfx_include, c_code
	assert gfx_include < fontstash_define, c_code
	assert fontstash_define < fontstash_include, c_code
}

fn test_c_directives_preserve_dotted_import_module_identity() {
	c_code := directive_order_gen_c_dotted_collision()
	short_bar := directive_order_index(c_code, '#define SHORT_BAR')
	foo_bar := directive_order_index(c_code, '#define FOO_BAR')
	foo_user := directive_order_index(c_code, '#define FOO_USER')
	assert short_bar >= 0, c_code
	assert foo_bar >= 0, c_code
	assert foo_user >= 0, c_code
	assert foo_bar < foo_user, c_code
}

fn test_inserted_local_headers_resolve_and_keep_inline_functions() {
	c_code := directive_order_gen_and_run_local_headers()
	use_foo := directive_order_index(c_code, '#define USE_FOO')
	foo_header := directive_order_index(c_code, 'foo.h"')
	assert use_foo >= 0, c_code
	assert foo_header > use_foo, c_code
	assert c_code.contains('dir_value.h"'), c_code
	assert c_code.contains('multiline_value.h"'), c_code
	for name in ['foo_value', 'dir_value', 'multiline_value'] {
		assert !c_code.contains('int ${name}(void);'), c_code
	}
}

fn test_quoted_includes_use_earlier_and_later_include_flags() {
	c_code := directive_order_gen_and_run_flag_include_headers()
	for name in ['flag_value', 'late_flag_value'] {
		assert c_code.contains('#include "${name}.c"\n'), c_code
		// Angle-bracket dependencies remain supplied through each recorded -I flag.
		assert !c_code.contains('#include <${name}.h>'), c_code
		assert !c_code.contains('int ${name}(void);'), c_code
	}
}

fn test_header_includes_are_emitted_before_extern_prototypes() {
	c_code := directive_order_gen_c_extern_after_header()
	header_idx := directive_order_index(c_code, 'native_thing.h"')
	proto_idx := directive_order_index(c_code, 'int native_use(NativeThing*')
	assert header_idx >= 0, c_code
	assert proto_idx >= 0, c_code
	assert header_idx < proto_idx, c_code
}

fn test_inserted_headers_supply_native_declarations() {
	c_code := directive_order_gen_c_header_declarations()
	assert c_code.contains('declared_proto.h"'), c_code
	assert c_code.contains('declared_text()'), c_code
	assert directive_order_count(c_code, 'declared_text(void);') == 0, c_code

	for header, declaration in {
		'field_thing.h"': 'struct main__Wrap {'
		'union_thing.h"': 'struct main__WrapUnion {'
	} {
		header_idx := directive_order_index(c_code, header)
		wrap_idx := directive_order_index(c_code, declaration)
		assert header_idx >= 0, c_code
		assert wrap_idx > header_idx, c_code
	}
	assert !c_code.contains('typedef struct FieldThing'), c_code
	assert !c_code.contains('} TaggedUnionAlias;'), c_code
	assert !c_code.contains('typedef union AnonymousUnion'), c_code
	assert !c_code.contains('union AnonymousUnion {\n'), c_code
	assert !c_code.contains('typedef union TaggedUnionAlias'), c_code
	assert !c_code.contains('union TaggedUnionAlias {\n'), c_code
}

fn test_native_typedef_headers_supply_types_and_inline_functions() {
	c_code := directive_order_gen_and_run_header_typedefs()
	assert c_code.contains('anonymous_thing.h"'), c_code
	assert !c_code.contains('static inline int anonymous_value'), c_code
	assert !c_code.contains('typedef struct AnonymousThing'), c_code

	assert c_code.contains('tagged_thing.h"'), c_code
	assert !c_code.contains('static inline int tagged_value'), c_code
	assert !c_code.contains('typedef struct TaggedAlias'), c_code

	assert c_code.contains('outer.h"'), c_code
	assert !c_code.contains('#define NESTED_BIAS 5'), c_code
	assert !c_code.contains('} NestedThing;'), c_code
	assert !c_code.contains('static inline int nested_value'), c_code
	assert !c_code.contains('typedef struct NestedThing'), c_code
}

fn test_supported_system_include_is_preserved_and_enables_system_preamble() {
	c_code := directive_order_gen_c_unresolved_system_include()
	assert c_code.contains('#include <platform_user_header.h>'), c_code
	assert c_code.contains('#include <dlfcn.h>'), c_code
	assert c_code.contains('#include <stdint.h>'), c_code
	api_compat_idx := directive_order_index(c_code, '#define PLATFORM_API_COMPAT 7')
	guard_idx := directive_order_index(c_code, '#ifdef USE_DLFCN')
	dlfcn_idx := directive_order_index(c_code, '#include <dlfcn.h>')
	preamble_idx := directive_order_index(c_code, 'typedef int8_t i8;')
	assert api_compat_idx >= 0, c_code
	assert guard_idx >= 0, c_code
	assert dlfcn_idx > guard_idx, c_code
	assert preamble_idx > dlfcn_idx, c_code
}

fn test_portable_c_preserves_unresolved_and_nested_includes() {
	c_code := directive_order_gen_c_opaque_headers()
	// The global preamble can define this macro earlier, too. The source
	// directives must still appear together in their original order.
	assert c_code.contains('#include "external_missing.h"\n#define AFTER_QUOTED_INCLUDE 1'), c_code

	assert c_code.contains('nested_preserved.h"'), c_code
	assert !c_code.contains('#define NESTED_PLATFORM_HEADER 1'), c_code
	assert !c_code.contains('#include <nested_platform_header.h>'), c_code
	assert !c_code.contains('typedef uint64_t NestedWord;'), c_code
}

fn test_system_headers_supply_sdk_aggregates_and_functions() {
	c_code := directive_order_gen_c_sdk_headers()
	assert c_code.contains('#include <X11/Xlib.h>'), c_code
	assert !c_code.contains('typedef union XEvent XEvent;'), c_code
	assert !c_code.contains('union XEvent {\n'), c_code
	assert !c_code.contains('typedef union XClientMessageData XClientMessageData;'), c_code
	assert !c_code.contains('union XClientMessageData {\n'), c_code
	assert c_code.contains('XEvent event;'), c_code

	assert c_code.contains('#include <bcrypt.h>'), c_code
	// The sole occurrence is the call, rather than an extra ABI declaration.
	assert directive_order_count(c_code, 'BCryptGenRandom(') == 1, c_code
}

fn test_mach_headers_keep_sdk_declarations_and_scope_panic_macro() {
	c_code := directive_order_gen_c_sdk_mach_headers()
	assert c_code.contains('#define panic mach_panic\n#include <mach/mach.h>\n#undef panic'), c_code
	assert c_code.contains('#define panic mach_panic\n#include <mach/mach_time.h>\n#undef panic'), c_code
	assert !c_code.contains('typedef unsigned int task_t;'), c_code
	assert !c_code.contains('struct task_basic_info {'), c_code
	assert !c_code.contains('typedef struct mach_timebase_info_data_t {'), c_code
	assert directive_order_count(c_code, 'mach_timebase_info(') == 1, c_code
}

fn test_inferred_mach_bindings_keep_guarded_sdk_declarations() {
	c_code := directive_order_gen_c_task_info_reference()
	assert c_code.contains('#ifdef __APPLE__\n#define panic mach_panic\n#include <mach/mach.h>\n#undef panic\n#include <mach/task.h>\n#endif'), c_code
	assert !c_code.contains('int task_info(void);'), c_code
	assert directive_order_count(c_code, 'task_info(') == 1, c_code
}

fn test_timerfd_header_keeps_sdk_declarations() {
	c_code := directive_order_gen_c_sdk_timerfd_header()
	assert c_code.contains('#include <sys/timerfd.h>'), c_code
	assert !c_code.contains('struct itimerspec {'), c_code
	assert !c_code.contains('struct timespec {'), c_code
	assert !c_code.contains('int timerfd_create('), c_code
	assert !c_code.contains('#define TFD_CLOEXEC '), c_code
	assert !c_code.contains('#define TFD_NONBLOCK '), c_code
	assert c_code.contains('timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC | TFD_NONBLOCK)'), c_code
	assert directive_order_count(c_code, 'timerfd_settime(') == 1, c_code
	assert directive_order_count(c_code, 'timerfd_gettime(') == 1, c_code
}

fn test_native_headers_supply_variadic_and_format_definitions() {
	c_code := directive_order_gen_and_run_standard_headers()
	assert c_code.contains('stdarg_user.h"'), c_code
	assert !c_code.contains('offsetof(struct StdargThing, value)'), c_code
	assert !c_code.contains('static inline int stdarg_sum(int count, ...)'), c_code

	assert c_code.contains('inttypes_user.h"'), c_code
	assert !c_code.contains('sizeof(PRId64) + sizeof(PRIuPTR) + sizeof(SCNi64)'), c_code
}

fn test_poll_in_native_header_uses_preserved_system_struct() {
	c_code := directive_order_gen_c_nested_poll_header()
	assert c_code.contains('poll_user.h"'), c_code
	assert !c_code.contains('static inline int poll_user_fd(struct pollfd* item)'), c_code
	assert !c_code.contains('struct pollfd {\n'), c_code
}

fn test_rwmutex_keeps_native_lock_prototypes() {
	c_code := directive_order_gen_c_rwmutex()
	$if windows {
		// Windows RwMutex uses SRWLOCK, not the Unix pthread API.
		assert c_code.contains('InitializeSRWLock(&m->mx);'), c_code
		assert c_code.contains('AcquireSRWLockExclusive(&m->mx);'), c_code
		assert c_code.contains('ReleaseSRWLockExclusive(&m->mx);'), c_code
		assert !c_code.contains('i32 pthread_rwlock_init(void*, void*);'), c_code
	} $else {
		// sync declares these returns as i32, preserving C's ABI when V int is pointer-sized.
		assert c_code.contains('i32 pthread_rwlockattr_setkind_np(void*, i32);'), c_code
		assert c_code.contains('i32 pthread_rwlockattr_init(void*);'), c_code
		assert c_code.contains('i32 pthread_rwlockattr_destroy(void*);'), c_code
		assert c_code.contains('i32 pthread_rwlock_init(void*, void*);'), c_code
		assert c_code.contains('i32 pthread_rwlock_wrlock(void*);'), c_code
		assert c_code.contains('i32 pthread_rwlock_unlock(void*);'), c_code
		assert !c_code.contains('__v_int pthread_rwlock'), c_code
	}
}

fn test_shared_runtime_keeps_rwmutex_init_prototypes() {
	c_code := directive_order_gen_c_shared_runtime()
	$if !windows {
		assert c_code.contains('pthread_rwlockattr_init(void*)'), c_code
		assert c_code.contains('pthread_rwlockattr_setkind_np(void*, i32)'), c_code
		assert c_code.contains('pthread_rwlock_init(void*, void*)'), c_code
	}
}

fn test_request_named_c_function_keeps_extern_prototype() {
	c_code := directive_order_gen_c_request_extern()
	assert c_code.contains('int request(int'), c_code
}

fn test_header_backed_request_macro_does_not_get_extern_prototype() {
	root := directive_order_fixture_dir('v3_c_directive_order_request_macro_project')
	defer { os.rmdir_all(root) or {} }
	c_code := directive_order_gen_c_request_macro(root)
	// request.h sits beside main.v, and the generated C is compiled elsewhere,
	// so the include names the header by its resolved path.
	header := os.real_path(os.join_path(root, 'request.h')).replace('\\', '/')
	assert c_code.contains('#include "${header}"'), c_code
	assert !c_code.contains('int request(int'), c_code
}
