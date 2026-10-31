import os

fn test_native_callback_escaped_parameter_keeps_its_enum_type() {
	root := os.join_path(os.vtmp_dir(), 'native_callback_escaped_param_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'wrapper')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_callback_escaped' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'wrapper', 'native.h'), '
typedef enum NativeKind { KIND_ONE = 1 } NativeKind;
typedef enum NativeStatus { STATUS_SUCCESS = 0 } NativeStatus;
typedef struct NativeInfo { int value; } NativeInfo;
typedef struct NativeOwner NativeOwner;
struct NativeOwner {
    NativeStatus (*callback)(NativeOwner*, NativeKind, NativeInfo*);
    NativeStatus (*named_callback)(NativeOwner*, NativeKind, NativeInfo*);
    int calls;
};
static NativeStatus native_callback(NativeOwner* owner, NativeKind kind, NativeInfo* info) {
    owner->calls += 1;
    info->value += (int)kind;
    return STATUS_SUCCESS;
}
static NativeOwner native_owner(void) {
    NativeOwner owner = {native_callback, native_callback, 0};
    return owner;
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'wrapper', 'wrapper.c.v'), 'module wrapper
#flag -I @DIR
#include "native.h"
pub enum Kind { one = C.KIND_ONE }
pub enum Status { success = C.STATUS_SUCCESS }
@[typedef]
pub struct C.NativeOwner {
pub mut:
	callback fn (p_owner &Owner, @type Kind, p_info &Info) Status
	named_callback fn (p_owner &Owner, kind Kind, p_info &Info) Status
	calls i32
}
pub type Owner = C.NativeOwner
@[typedef]
pub struct C.NativeInfo {
pub mut:
	value i32
}
pub type Info = C.NativeInfo
fn C.native_owner() C.NativeOwner
pub fn exercise() {
	mut owner := C.native_owner()
	mut info := Info{value: 39}
	assert owner.callback(&owner, .one, &info) == .success
	assert owner.named_callback(&owner, .one, &info) == .success
	assert info.value == 41
	assert owner.calls == 2
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'import wrapper
fn main() { wrapper.exercise() }
') or { panic(err) }
	mut compiler_flags := [[]string{}]
	for compiler in ['clang', 'gcc'] {
		if _ := os.find_abs_path_of_executable(compiler) {
			compiler_flags << ['-cc', compiler]
		}
	}
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	for flags in compiler_flags {
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation']
		args << flags
		args << ['-o', binary, root]
		built := os.exec(args)
		assert built.exit_code == 0, '${flags}: ${built.output}'
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
	}
}
