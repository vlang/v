import os
import v.cmdexec

fn test_native_callback_info_alias_preserves_by_value_c_types() {
	root := os.join_path(os.vtmp_dir(), 'c_struct_callback_info_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'native'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_callback_info' }\n")!
	os.write_file(os.join_path(root, 'native', 'native.h'), '#include <stddef.h>
#include <stdint.h>

typedef struct {
	const char* data;
	size_t length;
} NativeStringView;
typedef void (*NativeCallback)(int, NativeStringView, void*, void*);
typedef struct {
	const void* nextInChain;
	NativeCallback callback;
	void* userdata1;
	void* userdata2;
} NativeCallbackInfo;
typedef struct { uint64_t id; } NativeFuture;

static NativeFuture native_request(NativeCallbackInfo info) {
	NativeStringView message = {"ready", 5};
	NativeFuture future = {42};
	info.callback(1, message, info.userdata1, info.userdata2);
	return future;
}
')!
	os.write_file(os.join_path(root, 'native', 'native.c.v'), 'module native

#include "@DIR/native.h"

@[typedef]
struct C.NativeStringView {
pub:
	data &char
	length uint
}

pub type StringView = C.NativeStringView
pub type Callback = fn (i32, StringView, voidptr, voidptr)

@[typedef]
struct C.NativeCallbackInfo {
pub:
	nextInChain voidptr
	callback Callback = unsafe { nil }
	userdata1 voidptr
	userdata2 voidptr
}

pub type CallbackInfo = C.NativeCallbackInfo

@[typedef]
struct C.NativeFuture {
pub:
	id u64
}

pub type Future = C.NativeFuture

fn C.native_request(info C.NativeCallbackInfo) C.NativeFuture

pub fn request(info CallbackInfo) Future {
	return C.native_request(info)
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'import native

struct State {
mut:
	calls int
}

fn on_ready(status i32, message native.StringView, userdata1 voidptr, userdata2 voidptr) {
	assert status == 1
	assert message.length == 5
	assert unsafe { message.data.vstring_with_len(int(message.length)) } == "ready"
	// The C API returns the pointer to the V-owned callback state.
	mut state := unsafe { &State(userdata1) }
	state.calls++
	assert userdata2 == userdata1
}

fn main() {
	mut state := State{}
	info := native.CallbackInfo{
		nextInChain: unsafe { nil }
		callback: on_ready
		userdata1: &state
		userdata2: &state
	}
	assert native.request(info).id == 42
	assert native.request(native.CallbackInfo{callback: on_ready, userdata1: &state,
		userdata2: &state}).id == 42
	assert state.calls == 2
}
')!
	for flags in [[]string{}, ['-no-parallel', '-nocache']] {
		result := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-no-retry-compilation',
			'-gc', 'none', '-cc', @CCOMPILER, ...flags, 'run', root], '', {
			'VFLAGS':                 ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
		assert result.exit_code == 0, '${flags}: ${result.output}'
	}
}
