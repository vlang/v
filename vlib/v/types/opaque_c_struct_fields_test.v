module types

import os

fn test_opaque_c_typedef_alias_fields_keep_the_native_owner_layout() {
	root := os.join_path(os.vtmp_dir(), 'opaque_c_struct_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), '#include <stddef.h>

typedef struct NativeToken {
	unsigned char hidden[13];
	long long value;
} NativeToken;

typedef NativeToken NativeAlias;

typedef struct NativeOwner {
	unsigned int left;
	NativeToken token;
	NativeAlias native;
	unsigned int right;
} NativeOwner;

static NativeOwner native_owner_create(void) {
	NativeOwner owner = {0};
	owner.left = 0x12345678U;
	owner.token.hidden[12] = 17;
	owner.token.value = 1234567890123LL;
	owner.native.hidden[12] = 29;
	owner.native.value = 9876543210123LL;
	owner.right = 0x87654321U;
	return owner;
}

static long long native_token_value(NativeToken token) { return token.value; }
static long long native_alias_value(NativeAlias token) { return token.value; }
static size_t native_owner_size(void) { return sizeof(NativeOwner); }

static int native_owner_valid(NativeOwner owner) {
	return owner.left == 0x12345678U && owner.right == 0x87654321U
		&& owner.token.hidden[12] == 17 && owner.token.value == 1234567890123LL
		&& owner.native.hidden[12] == 29 && owner.native.value == 9876543210123LL;
}
')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "${os.join_path(root, 'native.h')}"

@[typedef]
struct C.NativeToken {}

type NativeToken = C.NativeToken
type C.NativeAlias = C.NativeToken

@[typedef]
struct C.NativeOwner {
mut:
	left u32
	token NativeToken
	native C.NativeAlias
	right u32
}

fn C.native_owner_create() C.NativeOwner
fn C.native_token_value(token NativeToken) i64
fn C.native_alias_value(token C.NativeAlias) i64
fn C.native_owner_size() uint
fn C.native_owner_valid(owner C.NativeOwner) i32

fn main() {
	owner := C.native_owner_create()
	assert sizeof(owner) == C.native_owner_size()
	assert owner.left == u32(0x12345678)
	assert owner.right == u32(0x87654321)
	assert C.native_token_value(owner.token) == i64(1234567890123)
	assert C.native_alias_value(owner.native) == i64(9876543210123)
	assert C.native_owner_valid(owner) == 1
	token := owner.token
	native := owner.native
	assert C.native_token_value(token) == i64(1234567890123)
	assert C.native_alias_value(native) == i64(9876543210123)
	mut copied := owner
	assert C.native_owner_valid(copied) == 1
	copied.left = 7
	copied.right = 11
	assert owner.left == u32(0x12345678) && owner.right == u32(0x87654321)
	assert C.native_token_value(copied.token) == i64(1234567890123)
	assert C.native_alias_value(copied.native) == i64(9876543210123)
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		source])
	assert result.exit_code == 0, result.output
}
