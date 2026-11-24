module main

#include "@DIR/sdk.h"

@[typedef]
struct C.sdk_payload_t {
mut:
	value u64
	code  u32
}

// SDK-generated MIG headers use private union tags without typedef aliases.
union C.__ReplyUnion__demo_subsystem {
mut:
	Reply C.sdk_payload_t
	wide  u64
}

union C.lowercase_reply {
mut:
	word  u64
	bytes [8]u8
}

union C.SdkReply {
mut:
	word  u64
	bytes [8]u8
}

@[typedef]
union C.__ReplyAlias_t {
mut:
	word u64
}

struct C.__TaggedRecord {
	value u64
}

type Reply = C.__ReplyUnion__demo_subsystem

fn C.sdk_reply_size() uint
fn C.sdk_payload_offset() uint
fn C.sdk_make_reply(u64) C.__ReplyUnion__demo_subsystem
fn C.sdk_read_reply(C.__ReplyUnion__demo_subsystem) u64
fn C.sdk_update_reply(&C.__ReplyUnion__demo_subsystem, u64)

fn identity_reply(reply Reply) Reply {
	return reply
}

fn optional_reply(reply Reply) ?Reply { return reply }

fn result_reply(reply Reply) !Reply { return reply }

fn optional_pointer(reply &Reply) ?&Reply { return reply }

fn main() {
	assert sizeof(C.__ReplyUnion__demo_subsystem) == C.sdk_reply_size()
	assert sizeof(C.__ReplyUnion__demo_subsystem) == 56
	assert __offsetof(C.sdk_payload_t, value) == C.sdk_payload_offset()
	assert C.sdk_payload_offset() == 8
	mut reply := C.sdk_make_reply(42)
	assert unsafe { reply.Reply.value } == 42
	assert C.sdk_read_reply(reply) == 51
	C.sdk_update_reply(&reply, 100)
	assert C.sdk_read_reply(identity_reply(reply)) == 109
	mut replies := []C.__ReplyUnion__demo_subsystem{len: 3}
	replies[2] = reply
	assert C.sdk_read_reply(replies[2]) == 109
	mut fixed := [2]C.__ReplyUnion__demo_subsystem{}
	fixed[1] = reply
	assert C.sdk_read_reply(fixed[1]) == 109
	assert sizeof(C.lowercase_reply) == 8
	mut lower := C.lowercase_reply{}
	unsafe {
		lower.word = u64(0xa5a5a5a5a5a5a5a5)
		for byte in lower.bytes {
			assert byte == 0xa5
		}
	}
	assert sizeof(C.SdkReply) == 8
	mut upper := C.SdkReply{}
	unsafe {
		upper.word = u64(0xa5a5a5a5a5a5a5a5)
		for byte in upper.bytes {
			assert byte == 0xa5
		}
	}
	assert sizeof(C.__ReplyAlias_t) == 24
	mut alias := C.__ReplyAlias_t{}
	unsafe { alias.word = 71 }
	assert unsafe { alias.word } == 71
	record := C.__TaggedRecord{ value: 19 }
	assert record.value == 19
	pointer := optional_pointer(&reply) or { panic('missing pointer') }
	assert unsafe { pointer.Reply.value } == 100
	optional := optional_reply(reply) or { panic('missing reply') }
	assert C.sdk_read_reply(optional) == 109
	result := result_reply(reply) or { panic('missing result') }
	assert C.sdk_read_reply(result) == 109
	call := identity_reply
	assert C.sdk_read_reply(call(reply)) == 109
	println('C union tags preserve native layout and ABI')
}
