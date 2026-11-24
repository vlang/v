module main

#include "@DIR/sdk.h"

@[typedef]
struct C.SDKRequest {
	value u64
}

fn C.sdk_read(const_request &C.SDKRequest) u64
fn C.sdk_bytes(const_data voidptr, count uint) u64
fn C.sdk_text(const_text &char) uint
fn C.sdk_store(target &u32, const_source &u32)
fn C.sdk_scalar(u64) u64
fn C.sdk_verify() i32

@[export: 'sdk_read']
fn read_request(request &C.SDKRequest) u64 {
	return request.value
}

@[export: 'sdk_bytes']
fn read_bytes(data voidptr, count uint) u64 {
	mut total := u64(0)
	bytes := unsafe { &u8(data) }
	for i in 0 .. count { total += unsafe { bytes[i] } }
	return total
}

@[export: 'sdk_text']
fn read_text(text &char) uint {
	mut count := uint(0)
	for unsafe { text[count] } != 0 { count++ }
	return count
}

@[export: 'sdk_store']
fn store_value(target &u32, source &u32) {
	unsafe { *target = *source }
}

@[export: 'sdk_scalar']
fn scalar_value(value u64) u64 { return value + 1 }

$if !export_const_object ? {
	fn main() {
		assert C.sdk_verify() == 0
		println('const-qualified C export arguments passed')
	}
}
