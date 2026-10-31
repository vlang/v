module fastc

import os
import v.cmdexec
import v.pref

@[unsafe]
fn test_fastc_compound_calls_preserve_generic_types_and_borrowed_pointer_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_call_recursion_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ports')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'call_recursion' }") or { panic(err) }
	os.write_file(os.join_path(root, 'raw.h'), '#include <stdint.h>\n#include <stddef.h>\nstruct RawNode { uint64_t value; };\nstatic struct RawNode node = { UINT64_C(0xf123456789abcdef) };\nstatic struct RawNode *node_pointer = &node;\nstatic struct RawNode **node_lists[1] = { &node_pointer };\nstatic void *raw_nodes(void) { return node_lists; }\ntypedef const uint8_t raw_const_byte;\nstatic uint64_t received;\nstatic void raw_receive(void *context, raw_const_byte *data, size_t length) { received = data[0] + length; }\nstatic uint64_t raw_received(void) { return received; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'ports', 'ports.v'), r'module ports
pub enum Bus { pci usb }
pub struct Device {
pub mut:
	calls int
	trace u64
	bus Bus
	next &Device = unsafe { nil }
}
pub fn (mut device Device) read[T](value u32) T {
	device.calls++
	device.trace = device.trace * 10 + value
	return T(value)
}
pub fn port_in[T](mut device Device, value u32) T {
	device.calls++
	device.trace = device.trace * 10 + value
	return T(value)
}
pub fn buf_u8(mut device Device, value u32) u8 {
	device.calls++
	return u8(value)
}
pub fn read_plain(device &Device, value u32) u32 { return value + u32(device.calls) }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'@[has_globals]
module main
import ports
#include "@DIR/raw.h"
struct C.RawNode { value u64 }
fn C.raw_nodes() voidptr
@[typedef]
struct C.raw_const_byte {}
fn C.raw_receive(context voidptr, data &C.raw_const_byte, length uint)
fn C.raw_received() u64
struct Ops { receive fn (voidptr, &C.raw_const_byte, uint) }
struct Words { addr [2]u64 }
struct Group { words Words }
struct Member { group Group }
struct Handle { flags u64 }
struct IntBucket { values []int }
__global global_numbers []int
fn compound_methods(mut device ports.Device) bool {
	return device.read[u8](1) == 1 && device.read[u8](2) == 2 && device.read[u16](3) == 3
}
fn compound_functions(mut device ports.Device) bool {
	return ports.port_in[u8](mut device, 1) == 1 && ports.port_in[u8](mut device, 2) == 2 && ports.port_in[u16](mut device, 3) == 3
}
fn short_circuit(mut device ports.Device) bool {
	return device.read[u8](4) == 0 && device.read[u8](5) == 5
}
fn combine(a u8, b u16, c u32) u64 { return u64(a) << 32 | u64(b) << 16 | u64(c) }
fn nested_arguments(mut device ports.Device) u64 {
	return combine(device.read[u8](1), device.read[u16](2), device.read[u32](3))
}
fn enum_compound(mut device ports.Device) bool {
	return device.bus == .pci && device.read[u32](1) == 1
}
fn mutable_calls(mut device ports.Device) bool {
	return ports.buf_u8(mut device, 3) & 1 != 0 && ports.buf_u8(mut device, 4) & 2 == 0
}
fn pointer_match(mut device ports.Device, head &ports.Device) bool {
	return head == device && device == head && head.next != device
}
fn handle_access(pointer voidptr) u64 {
	access := unsafe { &Handle(pointer).flags & u64(3) }
	return access
}
fn group_value(value u64) Group { return Group{ words: Words{ addr: [value, 2]! } } }
fn optional_device(device &ports.Device, present bool) ?&ports.Device {
	if present { return device }
	return none
}
fn typed_equalities(device &ports.Device) bool {
	group := group_value(1)
	return group == group_value(1) && group != group_value(3) && optional_device(device, true) != none && optional_device(device, false) == none
}
struct PointerHolder { device &ports.Device }
fn pointer_field_call(holder &PointerHolder) bool {
	return ports.buf_u8(mut holder.device, 3) == 3
}
fn nested_cast_call(mut device ports.Device) u64 {
	return u64(ports.buf_u8(mut device, 7))
}
fn borrowed_cast_call(mut device ports.Device) u64 {
	return u64(ports.read_plain(device, 7))
}
fn byte_mask(bitmap [2]u8, bit int) i32 {
	return i32(bitmap[bit >> 3]) & i32(u32(1) << u32(bit & 7))
}
fn main() {
	mut device := &ports.Device{ bus: .pci }
	assert compound_methods(mut device)
	assert device.calls == 3 && device.trace == 123
	device.calls = 0
	device.trace = 0
	assert compound_functions(mut device)
	assert device.calls == 3 && device.trace == 123
	device.calls = 0
	device.trace = 0
	assert !short_circuit(mut device)
	assert device.calls == 1 && device.trace == 4
	device.calls = 0
	assert nested_arguments(mut device) == u64(0x100020003)
	assert device.calls == 3
	device.calls = 0
	assert enum_compound(mut device)
	assert device.calls == 1
	device.calls = 0
	assert mutable_calls(mut device)
	assert device.calls == 2
	assert typed_equalities(device)
	holder := &PointerHolder{ device: device }
	assert pointer_field_call(holder)
	assert nested_cast_call(mut device) == 7
	assert borrowed_cast_call(mut device) == u64(device.calls + 7)
	assert byte_mask([u8(3), 1]!, 1) == 2
	assert pointer_match(mut device, device)
	other := &ports.Device{}
	assert !pointer_match(mut device, other)
	handle := &Handle{ flags: u64(0xf123456789abcdef) }
	assert handle_access(voidptr(handle)) == 3
	node := unsafe { *(&&&C.RawNode(C.raw_nodes()))[0] }
	assert node.value == u64(0xf123456789abcdef)
	ops := Ops{ receive: C.raw_receive }
	bytes := [u8(1), 2, 3]
	unsafe { ops.receive(voidptr(0), &C.raw_const_byte(bytes.data + 1), uint(2)) }
	assert C.raw_received() == 4
	group := Group{ words: Words{ addr: [u64(1), 2]! } }
	member := &Member{ group: Group{ words: Words{ addr: [u64(1), 3]! } } }
	assert !(member.group.words.addr[0] == group.words.addr[0] && member.group.words.addr[1] == group.words.addr[1])
	assert member.group.words.addr[0] == group.words.addr[0]
	assert member.group.words.addr[1] != group.words.addr[1]
	large := int(1) << 40
	global_numbers << large + 1
	global_numbers << large + 2
	assert global_numbers == [large + 1, large + 2]
	assert global_numbers.contains(large + 2)
	assert global_numbers.first() == large + 1
	assert global_numbers.last() == large + 2
	mapped := global_numbers.map(it + 3)
	assert mapped == [large + 4, large + 5]
	assert global_numbers.filter(it > large + 1) == [large + 2]
	assert global_numbers.count(it > large + 1) == 1
	assert global_numbers.any(it == large + 2)
	assert global_numbers.all(it > large)
	assert IntBucket{ values: [large + 1, large + 2] } != IntBucket{ values: [large + 1, large + 3] }
	initial := large + int(0xffffffff)
	mut counters := { "wide": initial }
	counters["wide"]++
	assert counters["wide"] == initial + 1
	mut nested := map[int]map[string][]int{}
	nested[large]["values"] << large + 3
	assert nested[large]["values"] == [large + 3]
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
