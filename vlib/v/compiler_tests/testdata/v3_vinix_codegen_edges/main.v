module main

import gpu.agx.power

fn C.consume_order(u32, &char, u64, int) int

struct Segment {
	flags u32
}

fn (segment &Segment) is_writable() bool {
	return segment.flags & 1 == 0
}

fn read_slot(mut entry &u64) u64 {
	old := unsafe { *entry }
	return old
}

fn ordering(name string, input u32) int {
	mut value := input
	return C.consume_order(value, unsafe { &char(name.str) }, u64(name.len), if value > 0 {
		value++
		1
	} else {
		0
	})
}

fn fixed_receiver() bool {
	segments := [Segment{flags: 1}, Segment{flags: 0}]!
	return segments[0].is_writable() || segments[1].is_writable()
}

fn release(pointer voidptr) {
	unsafe { free(pointer) }
}

fn main() {
	mut value := u64(1)
	mut pointer := &value
	_ = read_slot(mut pointer)
	_ = ordering('edge', 1)
	_ = fixed_receiver()
	_ = power.fixed_receiver()
	release(pointer)
}
