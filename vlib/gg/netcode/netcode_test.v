module netcode

import gg
import math

fn test_packet_roundtrip() {
	mut p := new_packet()
	p.write_u8(7)
	p.write_u16(513)
	p.write_u32(123456)
	p.write_f32(1.5)
	p.write_string('gg')
	p.write_vec3(gg.Vec3{1, 2, 3})
	mut r := from_bytes(p.bytes())
	assert r.read_u8()! == 7
	assert r.read_u16()! == 513
	assert r.read_u32()! == 123456
	assert math.abs(r.read_f32()! - 1.5) < 0.001
	assert r.read_string()! == 'gg'
	v := r.read_vec3()!
	assert v.x == 1 && v.y == 2 && v.z == 3
	assert r.remaining() == 0
}

fn test_packet_truncation() {
	mut p := from_bytes([u8(1)])
	assert p.read_u8()! == 1
	if _ := p.read_u32() {
		assert false, 'expected truncation'
	}
}

fn test_snapshot_interpolation() {
	mut a := new_snapshot(0, 1)
	mut b := new_snapshot(2, 1)
	a.positions[0] = gg.Vec3{0, 0, 0}
	b.positions[0] = gg.Vec3{10, 0, 0}
	mid := interpolate(a, b, 0.5)
	assert math.abs(mid.positions[0].x - 5) < 0.001
	mut buf := new_snapshot_buffer(4)
	buf.push(a)
	buf.push(b)
	assert buf.items.len == 2
	assert buf.items[0].time == 0 && buf.items[1].time == 2
}
