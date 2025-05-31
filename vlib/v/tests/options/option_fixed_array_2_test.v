module main

import encoding.binary

type Addr = [4]u8

fn Addr.from_u32(a u32) Addr {
	mut bytes := [4]u8{}
	binary.big_endian_put_u32_fixed(mut bytes, a)
	return Addr(bytes)
}

fn (a Addr) str() string {
	return '${a[0]}.${a[1]}.${a[2]}.${a[3]}'
}

fn (a Addr) u32() u32 {
	return binary.big_endian_u32_fixed(a)
}

struct Net {
	netaddr   Addr
	broadcast Addr
mut:
	h u32
}

fn (mut n Net) next() ?Addr {
	if n.h >= n.broadcast.u32() + 1 {
		return none
	}
	defer {
		n.h++
	}
	return Addr.from_u32(n.h)
}

fn test_main() {
	net := Net{
		netaddr:   Addr([u8(172), 16, 16, 0]!)
		broadcast: Addr([u8(172), 16, 16, 3]!)
		h:         u32(2886733824)
	}
	mut rets := []string{}
	for addr in net {
		rets << addr.str()
	}
	assert rets[0] == '172.16.16.0'
	assert rets[1] == '172.16.16.1'
	assert rets[2] == '172.16.16.2'
	assert rets[3] == '172.16.16.3'
}
