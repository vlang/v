import encoding.binary

pub type Addr = [4]u8

pub fn Addr.from_u32(a u32) Addr {
	mut bytes := [4]u8{}
	binary.big_endian_put_u32_fixed(mut bytes, a)
	return Addr(bytes)
}

pub fn (a Addr) u32() u32 {
	return binary.big_endian_u32_fixed(a)
}

struct Net {
	netaddr   Addr
	broadcast Addr
}

// returns Nth IP-address from the network if exists, else none
fn (n Net) nth(num i64) ?Addr {
	mut addr := Addr{}
	if num >= 0 {
		addr = Addr.from_u32(n.netaddr.u32() + u32(num))
	} else {
		addr = Addr.from_u32(n.broadcast.u32() + u32(num))
	}
	if !(n.netaddr.u32() < addr.u32() && addr.u32() < n.broadcast.u32()) {
		return none
	}
	return addr
}

fn test_aliased_fixed_array_option_fn_call() {
	net := Net{
		netaddr:   Addr([u8(172), 16, 16, 0]!)
		broadcast: Addr([u8(172), 16, 16, 3]!)
	}
	res1 := net.nth(1) or { panic(err) }
	res2 := net.nth(1) or { Addr{} }
	assert res1 == [u8(172), 16, 16, 1]!
	assert res2 == [u8(172), 16, 16, 1]!
}
