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
	res1 := net.nth(1) or { panic('missing address') }
	res2 := net.nth(1) or { Addr{} }
	assert res1 == [u8(172), 16, 16, 1]!
	assert res2 == [u8(172), 16, 16, 1]!
}

struct AddressCastTrace {
mut:
	calls int
}

fn address_bytes(mut trace AddressCastTrace) [4]u8 {
	trace.calls++
	return [u8(172), 16, 16, 1]!
}

fn address_from_pointer(ptr &[4]u8) Addr {
	return unsafe { Addr(*ptr) }
}

fn address_from_call(mut trace AddressCastTrace) Addr {
	return Addr(address_bytes(mut trace))
}

fn test_aliased_fixed_array_nonliteral_cast_copies_value() {
	mut bytes := [u8(172), 16, 16, 1]!
	ptr := unsafe { &bytes }
	copied := unsafe { Addr(*ptr) }
	returned := address_from_pointer(ptr)
	bytes[3] = 2
	assert copied == [u8(172), 16, 16, 1]!
	assert returned == copied
	assert bytes == [u8(172), 16, 16, 2]!

	mut trace := AddressCastTrace{}
	from_call := Addr(address_bytes(mut trace))
	assert trace.calls == 1
	assert from_call == copied
	assert address_from_call(mut trace) == copied
	assert trace.calls == 2
}
