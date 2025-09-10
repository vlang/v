// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
module ascon

// This test mostly taken from https://docs.rs/ascon/latest/src/ascon/lib.rs.html
fn test_ascon_round_one() {
	mut s := State{
		e0: u64(0x0123456789abcdef)
		e1: 0x23456789abcdef01
		e2: 0x456789abcdef0123
		e3: 0x6789abcdef012345
		e4: 0x89abcde01234567f
	}
	ascon_perm(mut s, 0x1f)

	assert s.e0 == u64(0x3c1748c9be2892ce)
	assert s.e1 == u64(0x5eafb305cd26164f)
	assert s.e2 == u64(0xf9470254bb3a4213)
	assert s.e3 == u64(0xf0428daf0c5d3948)
	assert s.e4 == u64(0x281375af0b294899)
}

fn test_ascon_round_p6() {
	mut s := State{
		e0: u64(0x0123456789abcdef)
		e1: 0xef0123456789abcd
		e2: 0xcdef0123456789ab
		e3: 0xabcdef0123456789
		e4: 0x89abcdef01234567
	}
	ascon_pnr(mut s, 6)
	assert s.e0 == u64(0xc27b505c635eb07f)
	assert s.e1 == u64(0xd388f5d2a72046fa)
	assert s.e2 == u64(0x9e415c204d7b15e7)
	assert s.e3 == u64(0xce0d71450fe44581)
	assert s.e4 == u64(0xdd7c5fef57befe48)
}

fn test_ascon_round_p8() {
	mut s := State{
		e0: u64(0x0123456789abcdef)
		e1: 0xef0123456789abcd
		e2: 0xcdef0123456789ab
		e3: 0xabcdef0123456789
		e4: 0x89abcdef01234567
	}
	ascon_pnr(mut s, 8)
	assert s.e0 == u64(0x67ed228272f46eee)
	assert s.e1 == u64(0x80bc0b097aad7944)
	assert s.e2 == u64(0x2fa599382c6db215)
	assert s.e3 == u64(0x368133fae2f7667a)
	assert s.e4 == u64(0x28cefb195a7c651c)
}

fn test_ascon_round_p12() {
	mut s := State{
		e0: u64(0x0123456789abcdef)
		e1: 0xef0123456789abcd
		e2: 0xcdef0123456789ab
		e3: 0xabcdef0123456789
		e4: 0x89abcdef01234567
	}
	ascon_pnr(mut s, 12)
	assert s.e0 == u64(0x206416dfc624bb14)
	assert s.e1 == u64(0x1b0c47a601058aab)
	assert s.e2 == u64(0x8934cfc93814cddd)
	assert s.e3 == u64(0xa9738d287a748e4b)
	assert s.e4 == u64(0xddd934f058afc7e1)
}
