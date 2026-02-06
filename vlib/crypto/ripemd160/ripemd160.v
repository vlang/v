// Based on: https://github.com/golang/crypto/blob/master/ripemd160/ripemd160.go

module ripemd160

// RIPEMD-160 is designed by Hans Dobbertin, Antoon Bosselaers, and Bart
// Preneel with specifications available at:
// http://homes.esat.kuleuven.be/~cosicart/pdf/AB-9601/AB-9601.pdf.

// The size of the checksum in bytes.
const size = 20

// The block size of the hash algorithm in bytes.
const block_size = 64

const _s0 = u32(0x67452301)
const _s1 = u32(0xefcdab89)
const _s2 = u32(0x98badcfe)
const _s3 = u32(0x10325476)
const _s4 = u32(0xc3d2e1f0)

// Digest represents the partial evaluation of a checksum.
struct Digest {
mut:
	s  []u32
	x  []u8
	nx int
	tc u64
}

// free the resources taken by the Digest `d`
@[unsafe]
pub fn (mut d Digest) free() {
	$if prealloc {
		return
	}
	unsafe { d.x.free() }
}

fn (mut d Digest) init() {
	d.s = []u32{len: 5}
	d.x = []u8{len: block_size}
	d.reset()
}

// reset the state of the Digest `d`
pub fn (mut d Digest) reset() {
	d.s[0] = u32(_s0)
	d.s[1] = u32(_s1)
	d.s[2] = u32(_s2)
	d.s[3] = u32(_s3)
	d.s[4] = u32(_s4)
	d.nx = 0
	d.tc = 0
}

fn (d &Digest) clone() &Digest {
	return &Digest{
		...d
		s: d.s.clone()
		x: d.x.clone()
	}
}

// new returns a new Digest (implementing hash.Hash) computing the MD5 checksum.
pub fn new() &Digest {
	mut d := &Digest{}
	d.init()
	return d
}

// size returns the size of the checksum in bytes.
pub fn (d &Digest) size() int {
	return size
}

// block_size returns the block size of the checksum in bytes.
pub fn (d &Digest) block_size() int {
	return block_size
}

// hexhash returns a hexadecimal RIPEMD-160 hash sum `string` of `s`.
pub fn hexhash(s string) string {
	mut d := new()
	d.init()
	d.write(s.bytes()) or { panic(err) }
	return d.sum([]).hex()
}

// write writes the contents of `p_` to the internal hash representation.
pub fn (mut d Digest) write(p_ []u8) !int {
	unsafe {
		mut p := p_
		mut nn := p.len
		d.tc += u64(nn)
		if d.nx > 0 {
			mut n := p.len
			if n > block_size - d.nx {
				n = block_size - d.nx
			}
			for i := 0; i < n; i++ {
				d.x[d.nx + i] = p[i]
			}
			d.nx += n
			if d.nx == block_size {
				block(mut d, d.x[0..])
				d.nx = 0
			}
			p = p[n..]
		}
		n := block(mut d, p)
		p = p[n..]
		if p.len > 0 {
			d.nx = copy(mut d.x[..], p)
		}
		return nn
	}
}

// sum returns the RIPEMD-160 sum of the bytes in `inp`.
pub fn (d0 &Digest) sum(inp []u8) []u8 {
	mut d := d0.clone()
	mut tc := d.tc
	mut tmp := []u8{len: 64}
	tmp[0] = 0x80
	if tc % 64 < 56 {
		d.write(tmp[0..56 - tc % 64]) or { panic(err) }
	} else {
		d.write(tmp[0..64 + 56 - tc % 64]) or { panic(err) }
	}

	// Length in bits.
	tc <<= 3
	for i := u16(0); i < 8; i++ {
		tmp[i] = u8(tc >> (8 * i))
	}
	d.write(tmp[0..8]) or { panic(err) }

	if d.nx != 0 {
		panic('v_crypto/ripemd160: d.nx != 0: ${d.nx}')
	}

	mut digest := []u8{len: size}
	for i, s in d.s {
		digest[i * 4] = u8(s)
		digest[i * 4 + 1] = u8(s >> 8)
		digest[i * 4 + 2] = u8(s >> 16)
		digest[i * 4 + 3] = u8(s >> 24)
	}
	return digest
}
