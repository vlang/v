// Copyright (c) 2024 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Poly1305 one-time message authentication code (MAC) module
module poly1305

import math
import math.unsigned
import encoding.binary
import crypto.internal.subtle

// Constants defined in this module
// -------------------------------
// block_size is the internal size of the Poly1305 block that is operates on
const block_size = 16
// key_size is a 256-bit one-time key size for input to Poly1305 mac in bytes.
const key_size = 32
// tag_size is the size of the output of the Poly1305 result, in bytes.
const tag_size = 16

// mask value for clamping low 64 bits of the r part, clearing 10 bits
const rmask0 = u64(0x0FFF_FFFC_0FFF_FFFF)
const not_rmask0 = ~rmask0
// mask value for clamping high 64 bits of the r part, clearing 12 bits
const rmask1 = u64(0x0FFF_FFFC_0FFF_FFFC)
const not_rmask1 = ~rmask1

// mask value for low 2 bits of u64 value
const mask_low2bits = u64(0x0000_0000_0000_0003)
// mask value for high 62 bits of u64 value
const mask_high62bits = u64(0xFFFF_FFFF_FFFF_FFFC)
// mask value for high 60 bits of u64 value
const mask_high60bits = u64(0xFFFF_FFFF_FFFF_FFF0)

// p is 130 bit of Poly1305 constant prime, ie 2^130-5
// as defined in rfc, p = 3fffffffffffffffffffffffffffffffb
const p = Uint192{
	lo: u64(0xFFFF_FFFF_FFFF_FFFB)
	mi: u64(0xFFFF_FFFF_FFFF_FFFF)
	hi: u64(0x0000_0000_0000_0003)
}

// Poly1305 mac instance
struct Poly1305 {
mut:
	// Poly1305 mac accepts 32 bytes (256 bits) of key input.
	// This key is partitioned into two's 128 bits parts, r and s
	// where r is clamped before stored and the s part is kept secret.
	r unsigned.Uint128
	s unsigned.Uint128
	// Poly1305 accumulator
	h Uint192
	// buffer
	buffer   []u8 = []u8{len: block_size}
	leftover int
	// The done flag tells us if the instance should not be used again.
	// It's set to true after calling finish or reset on the instance.
	done bool
}

// create_tag generates 16 bytes tag, i.e., a one-time message authenticator code (mac) stored into out.
// It accepts the message bytes to be authenticated and the 32 bytes of the key.
// This is an oneshot function to create a tag and reset internal state after the call.
// For incremental updates, use the method based on Poly1305 mac instance.
pub fn create_tag(mut out []u8, msg []u8, key []u8) ! {
	if out.len != tag_size {
		return error('poly1305: bad out tag_size')
	}
	mut po := new(key)!
	po.update(msg)
	po.finish(mut out)
}

// verify_tag verifies the tag is a valid message authentication code for the msg
// compared to the tag output from the calculated process.
// It returns `true` if two tags is matching, `false` otherwise.
pub fn verify_tag(tag []u8, msg []u8, key []u8) bool {
	mut po := new(key) or { panic(err) }
	mut out := []u8{len: tag_size}
	po.update(msg)
	po.finish(mut out)
	return subtle.constant_time_compare(tag, out) == 1
}

// new creates a new Poly1305 mac instance from 32 bytes of key provided.
@[direct_array_access]
pub fn new(key []u8) !&Poly1305 {
	if key.len != key_size {
		return error('poly1305: bad key length')
	}
	// Read the r part of the key and clamp it. Clamping was done by clearing
	// some bits of r before being used. The spec says the bits from 16 bytes of r,
	// that are required to be clamped are: some odd index bytes, i.e., r[3],
	// r[7], r[11], and r[15], are required to have their top four bits clear
	// (be smaller than 16), and some even index bytes, i.e., r[4], r[8], and r[12],
	// are required to have their bottom two bits clear (be divisible by 4),
	// totally clearing 22 bits. In 128-bit little-endian format, the clamping
	// mask value is 0x0ffffffc0ffffffc0ffffffc0fffffff.
	// See the rmask0 and rmask1 constants above.
	r := unsigned.Uint128{
		lo: binary.little_endian_u64(key[0..8]) & rmask0
		hi: binary.little_endian_u64(key[8..16]) & rmask1
	}

	// read s part from the rest bytes of key
	s := unsigned.Uint128{
		lo: binary.little_endian_u64(key[16..24])
		hi: binary.little_endian_u64(key[24..32])
	}

	ctx := &Poly1305{
		r: r
		s: s
	}
	return ctx
}

// update updates internal of Poly1305 state by message.
pub fn (mut po Poly1305) update(msg []u8) {
	poly1305_update_block(mut po, msg)
}

// verify verifies if the `tag` is a valid message authenticated code for current state of
// Poly1305 instance. Internally, it works on clone of the current instance.
pub fn (po Poly1305) verify(tag []u8) bool {
	assert tag.len == tag_size
	// we work on copy of current instance
	mut ctx := po
	mut out := []u8{len: tag_size}
	if ctx.leftover > 0 {
		poly1305_blocks(mut ctx, ctx.buffer[..ctx.leftover])
	}
	finalize(mut out, mut ctx.h, ctx.s)
	return subtle.constant_time_compare(tag[..], out) == 1
}

// finish finalizes the message authentication code calculation and stores the result into out.
// After calls this method, don't use the instance anymore to do most anything, but,
// you should reinitialize the instance with the new key with .`reinit` method instead.
pub fn (mut po Poly1305) finish(mut out []u8) {
	if po.done {
		panic('poly1305: has done, please reinit with the new key')
	}
	if po.leftover > 0 {
		poly1305_blocks(mut po, po.buffer[..po.leftover])
	}
	finalize(mut out, mut po.h, po.s)
	// we reset instance to make its in bad unusable state.
	po.reset()
}

// reinit reinitializes Poly1305 mac instance by resetting internal fields, and
// then reinit instance with the new key.
pub fn (mut po Poly1305) reinit(key []u8) {
	if key.len != key_size {
		panic('bad key size')
	}
	// first, we reset the instance and than setup its again
	po.reset()
	po.r = unsigned.Uint128{
		lo: binary.little_endian_u64(key[0..8]) & rmask0
		hi: binary.little_endian_u64(key[8..16]) & rmask1
	}
	po.s = unsigned.Uint128{
		lo: binary.little_endian_u64(key[16..24])
		hi: binary.little_endian_u64(key[24..32])
	}
	// we set po.done to false, to make its usable again.
	po.done = false
}

// poly1305_update_block updates the internals of Poly105 state instance by block of message.
fn poly1305_update_block(mut po Poly1305, msg []u8) {
	if msg.len == 0 {
		return
	}
	if po.done {
		panic('poly1305: has done, please reinit with the new key')
	}

	mut msglen := msg.len
	mut idx := 0
	// handle leftover
	if po.leftover > 0 {
		want := math.min(block_size - po.leftover, msglen)
		block := msg[idx..idx + want]
		_ := copy(mut po.buffer[po.leftover..], block)

		msglen -= want
		idx += want
		po.leftover += want

		if po.leftover < block_size {
			return
		}
		poly1305_blocks(mut po, po.buffer)
		po.leftover = 0
	}
	// process full blocks
	if msglen >= block_size {
		want := (msglen & ~(block_size - 1))
		mut block := unsafe { msg[idx..idx + want] }
		poly1305_blocks(mut po, block)
		idx += want
		msglen -= want
	}
	// store leftover
	if msglen > 0 {
		po.leftover += copy(mut po.buffer[po.leftover..], msg[idx..])
	}
}

// reset zeroes the Poly1305 mac instance and puts it in an unusable state.
// You should reinit the instance with the new key instead to make it usable again.
fn (mut po Poly1305) reset() {
	po.r = unsigned.uint128_zero
	po.s = unsigned.uint128_zero
	po.h = uint192_zero
	po.leftover = 0
	unsafe {
		po.buffer.reset()
	}
	// We set the done flag to true to prevent accidental calls
	// to update or finish methods on the instance.
	po.done = true
}

// poly1305_blocks updates internal state of Poly1305 instance `po` with message `msg`
fn poly1305_blocks(mut po Poly1305, msg []u8) {
	// nothing to do, just return
	if msg.len == 0 {
		return
	}
	// For correctness and clarity, we check whether r is properly clamped.
	if po.r.lo & not_rmask0 != 0 && po.r.hi & not_rmask1 != 0 {
		panic('poly1305: bad unclamped of r')
	}
	// We need the accumulator to be in correctly reduced form to make sure it is not overflowing.
	// To be safe when used, only maximum of four low bits of the high part of the accumulator (h.hi)
	// can be set, and the remaining high bits must not be set.
	if po.h.hi & mask_high60bits != 0 {
		panic('poly1305: h need to be reduced')
	}

	// localize the thing
	mut h := po.h
	mut t := [4]u64{}

	// The main routine for updating internal poly1305 state with blocks of messages done with step:
	// - chop messages into 16-byte blocks and read block as little-endian number;
	// - add one bit beyond the number (its dependz on the size of the block);
	// - add this number to the accumulator and then multiply the accumulator by "r".
	// - perform partial reduction modulo p on the result by calling `poly1305_squeeze` function.
	// - updates poly1305 accumulator with the new values
	mut msglen := msg.len
	mut idx := 0

	for msglen > 0 {
		// carry
		mut c := u64(0)
		if msglen >= block_size {
			// Read the 16 bytes msg block as a little-endian number
			// and stored into the 128 bits of Uint128
			block := msg[idx..idx + block_size]
			m := unsigned.Uint128{
				lo: binary.little_endian_u64(block[0..8])
				hi: binary.little_endian_u64(block[8..16])
			}
			// add msg block to accumulator, h += m
			h, c = h.add_128_checked(m, 0)

			// // The rfc requires us to set a bit just above the message size, ie,
			// add one bit beyond the number of octets. For a 16-byte block,
			// this is equivalent to adding 2^128 to the number.
			// so we can just add 1 to the high part of accumulator (h.hi += 1)
			// h.hi has been checked above, so, its safe to assume its not overflow
			h.hi += c + 1
			idx += block_size
			msglen -= block_size
		} else {
			// The last one msg block might be shorter than 16 bytes long,
			// pad it with zeros to align with block_size.
			mut buf := []u8{len: block_size}
			subtle.constant_time_copy(1, mut buf[..msglen], msg[idx..idx + msglen])

			// set a bit above msg size.
			buf[msglen] = u8(0x01)
			// loads 16 bytes of message block
			m := unsigned.Uint128{
				lo: binary.little_endian_u64(buf[0..8])
				hi: binary.little_endian_u64(buf[8..16])
			}

			// add this number to the accumulator, ie, h += m
			h, c = h.add_128_checked(m, 0)
			h.hi += c
			idx += block_size
			msglen -= block_size
		}

		// perform h *= r and then do partial reduction modulo p to the output.
		mul_h_by_r(mut t, mut h, po.r)
		poly1305_squeeze(mut h, t)
	}

	// updates internal accumulator
	po.h = h
}

// finalize finalizes the reduction of accumulator h, adds it with secret s,
// and then take 128 bits of h stored into out.
fn finalize(mut out []u8, mut ac Uint192, s unsigned.Uint128) {
	assert out.len == tag_size
	mut h := ac
	// compute t = h - p = h - (2¹³⁰ - 5), and select h as the result if the
	// subtraction underflows, and t otherwise.
	t, b := h.sub_checked(p)

	// h = h if h < p else h - p
	h.lo = select_64(b, h.lo, t.lo)
	h.mi = select_64(b, h.mi, t.mi)

	// Finally, we compute tag = h + s  mod  2¹²⁸
	// s is 128 bit of po.s, ie, Uint128
	tag, _ := h.add_128_checked(s, 0)

	// take only low 128 bit of x
	binary.little_endian_put_u64(mut out[0..8], tag.lo)
	binary.little_endian_put_u64(mut out[8..16], tag.mi)
}

// mul_h_by_r multiplies accumulator h by r and stores the result into four of the 64 bit limbs
fn mul_h_by_r(mut t [4]u64, mut h Uint192, r unsigned.Uint128) {
	// Let's multiply h by r, h *= r, and stores the result into raw 320 bits of xh and hb
	// In properly clamped r and reduced h, hb.hi bits should not be set, ie, hb.hi == 0
	// see comments on mul_128_checked for details.
	xh, hb := h.mul_128_checked(r)

	// check for high bits of the result is not overflowing 256 bits, so we can ignore
	// high bit (hb.hi) of the Uint128 part, fifth 64 bits limb.
	if hb.hi != 0 {
		panic('poly1305: unexpected overflow, non-null 5th limb')
	}

	// updates 4 64-bit limbs and ignore 5th limb
	t[0] = xh.lo
	t[1] = xh.mi
	t[2] = xh.hi
	t[3] = hb.lo
}

// poly1305_squeeze reduces by doing partial reduction module p
// where t is result of previous h*r from mul_h_by_r calls.
fn poly1305_squeeze(mut h Uint192, t [4]u64) {
	// we need to reduce 4 of 64 bit limbs in t modulo 2¹³⁰ - 5.
	// we follow the go version, by splitting result at the 2¹³⁰ mark into h and cc, the carry.
	mut ac := Uint192{
		lo: t[0]
		mi: t[1]
		hi: t[2] & mask_low2bits
	}
	mut cc := unsigned.uint128_new(t[2] & mask_high62bits, t[3])
	// reduction of general mersene prime, x = c * 2¹³⁰ + h  =  c * 5 + h  (mod  2¹³⁰ - 5)
	// because 2¹³⁰ = 5 (mod 2¹³⁰ - 5)
	// here, we follow the go version

	mut c := u64(0)
	ac, c = ac.add_128_checked(cc, c)
	cc = shift_right_by2(mut cc)

	// once again
	ac, c = ac.add_128_checked(cc, 0)
	// updates accumulator
	h = ac
}
