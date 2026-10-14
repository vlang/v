// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Covers the accessors of the Ascon digest types that ascon_test.v, hash_test.v,
// xof_test.v and cxof_test.v do not reach: Hash256.size, Hash256.block_size,
// Xof128.size, Xof128.block_size, CXof128.size, CXof128.block_size, and the
// three free methods.
//
// Every expectation against the published KAT is a vector; the remaining
// expectations are differential (two entry points into the same primitive) or
// measured with this compiler and recorded here.
module ascon

import encoding.hex

// test_digest_size_matches_output_length checks that each digest's size accessor
// agrees with both its one-shot function and its streaming sum.
fn test_digest_size_matches_output_length() {
	// Ascon-Hash256 always produces a 256-bit checksum.
	assert hash256_size == 32

	mut h := new_hash256()
	assert h.size() == 32
	assert h.sum([]u8{}) == sum256([]u8{})
	assert h.sum('anything at all'.bytes()).len == 32
	assert h.sum('anything at all'.bytes()) == sum256('anything at all'.bytes())

	// Ascon-XOF128 takes the output length at construction time.
	for size in [1, 8, 16, 64, 100, 4096] {
		mut x := new_xof128(size)
		assert x.size() == size, 'xof size ${size}'
		assert x.sum('message'.bytes()).len == size, 'xof size ${size}'

		oneshot := xof128('message'.bytes(), size)!
		assert x.sum('message'.bytes()) == oneshot, 'xof size ${size}'
	}

	// default_xof_size is the 512-bit size both defaults and _64 rely on.
	assert default_xof_size == 64
	mut d := new_xof128(default_xof_size)
	assert d.size() == 64

	cs := 'a customization string'.bytes()
	for size in [1, 8, 64, 300] {
		mut cx := new_cxof128(size, cs)!
		assert cx.size() == size, 'cxof size ${size}'
		assert cx.sum('message'.bytes()).len == size, 'cxof size ${size}'
		assert cx.sum('message'.bytes()) == cxof128('message'.bytes(), size, cs)!, 'cxof size ${size}'
	}
}

fn test_digest_block_size_is_eight_bytes() {
	// block_size is the rate of all three Ascon sponge variants here.
	assert block_size == 8

	assert new_hash256().block_size() == 8
	assert new_xof128(default_xof_size).block_size() == 8
	assert new_cxof128(default_xof_size, 'cs'.bytes())!.block_size() == 8
}

fn test_xof128_rejects_size_above_max_hash_size() {
	// max_hash_size is the documented ceiling, and the one-shot enforces it.
	assert max_hash_size == 4096

	short := []u8{len: max_hash_size + 1}
	if _ := xof128(short, max_hash_size + 1) {
		assert false, 'xof128 accepted a size above max_hash_size'
	} else {
		assert err.msg() == 'xof128: invalid size'
	}

	at_limit := []u8{len: max_hash_size}
	out := xof128(at_limit, max_hash_size)!
	assert out.len == max_hash_size
}

// NOTE: the two XOF constructors disagree about how they reject an
// out-of-range size: new_xof128 panics with 'new_xof128: invalid size' (xof.v:58,
// reached here with sizes 0 and max_hash_size + 1), while new_cxof128 returns an
// error. A panic cannot be asserted, so only new_cxof128's error is checked and
// new_xof128's panic is recorded as the current behaviour rather than pinned.
fn test_xof_constructors_disagree_on_bad_size() {
	assert max_hash_size == 4096

	if _ := new_cxof128(0, 'cs'.bytes()) {
		assert false, 'new_cxof128 accepted size 0'
	} else {
		assert err.msg() == 'CXof128: invalid size'
	}

	new_cxof128(default_xof_size, []u8{len: max_cxof128_cstring + 1}) or {
		assert err.msg() == 'CXof128: custom string length exceed limit'
		return
	}
	assert false, 'new_cxof128 accepted an oversized customization string'
}

// NOTE: Xof128.read() panics when dst.len exceeds max_hash_size (xof.v:118)
// rather than returning an error, so only the accepted range is asserted.
fn test_xof128_read_matches_read_fraction() {
	msg := 'read fraction'.bytes()

	// .read() produces exactly the ask-for length, and agrees with a sum of the
	// same length taken from the same message.
	for size in [1, 7, 8, 9, 64, 65] {
		mut x := new_xof128(default_xof_size)
		_ := x.write(msg)!
		mut dst := []u8{len: size}
		n := x.read(mut dst)!
		assert n == size, 'read size ${size}'
		assert dst == xof128(msg, size)!, 'read size ${size}'
	}

	// A zero length read changes nothing, which is what the guard says.
	mut y := new_xof128(default_xof_size)
	_ := y.write(msg)!
	mut empty := []u8{}
	n := y.read(mut empty)!
	assert n == 0
	assert empty.len == 0
}

// NOTE: free() is guarded by `$if prealloc { return }`. A default build is not a
// prealloc build, so the method reaches the free and the flag it sets.
// The observable effect is Digest.done, which also makes write() panic, so the
// flag is asserted rather than the panic exercised.
fn test_digest_free_marks_digest_done() {
	msg := 'message to absorb then free'.bytes()

	mut h := new_hash256()
	_ := h.write(msg)!

	mut x := new_xof128(default_xof_size)
	_ := x.write(msg)!

	mut c := new_cxof128(default_xof_size, 'cs'.bytes())!
	_ := c.write(msg)!

	// free() needs an unsafe block at the call site.
	unsafe {
		h.free()
		x.free()
		c.free()
	}
	assert h.Digest.done
	assert x.Digest.done
	assert c.Digest.done

	// NOTE: free() sets the done flag but does not reset Digest.length, so the
	// count of buffered bytes survives the free. Pinned as the current
	// behaviour: the length is left at the 3 bytes written above.
	assert h.Digest.length == 3
	assert x.Digest.length == 3
	assert c.Digest.length == 3
}

// NOTE: free() sets Digest.done but leaves the accessors working, and the
// struct is not zeroed. That is the current contract, so it is pinned rather
// than corrected: every accessor still answers after a free.
fn test_digest_accessors_survive_free() {
	mut h := new_hash256()
	mut x := new_xof128(32)
	mut c := new_cxof128(32, 'cs'.bytes())!
	unsafe {
		h.free()
		x.free()
		c.free()
	}
	assert h.size() == 32
	assert h.block_size() == 8
	assert x.size() == 32
	assert x.block_size() == 8
	assert c.size() == 32
	assert c.block_size() == 8
}

// reset restores the initial state after absorption, so the digest produces the
// checksum of the message it is asked for again rather than accumulating.
fn test_digest_reset_reinitializes_state() {
	mut h := new_hash256()
	_ := h.write('absorbed'.bytes())!
	assert h.sum([]u8{}) != sum256([]u8{})

	h.reset()
	assert h.sum([]u8{}) == sum256([]u8{})
	// The buffer was cleared, so a subsequent absorption starts clean.
	_ := h.write('absorbed'.bytes())!
	assert h.sum([]u8{}) != sum256([]u8{})
}

fn test_xof_reset_matches_fresh_instance() {
	cs := 'cs for cxof'.bytes()
	msg := 'absorbed then reset'.bytes()

	mut x := new_xof128(default_xof_size)
	_ := x.write(msg)!
	used := x.sum([]u8{})
	assert used != xof128([]u8{}, default_xof_size)!

	// sum() works on a clone, so x is still usable and reset returns it to the
	// initial state a fresh instance starts from.
	x.reset()
	mut fresh := new_xof128(default_xof_size)
	assert x.sum([]u8{}) == fresh.sum([]u8{})

	mut c := new_cxof128(default_xof_size, cs)!
	_ := c.write(msg)!
	assert c.sum([]u8{}) != cxof128([]u8{}, default_xof_size, cs)!

	c.reset()
	mut fresh_c := new_cxof128(default_xof_size, cs)!
	assert c.sum([]u8{}) == fresh_c.sum([]u8{})
}

// XOF output of one length is the prefix of the output of a longer length:
// the squeeze phase emits successive rate blocks, so a 32-byte output is the
// first half of a 64-byte output. This holds for both Xof128 and CXof128.
fn test_xof_output_is_prefix_extendable() {
	msg := 'prefix extension'.bytes()

	mut short := new_xof128(32)
	full := new_xof128(64)
	assert short.sum(msg) == full.sum(msg)[..32]

	cs := 'cs'.bytes()
	mut cshort := new_cxof128(32, cs)!
	cfull := new_cxof128(64, cs)!
	assert cshort.sum(msg) == cfull.sum(msg)[..32]

	// The 64-byte form is cxof128_64, so this is the same property checked
	// against the module's other public entry point rather than against a
	// published vector for this customization string.
	kat_cs := 'LWC_CXOF128_TEST'
	kat_msg := 'the quick brown fox jumps over the lazy dog'.bytes()
	mut k32 := new_cxof128(32, kat_cs.bytes())!
	assert k32.sum(kat_msg) == cxof128_64(kat_msg, kat_cs.bytes())![..32]
}

// CXof128 with an empty customization string is not the same as Xof128: the
// customization string length is absorbed first, so the two initial states
// differ even with nothing to absorb.
fn test_cxof128_empty_customization_is_not_xof128() {
	msg := 'same message'.bytes()

	x := xof128(msg, default_xof_size)!
	c := cxof128(msg, default_xof_size, []u8{})!
	assert c != x

	// Two customization strings of the same length but different content differ.
	assert cxof128(msg, default_xof_size, 'a'.bytes())! != cxof128(msg,
		default_xof_size, 'b'.bytes())!
}

fn test_ascon_hash256_streaming_matches_one_shot() {
	// 8 bytes is exactly one rate block, so 1, 7, 8 and 9 bytes straddle every
	// buffering path.
	for n in [1, 7, 8, 9, 16, 17, 63, 64, 65] {
		msg := []u8{len: n, init: u8(n)}
		oneshot := sum256(msg)

		mut h := new_hash256()
		for _ in 0 .. 3 {
			_ := h.write(msg)!
		}
		assert h.sum([]u8{}) == sum256(msg.repeat(3)), 'length ${n}'

		h.reset()
		_ := h.write(msg)!
		assert h.sum([]u8{}) == oneshot, 'length ${n}'

		// Byte at a time, to break a write across the block boundary.
		h.reset()
		for i in 0 .. n {
			_ := h.write(msg[i..i + 1])!
		}
		assert h.sum([]u8{}) == oneshot, 'length ${n} bytewise'

		// An empty write is a no-op, not an extra block.
		h.reset()
		_ := h.write([]u8{})!
		assert h.sum([]u8{}) == sum256([]u8{})
	}
}

fn test_xof128_streaming_matches_one_shot() {
	for n in [1, 7, 8, 9, 64, 65] {
		msg := []u8{len: n, init: u8(n)}
		exp := xof128(msg, default_xof_size)!

		mut x := new_xof128(default_xof_size)
		half := n / 2
		_ := x.write(msg[..half])!
		_ := x.write(msg[half..])!
		assert x.sum([]u8{}) == exp, 'length ${n}'

		// sum() does not consume the instance, so a second call gives the same
		// answer and a later write still contributes.
		x.reset()
		_ := x.write(msg)!
		assert x.sum([]u8{}) == exp, 'length ${n}'

		mut cx := new_cxof128(default_xof_size, 'cs'.bytes())!
		_ := cx.write(msg[..half])!
		_ := cx.write(msg[half..])!
		assert cx.sum([]u8{}) == cxof128(msg, default_xof_size, 'cs'.bytes())!, 'length ${n}'
	}
}

fn test_ascon_hash256_kat_is_reachable_from_hex() {
	// The first two rows of the LWC_HASH_KAT_128_256 vector, which hash_test.v
	// also covers, reached through hex rather than a `.v` const so the decoding
	// path is exercised independently of the shared table.
	empty := hex.decode('')!
	assert hex.encode(sum256(empty)) == '0b3be5850f2f6b98caf29f8fdea89b64a1fa70aa249b8f839bd53baa304d92b2'

	zero := hex.decode('00')!
	assert hex.encode(sum256(zero)) == '0728621035af3ed2bca03bf6fde900f9456f5330e4b5ee23e7f6a1e70291bc80'
}
