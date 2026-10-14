module poly1305

import encoding.hex

fn ci_tag(msg []u8, key []u8) []u8 {
	mut po := new(key) or { panic(err) }
	po.update(msg)
	mut out := []u8{len: 16}
	po.finish(mut out)
	return out
}

// clone must produce an independent state, not a second handle on the same
// one. `buffer` is a V array, so copying the struct field copies the array
// header and the two instances alias one allocation: each `update` appends
// at the other's `leftover` offset over the other's bytes, and `finish`'s
// reset zeroes the buffer out from under whichever finishes second.
//
// Fork, absorb the SAME remainder on both sides, and both must reach the tag
// of the whole message. Measured before the fix: the parent was right and the
// clone was not, at every fork point.
fn test_poly1305_clone_is_independent_of_its_parent() {
	key := [u8(0x85), 0xd6, 0xbe, 0x78, 0x57, 0x55, 0xc6, 0xe2, 0x47, 0x63, 0x92, 0x6b, 0xd9, 0x02,
		0xe7, 0x35, 0x47, 0x47, 0x24, 0x7b, 0x23, 0x9f, 0x03, 0xde, 0x22, 0x42, 0x9c, 0x29, 0x8c,
		0x0f, 0x87, 0x1b]
	msg := 'The quick brown fox jumps over the lazy dog, and the clone must not disturb the parent.'.bytes()
	full := ci_tag(msg, key)

	for n in [0, 1, 7, 16, 32] {
		mut parent := new(key) or { panic(err) }
		parent.update(msg[..n])
		mut clone := parent.clone()
		parent.update(msg[n..])
		clone.update(msg[n..])

		mut pout := []u8{len: 16}
		mut cout := []u8{len: 16}
		parent.finish(mut pout)
		clone.finish(mut cout)

		assert pout == full, 'fork at ${n}: parent tag does not match the whole message'
		assert cout == full, 'fork at ${n}: clone tag does not match the whole message'
	}
}

// A clone of an untouched instance must be indistinguishable from it, so
// authenticating the same message through either gives the same tag. This is
// the property a caller relying on clone() actually needs.
fn test_poly1305_clone_of_a_fresh_instance_matches_the_original() {
	key := [u8(0xee), 0xa6, 0xa7, 0x25, 0x1c, 0x1e, 0x72, 0x91, 0x6d, 0x11, 0xc2, 0xcb, 0x21, 0x4d,
		0x3c, 0x25, 0x25, 0x39, 0x12, 0x1d, 0x8e, 0x23, 0x4e, 0x65, 0x2d, 0x65, 0x1f, 0xa4, 0xc8,
		0xcf, 0xf8, 0x80]
	msg := 'Any submission to the IETF intended by the Contributor for publication as all or part of an IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is considered an "IETF Contribution". Such statements include oral statements in IETF sessions, as well as written and electronic communications made at any time or place, which are addressed to'.bytes()

	mut parent := new(key) or { panic(err) }
	mut clone := parent.clone()
	parent.update(msg)
	clone.update(msg)

	mut pout := []u8{len: 16}
	mut cout := []u8{len: 16}
	parent.finish(mut pout)
	clone.finish(mut cout)
	assert pout == cout
}

fn test_poly1305_clones_match_rfc8439_vector_at_every_offset() ! {
	// RFC 8439, section 2.5.2.
	key := hex.decode('85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b')!
	msg := 'Cryptographic Forum Research Group'.bytes()
	tag := hex.decode('a8061dc1305136c6c22b8baf0c0127a9')!
	for n in 0 .. msg.len + 1 {
		mut parent := new(key)!
		parent.update(msg[..n])
		mut child := parent.clone()
		parent.update(msg[n..])
		child.update(msg[n..])
		mut a := []u8{len: 16}
		mut b := []u8{len: 16}
		if n % 2 == 0 {
			parent.finish(mut a)
			child.finish(mut b)
		} else {
			child.finish(mut b)
			parent.finish(mut a)
		}
		assert a == tag
		assert b == tag
	}
}
