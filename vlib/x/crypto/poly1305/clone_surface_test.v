import encoding.hex
import x.crypto.poly1305

// Coverage for Poly1305.clone, which poly1305_test.v, usage_test.v and
// custom.v never reach.
//
// The anchor is the key/message/tag triple of RFC 8439 A.3 test vector 2, which
// usage_test.v also uses. Everything else is differential: the oneshot create_tag
// is compared against new + update + finish, with clone used to fork in the
// middle. No expected value is invented.
//
// Clones retain independent buffers, so each side may continue absorbing after a fork.
// finish consumes its instance; use another clone when checking the same state twice.

// rfc8439_key is the one-time Poly1305 key from RFC 8439 A.3 test vector 2.
fn rfc8439_key() ![]u8 {
	return hex.decode('0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e')!
}

// rfc8439_msg is the message RFC 8439 A.3 test vector 2 authenticates.
fn rfc8439_msg() []u8 {
	return 'Any submission to the IETF intended by the Contributor for publication as all or part of an IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is considered an "IETF Contribution". Such statements include oral statements in IETF sessions, as well as written and electronic communications made at any time or place, which are addressed to'
		.bytes()
}

// rfc8439_tag is the tag RFC 8439 A.3 test vector 2 publishes for that message.
fn rfc8439_tag() ![]u8 {
	return hex.decode('36e5f6b5c5e06070f0efca96227a863e')!
}

// ladder_key is a 32 byte key whose clamped r half is non-zero, so the resulting
// tag does depend on the message. Its tag values are not published anywhere; they
// are measured with the oneshot create_tag in this file and reused, so every
// expectation here is a cross-check of clone against another entry point rather
// than a remembered constant.
//
// The r half of rfc8439_key clamps to zero, so that key tags every message with
// the same value — the s half alone — and would let every assertion in this file
// pass for the wrong reason.
fn ladder_key() ![]u8 {
	return hex.decode('01020304050607080000000000000000a0a1a2a3a4a5a6a7a8a9aaabacadaeaf')!
}

// ladder_msg is 85 bytes, so it crosses the 16-byte block boundary repeatedly and
// its final partial block is one byte long.
fn ladder_msg() []u8 {
	return 'Any submission to the IETF intended by the Contributor for publication as all or part'.bytes()
}

fn tag_of(msg []u8, key []u8) ![]u8 {
	mut out := []u8{len: 16}
	poly1305.create_tag(mut out, msg, key)!
	return out
}

fn finish_of(mut po poly1305.Poly1305) []u8 {
	mut out := []u8{len: 16}
	po.finish(mut out)
	return out
}

fn test_poly1305_clone_of_fresh_instance() ! {
	key := rfc8439_key()!
	mut po := poly1305.new(key)!
	mut cl := po.clone()

	// Both are the tag of the empty message, which for that key is the s half
	// itself, and which RFC 8439 A.3 test vector 2 happens to publish.
	assert finish_of(mut cl) == rfc8439_tag()!
	assert finish_of(mut po.clone()) == rfc8439_tag()!
	assert finish_of(mut po.clone()) == tag_of([]u8{}, key)!
}

// The clone absorbs the whole message from the forked state across block boundaries.
fn test_poly1305_clone_absorbs_from_the_fork() ! {
	key := ladder_key()!
	msg := ladder_msg()

	for n in [0, 1, 8, 15, 16, 17, 31, 32, 33, 64] {
		full := tag_of(msg, key)!

		mut po := poly1305.new(key)!
		po.update(msg[..n])
		mut cl := po.clone()
		cl.update(msg[n..])

		assert finish_of(mut cl) == full, 'clone finished the whole message after ${n} bytes'
	}
}

// The parent finishes the message; the clone is left alone afterwards.
fn test_poly1305_clone_leaves_the_parent_intact() ! {
	key := ladder_key()!
	msg := ladder_msg()

	for n in [0, 1, 8, 15, 16, 17, 31, 32, 33, 64] {
		full := tag_of(msg, key)!

		mut po := poly1305.new(key)!
		po.update(msg[..n])
		_ := po.clone()
		po.update(msg[n..])

		assert finish_of(mut po) == full, 'parent finished after absorbing from ${n} bytes'
	}
}

// The published vector, reached through a fork at the midpoint rather than in one
// update. The clone then finishes independently.
fn test_poly1305_clone_converges_on_published_vector() ! {
	key := rfc8439_key()!
	msg := rfc8439_msg()

	half := msg.len / 2
	mut po := poly1305.new(key)!
	po.update(msg[..half])
	mut cl := po.clone()
	cl.update(msg[half..])

	assert finish_of(mut cl) == rfc8439_tag()!, 'clone tag after a fork at the midpoint'
}

// verify() works on a copy of the instance and does not consume it, so a clone
// taken before verification still verifies, and still finishes afterwards.
fn test_poly1305_clone_preserves_verify() ! {
	key := ladder_key()!
	msg := ladder_msg()
	tag := tag_of(msg, key)!

	mut po := poly1305.new(key)!
	po.update(msg)
	mut cl := po.clone()
	assert cl.verify(tag)
	assert po.verify(tag), 'verify consumed the instance it was called on'

	assert finish_of(mut cl) == tag

	// A wrong tag must not verify, whichever instance it is checked against.
	mut bad := tag.clone()
	bad[0] ^= 0xff
	mut fresh := poly1305.new(key)!
	fresh.update(msg)
	assert !fresh.verify(bad)
	fresh_clone := fresh.clone()
	assert !fresh_clone.verify(bad)
}

// A double fork. The clone of the clone finishes from the state it captured,
// which is the prefix the chain has absorbed.
fn test_poly1305_clone_of_clone() ! {
	key := ladder_key()!
	msg := ladder_msg()

	mut a := poly1305.new(key)!
	a.update(msg[..8])
	mut b := a.clone()
	a.update(msg[8..16])
	mut c := b.clone()

	// c holds 8 bytes; finishing it from there is safe because neither side
	// has absorbed anything more.
	assert finish_of(mut c) == tag_of(msg[..8], key)!, 'clone of clone'
}

fn test_poly1305_clone_has_an_independent_buffer() ! {
	key := ladder_key()!
	msg := ladder_msg()

	// Interleaving writes on both sides must preserve the complete message.
	for n in [0, 8, 16] {
		full := tag_of(msg, key)!

		mut po := poly1305.new(key)!
		po.update(msg[..n])
		mut cl := po.clone()
		po.update(msg[n..])
		cl.update(msg[n..])

		parent := finish_of(mut po)
		clone := finish_of(mut cl)
		assert parent == full, 'parent after a fork at ${n} bytes'
		assert clone == full, 'clone after a fork at ${n} bytes'
	}
}
