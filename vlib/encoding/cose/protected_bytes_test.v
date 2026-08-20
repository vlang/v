// Tests for RFC 9052 §4.4 / §6.3: the Sig_structure and MAC_structure
// must carry the protected bucket exactly as it was received, not a
// canonical re-serialisation of the parsed headers. A sender is free to
// use any legal CBOR encoding for that map, so re-encoding it would
// change the bytes under the signature and reject valid messages.
module cose

import encoding.cbor
import encoding.hex

// Same ed25519 key pair as the cose-wg ed25519-sig-01 vector; EdDSA is
// deterministic (RFC 8032 §5.1.6), so these messages are reproducible.
// The fixtures are re-declared here instead of being shared with
// sign1_test.v because `v test` compiles each _test.v file on its own,
// so a file borrowing constants from a sibling could not be run alone.
const nc_eddsa_d_hex = '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60'
const nc_eddsa_x_hex = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a'
const nc_text = 'This is the content.'

// A protected bucket holding {1: -8 (EdDSA), 3: 0 (content type)} with
// the two labels in non-canonical order: label 3 comes before label 1.
// Canonical CBOR would emit a2 01 27 03 00.
const nc_sign1_protected = [u8(0xa2), 0x03, 0x00, 0x01, 0x27]

// The same map for a MAC message: {1: 5 (HMAC 256/256), 3: 0}, again
// with label 3 first. Canonical would be a2 01 05 03 00.
const nc_mac0_protected = [u8(0xa2), 0x03, 0x00, 0x01, 0x05]

// build_sign1_with_protected hand-assembles a COSE_Sign1 whose protected
// bstr is `raw` verbatim, signing over the Sig_structure built from those
// exact bytes.
fn build_sign1_with_protected(raw []u8, payload []u8, key Key) ![]u8 {
	tbs := sig_structure_sign1(raw, []u8{}, payload)
	sig := sign_with_key(.eddsa, key, tbs)!
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_tag(tag_sign1)
	p.pack_array_header(4)
	p.pack_bytes(raw)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes(payload)
	p.pack_bytes(sig)
	return p.bytes()
}

// build_mac0_with_protected does the same for a COSE_Mac0.
fn build_mac0_with_protected(raw []u8, payload []u8, key Key) ![]u8 {
	tbm := mac_structure_mac0(raw, []u8{}, payload)
	tag := compute_mac(.hmac_256_256, key, tbm)!
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_tag(tag_mac0)
	p.pack_array_header(4)
	p.pack_bytes(raw)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes(payload)
	p.pack_bytes(tag)
	return p.bytes()
}

fn test_verify1_accepts_non_canonically_encoded_protected_header() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	payload := nc_text.bytes()

	msg := build_sign1_with_protected(nc_sign1_protected, payload, priv)!
	// Re-encoding the parsed headers would produce a2 01 27 03 00 and
	// break the signature; verification must use the received bytes.
	assert verify1(msg, pub_key)! == payload
}

fn test_verify_mac0_accepts_non_canonically_encoded_protected_header() {
	key := Key.symmetric([]u8{len: 32, init: 0x42})
	payload := nc_text.bytes()

	msg := build_mac0_with_protected(nc_mac0_protected, payload, key)!
	assert verify_mac0(msg, key)! == payload
}

fn test_sign1_decode_encode_round_trips_non_canonical_protected_bytes() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	payload := nc_text.bytes()

	msg := build_sign1_with_protected(nc_sign1_protected, payload, priv)!
	decoded := Sign1Message.decode(msg)!
	assert decoded.protected_bytes()! == nc_sign1_protected
	// The parsed view is still correct, it is just not what goes into
	// the Sig_structure.
	assert (decoded.protected.algorithm or { Algorithm.es256 }) == .eddsa
	assert (decoded.protected.content_type_int or { u64(1) }) == u64(0)
	assert decoded.protected.encode_protected()! == [u8(0xa2), 0x01, 0x27, 0x03, 0x00]
	// The rest of this message is canonically encoded, so preserving the
	// protected bucket is enough to reproduce it byte for byte.
	assert decoded.encode(true)! == msg
}

fn test_mac0_decode_encode_round_trips_non_canonical_protected_bytes() {
	key := Key.symmetric([]u8{len: 32, init: 0x42})
	payload := nc_text.bytes()

	msg := build_mac0_with_protected(nc_mac0_protected, payload, key)!
	decoded := Mac0Message.decode(msg)!
	assert decoded.protected_bytes()! == nc_mac0_protected
	assert decoded.encode(true)! == msg
}

fn test_sign1_resigning_drops_the_decoded_protected_bytes() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	payload := nc_text.bytes()

	msg := build_sign1_with_protected(nc_sign1_protected, payload, priv)!
	mut decoded := Sign1Message.decode(msg)!
	// Change the headers, then re-sign: the stale bytes from `decode`
	// must not survive, otherwise the new signature would cover headers
	// that are no longer the ones in the message.
	decoded.protected.content_type_int = none
	decoded.sign(priv, payload, []u8{})!
	assert decoded.protected_bytes()! == [u8(0xa1), 0x01, 0x27]

	out := decoded.encode(true)!
	assert out != msg
	assert verify1(out, pub_key)! == payload
}

fn test_mac0_recomputing_drops_the_decoded_protected_bytes() {
	key := Key.symmetric([]u8{len: 32, init: 0x42})
	payload := nc_text.bytes()

	msg := build_mac0_with_protected(nc_mac0_protected, payload, key)!
	mut decoded := Mac0Message.decode(msg)!
	decoded.protected.content_type_int = none
	decoded.compute(key, payload, []u8{})!
	assert decoded.protected_bytes()! == [u8(0xa1), 0x01, 0x05]

	out := decoded.encode(true)!
	assert out != msg
	assert verify_mac0(out, key)! == payload
}

fn test_messages_built_in_memory_still_use_canonical_protected_bytes() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	mut hp := Headers{}
	hp.algorithm = .eddsa
	hp.content_type_int = u64(0)

	mut m := Sign1Message{
		protected: hp
		payload:   nc_text.bytes()
	}
	assert m.protected_bytes()! == [u8(0xa2), 0x01, 0x27, 0x03, 0x00]
	m.sign(priv, nc_text.bytes(), []u8{})!
	assert m.protected_bytes()! == [u8(0xa2), 0x01, 0x27, 0x03, 0x00]
}

fn test_empty_protected_bucket_is_preserved_as_received() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	payload := nc_text.bytes()

	// RFC 9052 §3 says an empty protected bucket is a zero-length bstr
	// rather than h'a0', but a peer may still send the encoded empty map.
	// Whatever arrived is what the signature covered, so keep it.
	msg := build_sign1_with_protected([u8(0xa0)], payload, priv)!
	decoded := Sign1Message.decode(msg)!
	assert decoded.protected.is_empty()
	assert decoded.protected.encode_protected()! == []u8{}
	assert decoded.protected_bytes()! == [u8(0xa0)]
	assert decoded.encode(true)! == msg
}

// A body bucket holding {4: h'6b' (kid), 3: 0 (content type)} with the
// labels in non-canonical order. Canonical would be a2 03 00 04 41 6b.
const nc_body_protected = [u8(0xa2), 0x04, 0x41, 0x6b, 0x03, 0x00]

fn test_sign_uses_the_received_protected_bytes_for_body_and_signer() {
	d := hex.decode(nc_eddsa_d_hex)!
	x := hex.decode(nc_eddsa_x_hex)!
	priv := Key.okp_private(.ed25519, x, d)
	pub_key := Key.okp_public(.ed25519, x)
	payload := nc_text.bytes()

	// COSE_Sign feeds both the body and the per-signer bucket into the
	// Sig_structure, so both have to survive the decode.
	tbs := sig_structure_sign(nc_body_protected, nc_sign1_protected, []u8{}, payload)
	sig := sign_with_key(.eddsa, priv, tbs)!
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_tag(tag_sign)
	p.pack_array_header(4)
	p.pack_bytes(nc_body_protected)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes(payload)
	p.pack_array_header(1)
	p.pack_array_header(3)
	p.pack_bytes(nc_sign1_protected)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes(sig)
	msg := p.bytes()

	decoded := SignMessage.decode(msg)!
	decoded.verify(0, pub_key)!
	assert decoded.protected_bytes()! == nc_body_protected
	assert decoded.signatures[0].protected_bytes()! == nc_sign1_protected
	assert decoded.encode(true)! == msg
}

fn test_mac_uses_the_received_protected_bytes_for_body_and_recipient() {
	key := Key.symmetric([]u8{len: 32, init: 0x42})
	payload := nc_text.bytes()

	// Only the body bucket is covered by the MAC_structure; the
	// recipient bucket still has to round-trip through encode.
	tbm := mac_structure_mac(nc_mac0_protected, []u8{}, payload)
	tag := compute_mac(.hmac_256_256, key, tbm)!
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_tag(tag_mac)
	p.pack_array_header(5)
	p.pack_bytes(nc_mac0_protected)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes(payload)
	p.pack_bytes(tag)
	p.pack_array_header(1)
	p.pack_array_header(3)
	p.pack_bytes(nc_body_protected)
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes([]u8{})
	msg := p.bytes()

	assert verify_mac(msg, key)! == payload
	decoded := MacMessage.decode(msg)!
	assert decoded.protected_bytes()! == nc_mac0_protected
	assert decoded.recipients[0].protected_bytes()! == nc_body_protected
	assert decoded.encode(true)! == msg
}
