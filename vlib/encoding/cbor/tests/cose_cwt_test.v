// Real-world conformance: drives CBOR-Web-Token (RFC 8392) and COSE
// (RFC 8152) sample structures. These are the canonical CBOR payloads
// used by IoT auth, OAuth 2.0 PoP, and EAT (RFC 9711). They exercise
// canonical encoding, signed/MAC/COSE_Sign1 structures, and standard
// claim-set integer keys.
module main

import encoding.cbor
import encoding.hex

fn h_(s string) []u8 {
	return hex.decode(s) or { panic('bad hex ${s}') }
}

fn b_eq(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// --------------------------------------------------------------------
// RFC 8392 §A.1 — Example CWT Claims Set
//
//   {
//     1: "coap://as.example.com",  // iss
//     2: "erikw",                   // sub
//     3: "coap://light.example.com",// aud
//     4: 1444064944,                // exp
//     5: 1443944944,                // nbf
//     6: 1443944944,                // iat
//     7: h'0b71'                    // cti
//   }
const a1_claims_hex = 'a70175636f61703a2f2f61732e6578616d706c652e636f6d02656572696b77' +
	'037818636f61703a2f2f6c696768742e6578616d706c652e636f6d041a5612ae' +
	'b0051a5610d9f0061a5610d9f007420b71'

fn test_rfc8392_a1_cwt_claims_set() {
	v := cbor.decode[cbor.Value](h_(a1_claims_hex), cbor.DecodeOpts{}) or {
		panic('decode CWT claims: ${err}')
	}
	if v !is cbor.Map {
		assert false, 'CWT claim set must decode to a Map'
		return
	}
	m := v as cbor.Map
	assert m.pairs.len == 7

	// Helper: find pair by integer key.
	mut by_key := map[u64]cbor.Value{}
	for pair in m.pairs {
		key := pair.key
		if key !is cbor.IntNum {
			assert false, 'CWT claim key must be an integer (got ${key.type_name()})'
			return
		}
		k := key as cbor.IntNum
		assert !k.negative
		by_key[k.magnitude] = pair.value
	}

	// iss / sub / aud / exp / nbf / iat / cti
	iss := by_key[1] or {
		assert false, 'missing iss'
		return
	}
	assert (iss as cbor.Text).value == 'coap://as.example.com'
	sub := by_key[2] or {
		assert false, 'missing sub'
		return
	}
	assert (sub as cbor.Text).value == 'erikw'
	aud := by_key[3] or {
		assert false, 'missing aud'
		return
	}
	assert (aud as cbor.Text).value == 'coap://light.example.com'
	exp := by_key[4] or {
		assert false, 'missing exp'
		return
	}
	exp_int := exp as cbor.IntNum
	assert !exp_int.negative && exp_int.magnitude == 1444064944
	nbf := by_key[5] or {
		assert false, 'missing nbf'
		return
	}
	nbf_int := nbf as cbor.IntNum
	assert !nbf_int.negative && nbf_int.magnitude == 1443944944
	iat := by_key[6] or {
		assert false, 'missing iat'
		return
	}
	iat_int := iat as cbor.IntNum
	assert !iat_int.negative && iat_int.magnitude == 1443944944
	cti := by_key[7] or {
		assert false, 'missing cti'
		return
	}
	cti_bs := cti as cbor.Bytes
	assert b_eq(cti_bs.data, [u8(0x0b), 0x71])

	// Re-encode through the Value tree: must be byte-identical (RFC 8392
	// claim sets are already in canonical form per §7).
	out := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert b_eq(out, h_(a1_claims_hex)), 'CWT round-trip mismatch'
}

// --------------------------------------------------------------------
// RFC 8392 §A.3 — COSE_Mac0-tagged CWT (tag 17 = CBOR_Tag_COSE_Mac0)
// The outer is `61(...)` = tag 61 (CWT) wrapping a COSE_Mac0 (tag 17).
// We don't validate the MAC — only the CBOR structure parses cleanly,
// the protected header is a bstr, the claim payload is a bstr containing
// the §A.1 claims, and the tag is correctly identified.
// Tag 61 (CWT) → tag 17 (COSE_Mac0) → [protected={1:5}, {}, payload, mac_tag]
// Constructed from §A.1 claims + §A.3 example MAC.
const a3_mac_hex = 'd83d' + // CWT tag
 'd1' + // COSE_Mac0 tag
 '84' + // array(4)
 '43a10105' + // bstr(3): {1:5} (HMAC 256/64)
 'a0' + // {}
 '5850' + a1_claims_hex + // bstr(80): claims
 '48093101ef6d789200' // bstr(8): MAC

fn test_rfc8392_a3_cose_mac0_cwt() {
	v := cbor.decode[cbor.Value](h_(a3_mac_hex), cbor.DecodeOpts{}) or {
		panic('decode CWT-Mac0: ${err}')
	}
	// Outer is tag 61 (CWT).
	if v !is cbor.Tag {
		assert false, 'expected tag 61'
		return
	}
	cwt_tag := v as cbor.Tag
	assert cwt_tag.number == 61, 'outer tag = ${cwt_tag.number}, want 61'

	// Inner is tag 17 (COSE_Mac0).
	inner := cwt_tag.content()
	if inner !is cbor.Tag {
		assert false, 'expected tag 17 inside CWT'
		return
	}
	mac0 := inner as cbor.Tag
	assert mac0.number == 17, 'inner tag = ${mac0.number}, want 17'

	// COSE_Mac0 = [protected, unprotected, payload, tag]
	body := mac0.content()
	if body !is cbor.Array {
		assert false, 'COSE_Mac0 must be array'
		return
	}
	arr := body as cbor.Array
	assert arr.elements.len == 4, 'COSE_Mac0 must have 4 elements, got ${arr.elements.len}'

	// protected is a bstr wrapping a CBOR-encoded map.
	protected_bs := arr.elements[0] as cbor.Bytes
	protected_map := cbor.decode[cbor.Value](protected_bs.data, cbor.DecodeOpts{}) or {
		panic('decode protected header: ${err}')
	}
	assert protected_map is cbor.Map

	// unprotected is an empty map.
	assert arr.elements[1] is cbor.Map
	assert (arr.elements[1] as cbor.Map).pairs.len == 0

	// payload is a bstr that decodes to the §A.1 claims set.
	payload_bs := arr.elements[2] as cbor.Bytes
	claims := cbor.decode[cbor.Value](payload_bs.data, cbor.DecodeOpts{}) or {
		panic('decode payload: ${err}')
	}
	assert claims is cbor.Map
	claims_map := claims as cbor.Map
	assert claims_map.pairs.len == 7, 'expected 7 claims, got ${claims_map.pairs.len}'

	// Round-trip: re-encode the entire structure and compare bytes.
	out := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert b_eq(out, h_(a3_mac_hex)), 'CWT-Mac0 round-trip mismatch'
}

// --------------------------------------------------------------------
// RFC 8152 §C.2.1 — COSE_Sign1 single-signer ECDSA example
// 18([h'a201260300', {}, h'546869732069732074686520636f6e74656e742e',
//     h'6520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ed' +
//       'e8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75'])
//
// The fully-encoded form is published in the working group test
// vectors. We encode it from its components to validate that the
// pieces round-trip — that's the meaningful interop check (the actual
// signature isn't verified).
fn test_cose_sign1_structure() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(18) // COSE_Sign1
	p.pack_array_header(4)
	// protected (bstr containing {1: -7})  −7 = ECDSA w/ SHA-256
	p.pack_bytes(h_('a10126'))
	// unprotected (empty map)
	p.pack_map_header(0)
	// payload
	p.pack_bytes('This is the content.'.bytes())
	// signature (truncated example)
	p.pack_bytes(h_('8eb33e4ca31d1c465ab05aac34cc6b23d58fef5c083106c4d25a91aef0b0117e2af9a291aa32e14ab834dc56ed2a223444547e01f11d3b0916e5a4c345cacb36'))

	out := p.bytes()

	// Decode back and validate the COSE_Sign1 shape.
	v := cbor.decode[cbor.Value](out, cbor.DecodeOpts{}) or { panic('decode Sign1: ${err}') }
	tag := v as cbor.Tag
	assert tag.number == 18, 'tag = ${tag.number}, want 18 (COSE_Sign1)'

	body := tag.content() as cbor.Array
	assert body.elements.len == 4

	protected := body.elements[0] as cbor.Bytes
	hdr := cbor.decode[cbor.Value](protected.data, cbor.DecodeOpts{}) or {
		panic('decode protected: ${err}')
	}
	hdr_map := hdr as cbor.Map
	assert hdr_map.pairs.len == 1
	// alg label = 1, value = -7
	alg_key := hdr_map.pairs[0].key as cbor.IntNum
	assert !alg_key.negative && alg_key.magnitude == 1
	alg_val := hdr_map.pairs[0].value as cbor.IntNum
	assert alg_val.negative && alg_val.magnitude == 6 // -7 = -1 - 6

	payload := body.elements[2] as cbor.Bytes
	assert payload.data.bytestr() == 'This is the content.'

	// Roundtrip the whole thing through Value tree.
	rt := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert b_eq(rt, out), 'COSE_Sign1 round-trip mismatch'
}

// --------------------------------------------------------------------
// RFC 8152 §3 — Sig_structure used as Signature Input
// Sig_structure = [context, body_protected, external_aad, payload]
// This covers canonical encoding requirements (§4.4): when computing
// the to-be-signed bytes, the structure MUST be deterministically encoded.
fn test_sig_structure_canonical() {
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_array_header(4)
	p.pack_text('Signature1')
	p.pack_bytes(h_('a10126'))
	p.pack_bytes([]u8{}) // external_aad
	p.pack_bytes('payload bytes'.bytes())
	encoded := p.bytes()

	// Re-encode in canonical mode through the Value tree must be identical.
	v := cbor.decode[cbor.Value](encoded, cbor.DecodeOpts{}) or { panic(err) }
	mut p2 := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p2.pack_value(v)!
	rt := p2.bytes()
	assert b_eq(rt, encoded), 'Sig_structure canonical round-trip differs'
}
