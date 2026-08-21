// Construction of the data items signed (Sig_structure) and MACed
// (MAC_structure) by COSE messages, per RFC 9052 §4.4 and §6.3.
// These auxiliary structures never appear on the wire — they exist only
// to feed deterministic bytes into the cryptographic primitive.
module cose

import encoding.cbor

// sig_structure_sign1 builds the byte stream signed by COSE_Sign1
// (RFC 9052 §4.4):
//
//     Sig_structure1 = [
//         context     : "Signature1",
//         body_protected : empty_or_serialized_map,
//         external_aad   : bstr,
//         payload        : bstr
//     ]
//
// `body_protected` is the raw bytes of the protected header bstr (i.e.
// what `Headers.encode_protected` returns; not the surrounding bstr).
// `external_aad` is application-supplied additional data that is NOT
// transmitted in the message but participates in the signature.
fn sig_structure_sign1(body_protected []u8, external_aad []u8, payload []u8) []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(4)
	p.pack_text('Signature1')
	p.pack_bytes(body_protected)
	p.pack_bytes(external_aad)
	p.pack_bytes(payload)
	return p.bytes()
}

// sig_structure_sign builds the byte stream signed by one signer of a
// COSE_Sign message (RFC 9052 §4.4):
//
//     Sig_structure = [
//         context        : "Signature",
//         body_protected : empty_or_serialized_map,
//         sign_protected : empty_or_serialized_map,
//         external_aad   : bstr,
//         payload        : bstr
//     ]
//
// The `sign_protected` field is the per-signer protected headers
// (distinct from the body's own protected headers).
fn sig_structure_sign(body_protected []u8, sign_protected []u8, external_aad []u8, payload []u8) []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(5)
	p.pack_text('Signature')
	p.pack_bytes(body_protected)
	p.pack_bytes(sign_protected)
	p.pack_bytes(external_aad)
	p.pack_bytes(payload)
	return p.bytes()
}

// mac_structure_mac0 builds the byte stream MACed by COSE_Mac0
// (RFC 9052 §6.3):
//
//     MAC_structure = [
//         context        : "MAC0",
//         protected      : empty_or_serialized_map,
//         external_aad   : bstr,
//         payload        : bstr
//     ]
fn mac_structure_mac0(protected []u8, external_aad []u8, payload []u8) []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(4)
	p.pack_text('MAC0')
	p.pack_bytes(protected)
	p.pack_bytes(external_aad)
	p.pack_bytes(payload)
	return p.bytes()
}

// mac_structure_mac is the analogue of mac_structure_mac0 for COSE_Mac
// (multi-recipient). The `context` differs ("MAC" instead of "MAC0").
fn mac_structure_mac(protected []u8, external_aad []u8, payload []u8) []u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_header(4)
	p.pack_text('MAC')
	p.pack_bytes(protected)
	p.pack_bytes(external_aad)
	p.pack_bytes(payload)
	return p.bytes()
}
