module quic

import encoding.hex
import crypto.sha256

// RFC 8448 §3 ("Simple 1-RTT Handshake") test vectors — obtained directly
// from the raw RFC text (rfc-editor.org), not re-derived, and extracted
// programmatically (not hand-transcribed) to eliminate manual hex-copy
// error. RFC 8448 predates QUIC and its trace uses the TLS record layer,
// but the key-schedule MATH this file tests (Derive-Secret/HKDF-Extract
// chain) is identical for QUIC and non-QUIC TLS 1.3 — RFC 9001 §5 changes
// only how messages are transported and adds packet protection on top; it
// does not touch RFC 8446 §7.1's derivation itself. These vectors are
// therefore valid, independent cross-checks of this file's logic, distinct
// from RFC 9001 Appendix A.1 (already used in initial_secrets_test.v).
const rfc8448_client_hello = '010000c00303cb34ecb1e78163ba1c38c6dacb196a6dffa21a8d9912ec18a2ef6283024dece7000006130113031302010000910000000b0009000006736572766572ff01000100000a00140012001d0017001800190100010101020103010400230000003300260024001d002099381de560e4bd43d23d8e435a7dbafeb3c06e51c13cae4d5413691e529aaf2c002b0003020304000d0020001e040305030603020308040805080604010501060102010402050206020202002d00020101001c00024001'
const rfc8448_server_hello = '020000560303a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e2692800130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f002b00020304'
const rfc8448_early_secret = '33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a'
const rfc8448_derived_for_handshake = '6f2615a108c702c5678f54fc9dbab69716c076189c48250cebeac3576c3611ba'
const rfc8448_ecdhe_shared_secret = '8bd4054fb55b9d63fdfbacf9f04b9f0d35e6d63f537563efd46272900f89492d'
const rfc8448_handshake_secret = '1dc826e93606aa6fdc0aadc12f741b01046aa6b99f691ed221a9f0ca043fbeac'
const rfc8448_transcript_hash_ch_sh = '860c06edc07858ee8e78f0e7428c58edd6b43f2ca3e6e95f02ed063cf0e1cad8'
const rfc8448_client_hs_traffic = 'b3eddb126e067f35a780b3abf45e2d8f3b1a950738f52e9600746a0e27a55a21'
const rfc8448_server_hs_traffic = 'b67b7d690cc16c4e75e54213cb2d37b4e9c912bcded9105d42befd59d391ad38'
const rfc8448_derived_for_master = '43de77e0c77713859a944db9db2590b53190a65b3ee2e4f12dd7a0bb7ce254b4'
const rfc8448_master_secret = '18df06843d13a08bf2a449844c5f8a478001bc4d4c627984d5a41da8d0402919'
const rfc8448_transcript_hash_ch_sfin = '9608102a0f1ccc6db6250b7b7e417b1a000eaada3daae4777a7686c9ff83df13'
const rfc8448_client_ap_traffic_0 = '9e40646ce79a7f9dc05af8889bce6552875afa0b06df0087f792ebb7c17504a5'
const rfc8448_server_ap_traffic_0 = 'a11af9f05531f856ad47116b45a950328204b4f44bfb6b3a4b4f1f3fcb631643'

// SHA-256("") — the well-known empty-input hash, independent of this
// module's own empty_transcript_hash().
const sha256_of_empty_string = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'

fn test_empty_transcript_hash_matches_known_sha256_of_empty_string() {
	assert empty_transcript_hash() == hex.decode(sha256_of_empty_string)!
}

fn test_transcript_hash_covers_only_handshake_message_bytes_no_record_layer() {
	// RFC 8446 §4.4.1 / RFC 9001 §4: Transcript-Hash covers ONLY the
	// concatenated handshake message bytes (type+length header included,
	// per-message), never any record-layer framing. QUIC has no record
	// layer at all, so this equivalence is exactly what QUIC's transcript
	// hashing must do. Independently verifies both this file's use of
	// crypto.sha256 AND the "no framing" invariant, against real RFC 8448
	// ClientHello/ServerHello handshake message bytes (not record bytes).
	client_hello := hex.decode(rfc8448_client_hello)!
	server_hello := hex.decode(rfc8448_server_hello)!
	mut transcript := []u8{}
	transcript << client_hello
	transcript << server_hello
	got := sha256.sum256(transcript)
	assert got == hex.decode(rfc8448_transcript_hash_ch_sh)!
}

fn test_derive_early_secret_matches_rfc8448_vector() {
	assert derive_early_secret()! == hex.decode(rfc8448_early_secret)!
}

fn test_derive_secret_derived_for_handshake_matches_rfc8448_vector() {
	early_secret := hex.decode(rfc8448_early_secret)!
	got := derive_secret(early_secret, 'derived', empty_transcript_hash())!
	assert got == hex.decode(rfc8448_derived_for_handshake)!
}

fn test_derive_handshake_secrets_matches_rfc8448_vector() {
	early_secret := hex.decode(rfc8448_early_secret)!
	ecdhe_shared_secret := hex.decode(rfc8448_ecdhe_shared_secret)!
	transcript_hash_ch_sh := hex.decode(rfc8448_transcript_hash_ch_sh)!

	secrets := derive_handshake_secrets(early_secret, ecdhe_shared_secret, transcript_hash_ch_sh)!

	assert secrets.handshake_secret == hex.decode(rfc8448_handshake_secret)!
	assert secrets.client_secret == hex.decode(rfc8448_client_hs_traffic)!
	assert secrets.server_secret == hex.decode(rfc8448_server_hs_traffic)!
}

fn test_derive_secret_derived_for_master_matches_rfc8448_vector() {
	handshake_secret := hex.decode(rfc8448_handshake_secret)!
	got := derive_secret(handshake_secret, 'derived', empty_transcript_hash())!
	assert got == hex.decode(rfc8448_derived_for_master)!
}

fn test_derive_application_secrets_matches_rfc8448_vector() {
	handshake_secret := hex.decode(rfc8448_handshake_secret)!
	transcript_hash_ch_sfin := hex.decode(rfc8448_transcript_hash_ch_sfin)!

	secrets := derive_application_secrets(handshake_secret, transcript_hash_ch_sfin)!

	assert secrets.master_secret == hex.decode(rfc8448_master_secret)!
	assert secrets.client_secret == hex.decode(rfc8448_client_ap_traffic_0)!
	assert secrets.server_secret == hex.decode(rfc8448_server_ap_traffic_0)!
}

// test_full_chain_matches_rfc8448_end_to_end runs the entire Early ->
// Handshake -> Master chain back to back exactly as Phase 2c will call it
// (no test-only shortcuts feeding intermediate constants in from the
// side), so a wiring mistake between the three derive_* functions — not
// just a bug within one of them — would be caught.
fn test_full_chain_matches_rfc8448_end_to_end() {
	ecdhe_shared_secret := hex.decode(rfc8448_ecdhe_shared_secret)!
	transcript_hash_ch_sh := hex.decode(rfc8448_transcript_hash_ch_sh)!
	transcript_hash_ch_sfin := hex.decode(rfc8448_transcript_hash_ch_sfin)!

	early_secret := derive_early_secret()!
	handshake_secrets := derive_handshake_secrets(early_secret, ecdhe_shared_secret,
		transcript_hash_ch_sh)!
	application_secrets := derive_application_secrets(handshake_secrets.handshake_secret,
		transcript_hash_ch_sfin)!

	assert handshake_secrets.client_secret == hex.decode(rfc8448_client_hs_traffic)!
	assert handshake_secrets.server_secret == hex.decode(rfc8448_server_hs_traffic)!
	assert application_secrets.client_secret == hex.decode(rfc8448_client_ap_traffic_0)!
	assert application_secrets.server_secret == hex.decode(rfc8448_server_ap_traffic_0)!
}

fn test_synthetic_client_hello1_hash_wire_format() {
	client_hello1 := hex.decode(rfc8448_client_hello)!
	msg := synthetic_client_hello1_hash(client_hello1)

	// 1-byte type (message_hash = 0xfe) + 3-byte length (0x000020 = 32,
	// SHA-256's output length) + the 32-byte digest itself.
	assert msg.len == 36
	assert msg[0] == 0xfe
	assert msg[1] == 0x00
	assert msg[2] == 0x00
	assert msg[3] == 0x20
	assert msg[4..] == sha256.sum256(client_hello1)
}

fn test_synthetic_client_hello1_hash_is_deterministic_and_input_sensitive() {
	a := synthetic_client_hello1_hash([u8(1), 2, 3])
	b := synthetic_client_hello1_hash([u8(1), 2, 3])
	c := synthetic_client_hello1_hash([u8(1), 2, 4])
	assert a == b
	assert a != c
}
