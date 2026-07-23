module quic

import net.mbedtls
import encoding.hex
import crypto.sha256

// Real handshake messages captured from an independent reference
// implementation (Cloudflare quiche, via quic-interop-runner's published
// Docker image) -- see testdata/tls13_vectors/README.md for exact
// provenance, extraction method, and what this does/doesn't validate.
// Each constant is one complete handshake message, already framed with
// its 4-byte header (type + 3-byte length), exactly as
// parse_handshake_message expects.
const quiche_vector_client_hello = '010001070303aae46c569ed2c383818039c15f2b9b2136b22d570eac6bce08f02caa38249a32000006130113021303010000d800000010000e00000b717569635f736572766572000a00080006001d001700180010001900170268330a68712d696e7465726f7008687474702f302e39000d00140012040308040401050308050501080606010201003300260024001d00202248ca6660c73714e4fcd54feab9983b52af11f84bd65fcc3b8d36a2c4e3723c002d00020101002b000302030400390048010480007530030245460404809896800504800f42400604800f42400704800f424008024064090240640a01030b01190c000f14d7af43d3ec88a36d745b07a58c8cd29bffa31584'

const quiche_vector_server_hello = '020000560303e863b1b14a082d8779cbd921be0f5754773f06720da127e14a1dd598f233c12000130100002e00330024001d0020d9e7f04a55bd6f25505863974b9c0c21896009c2abf570a45567a350d7b16d41002b00020304'

const quiche_vector_encrypted_extensions = '0800006900670010000500030268330039005a0010a38666b5dd3bb8eb36857f2e457adaf2010480007530030245460404809896800504800f42400604800f42400704800f424008024064090240640a01030b01190c000f14edb1e5824271d5fc09713615f78c85e22e1ecbbc'

const quiche_vector_certificate = '0b00018a000001860001813082017d30820123a003020102021445362cb8dc601b0f3ee50e3da9f2118f95871e36300a06082a8648ce3d04030230143112301006035504030c096c6f63616c686f7374301e170d3236303732303132313431375a170d3236303831393132313431375a30143112301006035504030c096c6f63616c686f73743059301306072a8648ce3d020106082a8648ce3d030107034200047d89f3da057b835fca650463769596af45fe7a8181f834bf1604ecb286afe96bfbca9d11ea4c27d337909a09005ebbe81a01862457d7869f79c7d634f04f7566a3533051301d0603551d0e04160414e987a4681b949e0994a8aa2d76564aa9dd54f727301f0603551d23041830168014e987a4681b949e0994a8aa2d76564aa9dd54f727300f0603551d130101ff040530030101ff300a06082a8648ce3d0403020348003045022100ef425e98d88999e7f08989e14ea4a227db08a5a163e8f66f03df9df13a31dcf7022030d8e4e43d2fb2be5dd6f1a38b70a45088d177fd166759d584a0bde678f3b8ad0000'

const quiche_vector_certificate_verify = '0f00004a04030046304402206edebd40ca25b9243fb4d8136e5de4123e842500a160429cfe2d4177997be39802201a645fb881160d9b50effa6904a2555dae3d553a18e9135aeae79c4bdb4ad6b9'

const quiche_vector_server_finished = '14000020f98a05cf921e165bd80bc774c257981a834673e438310fbc38281308855e164b'

const quiche_vector_client_finished = '14000020596f5453ef03bb4a5502018c68fc006cb410d54fc74b33ce39c7fd9fca77a2e0'

// Traffic secrets from the capture's SSLKEYLOGFILE export (NSS/QUIC
// keylog format) -- the client-side and server-side keylogs were
// confirmed byte-identical before trusting either (see README.md).
const quiche_vector_client_handshake_traffic_secret = 'dc80f12f7ed7e46eda294c644e573d58dfdec06131420d8f75396f22b7fc11d5'[..64]
const quiche_vector_server_handshake_traffic_secret = '4a4b2dc67679dd74eba17ac6bba53953a692b2cd97234778a32d48826f905b44'[..64]

// test_quiche_reference_capture_full_handshake parses every message from a
// real quiche handshake capture with this module's own production
// functions, in order, accumulating the exact same running transcript a
// live client would -- then cross-checks the parts a standard keylog
// capture CAN prove (see README.md for exactly what that is and isn't):
// the real ECDSA CertificateVerify signature, and both directions' real
// Finished MACs. A byte-count match alone (which testdata's own
// extract_handshake.py already verifies against tshark's independent
// size accounting) does not prove correct parsing -- this test's
// successful parse + verify chain is the actual proof.
fn test_quiche_reference_capture_full_handshake() {
	mut transcript := []u8{}

	ch_bytes := hex.decode(quiche_vector_client_hello)!
	transcript << ch_bytes
	ch_msg, _ := parse_handshake_message(ch_bytes)!
	assert ch_msg.typ == .client_hello

	sh_bytes := hex.decode(quiche_vector_server_hello)!
	transcript << sh_bytes
	sh_msg, _ := parse_handshake_message(sh_bytes)!
	assert sh_msg.typ == .server_hello
	parsed_sh := parse_server_hello(sh_msg.body)!
	match parsed_sh {
		ParsedServerHello {
			// TLS_AES_128_GCM_SHA256 -- matches this module's own
			// pinned-single-cipher-suite scope (tls13_client_hello.v).
			assert parsed_sh.cipher_suite == cipher_suite_tls_aes_128_gcm_sha256
			// quiche's client offered x25519 first; unlike this module's
			// own ClientHello (secp256r1-only), it's real, independently-
			// produced wire data exercising a DIFFERENT negotiated group
			// than this module ever sends itself -- proving
			// parse_server_hello's key_share parsing isn't accidentally
			// coupled to only the one group this module happens to offer.
			assert parsed_sh.key_share_group == 0x001d
			assert parsed_sh.key_share_key_exchange.len == 32
		}
		ParsedHelloRetryRequest {
			assert false, 'expected a real ServerHello, got a HelloRetryRequest'
		}
	}

	ee_bytes := hex.decode(quiche_vector_encrypted_extensions)!
	transcript << ee_bytes
	ee_msg, _ := parse_handshake_message(ee_bytes)!
	assert ee_msg.typ == .encrypted_extensions
	ee_exts := parse_encrypted_extensions(ee_msg.body)!
	tp_ext := find_extension(ee_exts, ext_quic_transport_parameters) or {
		assert false, 'expected quic_transport_parameters extension'
		return
	}
	peer_params := decode_transport_parameters(tp_ext.data)!
	// Cross-checked against quiche client's own plaintext trace-log
	// report of these SAME server transport parameters at capture time
	// (see README.md) -- two independent sources (this module's decoder,
	// quiche's own internal state dump) agreeing on the same real bytes.
	assert peer_params.max_idle_timeout or { 0 } == 30000
	assert peer_params.max_udp_payload_size or { 0 } == 1350
	assert peer_params.initial_max_data or { 0 } == 10000000
	assert peer_params.initial_source_connection_id or { []u8{} } == hex.decode('edb1e5824271d5fc09713615f78c85e22e1ecbbc')!

	cert_bytes := hex.decode(quiche_vector_certificate)!
	transcript << cert_bytes
	cert_msg, _ := parse_handshake_message(cert_bytes)!
	assert cert_msg.typ == .certificate
	parsed_cert := parse_certificate(cert_msg.body)!
	assert parsed_cert.certificate_list.len == 1
	ch_cert_hash := sha256.sum256(transcript)

	cv_bytes := hex.decode(quiche_vector_certificate_verify)!
	transcript << cv_bytes
	cv_msg, _ := parse_handshake_message(cv_bytes)!
	assert cv_msg.typ == .certificate_verify
	parsed_cv := parse_certificate_verify(cv_msg.body)!
	assert parsed_cv.algorithm == sig_scheme_ecdsa_secp256r1_sha256

	// The one genuine, previously-missing positive case this capture
	// provides: a real ECDSA P-256 signature that MUST verify
	// successfully. No EC private key exists anywhere in this repo (see
	// net.mbedtls/x509_standalone_signature_test.v's own note), so every
	// other ECDSA test in this codebase can only exercise the rejection
	// path (a key-type mismatch), never a genuine accepted signature.
	mbedtls_chain := mbedtls.build_certificate_chain([parsed_cert.certificate_list[0].cert_data])!
	defer {
		mbedtls.free_certificate_chain(mbedtls_chain)
	}
	pk := mbedtls.get_leaf_public_key(mbedtls_chain)
	signed_content := certificate_verify_signed_content(.server, ch_cert_hash)
	cv_hash := sha256.sum256(signed_content)
	mbedtls.verify_ecdsa_signature(pk, .sha256, cv_hash, parsed_cv.signature)!

	ch_cv_hash := sha256.sum256(transcript)

	server_finished_bytes := hex.decode(quiche_vector_server_finished)!
	server_finished_msg, _ := parse_handshake_message(server_finished_bytes)!
	assert server_finished_msg.typ == .finished
	server_hs_secret := hex.decode(quiche_vector_server_handshake_traffic_secret)!
	server_finished_ok := verify_finished(server_hs_secret, ch_cv_hash, server_finished_msg.body)!
	assert server_finished_ok

	transcript << server_finished_bytes
	ch_sfin_hash := sha256.sum256(transcript)

	client_finished_bytes := hex.decode(quiche_vector_client_finished)!
	client_finished_msg, _ := parse_handshake_message(client_finished_bytes)!
	assert client_finished_msg.typ == .finished
	client_hs_secret := hex.decode(quiche_vector_client_handshake_traffic_secret)!
	client_finished_ok := verify_finished(client_hs_secret, ch_sfin_hash, client_finished_msg.body)!
	assert client_finished_ok
}

// test_quiche_reference_capture_rejects_tampered_certificate_verify is a
// negative-path sanity check using the same real material: flip one bit
// of the genuine captured ECDSA signature and confirm verification fails
// -- proving the positive result above isn't a vacuously-accepting no-op.
fn test_quiche_reference_capture_rejects_tampered_certificate_verify() {
	mut transcript := []u8{}
	transcript << hex.decode(quiche_vector_client_hello)!
	transcript << hex.decode(quiche_vector_server_hello)!
	transcript << hex.decode(quiche_vector_encrypted_extensions)!
	cert_bytes := hex.decode(quiche_vector_certificate)!
	transcript << cert_bytes
	cert_msg, _ := parse_handshake_message(cert_bytes)!
	parsed_cert := parse_certificate(cert_msg.body)!
	ch_cert_hash := sha256.sum256(transcript)

	cv_bytes := hex.decode(quiche_vector_certificate_verify)!
	cv_msg, _ := parse_handshake_message(cv_bytes)!
	parsed_cv := parse_certificate_verify(cv_msg.body)!
	mut tampered_signature := parsed_cv.signature.clone()
	tampered_signature[0] ^= 0xff

	mbedtls_chain := mbedtls.build_certificate_chain([parsed_cert.certificate_list[0].cert_data])!
	defer {
		mbedtls.free_certificate_chain(mbedtls_chain)
	}
	pk := mbedtls.get_leaf_public_key(mbedtls_chain)
	signed_content := certificate_verify_signed_content(.server, ch_cert_hash)
	cv_hash := sha256.sum256(signed_content)
	mbedtls.verify_ecdsa_signature(pk, .sha256, cv_hash, tampered_signature) or { return }
	assert false, 'expected a tampered real signature to be rejected'
}
