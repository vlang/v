// Copyright (c) 2026 blackshirt.
//
// server.v is a minimal, adaptive TLS 1.3 server flight generator for tests: it
// parses the client's real ClientHello and emits a byte-correct ServerHello plus
// encrypted flight (RFC 8446 §7.1), so a record handshaker can be driven to
// .connected without a socket. Scope: the two SHA-256 suites, chosen per instance
// via Config.cipher_suite.
module fakeserver

import crypto.ecdsa
import crypto.ed25519
import crypto.rand
import x.crypto.curve25519
import tls.wire

// Handshake message types (RFC 8446 §4) the fakeserver produces or parses.
const hs_client_hello = u8(1)
const hs_server_hello = u8(2)
const hs_new_session_ticket = u8(4)
const hs_encrypted_extensions = u8(8)
const hs_certificate = u8(11)
const hs_certificate_request = u8(13)
const hs_certificate_verify = u8(15)
const hs_finished = u8(20)
const hs_message_hash = u8(254)

// Extension types (RFC 8446 §4.2) the ClientHello parser understands.
const ext_supported_groups = u16(10)
const ext_signature_algorithms = u16(13)
const ext_alpn = u16(16)
const ext_supported_versions = u16(43)
const ext_cookie = u16(44)
const ext_key_share = u16(51)
const ext_quic_transport_parameters = u16(57)

// Named groups (RFC 8446 §4.2.7) the fakeserver can negotiate.
const group_secp256r1 = u16(0x0017)
const group_x25519 = u16(0x001d)

// The cipher suites the fakeserver can select (RFC 8446 §B.4), both SHA-256 with a
// 12-byte IV; AES-128-GCM keys are 16 bytes, ChaCha20-Poly1305 keys 32.
const suite_tls_aes_128_gcm_sha256 = u16(0x1301)
const suite_tls_chacha20_poly1305_sha256 = u16(0x1303)

// TLS 1.3's real version, hidden behind legacy 0x0303 on the wire (§4.2.1).
const tls13_version = u16(0x0304)

// hrr_random is the fixed HelloRetryRequest sentinel random of RFC 8446 §4.1.3.
const hrr_random = [u8(0xcf), 0x21, 0xad, 0x74, 0xe5, 0x9a, 0x61, 0x11, 0xbe, 0x1d, 0x8c, 0x02,
	0x1e, 0x65, 0xb8, 0x91, 0xc2, 0xa2, 0x11, 0x16, 0x7a, 0xbb, 0x8c, 0x5e, 0x07, 0x9e, 0x09, 0xe2,
	0xc8, 0xa8, 0x33, 0x9c]

// hrr_cookie is the opaque cookie the cookie-only HelloRetryRequest carries; the
// client must echo it verbatim (RFC 8446 §4.2.2).
const hrr_cookie = 'fakeserver-hrr-cookie-v1'.bytes()

// ClientHello is the subset of RFC 8446 §4.1.2 the fakeserver needs to pick a
// flight; everything else (SNI, signature_algorithms, ...) is parsed past.
struct ClientHello {
mut:
	random      []u8
	session_id  []u8
	suites      []u16
	groups      []u16
	share_group u16
	share       []u8
	cookie      []u8
	alpn        []string
	versions    []u16
	is_quic     bool
}

// HrrMode selects the HelloRetryRequest behavior of the next new_server.
pub enum HrrMode {
	none         // answer the first ClientHello with a normal ServerHello
	change_group // HRR that requests secp256r1 instead of the offered x25519
	cookie_only  // HRR that supplies a cookie but keeps the client's key share
}

// Config customises one fakeserver instance; it is @[params] so tests write
// fakeserver.new_server(hrr: .change_group) without a positional Config.
@[params]
pub struct Config {
pub:
	// alpn is the server's preference-ordered protocol list (RFC 7301 §3.1).
	alpn []string = []string{}
	// cipher_suite is the suite the server selects from the client's offer. It defaults
	// to TLS_AES_128_GCM_SHA256 so every existing test keeps its current flight; setting
	// TLS_CHACHA20_POLY1305_SHA256 drives the record layer's §7.5 key-ownership path
	// end to end (the suite where a caller-side key scrub used to corrupt the live AEAD).
	cipher_suite u16 = suite_tls_aes_128_gcm_sha256
	// hrr requests a HelloRetryRequest round trip before the real flight.
	hrr HrrMode = .none
	// bad_finished flips the last byte of the server Finished verify_data so the
	// client's Finished check must reject the flight.
	bad_finished bool
	// bad_certificate_verify corrupts the CertificateVerify signature so the
	// client's Ed25519 check must reject the flight.
	bad_certificate_verify bool
	// force_alpn puts this protocol into EncryptedExtensions regardless of what
	// the client offered, proving the RFC 7301 §3.1 check aborts on an unoffered selection.
	force_alpn string
	// client_auth sends a CertificateRequest after EncryptedExtensions and expects
	// the client's empty Certificate (RFC 8446 §4.3.2/§4.4.2) in the transcript.
	client_auth bool
	// quic_scid is sent as initial_source_connection_id when the ClientHello carries
	// the QUIC transport-parameters extension.
	quic_scid []u8
	// quic_original_dcid is sent as original_destination_connection_id: the
	// destination connection ID the client used in its first Initial packet.
	// RFC 9000 §7.3 makes a server that omits it non-compliant, so a QUIC fakeserver
	// must be given it.
	quic_original_dcid []u8
}

// Stage tracks where the fakeserver is in its two-hello state machine.
enum Stage {
	expect_ch              // awaiting the first ClientHello
	expect_ch2             // HelloRetryRequest sent; awaiting the retry ClientHello
	expect_client_cert     // CertificateRequest sent; awaiting the client's empty Certificate
	expect_client_finished // server flight sent; awaiting the client Finished
	done                   // client Finished verified; connection logically complete
}

// Server is one adaptive fakeserver instance, single-use per handshake:
// feed it the client's ClientHello(s), then its Finished.
@[noinit]
pub struct Server {
mut:
	cfg                 Config
	stage               Stage
	transcript          Transcript
	session_id          []u8
	shared_secret       []u8
	handshake_secret    []u8
	share_group         u16
	hrr_requested_group u16
	hrr_cookie_sent     []u8
	client_hsk_traffic  []u8
	server_hsk_traffic  []u8
	client_app_traffic  []u8
	server_app_traffic  []u8
	// pending_tail holds the server-flight messages deferred until the client's
	// empty Certificate is folded into the transcript.
	pending_tail [][]u8
}

// Flight is one server response: the (possibly HelloRetryRequest) ServerHello in
// plaintext, plus the remaining messages the caller must protect.
pub struct Flight {
pub:
	// Whether server_hello is a HelloRetryRequest.
	is_hrr bool
	// Complete plaintext ServerHello or HelloRetryRequest message.
	server_hello []u8
	// Remaining server handshake messages, which the caller must protect at
	// the Handshake level.
	rest [][]u8
}

// Transcript is a SHA-256 transcript of full handshake messages (RFC 8446 §4.4.1).
// It keeps raw messages because an HRR rewrites ClientHello1 into message_hash.
struct Transcript {
mut:
	msgs [][]u8
}

// append adds one complete handshake message (header included) to the transcript.
fn (mut t Transcript) append(msg []u8) {
	t.msgs << msg.clone()
}

// hash returns the running Transcript-Hash: SHA-256 over all appended messages.
fn (t &Transcript) hash() []u8 {
	mut total := []u8{}
	for m in t.msgs {
		total << m
	}
	return sha256_digest(total)
}

// apply_hello_retry_request replaces ClientHello1 with the synthetic message_hash
// and keeps the HRR: [message_hash, HRR, ClientHello2, ...] (RFC 8446 §4.4.1).
fn (mut t Transcript) apply_hello_retry_request(client_hello1 []u8, hrr []u8) {
	ch1_hash := sha256_digest(client_hello1)
	mut synthetic := []u8{cap: 4 + ch1_hash.len}
	synthetic << hs_message_hash
	synthetic << 0x00
	synthetic << 0x00
	synthetic << u8(ch1_hash.len)
	synthetic << ch1_hash
	t.msgs = [][]u8{}
	t.msgs << synthetic
	t.msgs << hrr.clone()
}

// new_server builds a fakeserver configured by cfg.
pub fn new_server(cfg Config) !Server {
	return Server{
		cfg:   cfg
		stage: .expect_ch
	}
}

// handle_client_hello consumes one complete ClientHello (or its retry) and
// returns the flight to process next; the first call may yield an HRR.
pub fn (mut s Server) handle_client_hello(msg []u8) !Flight {
	ch := parse_client_hello(msg)!
	if !ch.versions.contains(tls13_version) {
		return error('fakeserver: ClientHello did not offer TLS 1.3')
	}
	if !ch.suites.contains(s.cfg.cipher_suite) {
		return error('fakeserver: ClientHello did not offer the configured cipher suite 0x${s.cfg.cipher_suite:04x}')
	}
	if ch.share_group == 0 || ch.share.len == 0 {
		return error('fakeserver: ClientHello carried no key_share entry')
	}

	match s.stage {
		.expect_ch {
			s.session_id = ch.session_id.clone()
			if s.cfg.hrr != .none {
				return s.send_hello_retry_request(ch, msg)
			}
			return s.finish_flight(ch, msg)
		}
		.expect_ch2 {
			if ch.session_id != s.session_id {
				return error('fakeserver: retry ClientHello changed the legacy session id')
			}
			if s.hrr_requested_group != 0 && ch.share_group != s.hrr_requested_group {
				return error('fakeserver: retry ClientHello used group 0x${ch.share_group:04x}, expected 0x${s.hrr_requested_group:04x}')
			}
			if s.hrr_cookie_sent.len > 0 && ch.cookie != s.hrr_cookie_sent {
				return error('fakeserver: retry ClientHello echoed a wrong cookie')
			}
			return s.finish_flight(ch, msg)
		}
		.expect_client_cert, .expect_client_finished, .done {
			return error('fakeserver: unexpected extra ClientHello (stage ${s.stage})')
		}
	}
}

// send_hello_retry_request emits the HRR for the configured HrrMode, rewrites the
// transcript (RFC 8446 §4.4.1), and advances to the retry wait.
fn (mut s Server) send_hello_retry_request(ch ClientHello, ch1_msg []u8) !Flight {
	mut req_group := u16(0)
	mut cookie := []u8{}
	if s.cfg.hrr == .change_group {
		req_group = pick_hrr_group(ch.groups, ch.share_group)!
	} else {
		cookie = hrr_cookie.clone()
	}
	hrr := build_hrr(s.session_id, s.cfg.cipher_suite, req_group, cookie)!
	s.transcript.apply_hello_retry_request(ch1_msg, hrr)
	s.hrr_requested_group = req_group
	s.hrr_cookie_sent = cookie.clone()
	s.stage = .expect_ch2
	return Flight{
		is_hrr:       true
		server_hello: hrr
	}
}

// pick_hrr_group chooses the group an HRR requests: secp256r1 when offered
// alongside the current share, else another offered group (RFC 8446 §4.2.8).
fn pick_hrr_group(groups []u16, current u16) !u16 {
	for g in groups {
		if g == group_secp256r1 && g != current {
			return g
		}
	}
	for g in groups {
		if g != current && (g == group_x25519 || g == group_secp256r1) {
			return g
		}
	}
	return error('fakeserver: client offered no alternative group for HelloRetryRequest')
}

// finish_flight performs the (EC)DHE exchange, builds ServerHello, derives the
// handshake secrets and emits the flight (RFC 8446 §4.4.1/§4.4.2).
fn (mut s Server) finish_flight(ch ClientHello, ch_msg []u8) !Flight {
	server_share, shared := server_ecdhe(ch.share_group, ch.share)!
	s.shared_secret = shared
	s.share_group = ch.share_group

	sh := build_server_hello(s.session_id, s.cfg.cipher_suite, ch.share_group, server_share)!
	// Transcript now covers ClientHello2 (or 1) and ServerHello (th_ch_sh, §7.1).
	s.transcript.append(ch_msg)
	s.transcript.append(sh)
	s.derive_handshake_secrets()!

	alpn_proto := if s.cfg.force_alpn.len > 0 {
		s.cfg.force_alpn
	} else {
		pick_alpn(s.cfg.alpn, ch.alpn)
	}
	ee := build_encrypted_extensions(alpn_proto, ch.is_quic, s.cfg.quic_original_dcid,
		s.cfg.quic_scid)!
	s.transcript.append(ee)

	if s.cfg.client_auth {
		// §4.3.2: wait for the client's empty Certificate before the server tail.
		creq := build_certificate_request()!
		s.transcript.append(creq)
		s.stage = .expect_client_cert
		return Flight{
			is_hrr:       false
			server_hello: sh
			rest:         [ee, creq]
		}
	}

	tail := s.build_server_tail()!
	s.stage = .expect_client_finished
	mut rest := [][]u8{cap: 1 + tail.len}
	rest << ee
	rest << tail
	return Flight{
		is_hrr:       false
		server_hello: sh
		rest:         rest
	}
}

// build_server_tail builds Certificate + CertificateVerify + Finished over the
// current transcript (after EncryptedExtensions, and the client Certificate if any).
fn (mut s Server) build_server_tail() ![][]u8 {
	cert := build_certificate(leaf_der() or {
		return error('fakeserver: cannot decode embedded leaf certificate (${err.msg()})')
	})!

	// CertificateVerify signs the transcript through Certificate (§4.4.3).
	s.transcript.append(cert)
	th_cv := s.transcript.hash()
	signed := server_signed_content(th_cv)
	mut sig := ed25519.sign(leaf_private_key() or {
		return error('fakeserver: cannot load leaf signing key (${err.msg()})')
	}, signed) or {
		return error('fakeserver: CertificateVerify signing failed (${err.msg()})')
	}
	if s.cfg.bad_certificate_verify {
		sig[0] = sig[0] ^ 0xff
	}
	cv := build_certificate_verify(sig)!
	s.transcript.append(cv)

	// Server Finished covers the transcript through CertificateVerify (§4.4.4).
	th_sfin := s.transcript.hash()
	mut verify_data := finished_verify_data(s.server_hsk_traffic, th_sfin)!
	if s.cfg.bad_finished {
		verify_data[verify_data.len - 1] = verify_data[verify_data.len - 1] ^ 0xff
	}
	fin := wrap_handshake(hs_finished, verify_data)
	s.transcript.append(fin)

	// Application secrets bind to the transcript including server Finished (§7.1).
	s.derive_application_secrets()!
	return [cert, cv, fin]
}

// handle_client_certificate consumes the client's empty Certificate response,
// folds it into the transcript, and returns the deferred server tail.
pub fn (mut s Server) handle_client_certificate(msg []u8) ![][]u8 {
	if s.stage != .expect_client_cert {
		return error('fakeserver: client Certificate is unexpected in stage ${s.stage}')
	}
	mut r := wire.new_reader(msg)
	typ := r.read_u8()!
	if typ != hs_certificate {
		return error('fakeserver: expected Certificate (11), got handshake type ${typ}')
	}
	blen := int(r.read_u24()!)
	body := r.read_bytes(blen)!
	if r.remaining() != 0 {
		return error('fakeserver: trailing bytes after client Certificate')
	}
	// §4.4.2: the client echoes our request context and sends an empty list.
	mut br := wire.new_reader(body)
	req_ctx := br.read_vector_u8()!
	if req_ctx.len != 0 {
		return error('fakeserver: client returned a non-empty certificate_request_context')
	}
	cert_list := br.read_vector_u24()!
	if cert_list.len != 0 {
		return error('fakeserver: client Certificate list is not empty')
	}
	if br.remaining() != 0 {
		return error('fakeserver: trailing bytes in client Certificate body')
	}
	s.transcript.append(msg)
	tail := s.build_server_tail()!
	s.stage = .expect_client_finished
	return tail
}

// handle_client_finished verifies the client Finished against the full transcript,
// completing the handshake.
pub fn (mut s Server) handle_client_finished(msg []u8) ! {
	if s.stage != .expect_client_finished {
		return error('fakeserver: client Finished is unexpected in stage ${s.stage}')
	}
	mut r := wire.new_reader(msg)
	typ := r.read_u8()!
	if typ != hs_finished {
		return error('fakeserver: expected Finished (20), got handshake type ${typ}')
	}
	blen := int(r.read_u24()!)
	body := r.read_bytes(blen)!
	if r.remaining() != 0 {
		return error('fakeserver: trailing bytes after client Finished')
	}
	if body.len != hash_len {
		return error('fakeserver: client Finished verify_data is ${body.len} bytes, expected ${hash_len}')
	}
	th := s.transcript.hash()
	expected := finished_verify_data(s.client_hsk_traffic, th)!
	if body != expected {
		return error('fakeserver: client Finished verify_data mismatch')
	}
	s.transcript.append(msg)
	s.stage = .done
}

// derive_handshake_secrets runs the §7.1 handshake phase: Early Secret, Handshake
// Secret mixing in the ECDHE shared secret, and the traffic secrets over th_ch_sh.
fn (mut s Server) derive_handshake_secrets() ! {
	zeros := []u8{len: hash_len}
	empty := sha256_digest([]u8{})
	early_secret := hkdf_extract(zeros.clone(), zeros.clone())
	derived_early := derive_secret(early_secret, 'derived', empty)!
	handshake_secret := hkdf_extract(derived_early, s.shared_secret)
	th := s.transcript.hash()
	s.client_hsk_traffic = derive_secret(handshake_secret, 'c hs traffic', th)!
	s.server_hsk_traffic = derive_secret(handshake_secret, 's hs traffic', th)!
	// Retained: the Master Secret derives from it in the application phase (§7.1).
	s.handshake_secret = handshake_secret
}

// derive_application_secrets runs the §7.1 application phase from the retained
// Handshake Secret: Master Secret, then the application traffic secrets.
fn (mut s Server) derive_application_secrets() ! {
	empty := sha256_digest([]u8{})
	derived_hs := derive_secret(s.handshake_secret, 'derived', empty)!
	zeros := []u8{len: hash_len}
	master_secret := hkdf_extract(derived_hs, zeros)
	th := s.transcript.hash()
	s.client_app_traffic = derive_secret(master_secret, 'c ap traffic', th)!
	s.server_app_traffic = derive_secret(master_secret, 's ap traffic', th)!
}

// aead_key_len is the selected suite's AEAD key length (RFC 8446 §7.1): 32 bytes for
// ChaCha20-Poly1305, otherwise the AES-128-GCM default of 16.
fn (s &Server) aead_key_len() int {
	return if s.cfg.cipher_suite == suite_tls_chacha20_poly1305_sha256 { 32 } else { 16 }
}

// server_handshake_keys returns the server-direction handshake key and IV, so the
// test transport can encrypt the flight records.
pub fn (s &Server) server_handshake_keys() !([]u8, []u8) {
	if s.server_hsk_traffic.len == 0 {
		return error('fakeserver: handshake secrets not derived yet (ServerHello not built)')
	}
	return traffic_keys(s.server_hsk_traffic, s.aead_key_len())
}

// client_handshake_keys returns the client-direction handshake key and IV, so the
// test transport can decrypt the client Finished.
pub fn (s &Server) client_handshake_keys() !([]u8, []u8) {
	if s.client_hsk_traffic.len == 0 {
		return error('fakeserver: handshake secrets not derived yet (ServerHello not built)')
	}
	return traffic_keys(s.client_hsk_traffic, s.aead_key_len())
}

// server_handshake_secret returns the raw §7.1 server handshake traffic secret,
// for a QUIC test peer's RFC 9001 §5.1 "quic key"/"quic iv"/"quic hp" expansions.
pub fn (s &Server) server_handshake_secret() ![]u8 {
	if s.server_hsk_traffic.len == 0 {
		return error('fakeserver: handshake secrets not derived yet (ServerHello not built)')
	}
	return s.server_hsk_traffic.clone()
}

// client_handshake_secret returns the raw §7.1 client handshake traffic secret,
// for a QUIC test peer to derive packet protection.
pub fn (s &Server) client_handshake_secret() ![]u8 {
	if s.client_hsk_traffic.len == 0 {
		return error('fakeserver: handshake secrets not derived yet (ServerHello not built)')
	}
	return s.client_hsk_traffic.clone()
}

// server_app_keys returns the server-direction application key and IV, so the
// test transport can send post-handshake records the client will accept.
pub fn (s &Server) server_app_keys() !([]u8, []u8) {
	if s.server_app_traffic.len == 0 {
		return error('fakeserver: application secrets not derived yet (server Finished not built)')
	}
	return traffic_keys(s.server_app_traffic, s.aead_key_len())
}

// server_app_secret returns the raw §7.1 server application traffic secret, for a
// QUIC test peer to derive 1-RTT packet protection.
pub fn (s &Server) server_app_secret() ![]u8 {
	if s.server_app_traffic.len == 0 {
		return error('fakeserver: application secrets not derived yet (server Finished not built)')
	}
	return s.server_app_traffic.clone()
}

// new_session_ticket builds a valid NewSessionTicket (RFC 8446 §4.6.1) the
// client's post-handshake parser must accept and store.
pub fn (s &Server) new_session_ticket() ![]u8 {
	mut body := wire.new_writer(64)
	body.write_u32(3600) // ticket_lifetime (<= 604800)
	body.write_u32(0) // ticket_age_add
	body.write_vector_u8([u8(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16])! // ticket_nonce
	body.write_vector_u16([u8(0xaa), 0xbb, 0xcc, 0xdd])! // ticket
	body.write_vector_u16([]u8{})! // extensions
	return wrap_handshake(hs_new_session_ticket, body.bytes())
}

// flatten concatenates flight messages into one byte stream, in send order.
pub fn flatten(msgs [][]u8) []u8 {
	mut out := []u8{}
	for m in msgs {
		out << m
	}
	return out
}

// wrap_handshake frames a handshake body with its 4-byte header
// (HandshakeType || 3-byte length, RFC 8446 §4).
fn wrap_handshake(typ u8, body []u8) []u8 {
	mut w := wire.new_writer(4 + body.len)
	w.write_u8(typ)
	w.write_u24(u32(body.len))
	w.write_bytes(body)
	return w.bytes()
}

// parse_client_hello decodes the RFC 8446 §4.1.2 fields the fakeserver needs
// (groups, key_share, ALPN, versions, cookie); every framing error is returned.
fn parse_client_hello(msg []u8) !ClientHello {
	mut r := wire.new_reader(msg)
	typ := r.read_u8()!
	if typ != hs_client_hello {
		return error('fakeserver: expected ClientHello (1), got handshake type ${typ}')
	}
	blen := int(r.read_u24()!)
	body := r.read_bytes(blen)!
	if r.remaining() != 0 {
		return error('fakeserver: trailing bytes after ClientHello message')
	}

	mut b := wire.new_reader(body)
	legacy := b.read_u16()!
	if legacy != 0x0303 {
		return error('fakeserver: ClientHello legacy_version must be 0x0303, got 0x${legacy:04x}')
	}
	random := b.read_bytes(32)!
	session_id := b.read_vector_u8()!

	suites_bytes := b.read_vector_u16()!
	if suites_bytes.len % 2 != 0 {
		return error('fakeserver: ClientHello cipher_suites has odd length ${suites_bytes.len}')
	}
	mut suites := []u16{}
	for i := 0; i < suites_bytes.len; i += 2 {
		suites << (u16(suites_bytes[i]) << 8 | u16(suites_bytes[i + 1]))
	}

	comp := b.read_vector_u8()!
	if comp.len == 0 || comp[0] != 0x00 {
		return error('fakeserver: ClientHello missing null compression method')
	}

	ext_bytes := b.read_vector_u16()!
	if b.remaining() != 0 {
		return error('fakeserver: trailing bytes after ClientHello extensions')
	}

	mut ch := ClientHello{
		random:     random
		session_id: session_id
		suites:     suites
	}
	mut e := wire.new_reader(ext_bytes)
	for e.remaining() > 0 {
		etyp := e.read_u16()!
		edata := e.read_vector_u16()!
		mut d := wire.new_reader(edata)
		if etyp == ext_supported_groups {
			gb := d.read_vector_u16()!
			if gb.len % 2 != 0 {
				return error('fakeserver: supported_groups has odd length ${gb.len}')
			}
			for i := 0; i < gb.len; i += 2 {
				ch.groups << (u16(gb[i]) << 8 | u16(gb[i + 1]))
			}
		} else if etyp == ext_alpn {
			// RFC 7301: ProtocolNameList = u16 vec of (u8 vec ProtocolName).
			pl := d.read_vector_u16()!
			mut pr := wire.new_reader(pl)
			for pr.remaining() > 0 {
				proto := pr.read_vector_u8()!
				ch.alpn << proto.bytestr()
			}
		} else if etyp == ext_supported_versions {
			// In ClientHello this is a u8-length vector of versions (§4.2.1).
			vb := d.read_vector_u8()!
			if vb.len % 2 != 0 {
				return error('fakeserver: supported_versions has odd length ${vb.len}')
			}
			for i := 0; i < vb.len; i += 2 {
				ch.versions << (u16(vb[i]) << 8 | u16(vb[i + 1]))
			}
		} else if etyp == ext_cookie {
			ch.cookie = d.read_vector_u16()!
		} else if etyp == ext_key_share {
			// u16 vector of KeyShareEntry (group + key_exchange); take the first entry (§4.2.8).
			ks := d.read_vector_u16()!
			mut kr := wire.new_reader(ks)
			if kr.remaining() >= 4 {
				ch.share_group = kr.read_u16()!
				ch.share = kr.read_vector_u16()!
			}
		} else if etyp == ext_quic_transport_parameters {
			ch.is_quic = true
		} else if etyp == ext_signature_algorithms {
			// Offered; not needed for a server-auth-only flight.
		}
		// server_name (0) and anything else: ignored; SNI is not verified here.
	}
	return ch
}

// pick_alpn selects the first of the server's preference-ordered protocols the
// client also offered, or '' when there is no overlap (RFC 7301 §3.1).
fn pick_alpn(server_list []string, client_list []string) string {
	for p in server_list {
		if client_list.contains(p) {
			return p
		}
	}
	return ''
}

// server_ecdhe generates the server's ephemeral key pair for the negotiated group
// and computes the shared secret, returning (public key_exchange, shared secret).
fn server_ecdhe(group u16, client_share []u8) !([]u8, []u8) {
	if group == group_x25519 {
		if client_share.len != 32 {
			return error('fakeserver: x25519 key_share is ${client_share.len} bytes, expected 32')
		}
		mut priv := curve25519.PrivateKey.new() or {
			return error('fakeserver: X25519 key generation failed (${err.msg()})')
		}
		shared := priv.x25519(client_share) or {
			return error('fakeserver: X25519 ECDHE failed (${err.msg()})')
		}
		if shared == []u8{len: 32} {
			return error('fakeserver: X25519 shared secret is all-zero (small-order peer point)')
		}
		server_pub := priv.public_key() or {
			return error('fakeserver: X25519 public-key derivation failed (${err.msg()})')
		}
		raw := server_pub.bytes() or {
			return error('fakeserver: X25519 public-key encoding failed (${err.msg()})')
		}
		return raw, shared
	}
	if group == group_secp256r1 {
		if client_share.len != 65 || client_share[0] != 0x04 {
			return error('fakeserver: secp256r1 key_share must be a 65-byte uncompressed point')
		}
		mut priv := ecdsa.PrivateKey.new(nid: .prime256v1) or {
			return error('fakeserver: P-256 key generation failed (${err.msg()})')
		}
		peer := ecdsa.PublicKey.from_uncompressed_bytes(client_share, nid: .prime256v1) or {
			return error('fakeserver: cannot parse client P-256 key_share (${err.msg()})')
		}
		shared := priv.derive_shared_secret(peer) or {
			return error('fakeserver: P-256 ECDHE failed (${err.msg()})')
		}
		if shared == []u8{len: shared.len} {
			return error('fakeserver: P-256 shared secret is all-zero')
		}
		server_pub := priv.public_key() or {
			return error('fakeserver: P-256 public-key derivation failed (${err.msg()})')
		}
		raw := server_pub.uncompressed_bytes() or {
			return error('fakeserver: P-256 public-key encoding failed (${err.msg()})')
		}
		return raw, shared
	}
	return error('fakeserver: unsupported key_share group 0x${group:04x}')
}

// build_server_hello emits a TLS 1.3 ServerHello echoing the session id and
// selecting the negotiated suite and group (RFC 8446 §4.1.3).
fn build_server_hello(session_id []u8, suite u16, share_group u16, server_share []u8) ![]u8 {
	mut body := wire.new_writer(128)
	body.write_u16(0x0303) // legacy_version (§4.1.3)
	body.write_bytes(rand.bytes(32) or {
		return error('fakeserver: ServerHello random generation failed (${err.msg()})')
	})
	body.write_vector_u8(session_id)! // legacy_session_id_echo
	body.write_u16(suite)
	body.write_u8(0x00) // legacy_compression_method

	mut ext := wire.new_writer(64)
	// supported_versions: bare u16 in ServerHello (§4.2.1).
	ext.write_u16(ext_supported_versions)
	ext.write_vector_u16([u8(tls13_version >> 8), u8(tls13_version & 0xff)])!
	// key_share: extension_data is one KeyShareEntry, no outer vector wrapper (RFC 8446 §4.2.8).
	mut entry := wire.new_writer(server_share.len + 4)
	entry.write_u16(share_group)
	entry.write_vector_u16(server_share)!
	ext.write_u16(ext_key_share)
	ext.write_vector_u16(entry.bytes())!

	body.write_vector_u16(ext.bytes())!
	return wrap_handshake(hs_server_hello, body.bytes())
}

// build_hrr emits a HelloRetryRequest: a ServerHello whose random is the fixed
// §4.1.3 sentinel, carrying a selected_group or a cookie.
fn build_hrr(session_id []u8, suite u16, req_group u16, cookie []u8) ![]u8 {
	mut body := wire.new_writer(128)
	body.write_u16(0x0303)
	body.write_bytes(hrr_random.clone())
	body.write_vector_u8(session_id)!
	body.write_u16(suite)
	body.write_u8(0x00)

	mut ext := wire.new_writer(64)
	ext.write_u16(ext_supported_versions)
	ext.write_vector_u16([u8(tls13_version >> 8), u8(tls13_version & 0xff)])!
	if req_group != 0 {
		// HRR key_share is the bare selected_group u16, no entry framing (§4.1.4).
		mut kg := wire.new_writer(2)
		kg.write_u16(req_group)
		ext.write_u16(ext_key_share)
		ext.write_vector_u16(kg.bytes())!
	}
	if cookie.len > 0 {
		// RFC 8446 §4.2.2: extension_data is itself a u16-length-prefixed cookie.
		mut cookie_w := wire.new_writer(cookie.len + 2)
		cookie_w.write_vector_u16(cookie)!
		ext.write_u16(ext_cookie)
		ext.write_vector_u16(cookie_w.bytes())!
	}

	body.write_vector_u16(ext.bytes())!
	return wrap_handshake(hs_server_hello, body.bytes())
}

// build_encrypted_extensions emits an EncryptedExtensions body carrying the
// negotiated ALPN and, on a QUIC connection, this server's transport parameters
// (RFC 8446 §4.3.1, RFC 9001 §8.2).
fn build_encrypted_extensions(alpn_proto string, is_quic bool, quic_original_dcid []u8, quic_scid []u8) ![]u8 {
	mut ext := wire.new_writer(32)
	if alpn_proto.len > 0 {
		mut proto_list := wire.new_writer(alpn_proto.len + 3)
		proto_list.write_vector_u8(alpn_proto.bytes())!
		mut alpn_body := wire.new_writer(proto_list.len() + 2)
		alpn_body.write_vector_u16(proto_list.bytes())!
		ext.write_u16(ext_alpn)
		ext.write_vector_u16(alpn_body.bytes())!
	}
	if is_quic {
		if quic_original_dcid.len > 20 || quic_scid.len > 20 {
			return error('fakeserver: QUIC connection ID exceeds 20 bytes')
		}
		// The TLS handshake treats the extension content as opaque, and this test
		// server must not depend on tls.quic, so the two connection-ID triplets are
		// written by hand: parameter id, byte length, then the connection ID. All the
		// integers stay below 64, so each is a valid one-byte QUIC varint
		// (RFC 9000 §16); tls.quic's transport_params.v holds the canonical encoder
		// this mirrors.
		mut tp_cap := quic_original_dcid.len + quic_scid.len
		tp_cap += 6
		mut transport_parameters := []u8{cap: tp_cap}
		// RFC 9000 §18.2 parameter ids, written in ascending order.
		transport_parameters << 0x00 // original_destination_connection_id parameter ID
		transport_parameters << u8(quic_original_dcid.len)
		transport_parameters << quic_original_dcid
		transport_parameters << 0x0f // initial_source_connection_id parameter ID
		transport_parameters << u8(quic_scid.len)
		transport_parameters << quic_scid
		ext.write_u16(ext_quic_transport_parameters)
		ext.write_vector_u16(transport_parameters)!
	}
	mut body := wire.new_writer(ext.len() + 2)
	body.write_vector_u16(ext.bytes())!
	return wrap_handshake(hs_encrypted_extensions, body.bytes())
}

// build_certificate emits a server Certificate with a single leaf entry and an
// empty certificate_request_context (RFC 8446 §4.4.2).
fn build_certificate(leaf []u8) ![]u8 {
	mut cert_list := wire.new_writer(leaf.len + 8)
	cert_list.write_vector_u24(leaf)!
	cert_list.write_vector_u16([]u8{})! // per-certificate extensions (unused)
	mut body := wire.new_writer(cert_list.len() + 8)
	body.write_vector_u8([]u8{})! // certificate_request_context: empty in server auth
	body.write_vector_u24(cert_list.bytes())!
	return wrap_handshake(hs_certificate, body.bytes())
}

// server_signed_content builds the RFC 8446 §4.4.3 CertificateVerify input:
// 64 space octets || context || 0x00 || transcript hash.
fn server_signed_content(transcript_hash []u8) []u8 {
	context := 'TLS 1.3, server CertificateVerify'.bytes()
	mut out := []u8{cap: 64 + context.len + 1 + transcript_hash.len}
	for _ in 0 .. 64 {
		out << 0x20
	}
	out << context
	out << 0x00
	out << transcript_hash
	return out
}

// build_certificate_verify emits a CertificateVerify carrying an Ed25519 signature
// over signed_content (RFC 8446 §4.4.3).
fn build_certificate_verify(signature []u8) ![]u8 {
	mut body := wire.new_writer(signature.len + 4)
	body.write_u16(0x0807) // ed25519 (§4.2.3)
	body.write_vector_u16(signature)!
	return wrap_handshake(hs_certificate_verify, body.bytes())
}

// build_certificate_request emits a CertificateRequest (RFC 8446 §4.3.2) with an
// empty request context and only the signature_algorithms extension (Ed25519).
fn build_certificate_request() ![]u8 {
	// signature_algorithms: a u16-length list of schemes, here just Ed25519.
	mut sa_body := wire.new_writer(4)
	sa_body.write_vector_u16([u8(0x08), 0x07])!
	mut ext := wire.new_writer(sa_body.len() + 4)
	ext.write_u16(ext_signature_algorithms)
	ext.write_vector_u16(sa_body.bytes())!

	mut body := wire.new_writer(ext.len() + 3)
	body.write_vector_u8([]u8{})! // certificate_request_context: empty
	body.write_vector_u16(ext.bytes())!
	return wrap_handshake(hs_certificate_request, body.bytes())
}
