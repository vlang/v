module quic

import crypto.ecdsa
import crypto.rand

// test_full_initial_round_trip_over_fake_transport is Phase 4's capstone
// integration test: assembles a REAL client Initial packet -- a real
// ClientHello (Phase 2's build_client_hello), real CRYPTO framing (this
// phase's frame.v), real packet + header protection (Phase 3) -- then
// "transmits" it over a plain []u8 (an in-memory fake transport; no actual
// socket is involved, this is simply the first point in the whole stack
// where crypto+packet+framing can be exercised together end-to-end without
// one). Every step is then fully reversed on the "receive" side, ending
// with the reassembled CRYPTO stream reproducing the exact original
// ClientHello bytes and re-parsing as a valid handshake message.
fn test_full_initial_round_trip_over_fake_transport() {
	// --- Build a real ClientHello (Phase 2) ---
	priv := ecdsa.PrivateKey.new()!
	pub_key := priv.public_key()!
	ecdhe_public_key := pub_key.uncompressed_bytes()!
	client_hello_random := rand.bytes(32)!
	dcid := rand.bytes(8)! // client's original, self-chosen DCID
	scid := rand.bytes(8)!

	client_hello := build_client_hello(ClientHelloParams{
		random:               client_hello_random
		server_name:          'example.com'
		ecdhe_public_key:     ecdhe_public_key
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: scid
		}
	})!

	// --- Frame it as CRYPTO data at offset 0 (Phase 4: frame.v) ---
	payload := encode_crypto_frame(0, client_hello)!

	// --- Derive Initial secrets + packet protection keys (Phase 2a/3) ---
	secrets := derive_initial_secrets(dcid)!
	client_keys := derive_packet_protection_keys(secrets.client)!

	// --- Build the unprotected header (Phase 1: header.v) ---
	pn_length := 2
	packet_number := u64(0)
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + 16 // + AEAD tag
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << [u8(packet_number >> 8), u8(packet_number)]

	// --- Protect the packet (Phase 3: packet_protection.v + header_protection.v) ---
	protected := protect_packet(header, .long, packet_number, pn_length, payload, client_keys)!

	// --- "Transmit": pad to the real minimum datagram size (Phase 4: coalesce.v) ---
	datagram := pad_datagram_for_initial(protected)
	assert datagram.len >= min_initial_datagram_size

	// === Receive side ===
	packets := split_coalesced_datagram(datagram)!
	assert packets.len == 1
	assert packets[0].form == .long

	long_header, header_len := parse_long_header(packets[0].bytes)!
	assert long_header.typ == .initial
	assert long_header.dcid == dcid
	assert long_header.scid == scid

	// The receiver derives its OWN copy of the keys purely from the
	// packet's own visible DCID -- exactly as a real server would, with no
	// out-of-band key sharing.
	server_view_secrets := derive_initial_secrets(long_header.dcid)!
	server_view_client_keys := derive_packet_protection_keys(server_view_secrets.client)!

	mut received_packet := packets[0].bytes.clone()
	unprotected := unprotect_packet(mut received_packet, header_len, .long,
		server_view_client_keys, none)!
	assert unprotected.packet_number == packet_number

	frames := parse_frames(unprotected.payload)!
	assert frames.len == 1
	mut reassembler := new_crypto_stream_reassembler()
	match frames[0] {
		CryptoFrame {
			reassembler.add(frames[0].offset, frames[0].data)!
		}
		else {
			assert false, 'expected the payload to contain exactly one CryptoFrame'
		}
	}
	assert reassembler.data() == client_hello

	// Full circle: the reassembled bytes parse as a real ClientHello
	// handshake message via Phase 2's own parser.
	msg, consumed := parse_handshake_message(reassembler.data())!
	assert consumed == reassembler.data().len
	assert msg.typ == .client_hello
}

// test_full_initial_round_trip_rejects_tampered_datagram mirrors the happy
// path above but flips a bit in the transmitted datagram's ciphertext
// region before the receive side processes it, confirming the whole
// pipeline fails closed (a clean error, never a panic) rather than
// delivering corrupted CRYPTO data into the reassembler.
fn test_full_initial_round_trip_rejects_tampered_datagram() {
	priv := ecdsa.PrivateKey.new()!
	pub_key := priv.public_key()!
	ecdhe_public_key := pub_key.uncompressed_bytes()!
	dcid := rand.bytes(8)!
	scid := rand.bytes(8)!

	client_hello := build_client_hello(ClientHelloParams{
		random:               rand.bytes(32)!
		server_name:          'example.com'
		ecdhe_public_key:     ecdhe_public_key
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: scid
		}
	})!
	payload := encode_crypto_frame(0, client_hello)!
	secrets := derive_initial_secrets(dcid)!
	client_keys := derive_packet_protection_keys(secrets.client)!

	pn_length := 2
	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + 16
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << [u8(0), 0]
	protected := protect_packet(header, .long, 0, pn_length, payload, client_keys)!
	mut datagram := pad_datagram_for_initial(protected)
	datagram[header.len + 5] ^= 0x01 // deep inside the AEAD ciphertext

	packets := split_coalesced_datagram(datagram)!
	long_header, header_len := parse_long_header(packets[0].bytes)!
	server_view_secrets := derive_initial_secrets(long_header.dcid)!
	server_view_client_keys := derive_packet_protection_keys(server_view_secrets.client)!
	mut received_packet := packets[0].bytes.clone()

	unprotect_packet(mut received_packet, header_len, .long, server_view_client_keys, none) or {
		return
	}
	assert false, 'expected the tampered datagram to be rejected'
}
