// Copyright (c) 2026 blackshirt.
//
// state_machine_extra.v holds the core's intake plumbing and terminal-failure
// primitives: the reassembly buffer that holds inbound bytes until they frame into a
// complete message, the receive/dispatch routing that bounds, frames and gates those
// bytes against the state machine's ordering, and the failure-classification
// primitives every error path funnels through. The phase transitions themselves —
// ServerHello/HelloRetryRequest and the encrypted flight — are state_machine.v's.
module handshake

// failure_alert returns the alert description an adapter should surface when
// this handshake dies (RFC 8446 §6).
pub fn (h &HandshakeCore) failure_alert() AlertDescription {
	return h.fail_alert
}

// classify_failure is the core's single terminal-failure primitive: it pins the alert,
// zeroes every derived secret, parks .failed, and returns the IError (§7.5), so no site
// can record an alert and forget to zeroize. Idempotent, so safety nets stay harmless.
fn (mut h HandshakeCore) classify_failure(alert AlertDescription, message string) IError {
	h.fail_alert = alert
	h.fail_and_zeroize_secrets()
	return error(message)
}

// fail_with classifies a propagated error without each call site repeating the
// err.msg() unwrap.
fn (mut h HandshakeCore) fail_with(alert AlertDescription, err IError) IError {
	return h.classify_failure(alert, err.msg())
}

// ReceivePhase names which inbound reassembly stream bytes belong to. Module-private:
// adapters reach each phase through the core's phase-named receive entry points.
enum ReceivePhase {
	initial   // pre-ServerHello (plaintext) bytes
	handshake // handshake-key-protected bytes
}

// receive_bytes guards and dispatches inbound handshake bytes for one phase:
// pre-ServerHello bytes are accepted only while awaiting ServerHello (state == .wait_sh),
// handshake-key-protected bytes only inside the encrypted server flight (is_in_flight()).
// Misuse is rejected without consuming bytes or changing state; legal bytes append then
// consume.
fn (mut h HandshakeCore) receive_bytes(bytes []u8, phase ReceivePhase) ![]HandshakeMessage {
	if h.state == .start {
		return error('handshake: handshake bytes fed before create_client_hello() — no ClientHello has been sent yet')
	}
	if h.state == .failed {
		return error('handshake: state machine has failed; further handshake bytes are refused')
	}
	match phase {
		.initial {
			if h.state != .wait_sh {
				return error('handshake: initial handshake bytes are only expected while awaiting ServerHello (state: ${h.state()})')
			}
			return h.append_and_consume(bytes, 'initial-phase')
		}
		.handshake {
			if !h.is_in_flight() {
				return error('handshake: handshake-phase bytes are only expected after ServerHello and before Finished (state: ${h.state()})')
			}
			return h.append_and_consume(bytes, 'handshake-phase')
		}
	}
}

// append_and_consume bounds each append of `bytes` to the reassembly buffer before
// allocating, then drains complete messages. `phase` names the phase for the over-limit
// alert. The hard cap is max_handshake_message_len; exceeding it is a terminal
// decode_error. One reassembly loop serves both the initial (pre-ServerHello) and
// handshake (encrypted-flight) phases.
fn (mut h HandshakeCore) append_and_consume(bytes []u8, phase string) ![]HandshakeMessage {
	mut out := []HandshakeMessage{}
	mut offset := 0
	limit := handshake_header_len + max_handshake_message_len
	for {
		available := limit - h.reassembly.len
		if offset < bytes.len && available == 0 {
			return h.classify_failure(.decode_error, 'handshake: ${phase} reassembly exceeds ${max_handshake_message_len} bytes without completing a message')
		}
		take := if bytes.len - offset > available { available } else { bytes.len - offset }
		if take > 0 {
			h.reassembly << bytes[offset..offset + take]
			offset += take
		}
		messages := h.consume_msgs() or {
			h.fail_and_zeroize_secrets()
			return err
		}
		out << messages
		if offset == bytes.len {
			break
		}
	}
	return out
}

// is_consuming_phase reports whether the core is in a phase that accepts inbound
// handshake bytes and can drain complete messages: awaiting ServerHello, or inside
// the encrypted server flight.
fn (h &HandshakeCore) is_consuming_phase() bool {
	return h.state == .wait_sh || h.is_in_flight()
}

// dispatch_handshake_message routes one complete framed message to the phase-correct
// handler: ServerHello/HelloRetryRequest while awaiting ServerHello, otherwise the
// encrypted-flight handler (EE, CertificateRequest, Certificate, CertificateVerify,
// Finished).
fn (mut h HandshakeCore) dispatch_handshake_message(full_msg []u8) ![]HandshakeMessage {
	if h.state == .wait_sh {
		return h.handle_server_hello_message(full_msg)
	}
	return h.handle_handshake_message(full_msg)
}

// consume_msgs drains complete framed messages from the reassembly buffer, dispatching
// each through dispatch_handshake_message. It stops when the buffer holds an incomplete
// header or body, or when the core leaves a consuming phase (ServerHello moves out of
// .wait_sh; Finished moves to .connected).
fn (mut h HandshakeCore) consume_msgs() ![]HandshakeMessage {
	mut out := []HandshakeMessage{}
	for h.is_consuming_phase() {
		msg := next_unframed_message(mut h.reassembly) or {
			return h.fail_with(.decode_error, err)
		}
		if msg.len == 0 {
			break // header or body still incomplete
		}
		out_msgs := h.dispatch_handshake_message(msg) or { return err }
		out << out_msgs
	}
	return out
}

// is_handshake_message_accepted is the single ordering check for the encrypted
// flight: it names the message types legal in the current state, so each handler
// only parses and transitions.
fn (h &HandshakeCore) is_handshake_message_accepted(msg_type HandshakeType) bool {
	return match h.state {
		.wait_ee { msg_type == .encrypted_extensions }
		.wait_cert_cr {
			msg_type == .certificate_request || msg_type == .certificate
		}
		.wait_cert { msg_type == .certificate }
		.wait_cv { msg_type == .certificate_verify }
		.wait_finished { msg_type == .finished }
		else { false }
	}
}

// unexpected_error_hsk_message preserves the per-message wording adapters and
// tests rely on when ordering rejects a message.
fn unexpected_error_hsk_message(msg_type HandshakeType) string {
	return match msg_type {
		.encrypted_extensions { 'handshake: unexpected EncryptedExtensions' }
		.certificate_request { 'handshake: unexpected CertificateRequest' }
		.certificate { 'handshake: unexpected Certificate' }
		.certificate_verify { 'handshake: unexpected CertificateVerify' }
		.finished { 'handshake: unexpected Finished message' }
		else { 'handshake: unexpected handshake message ${msg_type}' }
	}
}

// max_handshake_message_len bounds one reassembled handshake message: the 24-bit
// length field permits ~16 MiB, so a cap keeps a hostile peer from forcing a huge buffer.
pub const max_handshake_message_len = 1 << 20

// handshake_header_len is the self-framing header of one recordless handshake
// message: HandshakeType (1 byte) || 24-bit body length (RFC 8446 §4 / RFC 9001 §4).
const handshake_header_len = 4

// next_unframed_message extracts one complete handshake message from the front of a
// CRYPTO byte stream, capped at max_handshake_message_len. "Unframed" means the TLS
// record layer or QUIC packet framing has already been stripped: the message self-frames
// with its own 4-byte header inside an opaque stream, so one framer serves every
// unframed surface — core reassembly, QUIC CRYPTO streams, and the adapter's
// post-handshake buffer.
//
// Contract: an incomplete header or body returns an empty slice — NOT an error — so
// callers loop until empty; a non-empty result is consumed from buf in place. Only an
// over-cap declared length errors, rejected at the header before buffering.
@[direct_array_access]
pub fn next_unframed_message(mut buf []u8) ![]u8 {
	if buf.len < handshake_header_len {
		return []u8{} // even the handshake header has not fully arrived
	}
	msg_len := (int(buf[1]) << 16) | (int(buf[2]) << 8) | int(buf[3])
	if msg_len > max_handshake_message_len {
		return error('handshake: message length ${msg_len} exceeds maximum ${max_handshake_message_len}')
	}
	total := handshake_header_len + msg_len
	if buf.len < total {
		return []u8{} // body not fully received yet
	}
	msg := buf[..total].clone()
	buf.delete_many(0, total) // in-place consumption; the caller's buffer keeps its identity
	return msg
}
