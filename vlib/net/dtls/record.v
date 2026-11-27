// dtls record layer (RFC 9147 minimal, epoch 0/1).
// File-disjoint from net.tls: own DTLS header, own replay/RTO logic.
// Reuses net.tls only for content-type/handshake enums via import.
module dtls

import net.tls
import net
import time

// dtls_legacy_version is the on-wire legacy version for DTLS datagrams.
pub const dtls_legacy_version = u16(0xfefd)

// dtls_header_len is 1 (type) + 2 (version) + 2 (epoch) + 6 (seq) + 2 (len).
pub const dtls_header_len = 13

// dtls_max_fragment caps a single DTLS fragment (handshake or sealed app bytes).
pub const dtls_max_fragment = 2048

// dtls_max_datagram caps a UDP datagram we accept.
pub const dtls_max_datagram = 4096

// dtls_epoch_handshake carries plaintext handshake flights.
pub const dtls_epoch_handshake = u16(0)

// dtls_epoch_application carries encrypted application data.
pub const dtls_epoch_application = u16(1)

// DtlsRecord is one DTLS datagram payload (minimal RFC 9147 §4 header).
pub struct DtlsRecord {
pub:
	content_type    tls.ContentType
	epoch           u16
	sequence_number u64
	fragment        []u8
}

// encode_dtls_record encodes a record header plus fragment.
pub fn encode_dtls_record(rec DtlsRecord) ![]u8 {
	if rec.sequence_number >= (u64(1) << 48) {
		return error('dtls: sequence number too large')
	}
	if rec.fragment.len > dtls_max_fragment {
		return error('dtls: fragment too large')
	}
	mut out := []u8{cap: dtls_header_len + rec.fragment.len}
	out << u8(rec.content_type)
	out << u8(dtls_legacy_version >> 8)
	out << u8(dtls_legacy_version & 0xff)
	out << u8(rec.epoch >> 8)
	out << u8(rec.epoch & 0xff)
	out << u8((rec.sequence_number >> 40) & 0xff)
	out << u8((rec.sequence_number >> 32) & 0xff)
	out << u8((rec.sequence_number >> 24) & 0xff)
	out << u8((rec.sequence_number >> 16) & 0xff)
	out << u8((rec.sequence_number >> 8) & 0xff)
	out << u8(rec.sequence_number & 0xff)
	out << u8(u16(rec.fragment.len) >> 8)
	out << u8(u16(rec.fragment.len) & 0xff)
	out << rec.fragment
	return out
}

// decode_dtls_record decodes one datagram into a record.
// decode_dtls_record requires header + fragment to match exactly.
pub fn decode_dtls_record(data []u8) !DtlsRecord {
	if data.len < dtls_header_len {
		return error('dtls: truncated record header')
	}
	ct := match data[0] {
		20 { tls.ContentType.change_cipher_spec }
		21 { tls.ContentType.alert }
		22 { tls.ContentType.handshake }
		23 { tls.ContentType.application_data }
		else { return error('dtls: invalid outer content type') }
	}
	ver := (u16(data[1]) << 8) | u16(data[2])
	if ver != dtls_legacy_version {
		return error('dtls: invalid legacy version')
	}
	epoch := (u16(data[3]) << 8) | u16(data[4])
	mut seq := u64(0)
	seq |= u64(data[5]) << 40
	seq |= u64(data[6]) << 32
	seq |= u64(data[7]) << 24
	seq |= u64(data[8]) << 16
	seq |= u64(data[9]) << 8
	seq |= u64(data[10])
	ln := (int(data[11]) << 8) | int(data[12])
	if ln > dtls_max_fragment {
		return error('dtls: fragment too large')
	}
	if dtls_header_len + ln != data.len {
		return error('dtls: length mismatch')
	}
	frag := data[dtls_header_len..].clone()
	return DtlsRecord{
		content_type:    ct
		epoch:           epoch
		sequence_number: seq
		fragment:        frag
	}
}

// ReplayWindow is a 64-packet sliding window for one epoch.
pub struct ReplayWindow {
mut:
	highest  u64
	mask     u64
	seen_any bool
}

// is_replay reports whether seq was already seen or is too old.
pub fn (w &ReplayWindow) is_replay(seq u64) bool {
	if !w.seen_any {
		return false
	}
	if seq > w.highest {
		return false
	}
	diff := w.highest - seq
	if diff >= 64 {
		return true
	}
	return ((w.mask >> diff) & u64(1)) == u64(1)
}

// mark records seq, returning false on replay or too-old.
pub fn (mut w ReplayWindow) mark(seq u64) bool {
	if !w.seen_any {
		w.highest = seq
		w.mask = u64(1)
		w.seen_any = true
		return true
	}
	if seq > w.highest {
		shift := seq - w.highest
		if shift >= 64 {
			w.mask = u64(1)
		} else {
			w.mask = (w.mask << shift) | u64(1)
		}
		w.highest = seq
		return true
	}
	diff := w.highest - seq
	if diff >= 64 {
		return false
	}
	bit := u64(1) << diff
	if w.mask & bit != u64(0) {
		return false
	}
	w.mask |= bit
	return true
}

// reset clears the window (used when entering a new epoch).
pub fn (mut w ReplayWindow) reset() {
	w.highest = 0
	w.mask = 0
	w.seen_any = false
}

// RetransmitTimer tracks exponential-backoff timeouts (RFC 9147 §4.2.4).
pub struct RetransmitTimer {
pub mut:
	initial_ms int
	max_ms     int
	attempts   int
}

// new_retransmit_timer builds a timer with the given bounds.
pub fn new_retransmit_timer(initial_ms int, max_ms int) RetransmitTimer {
	return RetransmitTimer{
		initial_ms: initial_ms
		max_ms:     max_ms
	}
}

// backoff_for_attempt returns min(initial * 2^attempt, max).
pub fn backoff_for_attempt(initial_ms int, max_ms int, attempt int) int {
	mut v := initial_ms
	for _ in 0 .. attempt {
		v *= 2
		if v >= max_ms {
			return max_ms
		}
	}
	if v > max_ms {
		return max_ms
	}
	return v
}

// next_timeout returns the current RTO and advances the attempt counter.
pub fn (mut t RetransmitTimer) next_timeout() time.Duration {
	ms := backoff_for_attempt(t.initial_ms, t.max_ms, t.attempts)
	t.attempts++
	return time.Duration(i64(ms) * 1_000_000)
}

// reset clears the attempt counter after progress.
pub fn (mut t RetransmitTimer) reset() {
	t.attempts = 0
}

// is_timeout reports whether err is a UDP read timeout.
pub fn is_timeout(err IError) bool {
	return err.code() == net.err_timed_out_code
}
