module quic

// RFC 9001 §6 — 1-RTT Key Update, RECEIVE side only. Client-initiated key
// rotation (this side deciding to start sending with a NEW phase) is
// explicitly deferred past v1 per this project's plan; tolerating a
// PEER-initiated update -- including correctly decoding a reordered
// packet that arrives late, still using the phase from BEFORE that
// update -- is must-have, since a real server can rotate its own write
// keys at any time and this client must not lose data or desynchronize
// when it does.
//
// Applies ONLY to the Application Data (1-RTT) packet number space --
// Initial and Handshake packets never carry a Key Phase bit at all (it
// only exists in the short header, RFC 9000 §17.3.1) and never rotate
// keys this way.

// key_update_label is RFC 9001 §6.1's fixed HKDF-Expand-Label label:
// next_secret = HKDF-Expand-Label(current_secret, "quic ku", "", Hash.length).
const key_update_label = 'quic ku'

// derive_updated_secret computes the NEXT traffic secret from the current
// one (RFC 9001 §6.1). The same derivation applies independently to a
// connection's client_secret and server_secret -- each side's traffic
// secret chain advances on its own schedule, which is exactly why this
// function takes a bare secret rather than anything connection-scoped.
pub fn derive_updated_secret(current_secret []u8) ![]u8 {
	return hkdf_expand_label(current_secret, key_update_label, []u8{}, current_secret.len)
}

// max_key_updates_accepted bounds how many PEER-INITIATED key updates one
// KeyUpdateState will honor over the connection's life. A compliant peer
// paces updates out (RFC 9001 §6.1: waits for an acknowledgment of a
// packet sent in the current phase; §6.5: then roughly 3x the probe
// timeout) -- but that norm can't be enforced by a receiver, only assumed
// of a compliant peer, and real time-based pacing needs RTT/PTO
// estimation this module doesn't have yet (Phase 7). This is a coarse,
// time-independent stand-in that still stops a peer from forcing
// unbounded HKDF/AES-key-schedule re-derivation by cycling the key phase
// bit across many packets.
pub const max_key_updates_accepted = 1000

// KeyResolution is resolve_read_keys' result: which keys to attempt
// decrypting an incoming packet with, and how the situation looked AT
// RESOLVE TIME. `is_new_update`/`is_previous_phase` are informational only
// -- a caller may use them for logging, but note_successful_decrypt does
// NOT trust them for its own state transition (see that function's doc
// comment for why: they can go stale between resolution and commit).
pub struct KeyResolution {
pub:
	keys         QuicPacketProtectionKeys
	secret       []u8
	packet_phase bool
	// is_new_update is true when this resolution represents a genuine new
	// key update (the packet's phase differs from current, and its packet
	// number is higher than anything seen in the current phase) -- AS
	// OBSERVED AT RESOLVE TIME.
	is_new_update bool
	// is_previous_phase is true when this resolution represents a
	// reordered packet from BEFORE the current phase (the packet's phase
	// differs from current, and its packet number is lower than anything
	// seen in the current phase) -- AS OBSERVED AT RESOLVE TIME. Mutually
	// exclusive with is_new_update.
	is_previous_phase bool
}

// KeyUpdateState tracks ONE direction's (specifically: the direction used
// to READ packets FROM the peer) current and previous 1-RTT packet
// protection keys, plus the packet-number bookkeeping RFC 9001 §6.5
// requires to tell a genuine new update apart from a reordered packet
// still using the previous phase.
@[heap]
pub struct KeyUpdateState {
mut:
	current_phase           bool
	current_keys            QuicPacketProtectionKeys
	current_secret          []u8
	previous_keys           ?QuicPacketProtectionKeys
	min_pn_in_current_phase ?u64
	updates_accepted        int
}

// new_key_update_state seeds tracking with the FIRST 1-RTT traffic secret
// (Phase 2's application traffic secret, derived once at handshake
// completion) and phase 0 -- RFC 9001 §6: "the Key Phase bit... is
// initially set to 0 for the first set of 1-RTT packets".
pub fn new_key_update_state(initial_secret []u8) !&KeyUpdateState {
	keys := derive_packet_protection_keys(initial_secret)!
	return &KeyUpdateState{
		current_phase:  false
		current_keys:   keys
		current_secret: initial_secret
	}
}

pub fn (s &KeyUpdateState) current_phase_bit() bool {
	return s.current_phase
}

// resolve_read_keys decides which keys to TRY decrypting an incoming
// 1-RTT packet with, given its key phase bit (already revealed by header
// protection removal) and its reconstructed packet number. Per RFC 9001
// §6.5: a phase matching the current one always uses the current keys. A
// MISMATCHED phase is resolved by packet number, not by the mismatch
// alone -- lower than anything already seen in the current phase means a
// reordered packet from BEFORE the peer's update (use the retained
// previous keys, if any); higher means a genuine new update (derive and
// try the next keys).
//
// This function does NOT mutate any state and does NOT itself
// authenticate anything -- resolving which keys to attempt is a
// plaintext-visible decision (the key phase bit and packet number are
// both unprotected once header protection is removed), never a
// substitute for the AEAD check that follows. The caller MUST attempt
// AEAD decryption with the returned keys and must call
// note_successful_decrypt ONLY if that decryption actually succeeds --
// never on the resolution alone (RFC 9001 §6.5's warning against turning
// key-update handling into a decryption oracle).
pub fn (s &KeyUpdateState) resolve_read_keys(packet_phase bool, packet_number u64) !KeyResolution {
	if packet_phase == s.current_phase {
		return KeyResolution{
			keys:         s.current_keys
			secret:       s.current_secret
			packet_phase: packet_phase
		}
	}

	min_current := s.min_pn_in_current_phase or {
		// No packet has been processed under the current phase yet (this
		// would be the very first 1-RTT packet ever received) -- a phase
		// mismatch here has no "previous phase" to belong to, so it can
		// only be a new update.
		return s.next_key_resolution(packet_phase)!
	}

	if packet_number < min_current {
		prev := s.previous_keys or {
			return error('quic: received a packet using the previous key phase, but no previous keys are retained')
		}

		return KeyResolution{
			keys:              prev
			secret:            []u8{} // not meaningful for a previous-phase resolution
			packet_phase:      packet_phase
			is_previous_phase: true
		}
	}
	return s.next_key_resolution(packet_phase)!
}

fn (s &KeyUpdateState) next_key_resolution(packet_phase bool) !KeyResolution {
	next_secret := derive_updated_secret(s.current_secret)!
	next_keys := derive_packet_protection_keys(next_secret)!
	return KeyResolution{
		keys:          next_keys
		secret:        next_secret
		packet_phase:  packet_phase
		is_new_update: true
	}
}

// note_successful_decrypt commits the outcome of a resolve_read_keys
// resolution AFTER the caller has verified it by successfully
// AEAD-decrypting a real packet with it -- never before.
//
// Deliberately does NOT trust resolution.is_new_update/is_previous_phase:
// those reflect the situation as observed AT RESOLVE TIME, and this
// function can be called with a STALE resolution if a caller resolves
// more than one packet (e.g. several packets coalesced into one datagram)
// before committing either -- committing the first packet's genuine new
// update would flip s.current_phase, and a second, now-stale "is_new_update"
// resolution for a packet that ACTUALLY matches the just-updated current
// phase would otherwise be mis-committed as a SECOND update, corrupting
// current_secret two generations ahead of where the peer actually is.
// Re-deriving the classification fresh here, from resolution.packet_phase
// (the packet's own real, immutable phase bit) against CURRENT state,
// makes this function correct regardless of how many resolutions were
// computed before it, or in what order they are committed.
pub fn (mut s KeyUpdateState) note_successful_decrypt(resolution KeyResolution, packet_number u64) ! {
	if resolution.packet_phase == s.current_phase {
		current := s.min_pn_in_current_phase or {
			s.min_pn_in_current_phase = packet_number
			return
		}

		if packet_number < current {
			s.min_pn_in_current_phase = packet_number
		}
		return
	}

	min_current := s.min_pn_in_current_phase or {
		s.commit_new_update(packet_number)!
		return
	}

	if packet_number < min_current {
		// Still an old-phase packet as of now -- no bookkeeping change.
		return
	}
	s.commit_new_update(packet_number)!
}

// commit_new_update performs the actual key-generation advance, always
// re-deriving the next secret/keys fresh from the CURRENT current_secret
// (never trusting a resolution's possibly-stale secret/keys) so it is
// correct no matter how many resolutions were computed before it committed.
fn (mut s KeyUpdateState) commit_new_update(packet_number u64) ! {
	if s.updates_accepted >= max_key_updates_accepted {
		return error('quic: too many key updates accepted on this connection (limit ${max_key_updates_accepted})')
	}
	new_secret := derive_updated_secret(s.current_secret)!
	new_keys := derive_packet_protection_keys(new_secret)!
	s.previous_keys = s.current_keys
	s.current_keys = new_keys
	s.current_secret = new_secret
	s.current_phase = !s.current_phase
	s.min_pn_in_current_phase = packet_number
	s.updates_accepted++
}

// discard_previous_keys drops the retained previous-phase keys. RFC 9001
// §6.5 recommends retaining them for about 3x the probe timeout after
// receiving a packet in the new phase, then discarding -- that timing
// judgment needs RTT/PTO estimation (Phase 7), so this function only
// performs the mechanical discard; deciding WHEN to call it is a later
// phase's job.
pub fn (mut s KeyUpdateState) discard_previous_keys() {
	s.previous_keys = none
}
