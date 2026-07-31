module quic

import crypto.rand
import crypto.hkdf
import crypto.sha256

fn test_derive_updated_secret_matches_independent_hkdf_computation() {
	secret := rand.bytes(32)!
	got := derive_updated_secret(secret)!

	// Cross-check against a hand-assembled HkdfLabel + hkdf.expand call,
	// bypassing derive_updated_secret/hkdf_expand_label entirely, so this
	// isn't just checking derive_updated_secret against itself.
	full_label := 'tls13 quic ku'
	mut hkdf_label := []u8{}
	hkdf_label << u8(32 >> 8)
	hkdf_label << u8(32)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(0) // empty context
	independently := hkdf.expand(sha256.new, secret, hkdf_label.bytestr(), 32)!
	assert got == independently
	assert got.len == 32
}

fn test_key_update_state_starts_at_phase_zero() {
	secret := rand.bytes(32)!
	s := new_key_update_state(secret)!
	assert s.current_phase_bit() == false
}

fn test_key_update_state_matched_phase_resolves_to_current_keys() {
	secret := rand.bytes(32)!
	s := new_key_update_state(secret)!
	resolution := s.resolve_read_keys(false, 5)!
	assert resolution.is_new_update == false
	assert resolution.is_previous_phase == false
	assert resolution.keys == s.current_keys
}

fn test_key_update_state_full_update_and_reordered_previous_phase_packet() {
	secret := rand.bytes(32)!
	mut s := new_key_update_state(secret)!
	original_keys := s.current_keys

	// First mismatched-phase packet: no packet has been processed yet in
	// the current phase, so this can only be a genuine new update.
	first_resolution := s.resolve_read_keys(true, 100)!
	assert first_resolution.is_new_update == true
	assert first_resolution.is_previous_phase == false
	assert first_resolution.keys != original_keys

	s.note_successful_decrypt(first_resolution, 100)!
	assert s.current_phase_bit() == true
	assert s.current_keys == first_resolution.keys

	// A packet matching the NEW current phase, with a higher packet
	// number, resolves normally.
	matched := s.resolve_read_keys(true, 150)!
	assert matched.is_new_update == false
	assert matched.is_previous_phase == false
	s.note_successful_decrypt(matched, 150)!

	// A reordered packet from BEFORE the update (old phase bit, packet
	// number lower than anything seen in the current phase) must resolve
	// to the RETAINED previous keys -- exactly the original ones.
	reordered := s.resolve_read_keys(false, 50)!
	assert reordered.is_new_update == false
	assert reordered.is_previous_phase == true
	assert reordered.keys == original_keys

	// Processing the reordered packet must NOT disturb current-phase
	// bookkeeping.
	s.note_successful_decrypt(reordered, 50)!
	still_matched := s.resolve_read_keys(true, 149)!
	// 149 < 150 (the min_pn_in_current_phase) but STILL matches the
	// current phase bit, so this is not a phase mismatch at all --
	// matched-phase packets always use current keys regardless of
	// ordering.
	assert still_matched.is_new_update == false
	assert still_matched.is_previous_phase == false
}

// test_key_update_state_tolerates_resolving_before_committing simulates a
// caller that resolves TWO packets (e.g. from the same coalesced datagram)
// before committing EITHER -- both resolutions look like a genuine new
// update at resolve time, since neither has actually been committed yet.
// note_successful_decrypt must still only apply ONE real key update: the
// second commit re-derives its classification fresh against the state AS
// OF THAT COMMIT (already advanced by the first), recognizing it as an
// ordinary matched-phase packet rather than mis-promoting a second time.
fn test_key_update_state_tolerates_resolving_before_committing() {
	secret := rand.bytes(32)!
	mut s := new_key_update_state(secret)!

	resolution_a := s.resolve_read_keys(true, 100)!
	resolution_b := s.resolve_read_keys(true, 105)!
	assert resolution_a.is_new_update == true
	assert resolution_b.is_new_update == true // stale-but-expected: nothing committed yet

	s.note_successful_decrypt(resolution_a, 100)!
	assert s.current_phase_bit() == true
	assert s.updates_accepted == 1

	s.note_successful_decrypt(resolution_b, 105)!
	assert s.updates_accepted == 1
	assert s.current_phase_bit() == true
}

fn test_key_update_state_rejects_previous_phase_packet_once_discarded() {
	secret := rand.bytes(32)!
	mut s := new_key_update_state(secret)!
	first_resolution := s.resolve_read_keys(true, 100)!
	s.note_successful_decrypt(first_resolution, 100)!

	s.discard_previous_keys()
	s.resolve_read_keys(false, 50) or {
		assert err.msg().contains('no previous keys are retained')
		return
	}
	assert false, 'expected resolving a previous-phase packet to fail once discarded'
}

fn test_key_update_state_enforces_update_count_limit() {
	secret := rand.bytes(32)!
	mut s := new_key_update_state(secret)!
	mut phase_to_use := true
	mut pn := u64(1)

	for _ in 0 .. max_key_updates_accepted {
		resolution := s.resolve_read_keys(phase_to_use, pn)!
		s.note_successful_decrypt(resolution, pn)!
		phase_to_use = !phase_to_use
		pn += 10
	}

	resolution := s.resolve_read_keys(phase_to_use, pn)!
	s.note_successful_decrypt(resolution, pn) or {
		assert err.msg().contains('too many key updates')
		return
	}
	assert false, 'expected the update count limit to be enforced'
}
