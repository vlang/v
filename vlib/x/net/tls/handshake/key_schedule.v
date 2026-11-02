// Copyright (c) 2026 blackshirt.
//
// key_schedule.v runs the RFC 8446 §7.1 HKDF key-derivation ladder that turns the
// (EC)DHE secret into every handshake and application key, and owns the §7.5
// zeroization lifecycle declared on HandshakeCore. The opaque KeyMaterial /
// TrafficKeys handles — the one generation shape shared by both transport modes —
// plus the public phase accessors and the retained secret holders are declared
// here, beside the derivations that fill them.
module handshake

import crypto.hmac
import crypto.sha256
import crypto.sha512
import tls.pki

// HashAlgorithm specifies the hash function used by the negotiated cipher suite.
// Pub because hkdf_expand_label needs it, and a QUIC engine names the suite's
// hash to expand the RFC 9001 §5.1 labels.
pub enum HashAlgorithm {
	sha256
	sha384
}

// hash_len returns the digest length in bytes.
fn (h HashAlgorithm) hash_len() int {
	return match h {
		.sha256 { 32 }
		.sha384 { 48 }
	}
}

// compute_hash calculates the hash of data.
fn (h HashAlgorithm) compute_hash(data []u8) []u8 {
	return match h {
		.sha256 { sha256.sum256(data) }
		.sha384 { sha512.sum384(data) }
	}
}

// HMAC block sizes in bytes (RFC 2104), which must match the underlying hash.
const sha256_block_size = 64
const sha384_block_size = 128

// compute_hmac calculates the HMAC of data using key.
fn (h HashAlgorithm) compute_hmac(key []u8, data []u8) []u8 {
	return match h {
		.sha256 { hmac.new(key, data, sha256.sum256, sha256_block_size) }
		.sha384 { hmac.new(key, data, sha512.sum384, sha384_block_size) }
	}
}

// hkdf_extract implements HKDF-Extract(salt, ikm) (RFC 5869 §2.2).
// An empty salt means a string of Hash.length zeros, the RFC 5869 default.
fn hkdf_extract(hash HashAlgorithm, salt []u8, ikm []u8) []u8 {
	effective_salt := if salt.len == 0 { []u8{len: hash.hash_len()} } else { salt }
	return hash.compute_hmac(effective_salt, ikm)
}

// hkdf_expand implements HKDF-Expand(prk, info, length) (RFC 5869 §2.3), producing
// T(1) || T(2) || … where T(i) = HMAC(PRK, T(i-1) || info || i).
@[direct_array_access]
fn hkdf_expand(hash HashAlgorithm, prk []u8, info []u8, length int) ![]u8 {
	hash_len := hash.hash_len()
	// A zero or negative expansion is meaningless; fail closed instead of returning
	// an empty slice that would silently shorten every derived key.
	if length <= 0 {
		return error('handshake: HKDF-Expand requested length ${length} is not positive')
	}
	// At most 255 blocks are reachable (one byte of counter); more would repeat a block (RFC 5869 §2.3).
	if length > 255 * hash_len {
		return error('handshake: HKDF-Expand requested length ${length} exceeds maximum (${255 * hash_len})')
	}
	mut out := []u8{cap: length}
	mut prev := []u8{} // T(0) is the empty string, so the first round has no prior block
	mut counter := u8(1) // blocks are indexed from 1; appended to every HMAC input

	for out.len < length {
		// T(i) = HMAC(PRK, T(i-1) || info || i): each round is unique to its index.
		mut step_input := []u8{cap: prev.len + info.len + 1}
		step_input << prev
		step_input << info
		step_input << counter
		// prev is now copied into step_input, so wipe the previous block before the
		// handle is rebound — orphaned HMAC outputs are key material (§7.5).
		wipe_secret(mut prev)

		prev = hash.compute_hmac(prk, step_input)
		// Append only the bytes still required, landing `out` on exactly `length`.
		needed := length - out.len
		to_copy := if needed < prev.len { needed } else { prev.len }
		out << prev[..to_copy]
		counter++
		// The round input embeds the prk-derived chain; zero it as the handle dies.
		wipe_secret(mut step_input)
	}
	// The final T(i) was copied into out; wipe the live handle too.
	wipe_secret(mut prev)
	return out
}

// hkdf_expand_label implements HKDF-Expand-Label (RFC 8446 §7.1), prefixing the
// label with "tls13 "; pub for the RFC 9001 §5.1 quic key/iv/hp derivations.
pub fn hkdf_expand_label(hash HashAlgorithm, secret []u8, label string, context []u8, length int) ![]u8 {
	full_label := 'tls13 ' + label
	if full_label.len > 255 {
		return error('handshake: HKDF-Expand-Label label too long')
	}
	if context.len > 255 {
		return error('handshake: HKDF-Expand-Label context too long')
	}

	mut hkdf_label := []u8{cap: 2 + 1 + full_label.len + 1 + context.len}
	hkdf_label << u8((length >> 8) & 0xff)
	hkdf_label << u8(length & 0xff)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(context.len)
	hkdf_label << context

	return hkdf_expand(hash, secret, hkdf_label, length)!
}

// derive_secret implements Derive-Secret (RFC 8446 §7.1):
// HKDF-Expand-Label(Secret, Label, Transcript-Hash(Messages), Hash.length).
fn derive_secret(hash HashAlgorithm, secret []u8, label string, transcript_hash []u8) ![]u8 {
	return hkdf_expand_label(hash, secret, label, transcript_hash, hash.hash_len())!
}

// empty_transcript_hash returns the hash of an empty input (RFC 8446 §7.1 "derived" steps).
// SHA-256("") and SHA-384("") are fixed constants — no runtime hashing needed.
const empty_sha256_transcript_hash = [u8(0xe3), 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a,
	0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4,
	0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55]

const empty_sha384_transcript_hash = [u8(0xcb), 0x00, 0x75, 0x3f, 0x45, 0xa3, 0x5e, 0x8b, 0xb3,
	0xa0, 0x3d, 0x26, 0x82, 0x12, 0x3a, 0x6d, 0x4f, 0x81, 0x8c, 0x8e, 0x8c, 0x1e, 0x8c, 0x8e, 0x8c,
	0x1e, 0x8c, 0x8e, 0x8c, 0x1e, 0x8c, 0x8e, 0x8c, 0x1e, 0x8c, 0x8e, 0x8c, 0x1e, 0x8c, 0x8e, 0x8c,
	0x1e, 0x8c, 0x8e, 0x8c, 0x1e, 0x8c]

fn empty_transcript_hash(hash HashAlgorithm) []u8 {
	return match hash {
		.sha256 { empty_sha256_transcript_hash }
		.sha384 { empty_sha384_transcript_hash }
	}
}

// derive_handshake_secret runs the §7.1 Early->Handshake ladder: the zero-PSK Early
// Secret, then the Handshake Secret mixing in the ECDHE shared secret. The early chain
// is dead once the Handshake extract consumed it (§7.5), so its locals zeroize here.
fn derive_handshake_secret(hash_algo HashAlgorithm, shared_secret []u8) ![]u8 {
	// Early Secret = HKDF-Extract(0, 0); without a PSK the IKM is empty but is still mixed in.
	mut early_secret := hkdf_extract(hash_algo, []u8{}, []u8{len: hash_algo.hash_len()})
	mut derived_early := derive_secret(hash_algo, early_secret, 'derived', empty_transcript_hash(hash_algo))!
	wipe_secret(mut early_secret)
	// Handshake Secret = HKDF-Extract(Derive-Secret(Early, "derived"), ECDHE)
	handshake_secret := hkdf_extract(hash_algo, derived_early, shared_secret)
	wipe_secret(mut derived_early)
	return handshake_secret
}

// derive_client_hs_secret expands the client handshake traffic secret (RFC 8446
// §7.1, "c hs traffic" label) from the Handshake Secret over the ClientHello..ServerHello transcript.
fn derive_client_hs_secret(hash_algo HashAlgorithm, handshake_secret []u8, th_ch_sh []u8) ![]u8 {
	return derive_secret(hash_algo, handshake_secret, 'c hs traffic', th_ch_sh)!
}

// derive_server_hs_secret expands the server handshake traffic secret (RFC 8446
// §7.1, "s hs traffic" label) from the Handshake Secret over the same transcript.
fn derive_server_hs_secret(hash_algo HashAlgorithm, handshake_secret []u8, th_ch_sh []u8) ![]u8 {
	return derive_secret(hash_algo, handshake_secret, 's hs traffic', th_ch_sh)!
}

// derive_master_secret advances the §7.1 Handshake->Master ladder from the Handshake
// Secret: the "derived" salt then a zero-PSK extract. The derived_hs intermediate is
// wiped here (§7.5); the returned Master Secret is zeroized by the caller after both
// application traffic secrets are expanded, since it is never stored.
fn derive_master_secret(hash_algo HashAlgorithm, handshake_secret []u8) ![]u8 {
	mut derived_hs := derive_secret(hash_algo, handshake_secret, 'derived', empty_transcript_hash(hash_algo))!
	master_secret := hkdf_extract(hash_algo, derived_hs, []u8{len: hash_algo.hash_len()})
	wipe_secret(mut derived_hs)
	return master_secret
}

// derive_client_app_secret expands the client application traffic secret (RFC 8446
// §7.1, "c ap traffic" label) from the Master Secret; KeyUpdate chains from it (§7.2).
fn derive_client_app_secret(hash_algo HashAlgorithm, master_secret []u8, th_sfin []u8) ![]u8 {
	return derive_secret(hash_algo, master_secret, 'c ap traffic', th_sfin)!
}

// derive_server_app_secret expands the server application traffic secret (RFC 8446
// §7.1, "s ap traffic" label) from the Master Secret; KeyUpdate chains from it (§7.2).
fn derive_server_app_secret(hash_algo HashAlgorithm, master_secret []u8, th_sfin []u8) ![]u8 {
	return derive_secret(hash_algo, master_secret, 's ap traffic', th_sfin)!
}

// wipe_secret overwrites a secret buffer before releasing it. The zeroing loop is
// the security-relevant step; the rebinding merely clears the local handle. This is
// tls.handshake's one zero-fill — tls and tls.quic each keep a private zero_bytes
// (V cannot share a private fn across a module boundary), so the three loops stay
// byte-identical by convention and every scrub in a module routes through its own.
fn wipe_secret(mut secret []u8) {
	for i in 0 .. secret.len {
		secret[i] = 0
	}
	secret = []u8{}
}

// derive_traffic_key expands a traffic secret into the AEAD key
// (RFC 8446 §7.1, "key" label).
@[direct_array_access]
fn derive_traffic_key(hash HashAlgorithm, secret []u8, key_len int) ![]u8 {
	return hkdf_expand_label(hash, secret, 'key', []u8{}, key_len)!
}

// derive_traffic_iv expands a traffic secret into the static nonce base the
// adapter consumes (RFC 8446 §7.1, "iv" label).
@[direct_array_access]
fn derive_traffic_iv(hash HashAlgorithm, secret []u8, iv_len int) ![]u8 {
	return hkdf_expand_label(hash, secret, 'iv', []u8{}, iv_len)!
}

// derive_key_material expands one traffic secret into a direction's key material. It
// is the ONLY constructor of KeyMaterial, shared by both transports: they hand over
// the same handle shape, and the is_quic flag (the same mode bit HandshakeCore guards
// on) selects only the label vocabulary:
//   - TLS record: 'key', 'iv' (RFC 8446 §7.1), with the hp slot set explicitly empty —
//     KeyMaterial is @[noinit], so an omitted field would be garbage, not zeros, and
//     record headers are unprotected.
//   - QUIC: 'quic key', 'quic iv', 'quic hp' (RFC 9001 §5.1), filling every slot.
@[direct_array_access]
fn derive_key_material(hash HashAlgorithm, secret []u8, key_len int, is_quic bool) !KeyMaterial {
	if is_quic {
		return KeyMaterial{
			key: hkdf_expand_label(hash, secret, 'quic key', []u8{}, key_len)!
			iv:  hkdf_expand_label(hash, secret, 'quic iv', []u8{}, aead_nonce_len)!
			hp:  hkdf_expand_label(hash, secret, 'quic hp', []u8{}, key_len)!
		}
	}
	return KeyMaterial{
		key: derive_traffic_key(hash, secret, key_len)!
		iv:  derive_traffic_iv(hash, secret, aead_nonce_len)!
		hp:  []u8{}
	}
}

// derive_finished_key computes the finished_key = HKDF-Expand-Label(BaseKey, "finished",
// "", Hash.length) MAC key RFC 8446 §4.4.4 binds to a Finished message.
@[direct_array_access]
fn derive_finished_key(hash HashAlgorithm, base_key []u8) ![]u8 {
	return hkdf_expand_label(hash, base_key, 'finished', []u8{}, hash.hash_len())!
}

// compute_finished_verify_data derives finished_key and computes HMAC over
// transcript_hash (RFC 8446 §4.4.4).
@[direct_array_access]
fn compute_finished_verify_data(hash HashAlgorithm, base_key []u8, transcript_hash []u8) ![]u8 {
	mut finished_key := derive_finished_key(hash, base_key)!
	// The finished MAC key is single-use: wipe it the moment the HMAC consumed it.
	defer {
		wipe_secret(mut finished_key)
	}
	return hash.compute_hmac(finished_key, transcript_hash)
}

// KeyMaterial is one direction's protection material: the AEAD key, the nonce base it
// must always travel with, and — for a transport that protects headers — the
// header-protection key. The fields are private, which makes the handle opaque:
// tls.handshake constructs it, and a caller outside the module can only take values out
// of it or wipe it, so a key can never be installed without the material that travels
// with it. The direction is carried by the field name it is stored under (`client` /
// `server`), not by the type. TLS record legs leave `hp` empty (RFC 8446 §5.2 protects
// only the record body); QUIC packet legs fill all three slots (RFC 9001 §5.1).
@[noinit]
pub struct KeyMaterial {
mut:
	key []u8 // AEAD key
	iv  []u8 // nonce base
	hp  []u8 // header-protection key; empty on TLS record legs
}

// take_key moves the AEAD key out and leaves the field empty, so the bytes have exactly
// one owner and that owner can prove it scrubbed them (§7.5). Moving instead of cloning
// keeps the key copy count across the module boundary at one per generation.
pub fn (mut k KeyMaterial) take_key() []u8 {
	bytes := k.key
	k.key = []u8{}
	return bytes
}

// take_iv moves the nonce base out on the same terms as take_key: a leg is drained whole,
// never a half of it, so no key reaches a protection state without the material beside it.
pub fn (mut k KeyMaterial) take_iv() []u8 {
	bytes := k.iv
	k.iv = []u8{}
	return bytes
}

// take_hp moves the header-protection key out on the same terms. A TLS record leg hands
// over an empty slice: the slot exists for QUIC, whose new_protection() refuses an empty
// hp, so record material can never masquerade as packet material.
pub fn (mut k KeyMaterial) take_hp() []u8 {
	bytes := k.hp
	k.hp = []u8{}
	return bytes
}

// wipe zeroes every value the handle still owns and drops them; idempotent, so it is
// safe as a deferred safety net after a partial drain.
pub fn (mut k KeyMaterial) wipe() {
	wipe_secret(mut k.key)
	wipe_secret(mut k.iv)
	wipe_secret(mut k.hp)
}

// is_ready reports whether this handle holds a key. Module-private: presence is an internal
// precondition of the phase accessor, whose error says more than a bool.
fn (k &KeyMaterial) is_ready() bool {
	return k.key.len > 0
}

// TrafficKeys is an opaque handle to one generation of protection keys, holding one
// KeyMaterial per direction (client and server). It is the shared cross-boundary shape:
// the record accessors fill key+iv per leg, the QUIC packet accessors fill key+iv+hp.
@[noinit]
pub struct TrafficKeys {
mut:
	client KeyMaterial // client-direction material
	server KeyMaterial // server-direction material
}

// take_client_material moves the client-direction material out of the generation, leaving
// an empty handle behind; take_server_material does the same for the server direction. The
// material travels whole, which is the reason a leg has no separate take method of its own.
pub fn (mut k TrafficKeys) take_client_material() KeyMaterial {
	leg := k.client
	k.client = KeyMaterial{}
	return leg
}

// take_server_material moves the server-direction material out of the generation;
// take_client_material is its client-side mirror.
pub fn (mut k TrafficKeys) take_server_material() KeyMaterial {
	leg := k.server
	k.server = KeyMaterial{}
	return leg
}

// wipe zeroes both directions of this generation (§7.5).
pub fn (mut k TrafficKeys) wipe() {
	k.client.wipe()
	k.server.wipe()
}

// is_ready reports whether both directions hold a key: an install or a wipe always takes
// a whole generation, never one leg of it. Module-private, as on the direction handles.
fn (k &TrafficKeys) is_ready() bool {
	return k.client.is_ready() && k.server.is_ready()
}

// release_ephemeral_keys drops both ephemeral ECDHE private halves: the vlib key
// types expose no in-place wipe, so releasing the handles is the available
// destruction (§7.5). Sole caller is wipe_key_schedule_secrets(), reached from both
// the .connected lifecycle and the failure path.
fn (mut h HandshakeCore) release_ephemeral_keys() {
	h.priv_x25519 = unsafe { nil }
	h.priv_p256 = none
}

// traffic_keys returns the current traffic-key generation for the calling transport,
// derived on demand: record legs (hp empty) on a TLS-mode core, packet legs (key+iv+hp)
// on a QUIC-mode core. Which secret set is used is implied by HandshakeState:
//   - .wait_ee … .wait_finished: handshake traffic secrets (after ServerHello)
//   - .connected: application traffic secrets (after server Finished)
//   - .start, .wait_sh, .failed: no traffic keys available
// Each call yields a fresh, caller-owned generation — drain it with
// take_client_material() / take_server_material() and wipe() the rest, so no copy
// escapes unscrubbed (§7.5).
pub fn (h &HandshakeCore) traffic_keys() !TrafficKeys {
	match h.state {
		.start, .wait_sh, .failed {
			return error('handshake: traffic keys unavailable (handshake secrets after ServerHello, application secrets at .connected)')
		}
		.wait_ee, .wait_cert_cr, .wait_cert, .wait_cv, .wait_finished {
			if h.c_hsk_tsec.len == 0 || h.s_hsk_tsec.len == 0 {
				return error('handshake: handshake traffic keys unavailable (available after ServerHello)')
			}
			return TrafficKeys{
				client: derive_key_material(h.hash_algo, h.c_hsk_tsec, h.key_len, h.is_quic)!
				server: derive_key_material(h.hash_algo, h.s_hsk_tsec, h.key_len, h.is_quic)!
			}
		}
		.connected {
			if h.c_app_tsec.len == 0 || h.s_app_tsec.len == 0 {
				return error('handshake: application traffic keys unavailable before the server Finished')
			}
			return TrafficKeys{
				client: derive_key_material(h.hash_algo, h.c_app_tsec, h.key_len, h.is_quic)!
				server: derive_key_material(h.hash_algo, h.s_app_tsec, h.key_len, h.is_quic)!
			}
		}
	}
}

// fail_and_zeroize_secrets is the unguarded terminal entry: it composes every wipe
// above — key-schedule intermediate, ephemeral halves, both traffic-secret phases —
// releases the reassembly buffer, and parks the machine in .failed (RFC 8446
// §1.3.1/§7.5). Wiping the traffic secrets also retires the record keys, since they are
// derived from those secrets on demand. It bypasses the zeroize_* lifecycle guard: a
// failure can land before .connected ever held. Plaintext handshake history
// (transcript) and the server certificate chain are also scrubbed: they survive
// failure otherwise and would leak SNI/ALPN/certificate material on the heap.
pub fn (mut h HandshakeCore) fail_and_zeroize_secrets() {
	h.wipe_key_schedule_secrets()
	// Scrub plaintext handshake history: every message exchanged so far, plus the
	// running digest state (its partial block can hold recent plaintext bytes).
	h.transcript.scrub()
	// Scrub the server certificate chain: raw DER and every parsed byte field.
	for i in 0 .. h.server_certs.len {
		wipe_secret(mut h.server_certs[i].raw)
		wipe_secret(mut h.server_certs[i].tbs_raw)
		wipe_secret(mut h.server_certs[i].public_key_raw)
		wipe_secret(mut h.server_certs[i].issuer_dn)
		wipe_secret(mut h.server_certs[i].subject_dn)
		wipe_secret(mut h.server_certs[i].serial_number)
	}
	h.server_certs = []pki.Certificate{}
	h.reassembly = []u8{}
	h.state = .failed
}

// wipe_key_schedule_secrets destroys the key-schedule intermediate, both
// handshake traffic secrets, and the dead ephemeral ECDHE halves at .connected
// (RFC 8446 §1.3.1/§7.5). Handshake traffic secrets are dead once the client
// Finished is sent / server Finished verified, so they are wiped here rather
// than surviving until connection close.
pub fn (mut h HandshakeCore) wipe_key_schedule_secrets() {
	wipe_secret(mut h.handshake_secret)
	wipe_secret(mut h.c_hsk_tsec)
	wipe_secret(mut h.s_hsk_tsec)
	h.release_ephemeral_keys()
}
