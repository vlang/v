// Copyright (c) 2026 blackshirt.
//
// RSA tests: PKCS #1 RSAPublicKey decode against hand-built DER, plus verify
// paths for PKCS1-v1_5 and PSS. Every malformed key shape must reject.
module pki

import crypto.sha256
import encoding.hex

// rsa_seq concatenates the given child TLV byte arrays and wraps them in a
// short-form DER SEQUENCE (tag 0x30). Every fixture here stays under 128 bytes,
// so a single length octet suffices. It exists because V rejects `[]u8 + []u8`
// concatenation of these literals; appending via `<<` is the portable form.
fn rsa_seq(parts [][]u8) []u8 {
	mut body := []u8{}
	for p in parts {
		body << p
	}
	mut der := []u8{}
	der << u8(0x30)
	der << u8(body.len)
	der << body
	return der
}

fn test_parse_rsa_public_key_valid() ! {
	// SEQUENCE { INTEGER n=0x00ABCD, INTEGER e=65537 (0x010001) }.
	n_int := [u8(0x02), 0x03, 0x00, 0xab, 0xcd]
	e_int := [u8(0x02), 0x03, 0x01, 0x00, 0x01]
	der := rsa_seq([n_int, e_int])

	k := parse_rsa_public_key(der)!
	assert k.e == 65537
	// The leading 0x00 sign byte must be stripped, leaving a 2-byte modulus.
	assert k.size_bytes == 2
}

fn test_parse_rsa_public_key_rejects_non_sequence() ! {
	// The outer element must be a SEQUENCE (0x30); an INTEGER (0x02) is refused.
	parse_rsa_public_key([u8(0x02), 0x01, 0x05]) or {
		assert err.msg().contains('expected SEQUENCE')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_too_few_elements() ! {
	// A SEQUENCE holding a single INTEGER is not a valid RSAPublicKey.
	one := [u8(0x02), 0x01, 0x05]
	der := rsa_seq([one])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('invalid RSAPublicKey sequence length')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_empty_modulus() ! {
	// n = INTEGER 0x00 strips its sign byte down to an empty modulus.
	n_int := [u8(0x02), 0x01, 0x00]
	e_int := [u8(0x02), 0x01, 0x01]
	der := rsa_seq([n_int, e_int])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('empty modulus')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_zero_exponent() ! {
	// e = 0 is invalid even when the modulus itself parses.
	n_int := [u8(0x02), 0x01, 0x07]
	e_int := [u8(0x02), 0x01, 0x00]
	der := rsa_seq([n_int, e_int])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('exponent must be non-zero')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_degenerate_exponent() ! {
	// e = 1 makes RSAVP1 the identity (m = s^1 mod n = s), so any key-length
	// byte string would "verify" — a universal forgery, hence refused.
	n_int := [u8(0x02), 0x01, 0x07]
	e_int := [u8(0x02), 0x01, 0x01]
	der := rsa_seq([n_int, e_int])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('exponent 1 is degenerate')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_even_exponent() ! {
	// RFC 8017 §5: the RSA public exponent must be odd; e=2 (and any other
	// even value) lives in a small multiplicative subgroup mod n and would
	// halve the effective security of every signature checked with the key.
	n_int := [u8(0x02), 0x01, 0x07]
	e_int := [u8(0x02), 0x01, 0x02]
	der := rsa_seq([n_int, e_int])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('must be odd')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_rejects_oversized_exponent() ! {
	// A 9-byte exponent overflows the u64 accumulator and must be rejected.
	n_int := [u8(0x02), 0x01, 0x07]
	e_int := [u8(0x02), 0x09, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
	der := rsa_seq([n_int, e_int])
	parse_rsa_public_key(der) or {
		assert err.msg().contains('exponent exceeds 64 bits')
		return
	}
	assert false
}

fn test_digest_bytes_known_answers() ! {
	// SHA-256("abc") is a fixed published value; pins digest_bytes' routing.
	assert hex.encode(digest_bytes('sha256', 'abc'.bytes())!) == 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
	// The other selectors must yield their correct output length.
	assert digest_bytes('sha384', 'abc'.bytes())!.len == 48
	assert digest_bytes('sha512', 'abc'.bytes())!.len == 64
	// Any other name is unsupported.
	digest_bytes('md5', 'abc'.bytes()) or {
		assert err.msg().contains('unsupported digest')
		return
	}
	assert false
}

fn test_mgf1_framing_and_truncation() ! {
	seed := 'seed'.bytes()
	// MGF1's first block is Hash(seed || 0x00000000): a big-endian counter appended
	// to the seed. Reconstructing it independently confirms the counter endianness
	// and concatenation order that RSA-PSS mask generation relies on.
	mut b0_in := seed.clone()
	b0_in << [u8(0), 0x00, 0x00, 0x00]
	b0 := sha256.sum256(b0_in)
	// A mask_len of exactly one digest length returns that block verbatim...
	assert mgf1(seed, 32, sha256.sum256) == b0
	// ...and a shorter request truncates it.
	assert mgf1(seed, 16, sha256.sum256) == b0[..16]
	// A longer request spans a second block (counter = 1) while keeping total length.
	mut b1_in := seed.clone()
	b1_in << [u8(0), 0x00, 0x00, 0x01]
	b1 := sha256.sum256(b1_in)
	full := mgf1(seed, 40, sha256.sum256)
	assert full.len == 40
	assert full[..32] == b0
	assert full[32..] == b1[..8]
}

fn test_mgf1_for_selects_hash() ! {
	seed := 'seed'.bytes()
	// The sha256 branch must match MGF1 run directly under sum256.
	assert mgf1_for('sha256', seed, 20)! == mgf1(seed, 20, sha256.sum256)
	// An unknown digest is rejected.
	mgf1_for('bogus', seed, 20) or {
		assert err.msg().contains('unsupported digest')
		return
	}
	assert false
}

fn test_rsa_verify_raw_rejects_bad_length() ! {
	// A 2-byte modulus key: a signature of the wrong length is rejected before any
	// modular arithmetic (RFC 8017 §5.2.2 requires s be k octets).
	n_int := [u8(0x02), 0x03, 0x00, 0xab, 0xcd]
	e_int := [u8(0x02), 0x03, 0x01, 0x00, 0x01]
	der := rsa_seq([n_int, e_int])
	k := parse_rsa_public_key(der)!
	assert k.size_bytes == 2
	k.rsa_verify_raw([u8(0x01)]) or {
		assert err.msg().contains('does not match key size')
		return
	}
	assert false
}

fn test_rsa_verify_raw_rejects_out_of_range() ! {
	n_int := [u8(0x02), 0x03, 0x00, 0xab, 0xcd]
	e_int := [u8(0x02), 0x03, 0x01, 0x00, 0x01]
	der := rsa_seq([n_int, e_int])
	k := parse_rsa_public_key(der)!
	// s = 0xffff (65535) >= n = 0xabcd (43981); RSAVP1 requires s < n.
	k.rsa_verify_raw([u8(0xff), 0xff]) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false
}

// rsa_long_form builds a SEQUENCE over long-form (>=128-byte) DER lengths. The
// short-form rsa_seq helper cannot encode a >127-byte body, and the oversized
// modulus test needs a 1025-byte modulus — exactly the shape a malicious cert
// would carry to make each RSA verification do a multi-megabit modexp.
fn rsa_long_form(n_content []u8, e_content []u8) []u8 {
	// INTEGER tag + long-form length + content, for the modulus and the exponent.
	mut n_tlv := []u8{}
	n_tlv << u8(0x02)
	n_tlv << u8(0x82)
	n_tlv << u8((n_content.len >> 8) & 0xff)
	n_tlv << u8(n_content.len & 0xff)
	n_tlv << n_content
	mut e_tlv := []u8{}
	e_tlv << u8(0x02)
	e_tlv << u8(0x03)
	e_tlv << e_content
	// Wrap both in a SEQUENCE with a long-form length.
	mut body := []u8{}
	body << n_tlv
	body << e_tlv
	mut der := []u8{}
	der << u8(0x30)
	der << u8(0x82)
	der << u8((body.len >> 8) & 0xff)
	der << u8(body.len & 0xff)
	der << body
	return der
}

fn test_parse_rsa_public_key_rejects_oversized_modulus() ! {
	// A 1025-byte modulus (8200 bits) exceeds the 1024-byte (8192-bit) ceiling.
	// Leading 0x7f keeps it positive without an ASN.1 sign byte, so size_bytes is
	// exactly 1025. Must be rejected before any verification work is possible.
	mut n_content := []u8{len: max_rsa_modulus_bytes + 1, init: 0x7f}
	n_content[0] = 0x7f
	e_content := [u8(0x01), 0x00, 0x01]
	der := rsa_long_form(n_content, e_content)
	parse_rsa_public_key(der) or {
		assert err.msg().contains('exceeds maximum supported')
		return
	}
	assert false
}

fn test_parse_rsa_public_key_accepts_modulus_at_ceiling() ! {
	// Exactly max_rsa_modulus_bytes must still parse — only strictly-larger is refused.
	n_content := []u8{len: max_rsa_modulus_bytes, init: 0x7f}
	e_content := [u8(0x01), 0x00, 0x01]
	der := rsa_long_form(n_content, e_content)
	k := parse_rsa_public_key(der)!
	assert k.size_bytes == max_rsa_modulus_bytes
	assert k.e == 65537
}
