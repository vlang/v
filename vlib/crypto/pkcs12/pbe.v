module pkcs12

import crypto.aes
import crypto.cipher
import crypto.des
import crypto.hmac
import crypto.pbkdf2
import crypto.rand
import crypto.sha1
import crypto.sha256
import crypto.sha512

// Supported PBE OIDs.
const oid_pbe_sha1_3des = '1.2.840.113549.1.12.1.3'
const oid_pbe_sha1_rc2_40 = '1.2.840.113549.1.12.1.6'
const oid_pbes2 = '1.2.840.113549.1.5.13'
const oid_pbkdf2 = '1.2.840.113549.1.5.12'
const oid_hmac_sha1 = '1.2.840.113549.2.7'
const oid_hmac_sha224 = '1.2.840.113549.2.8'
const oid_hmac_sha256 = '1.2.840.113549.2.9'
const oid_hmac_sha384 = '1.2.840.113549.2.10'
const oid_hmac_sha512 = '1.2.840.113549.2.11'
const oid_aes128_cbc = '2.16.840.1.101.3.4.1.2'
const oid_aes192_cbc = '2.16.840.1.101.3.4.1.22'
const oid_aes256_cbc = '2.16.840.1.101.3.4.1.42'
const oid_des_ede3_cbc = '1.2.840.113549.3.7'

// Documented unsupported PBE OIDs (explicit error, not silent fallback).
// pbeWithSHAAnd128BitRC4 (1.12.1.1), pbeWithSHAAnd40BitRC4 (1.12.1.2),
// pbeWithSHAAnd3-KeyTripleDES-CBC is supported (1.12.1.3),
// pbeWithSHAAnd2-KeyTripleDES-CBC (1.12.1.4),
// pbeWithSHAAnd128BitRC2-CBC (1.12.1.5), pbewithSHAAnd40BitRC2-CBC supported (1.12.1.6),
// PBES1/pbkdf1, scrypt (1.3.6.1.4.1.11591.4.11), and RC5/DES via PBES2.
const oid_pbe_sha1_rc4_128 = '1.2.840.113549.1.12.1.1'
const oid_pbe_sha1_rc4_40 = '1.2.840.113549.1.12.1.2'

// parse_algorithm_identifier parses SEQ { OID, params? } at `pos`.
// Returns oid dotted string, raw params TLV (empty when absent),
// whether params were present, and the next position.
fn parse_algorithm_identifier(data []u8, pos int) !(string, []u8, bool, int) {
	seq := expect_tag(data, pos, 0x30, 'AlgorithmIdentifier')!
	after := skip_tlv(data, pos)!
	if seq.len < 2 {
		return error('pkcs12: empty AlgorithmIdentifier')
	}
	oid_content := expect_tag(seq, 0, 0x06, 'algorithm OID')!
	oid := oid_decode(oid_content)!
	p := skip_tlv(seq, 0)!
	mut params_tlv := []u8{}
	mut has := false
	if p < seq.len {
		tlv, next := split_tlv(seq, p)!
		if next != seq.len {
			return error('pkcs12: trailing AlgorithmIdentifier data')
		}
		params_tlv = tlv.clone()
		has = true
	}
	return oid, params_tlv, has, after
}

// encode_algorithm_identifier builds SEQ { OID, params-TLV? }.
fn encode_algorithm_identifier(oid string, params_tlv []u8, has_params bool) ![]u8 {
	mut content := der_oid_tlv(oid)!
	if has_params {
		if params_tlv.len == 0 {
			return error('pkcs12: empty params')
		}
		content << params_tlv
	}
	return der_seq(content)
}

// parse_pkcs12_pbe_params parses SEQ { salt OCTET STRING, iterations INTEGER }.
fn parse_pkcs12_pbe_params(params_tlv []u8) !([]u8, int) {
	if params_tlv.len < 2 {
		return error('pkcs12: truncated PBE params')
	}
	tag, content, total := tlv_content(params_tlv, 0)!
	if tag != 0x30 {
		return error('pkcs12: PBE params not SEQUENCE')
	}
	if total != params_tlv.len {
		return error('pkcs12: trailing PBE params')
	}
	salt := expect_tag(content, 0, 0x04, 'PBE salt')!.clone()
	ip := skip_tlv(content, 0)!
	iter_content := expect_tag(content, ip, 0x02, 'PBE iterations')!
	if skip_tlv(content, ip)! != content.len {
		return error('pkcs12: trailing PBE iterations')
	}
	iter := der_parse_integer(iter_content)!
	if iter <= 0 || iter > 10000000 {
		return error('pkcs12: bad PBE iteration count')
	}
	return salt, iter
}

fn encode_pkcs12_pbe_params(salt []u8, iterations int) []u8 {
	mut c := der_octet_string(salt)
	c << der_integer_from_int(iterations)
	return der_seq(c)
}

struct Pbkdf2Params {
	salt   []u8
	iter   int
	keylen int    // 0 when absent
	prf    string // 'sha1' | 'sha256' | 'sha384' | 'sha512' | 'sha224'
}

// parse_pbkdf2_params parses the PBKDF2-params SEQUENCE content.
fn parse_pbkdf2_params(content []u8) !Pbkdf2Params {
	salt := expect_tag(content, 0, 0x04, 'PBKDF2 salt')!.clone()
	mut pos := skip_tlv(content, 0)!
	iter_content := expect_tag(content, pos, 0x02, 'PBKDF2 iterations')!
	iter := der_parse_integer(iter_content)!
	if iter <= 0 || iter > 10000000 {
		return error('pkcs12: bad PBKDF2 iteration count')
	}
	pos = skip_tlv(content, pos)!
	mut keylen := 0
	mut prf := 'sha1'
	if pos < content.len {
		// Could be keyLength INTEGER or prf AlgorithmIdentifier.
		if content[pos] == 0x02 {
			kl_content := expect_tag(content, pos, 0x02, 'PBKDF2 key length')!
			keylen = der_parse_integer(kl_content)!
			if keylen <= 0 || keylen > 1024 {
				return error('pkcs12: bad PBKDF2 key length')
			}
			pos = skip_tlv(content, pos)!
		}
		if pos < content.len {
			prf_seq := expect_tag(content, pos, 0x30, 'PBKDF2 prf')!
			prf_after := skip_tlv(content, pos)!
			if prf_after != content.len {
				return error('pkcs12: trailing PBKDF2 params')
			}
			prf_oid_content := expect_tag(prf_seq, 0, 0x06, 'PRF OID')!
			prf_oid := oid_decode(prf_oid_content)!
			prf = match prf_oid {
				oid_hmac_sha1 { 'sha1' }
				oid_hmac_sha224 { 'sha224' }
				oid_hmac_sha256 { 'sha256' }
				oid_hmac_sha384 { 'sha384' }
				oid_hmac_sha512 { 'sha512' }
				else { return error('pkcs12: unsupported PBES2 PRF ${prf_oid}') }
			}
			// PRF params must be NULL or absent.
			pp := skip_tlv(prf_seq, 0)!
			if pp < prf_seq.len {
				ptag, _, ptotal := tlv_content(prf_seq, pp)!
				if ptag != 0x05 {
					return error('pkcs12: bad PRF params')
				}
				if ptotal + pp != prf_seq.len {
					return error('pkcs12: trailing PRF params')
				}
			}
			pos = prf_after
		}
	}
	if pos != content.len {
		return error('pkcs12: trailing PBKDF2 params')
	}
	return Pbkdf2Params{ salt: salt, iter: iter, keylen: keylen, prf: prf }
}

// cipher_info_for_oid maps an encryption-scheme OID to key/iv lengths.
fn cipher_info_for_oid(oid string) !(int, int) {
	if oid == oid_aes128_cbc {
		return 16, 16
	}
	if oid == oid_aes192_cbc {
		return 24, 16
	}
	if oid == oid_aes256_cbc {
		return 32, 16
	}
	if oid == oid_des_ede3_cbc {
		return 24, 8
	}
	return error('pkcs12: unsupported PBES2 cipher ${oid}')
}

// pbkdf2_derive derives a key with the named PRF hash.
fn pbkdf2_derive(password []u8, salt []u8, iter int, keylen int, prf string) ![]u8 {
	if prf == 'sha1' {
		return pbkdf2.key(password, salt, iter, keylen, sha1.new())
	}
	if prf == 'sha256' {
		return pbkdf2.key(password, salt, iter, keylen, sha256.new())
	}
	if prf == 'sha224' {
		return pbkdf2.key(password, salt, iter, keylen, sha256.new224())
	}
	if prf == 'sha384' {
		return pbkdf2.key(password, salt, iter, keylen, sha512.new384())
	}
	if prf == 'sha512' {
		return pbkdf2.key(password, salt, iter, keylen, sha512.new())
	}
	return error('pkcs12: unsupported PRF ${prf}')
}

// pkcs7_pad adds PKCS#7 padding.
fn pkcs7_pad(data []u8, block_size int) ![]u8 {
	if block_size <= 0 || block_size > 32 {
		return error('pkcs12: bad block size')
	}
	pad_len := block_size - (data.len % block_size)
	mut out := data.clone()
	for _ in 0 .. pad_len {
		out << u8(pad_len)
	}
	return out
}

// pkcs7_unpad removes PKCS#7 padding with full validation.
fn pkcs7_unpad(data []u8, block_size int) ![]u8 {
	if block_size <= 0 || block_size > 32 {
		return error('pkcs12: bad block size')
	}
	if data.len == 0 || data.len % block_size != 0 {
		return error('pkcs12: bad padded length')
	}
	pad := int(data[data.len - 1])
	if pad <= 0 || pad > block_size || pad > data.len {
		return error('pkcs12: bad padding')
	}
	for i in data.len - pad .. data.len {
		if i < 0 || i >= data.len {
			return error('pkcs12: bad padding range')
		}
		if int(data[i]) != pad {
			return error('pkcs12: bad padding bytes')
		}
	}
	return data[..data.len - pad].clone()
}

// cbc_encrypt encrypts with the given block cipher in CBC mode.
fn cbc_encrypt(block cipher.Block, iv []u8, plaintext []u8) ![]u8 {
	bs := block.block_size
	if iv.len != bs {
		return error('pkcs12: bad IV length')
	}
	padded := pkcs7_pad(plaintext, bs)!
	mut out := []u8{len: padded.len}
	mut mode := cipher.new_cbc(block, iv)
	mode.encrypt_blocks(mut out, padded)
	return out
}

// cbc_decrypt decrypts with the given block cipher in CBC mode.
fn cbc_decrypt(block cipher.Block, iv []u8, ciphertext []u8) ![]u8 {
	bs := block.block_size
	if iv.len != bs {
		return error('pkcs12: bad IV length')
	}
	if ciphertext.len == 0 || ciphertext.len % bs != 0 {
		return error('pkcs12: bad ciphertext length')
	}
	mut out := []u8{len: ciphertext.len}
	mut mode := cipher.new_cbc(block, iv)
	mode.decrypt_blocks(mut out, ciphertext)
	return pkcs7_unpad(out, bs)
}

// make_block builds a block cipher for an encryption-scheme OID + key.
fn make_block(oid string, key []u8) !cipher.Block {
	if oid == oid_aes128_cbc || oid == oid_aes192_cbc || oid == oid_aes256_cbc {
		return aes.new_cipher(key)
	}
	if oid == oid_des_ede3_cbc {
		if key.len != 24 {
			return error('pkcs12: bad 3DES key length')
		}
		return des.new_triple_des_cipher(key)
	}
	return error('pkcs12: unsupported cipher ${oid}')
}

// pbe_decrypt decrypts `ciphertext` with the AlgorithmIdentifier at hand.
// `alg_oid` is the PBE OID, `alg_params_tlv` the raw params TLV (or empty).
fn pbe_decrypt(alg_oid string, alg_params_tlv []u8, has_params bool, ciphertext []u8, password []u8) ![]u8 {
	if alg_oid == oid_pbe_sha1_3des {
		if !has_params {
			return error('pkcs12: missing 3DES PBE params')
		}
		salt, iter := parse_pkcs12_pbe_params(alg_params_tlv)!
		pwd := password_to_ucs2(password.bytestr())
		key, iv := pkcs12_derive_key_iv(pwd, salt, iter, 24, 8)!
		block := des.new_triple_des_cipher(key)
		return cbc_decrypt(block, iv, ciphertext)
	}
	if alg_oid == oid_pbe_sha1_rc2_40 {
		if !has_params {
			return error('pkcs12: missing RC2 PBE params')
		}
		salt, iter := parse_pkcs12_pbe_params(alg_params_tlv)!
		pwd := password_to_ucs2(password.bytestr())
		key, iv := pkcs12_derive_key_iv(pwd, salt, iter, 5, 8)!
		block := new_rc2_cipher(key, 40)!
		return cbc_decrypt(block, iv, ciphertext)
	}
	if alg_oid == oid_pbes2 {
		if !has_params {
			return error('pkcs12: missing PBES2 params')
		}
		_, pbes2_content, ptotal := tlv_content(alg_params_tlv, 0)!
		if ptotal != alg_params_tlv.len {
			return error('pkcs12: trailing PBES2 params')
		}
		// pbes2_content = SEQ { kdf SEQ, enc SEQ }.
		kdf_tlv, after_kdf := split_tlv(pbes2_content, 0)!
		enc_tlv, after_enc := split_tlv(pbes2_content, after_kdf)!
		if after_enc != pbes2_content.len {
			return error('pkcs12: trailing PBES2 sequence')
		}
		// KDF must be PBKDF2.
		kdf_oid, kdf_params_tlv, kdf_has, _ := parse_algorithm_identifier(kdf_tlv, 0)!
		if kdf_oid != oid_pbkdf2 {
			return error('pkcs12: unsupported PBES2 KDF ${kdf_oid}')
		}
		if !kdf_has {
			return error('pkcs12: missing PBKDF2 params')
		}
		_, kdf_content, ktotal := tlv_content(kdf_params_tlv, 0)!
		if ktotal != kdf_params_tlv.len {
			return error('pkcs12: trailing PBKDF2 wrapper')
		}
		pbk := parse_pbkdf2_params(kdf_content)!
		enc_oid, enc_params_tlv, enc_has, _ := parse_algorithm_identifier(enc_tlv, 0)!
		key_len, iv_len := cipher_info_for_oid(enc_oid)!
		want := if pbk.keylen != 0 { pbk.keylen } else { key_len }
		if want != key_len {
			return error('pkcs12: PBKDF2 key length mismatch')
		}
		if !enc_has {
			return error('pkcs12: missing encryption params (IV)')
		}
		iv := expect_tag(enc_params_tlv, 0, 0x04, 'PBES2 IV')!.clone()
		if iv.len != iv_len {
			return error('pkcs12: bad PBES2 IV length')
		}
		if skip_tlv(enc_params_tlv, 0)! != enc_params_tlv.len {
			return error('pkcs12: trailing PBES2 IV')
		}
		key := pbkdf2_derive(password, pbk.salt, pbk.iter, key_len, pbk.prf)!
		block := make_block(enc_oid, key)!
		return cbc_decrypt(block, iv, ciphertext)
	}
	if alg_oid == oid_pbe_sha1_rc4_128 || alg_oid == oid_pbe_sha1_rc4_40 {
		return error('pkcs12: unsupported PBE ${alg_oid} (RC4-based, not implemented)')
	}
	if alg_oid == '1.2.840.113549.1.12.1.5' || alg_oid == '1.2.840.113549.1.12.1.4'
		|| alg_oid == '1.2.840.113549.1.5.12' || alg_oid == '1.2.840.113549.1.5.11' {
		return error('pkcs12: unsupported PBE ${alg_oid}')
	}
	return error('pkcs12: unsupported PBE ${alg_oid}')
}

// pbe_encrypt encrypts with a freshly derived key; returns ciphertext + AlgId DER.
fn pbe_encrypt(alg_oid string, password []u8, salt []u8, iterations int, plaintext []u8, extra string) !([]u8, []u8) {
	if alg_oid == oid_pbe_sha1_3des {
		pwd := password_to_ucs2(password.bytestr())
		key, iv := pkcs12_derive_key_iv(pwd, salt, iterations, 24, 8)!
		block := des.new_triple_des_cipher(key)
		ct := cbc_encrypt(block, iv, plaintext)!
		alg := encode_algorithm_identifier(alg_oid, encode_pkcs12_pbe_params(salt,
			iterations), true)!
		return ct, alg
	}
	if alg_oid == oid_pbe_sha1_rc2_40 {
		pwd := password_to_ucs2(password.bytestr())
		key, iv := pkcs12_derive_key_iv(pwd, salt, iterations, 5, 8)!
		block := new_rc2_cipher(key, 40)!
		ct := cbc_encrypt(block, iv, plaintext)!
		alg := encode_algorithm_identifier(alg_oid, encode_pkcs12_pbe_params(salt,
			iterations), true)!
		return ct, alg
	}
	if alg_oid == oid_pbes2 {
		// extra encodes "aes128:sha256" style; default aes256+sha256.
		mut enc_oid := oid_aes256_cbc
		mut prf := 'sha256'
		mut prf_oid := oid_hmac_sha256
		if extra == 'aes128-sha256' {
			enc_oid = oid_aes128_cbc
		} else if extra == 'aes128-sha1' {
			enc_oid = oid_aes128_cbc
			prf = 'sha1'
			prf_oid = oid_hmac_sha1
		} else if extra == 'aes256-sha1' {
			enc_oid = oid_aes256_cbc
			prf = 'sha1'
			prf_oid = oid_hmac_sha1
		} else if extra == 'aes256-sha256' {
		} else if extra == '' {
		} else {
			return error('pkcs12: bad PBES2 extra ${extra}')
		}
		key_len, iv_len := cipher_info_for_oid(enc_oid)!
		iv := rand.bytes(iv_len)!
		key := pbkdf2_derive(password, salt, iterations, key_len, prf)!
		block := make_block(enc_oid, key)!
		ct := cbc_encrypt(block, iv, plaintext)!
		// Build PBKDF2 params: SEQ { salt OCT, iter INT, prf AlgId }.
		// Include explicit PRF so decoders (incl. ours) agree.
		mut prf_alg := der_oid_tlv(prf_oid)!
		prf_alg << der_null()
		prf_alg = der_seq(prf_alg)
		mut kdf_content := der_octet_string(salt)
		kdf_content << der_integer_from_int(iterations)
		kdf_content << prf_alg
		kdf_params := der_seq(kdf_content)
		mut kdf_alg := der_oid_tlv(oid_pbkdf2)!
		kdf_alg << kdf_params
		kdf_alg = der_seq(kdf_alg)
		mut enc_alg := der_oid_tlv(enc_oid)!
		enc_alg << der_octet_string(iv)
		enc_alg = der_seq(enc_alg)
		mut pbes2_content := kdf_alg
		pbes2_content << enc_alg
		pbes2_params := der_seq(pbes2_content)
		alg := encode_algorithm_identifier(oid_pbes2, pbes2_params, true)!
		return ct, alg
	}
	return error('pkcs12: unsupported PBE ${alg_oid} for encryption')
}

// verify_hmac compares two MACs in constant time.
fn verify_hmac(a []u8, b []u8) bool {
	return hmac.equal(a, b)
}
