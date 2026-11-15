module pkcs12

import crypto.hmac
import crypto.rand
import crypto.sha1
import crypto.sha256

// Content-type OIDs.
const oid_data = '1.2.840.113549.1.7.1'
const oid_encrypted_data = '1.2.840.113549.1.7.6'

// Bag OIDs.
const oid_key_bag = '1.2.840.113549.1.12.10.1.1'
const oid_pkcs8_shrouded_bag = '1.2.840.113549.1.12.10.1.2'
const oid_cert_bag = '1.2.840.113549.1.12.10.1.3'

// Certificate OIDs.
const oid_cert_x509 = '1.2.840.113549.1.9.22.1'

// Attribute OIDs.
const oid_friendly_name = '1.2.840.113549.1.9.20'
const oid_local_key_id = '1.2.840.113549.1.9.21'

// Digest OIDs.
const oid_sha1 = '1.3.14.3.2.26'
const oid_sha256 = '2.16.840.1.101.3.4.2.1'

// Supported PBE OIDs for decode/encode (see pbe.v for the cipher set).
// pbeWithSHAAnd3-KeyTripleDES-CBC, pbeWithSHAAnd40BitRC2-CBC and PBES2.
// All other PBE schemes (RC4-based, 2-key 3DES, 128-bit RC2, PBES1,
// scrypt, ...) return explicit "unsupported PBE" errors.

// DecodedPfx is the result of decode: one private key, its chain and names.
pub struct DecodedPfx {
pub:
	key_der        []u8
	certs          [][]u8
	friendly_names []string
}

// PbeKind selects the password-based encryption for encode.
pub enum PbeKind {
	pbe_sha1_3des
	pbe_sha1_rc2_40
	pbes2_aes128_sha256
	pbes2_aes256_sha256
	pbes2_aes128_sha1
	pbes2_aes256_sha1
}

// EncodeConfig controls encode output.
pub struct EncodeConfig {
pub:
	key_pbe       PbeKind = .pbes2_aes256_sha256
	cert_pbe      PbeKind = .pbes2_aes256_sha256
	mac_hash      string  = 'sha256'
	iterations    int     = 2048
	friendly_name string  = 'v-pkcs12'
}

// pbe_kind_oid maps a PbeKind to its OID + PBES2 extra tag.
fn pbe_kind_oid(k PbeKind) (string, string) {
	match k {
		.pbe_sha1_3des { return oid_pbe_sha1_3des, '' }
		.pbe_sha1_rc2_40 { return oid_pbe_sha1_rc2_40, '' }
		.pbes2_aes128_sha256 { return oid_pbes2, 'aes128-sha256' }
		.pbes2_aes256_sha256 { return oid_pbes2, 'aes256-sha256' }
		.pbes2_aes128_sha1 { return oid_pbes2, 'aes128-sha1' }
		.pbes2_aes256_sha1 { return oid_pbes2, 'aes256-sha1' }
	}
}

// decode parses and verifies a PKCS#12 file. It checks the MAC when present
// (wrong/empty password mismatches are errors) and decrypts all SafeContents.
pub fn decode(data []u8, password string) !DecodedPfx {
	if data.len == 0 || data.len > 16 * 1024 * 1024 {
		return error('pkcs12: bad input length')
	}
	top := expect_tag(data, 0, 0x30, 'PFX')!
	if tlv_total_len(data, 0)! != data.len {
		return error('pkcs12: trailing PFX data')
	}
	version_content := expect_tag(top, 0, 0x02, 'PFX version')!
	if der_parse_integer(version_content)! != 3 {
		return error('pkcs12: unsupported PFX version')
	}
	mut pos := skip_tlv(top, 0)!
	auth_tlv, after_auth := split_tlv(top, pos)!
	pos = after_auth
	mut mac_tlv := []u8{}
	mut has_mac := false
	if pos < top.len {
		m, next := split_tlv(top, pos)!
		if next != top.len {
			return error('pkcs12: trailing PFX field')
		}
		mac_tlv = m.clone()
		has_mac = true
		pos = next
	}
	if pos != top.len {
		return error('pkcs12: trailing PFX data')
	}
	// authSafe ContentInfo must be of type data.
	auth_oid, auth_params, auth_has, _ := parse_content_info(auth_tlv, 0)!
	if auth_oid != oid_data {
		return error('pkcs12: unsupported authSafe type ${auth_oid}')
	}
	if !auth_has {
		return error('pkcs12: missing authSafe content')
	}
	// auth_params is the OCTET STRING TLV inside [0]; MAC and parsing
	// operate on its contents (the AuthenticatedSafe DER).
	auth_bytes := expect_tag(auth_params, 0, 0x04, 'authSafe OCTET STRING')!
	if skip_tlv(auth_params, 0)! != auth_params.len {
		return error('pkcs12: trailing authSafe content')
	}
	// Verify MAC before decrypting anything.
	if has_mac {
		verify_mac(mac_tlv, auth_bytes, password)!
	}
	bags := parse_authenticated_safe(auth_bytes, password)!
	mut key_der := []u8{}
	mut certs := [][]u8{}
	mut names := []string{}
	mut got_key := false
	for b in bags {
		if b.is_key {
			if got_key {
				return error('pkcs12: multiple keys unsupported')
			}
			key_der = b.der.clone()
			got_key = true
			if b.friendly_name.len > 0 {
				names << b.friendly_name
			}
		} else {
			certs << b.der.clone()
			if b.friendly_name.len > 0 {
				names << b.friendly_name
			}
		}
	}
	if !got_key {
		return error('pkcs12: no private key found')
	}
	if certs.len == 0 {
		return error('pkcs12: no certificate found')
	}
	return DecodedPfx{
		key_der:        key_der
		certs:          certs
		friendly_names: names
	}
}

struct ParsedBag {
	is_key        bool
	der           []u8
	friendly_name string
}

// parse_content_info parses ContentInfo at `pos`, returning the content OID,
// the raw content bytes inside [0] (for data: OCTET STRING content;
// for encryptedData: EncryptedData TLV), presence flag and next position.
fn parse_content_info(data []u8, pos int) !(string, []u8, bool, int) {
	seq := expect_tag(data, pos, 0x30, 'ContentInfo')!
	after := skip_tlv(data, pos)!
	oid_content := expect_tag(seq, 0, 0x06, 'contentType')!
	oid := oid_decode(oid_content)!
	p := skip_tlv(seq, 0)!
	if p == seq.len {
		return oid, []u8{}, false, after
	}
	// Must be [0] EXPLICIT constructed.
	if p < 0 || p >= seq.len || seq[p] != 0xa0 {
		return error('pkcs12: expected [0] content')
	}
	inner := expect_tag(seq, p, 0xa0, 'content [0]')!
	if skip_tlv(seq, p)! != seq.len {
		return error('pkcs12: trailing ContentInfo data')
	}
	return oid, inner, true, after
}

// parse_authenticated_safe parses AuthenticatedSafe (SEQ OF ContentInfo).
fn parse_authenticated_safe(auth_bytes []u8, password string) ![]ParsedBag {
	// auth_bytes is the DER of SEQUENCE OF ContentInfo.
	if auth_bytes.len == 0 {
		return error('pkcs12: empty authSafe')
	}
	// It must start with SEQUENCE.
	outer := expect_tag(auth_bytes, 0, 0x30, 'AuthenticatedSafe')!
	if tlv_total_len(auth_bytes, 0)! != auth_bytes.len {
		return error('pkcs12: trailing AuthenticatedSafe data')
	}
	mut out := []ParsedBag{}
	mut pos := 0
	for pos < outer.len {
		ci_tlv, next := split_tlv(outer, pos)!
		pos = next
		ci_oid, ci_content, ci_has, _ := parse_content_info(ci_tlv, 0)!
		if ci_oid == oid_data {
			if !ci_has {
				return error('pkcs12: missing data content')
			}
			// ci_content is the inner of [0]: should be OCTET STRING TLV.
			octets := expect_tag(ci_content, 0, 0x04, 'data OCTET STRING')!
			if skip_tlv(ci_content, 0)! != ci_content.len {
				return error('pkcs12: trailing data content')
			}
			for b in parse_safe_contents(octets, password)! {
				out << b
			}
		} else if ci_oid == oid_encrypted_data {
			if !ci_has {
				return error('pkcs12: missing encryptedData content')
			}
			enc_bytes := parse_encrypted_data(ci_content, password)!
			for b in parse_safe_contents(enc_bytes, password)! {
				out << b
			}
		} else {
			return error('pkcs12: unsupported ContentInfo type ${ci_oid}')
		}
	}
	if pos != outer.len {
		return error('pkcs12: trailing AuthenticatedSafe entry')
	}
	return out
}

// parse_encrypted_data decrypts EncryptedData content (already inside [0]).
fn parse_encrypted_data(enc_tlv_inner []u8, password string) ![]u8 {
	// enc_tlv_inner holds the EncryptedData SEQUENCE TLV.
	enc_seq := expect_tag(enc_tlv_inner, 0, 0x30, 'EncryptedData')!
	if skip_tlv(enc_tlv_inner, 0)! != enc_tlv_inner.len {
		return error('pkcs12: trailing EncryptedData')
	}
	ver := expect_tag(enc_seq, 0, 0x02, 'EncryptedData version')!
	if der_parse_integer(ver)! != 0 {
		return error('pkcs12: unsupported EncryptedData version')
	}
	mut pos := skip_tlv(enc_seq, 0)!
	eci_tlv, after_eci := split_tlv(enc_seq, pos)!
	pos = after_eci
	if pos != enc_seq.len {
		return error('pkcs12: trailing EncryptedData field')
	}
	eci := expect_tag(eci_tlv, 0, 0x30, 'EncryptedContentInfo')!
	ct_oid := expect_tag(eci, 0, 0x06, 'enc content type')!
	if oid_decode(ct_oid)! != oid_data {
		return error('pkcs12: unsupported encrypted content type')
	}
	mut epos := skip_tlv(eci, 0)!
	alg_tlv, after_alg := split_tlv(eci, epos)!
	epos = after_alg
	alg_oid, alg_params, alg_has, _ := parse_algorithm_identifier(alg_tlv, 0)!
	// Optional [0] IMPLICIT encrypted content.
	mut encrypted := []u8{}
	if epos < eci.len {
		if epos < 0 || epos >= eci.len || eci[epos] != 0x80 {
			return error('pkcs12: expected [0] encrypted content')
		}
		_, enc_content, _ := tlv_content(eci, epos)!
		encrypted = enc_content.clone()
		if skip_tlv(eci, epos)! != eci.len {
			return error('pkcs12: trailing encrypted content')
		}
	}
	return pbe_decrypt(alg_oid, alg_params, alg_has, encrypted, password.bytes())
}

// verify_mac checks MacData against the authSafe bytes.
fn verify_mac(mac_tlv []u8, auth_bytes []u8, password string) ! {
	mac_seq := expect_tag(mac_tlv, 0, 0x30, 'MacData')!
	if skip_tlv(mac_tlv, 0)! != mac_tlv.len {
		return error('pkcs12: trailing MacData wrapper')
	}
	digest_info_tlv, after_mac := split_tlv(mac_seq, 0)!
	salt_tlv, after_salt := split_tlv(mac_seq, after_mac)!
	salt := expect_tag(salt_tlv, 0, 0x04, 'MAC salt')!.clone()
	mut iterations := 1
	if after_salt < mac_seq.len {
		iter_tlv, after_iter := split_tlv(mac_seq, after_salt)!
		if after_iter != mac_seq.len {
			return error('pkcs12: trailing MacData')
		}
		iter_content := expect_tag(iter_tlv, 0, 0x02, 'MAC iterations')!
		iterations = der_parse_integer(iter_content)!
		if iterations <= 0 || iterations > 10000000 {
			return error('pkcs12: bad MAC iteration count')
		}
	} else {
		if after_salt != mac_seq.len {
			return error('pkcs12: trailing MacData')
		}
	}
	// DigestInfo ::= SEQ { algId SEQ { OID, params? }, digest OCTET STRING }.
	digest_info := expect_tag(digest_info_tlv, 0, 0x30, 'DigestInfo')!
	if skip_tlv(digest_info_tlv, 0)! != digest_info_tlv.len {
		return error('pkcs12: trailing DigestInfo wrapper')
	}
	alg_tlv, after_alg := split_tlv(digest_info, 0)!
	digest_tlv, after_digest := split_tlv(digest_info, after_alg)!
	if after_digest != digest_info.len {
		return error('pkcs12: trailing DigestInfo')
	}
	alg_oid, _, _, _ := parse_algorithm_identifier(alg_tlv, 0)!
	mut hash_name := ''
	if alg_oid == oid_sha1 {
		hash_name = 'sha1'
	} else if alg_oid == oid_sha256 {
		hash_name = 'sha256'
	} else {
		return error('pkcs12: unsupported MAC hash ${alg_oid}')
	}
	want := expect_tag(digest_tlv, 0, 0x04, 'MAC digest')!.clone()
	pwd := password_to_ucs2(password)
	mac_key := pkcs12_derive_mac_key(pwd, salt, iterations, hash_name)!
	mut got := []u8{}
	if hash_name == 'sha1' {
		got = hmac.new(mac_key, auth_bytes, sha1.sum, 64)
	} else {
		got = hmac.new(mac_key, auth_bytes, sha256.sum256, 64)
	}
	if !verify_hmac(got, want) {
		return error('pkcs12: bad MAC (wrong password or tampered data)')
	}
}

// parse_safe_contents parses SafeContents (SEQ OF SafeBag), decrypting
// shrouded keys with `password`.
fn parse_safe_contents(safe_der []u8, password string) ![]ParsedBag {
	outer := expect_tag(safe_der, 0, 0x30, 'SafeContents')!
	if tlv_total_len(safe_der, 0)! != safe_der.len {
		return error('pkcs12: trailing SafeContents data')
	}
	mut out := []ParsedBag{}
	mut pos := 0
	for pos < outer.len {
		bag_tlv, next := split_tlv(outer, pos)!
		pos = next
		out << parse_safe_bag(bag_tlv, password)!
	}
	if pos != outer.len {
		return error('pkcs12: trailing SafeContents entry')
	}
	return out
}

// parse_safe_bag parses one SafeBag, returning key or cert bytes.
fn parse_safe_bag(bag_tlv []u8, password string) ![]ParsedBag {
	bag := expect_tag(bag_tlv, 0, 0x30, 'SafeBag')!
	if skip_tlv(bag_tlv, 0)! != bag_tlv.len {
		return error('pkcs12: trailing SafeBag wrapper')
	}
	bag_id_content := expect_tag(bag, 0, 0x06, 'bagId')!
	bag_id := oid_decode(bag_id_content)!
	mut pos := skip_tlv(bag, 0)!
	val_tlv, after_val := split_tlv(bag, pos)!
	pos = after_val
	mut attrs := []u8{}
	mut has_attrs := false
	if pos < bag.len {
		a_tlv, after_a := split_tlv(bag, pos)!
		pos = after_a
		if pos != bag.len {
			return error('pkcs12: trailing SafeBag attributes')
		}
		// Must be SET OF Attribute.
		if a_tlv.len == 0 || a_tlv[0] != 0x31 {
			return error('pkcs12: bad bag attributes tag')
		}
		attrs = a_tlv.clone()
		has_attrs = true
	}
	if pos != bag.len {
		return error('pkcs12: trailing SafeBag data')
	}
	// bagValue is [0] EXPLICIT.
	if val_tlv.len == 0 || val_tlv[0] != 0xa0 {
		return error('pkcs12: expected bagValue [0]')
	}
	val_inner := expect_tag(bag, skip_tlv(bag, 0)!, 0xa0, 'bagValue')!
	friendly := if has_attrs { parse_friendly_name(attrs)! } else { '' }
	if bag_id == oid_key_bag {
		// Inner is PrivateKeyInfo DER (full TLV).
		if val_inner.len < 2 || val_inner[0] != 0x30 {
			return error('pkcs12: bad PrivateKeyInfo')
		}
		// Validate it is one full TLV.
		if tlv_total_len(val_inner, 0)! != val_inner.len {
			return error('pkcs12: trailing PrivateKeyInfo')
		}
		return [ParsedBag{ is_key: true, der: val_inner.clone(), friendly_name: friendly }]
	}
	if bag_id == oid_pkcs8_shrouded_bag {
		epi := expect_tag(val_inner, 0, 0x30, 'EncryptedPrivateKeyInfo')!
		if skip_tlv(val_inner, 0)! != val_inner.len {
			return error('pkcs12: trailing EncryptedPrivateKeyInfo')
		}
		alg_tlv, after_alg := split_tlv(epi, 0)!
		enc_tlv, after_enc := split_tlv(epi, after_alg)!
		if after_enc != epi.len {
			return error('pkcs12: trailing EncryptedPrivateKeyInfo field')
		}
		alg_oid, alg_params, alg_has, _ := parse_algorithm_identifier(alg_tlv, 0)!
		encrypted := expect_tag(enc_tlv, 0, 0x04, 'encrypted key')!.clone()
		plain := pbe_decrypt(alg_oid, alg_params, alg_has, encrypted, password.bytes())!
		if plain.len < 2 || plain[0] != 0x30 || tlv_total_len(plain, 0)! != plain.len {
			return error('pkcs12: bad decrypted PrivateKeyInfo (wrong password?)')
		}
		return [ParsedBag{ is_key: true, der: plain.clone(), friendly_name: friendly }]
	}
	if bag_id == oid_cert_bag {
		cert_bag := expect_tag(val_inner, 0, 0x30, 'CertBag')!
		if skip_tlv(val_inner, 0)! != val_inner.len {
			return error('pkcs12: trailing CertBag')
		}
		cert_id := expect_tag(cert_bag, 0, 0x06, 'certId')!
		if oid_decode(cert_id)! != oid_cert_x509 {
			return error('pkcs12: unsupported cert type')
		}
		cpos := skip_tlv(cert_bag, 0)!
		cval_tlv, after_cval := split_tlv(cert_bag, cpos)!
		if after_cval != cert_bag.len {
			return error('pkcs12: trailing CertBag field')
		}
		if cval_tlv.len == 0 || cval_tlv[0] != 0xa0 {
			return error('pkcs12: expected certValue [0]')
		}
		cval_inner := expect_tag(cert_bag, cpos, 0xa0, 'certValue')!
		cert_der := expect_tag(cval_inner, 0, 0x04, 'cert OCTET STRING')!.clone()
		if skip_tlv(cval_inner, 0)! != cval_inner.len {
			return error('pkcs12: trailing certValue')
		}
		if cert_der.len < 2 || cert_der[0] != 0x30
			|| tlv_total_len(cert_der, 0)! != cert_der.len {
			return error('pkcs12: bad certificate DER')
		}
		return [ParsedBag{ is_key: false, der: cert_der, friendly_name: friendly }]
	}
	return error('pkcs12: unsupported bag ${bag_id}')
}

// parse_friendly_name extracts the friendlyName attribute, if present.
fn parse_friendly_name(attrs_tlv []u8) !string {
	attrs := expect_tag(attrs_tlv, 0, 0x31, 'attributes')!
	if skip_tlv(attrs_tlv, 0)! != attrs_tlv.len {
		return error('pkcs12: trailing attributes wrapper')
	}
	mut pos := 0
	mut name := ''
	for pos < attrs.len {
		attr_tlv, next := split_tlv(attrs, pos)!
		pos = next
		attr := expect_tag(attr_tlv, 0, 0x30, 'Attribute')!
		if skip_tlv(attr_tlv, 0)! != attr_tlv.len {
			return error('pkcs12: trailing Attribute wrapper')
		}
		oid_content := expect_tag(attr, 0, 0x06, 'attribute OID')!
		oid := oid_decode(oid_content)!
		apos := skip_tlv(attr, 0)!
		vals_tlv, after_vals := split_tlv(attr, apos)!
		if after_vals != attr.len {
			return error('pkcs12: trailing Attribute values')
		}
		if oid == oid_friendly_name {
			vals := expect_tag(vals_tlv, 0, 0x31, 'friendlyName SET')!
			if skip_tlv(vals_tlv, 0)! != vals_tlv.len {
				return error('pkcs12: trailing friendlyName SET')
			}
			bmp := expect_tag(vals, 0, 0x1e, 'friendlyName BMPString')!
			if skip_tlv(vals, 0)! != vals.len {
				return error('pkcs12: trailing friendlyName value')
			}
			name = bmpstring_decode(bmp)!
		}
		// Other attributes (localKeyId, ...) are ignored for names.
	}
	return name
}

// encode builds a .p12 file from a PKCS#8 private key and certificate chain.
// `certs` holds DER-encoded X.509 certificates (leaf first).
pub fn encode(key_der []u8, certs [][]u8, password string, cfg EncodeConfig) ![]u8 {
	if key_der.len < 2 || key_der[0] != 0x30 || tlv_total_len(key_der, 0)! != key_der.len {
		return error('pkcs12: bad private key DER')
	}
	if certs.len == 0 {
		return error('pkcs12: no certificates provided')
	}
	for c in certs {
		if c.len < 2 || c[0] != 0x30 || tlv_total_len(c, 0)! != c.len {
			return error('pkcs12: bad certificate DER')
		}
	}
	if cfg.mac_hash != 'sha1' && cfg.mac_hash != 'sha256' {
		return error('pkcs12: unsupported MAC hash ${cfg.mac_hash}')
	}
	if cfg.iterations <= 0 || cfg.iterations > 10000000 {
		return error('pkcs12: bad iteration count')
	}
	// localKeyId shared by key and certs: SHA-1 of the leaf cert.
	leaf_id := sha1.sum(certs[0])
	friendly_bmp := bmpstring_encode(cfg.friendly_name)
	// --- Key SafeContents: one shroudedKeyBag (plain outer ContentInfo). ---
	key_oid, key_extra := pbe_kind_oid(cfg.key_pbe)
	key_salt := rand.bytes(8)!
	key_ct, key_alg := pbe_encrypt(key_oid, password.bytes(), key_salt, cfg.iterations,
		key_der, key_extra)!
	mut epi_content := key_alg.clone()
	epi_content << der_octet_string(key_ct)
	epi := der_seq(epi_content)
	key_bag_value := explicit_wrap(0, epi)
	mut key_bag_content := der_oid_tlv(oid_pkcs8_shrouded_bag)!
	key_bag_content << key_bag_value
	key_bag_content << encode_bag_attrs(friendly_bmp, leaf_id)
	key_bag := der_seq(key_bag_content)
	key_safe := der_seq(key_bag)
	// Outer ContentInfo data (plain) for keys.
	key_octets := der_octet_string(key_safe)
	mut key_ci_content := der_oid_tlv(oid_data)!
	key_ci_content << explicit_wrap(0, key_octets)
	key_ci := der_seq(key_ci_content)
	// --- Cert SafeContents: certBags, whole block encrypted. ---
	mut cert_bags := []u8{}
	for cert_der in certs {
		cert_bag := encode_cert_bag(cert_der, friendly_bmp, leaf_id)!
		cert_bags << cert_bag
	}
	cert_safe := der_seq(cert_bags)
	cert_oid, cert_extra := pbe_kind_oid(cfg.cert_pbe)
	cert_salt := rand.bytes(8)!
	// PBES2 wants a 16-byte salt for modern interop; pad the 8-byte salt
	// is fine too, but use 16 bytes when PBES2 for closer openssl parity.
	mut use_salt := cert_salt.clone()
	if cert_oid == oid_pbes2 {
		use_salt = rand.bytes(16)!
	}
	cert_ct, cert_alg := pbe_encrypt(cert_oid, password.bytes(), use_salt, cfg.iterations,
		cert_safe, cert_extra)!
	cert_ci := encode_encrypted_data_ci(cert_alg, cert_ct)!
	// AuthenticatedSafe: certs first (openssl order), then keys.
	mut authed := cert_ci.clone()
	authed << key_ci
	authed_safe := der_seq(authed)
	// Outer authSafe ContentInfo (data wrapping AuthenticatedSafe).
	outer_octets := der_octet_string(authed_safe)
	mut outer_content := der_oid_tlv(oid_data)!
	outer_content << explicit_wrap(0, outer_octets)
	outer_ci := der_seq(outer_content)
	// MAC over AuthenticatedSafe bytes.
	mac := encode_mac(authed_safe, password, cfg)!
	mut pfx_content := der_integer_from_int(3)
	pfx_content << outer_ci
	pfx_content << mac
	return der_seq(pfx_content)
}

// encode_cert_bag builds one certBag SafeBag.
fn encode_cert_bag(cert_der []u8, friendly_bmp []u8, local_id []u8) ![]u8 {
	mut cert_value := explicit_wrap(0, der_octet_string(cert_der))
	mut bag_content := der_oid_tlv(oid_cert_x509)!
	bag_content << cert_value
	bag_seq := der_seq(bag_content)
	mut outer := der_oid_tlv(oid_cert_bag)!
	outer << explicit_wrap(0, bag_seq)
	outer << encode_bag_attrs(friendly_bmp, local_id)
	return der_seq(outer)
}

// encode_bag_attrs builds SET { friendlyName, localKeyId }.
fn encode_bag_attrs(friendly_bmp []u8, local_id []u8) []u8 {
	mut fname_attr := der_oid_tlv(oid_friendly_name) or { return []u8{} }
	mut fset := der_tlv(0x1e, friendly_bmp)
	fset = der_set(fset)
	fname_attr << fset
	fname_attr = der_seq(fname_attr)
	mut kid_attr := der_oid_tlv(oid_local_key_id) or { return fname_attr }
	mut kset := der_octet_string(local_id)
	kset = der_set(kset)
	kid_attr << kset
	kid_attr = der_seq(kid_attr)
	mut s := fname_attr.clone()
	s << kid_attr
	return der_set(s)
}

// encode_encrypted_data_ci builds ContentInfo{encryptedData}.
fn encode_encrypted_data_ci(enc_alg []u8, ciphertext []u8) ![]u8 {
	mut eci := der_oid_tlv(oid_data)!
	eci << enc_alg
	eci << implicit_wrap(0, ciphertext)
	eci = der_seq(eci)
	mut enc_data := der_integer_from_int(0)
	enc_data << eci
	enc_data = der_seq(enc_data)
	mut ci := der_oid_tlv(oid_encrypted_data)!
	ci << explicit_wrap(0, enc_data)
	return der_seq(ci)
}

// encode_mac builds MacData for the authSafe bytes.
fn encode_mac(auth_bytes []u8, password string, cfg EncodeConfig) ![]u8 {
	salt := rand.bytes(8)!
	// Use 16-byte MAC salt for modern parity.
	mut use_salt := salt.clone()
	if cfg.mac_hash == 'sha256' {
		use_salt = rand.bytes(16)!
	}
	pwd := password_to_ucs2(password)
	mac_key := pkcs12_derive_mac_key(pwd, use_salt, cfg.iterations, cfg.mac_hash)!
	mut digest := []u8{}
	mut alg_oid := oid_sha1
	if cfg.mac_hash == 'sha256' {
		alg_oid = oid_sha256
		digest = hmac.new(mac_key, auth_bytes, sha256.sum256, 64)
	} else {
		digest = hmac.new(mac_key, auth_bytes, sha1.sum, 64)
	}
	mut alg := der_oid_tlv(alg_oid)!
	alg << der_null()
	alg = der_seq(alg)
	mut di := alg.clone()
	di << der_octet_string(digest)
	di = der_seq(di)
	mut mac := di.clone()
	mac << der_octet_string(use_salt)
	mac << der_integer_from_int(cfg.iterations)
	return der_seq(mac)
}
