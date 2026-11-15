module x509

import crypto.ecdsa
import crypto.rand
import crypto.sha1
import time

pub struct CertGenOptions {
pub:
	common_name string
	dns_names   []string
	// ip_names holds raw IP SANs (each 4 bytes IPv4 or 16 bytes IPv6).
	ip_names   [][]u8
	not_before time.Time
	not_after  time.Time
	is_ca      bool
	// key_usage is an RFC 5280 LSB-indexed mask (see ku_* constants);
	// the extension is omitted unless has_key_usage is set.
	key_usage     u16
	has_key_usage bool
	// ext_key_usage holds EKU purpose OIDs; empty omits the extension.
	ext_key_usage []string
	// Name-constraint subtrees (CA use): DNS strings and raw IP
	// ranges (address bytes ++ mask bytes, 8 or 32 bytes each).
	permitted_dns []string
	permitted_ips [][]u8
	excluded_dns  []string
	excluded_ips  [][]u8
}

fn der_len_bytes(n int) []u8 {
	if n < 0x80 {
		return [u8(n)]
	}
	mut out := []u8{}
	mut v := n
	mut bytes := []u8{}
	for v > 0 {
		bytes << u8(v & 0xff)
		v >>= 8
	}
	for i := bytes.len - 1; i >= 0; i-- {
		out << bytes[i]
	}
	mut r := []u8{}
	r << u8(0x80 | bytes.len)
	for b in out { r << b }
	return r
}

fn der_tlv(tag u8, content []u8) []u8 {
	mut out := []u8{}
	out << tag
	lb := der_len_bytes(content.len)
	for b in lb { out << b }
	for b in content { out << b }
	return out
}

fn der_seq(content []u8) []u8 { return der_tlv(0x30, content) }

fn der_set(content []u8) []u8 { return der_tlv(0x31, content) }

fn der_int(val []u8) []u8 { return der_tlv(0x02, val) }

fn is_all_digits(s string) bool {
	if s.len == 0 {
		return false
	}
	for c in s.bytes() {
		if c < u8(`0`) || c > u8(`9`) {
			return false
		}
	}
	return true
}

fn encode_oid(oid string) []u8 {
	parts := oid.split('.')
	if parts.len < 2 { return []u8{} }
	for p in parts {
		if !is_all_digits(p) { return []u8{} }
	}
	mut content := []u8{}
	a := parts[0].int()
	b := parts[1].int()
	content << u8(a * 40 + b)
	for i := 2; i < parts.len; i++ {
		if !is_all_digits(parts[i]) { return []u8{} }
		v := parts[i].int()
		if v < 0x80 {
			content << u8(v)
		} else {
			mut bytes := []u8{}
			mut x := v
			bytes << u8(x & 0x7f)
			x >>= 7
			for x > 0 {
				bytes << u8((x & 0x7f) | 0x80)
				x >>= 7
			}
			for j := bytes.len - 1; j >= 0; j-- {
				content << bytes[j]
			}
		}
	}
	return der_tlv(0x06, content)
}

fn der_bitstr(content []u8) []u8 {
	mut c := []u8{}
	c << u8(0x00)
	for b in content { c << b }
	return der_tlv(0x03, c)
}

fn der_octstr(content []u8) []u8 { return der_tlv(0x04, content) }

fn der_utf8(s string) []u8 { return der_tlv(0x0c, s.bytes()) }

fn der_ia5(s string) []u8 { return der_tlv(0x16, s.bytes()) }

fn two(v int) string {
	if v < 10 { return '0${v}' }
	return v.str()
}

fn der_utc(t time.Time) []u8 {
	// X.509 times are UTC (Zulu): normalize local wall fields first so
	// the DER bytes match what the UTC-assuming parser reconstructs.
	u := t.local_to_utc()
	mut ss := ''
	ss += two(u.year % 100)
	ss += two(u.month)
	ss += two(u.day)
	ss += two(u.hour)
	ss += two(u.minute)
	ss += two(u.second)
	ss += 'Z'
	return der_tlv(0x17, ss.bytes())
}

fn der_ctx(tag u8, content []u8) []u8 {
	return der_tlv(0xa0 | tag, content)
}

fn ecdsa_pub_uncompressed(priv ecdsa.PrivateKey) ![]u8 {
	pb := priv.public_key()!
	return pb.bytes()!
}

// ski_for_raw computes the RFC 5280 method-1 subject key identifier:
// the SHA-1 hash of the BIT STRING value (raw public key bytes).
fn ski_for_raw(raw []u8) []u8 {
	return sha1.sum(raw)
}

// ski_ext assembles a subjectKeyIdentifier extension (non-critical).
// The extnValue wraps the DER-encoded KeyIdentifier OCTET STRING.
fn ski_ext(ski []u8) []u8 {
	mut content := []u8{}
	for b in encode_oid(oid_subject_key_id) {
		content << b
	}
	for b in der_octstr(der_octstr(ski)) {
		content << b
	}
	return der_seq(content)
}

// aki_ext assembles an authorityKeyIdentifier extension carrying only
// the keyIdentifier (non-critical), per RFC 5280 §4.2.1.1.
fn aki_ext(aki []u8) []u8 {
	mut content := []u8{}
	for b in encode_oid(oid_authority_key_id) {
		content << b
	}
	mut kid := []u8{}
	kid << der_tlv(0x80, aki)
	for b in der_octstr(der_seq(kid)) {
		content << b
	}
	return der_seq(content)
}

// bc_ext assembles a critical basicConstraints extension for is_ca
// (CA:TRUE) or a leaf (empty sequence, CA:FALSE).
fn bc_ext(is_ca bool) []u8 {
	mut content := []u8{}
	for b in encode_oid(oid_basic_constraints) {
		content << b
	}
	for b in der_tlv(0x01, [u8(0xff)]) {
		content << b
	}
	inner := if is_ca { der_tlv(0x01, [u8(0xff)]) } else { []u8{} }
	for b in der_octstr(der_seq(inner)) {
		content << b
	}
	return der_seq(content)
}

// check_ip_names rejects malformed raw IP SANs (must be 4 or 16 bytes).
fn check_ip_names(ip_names [][]u8) ! {
	for ip in ip_names {
		if ip.len != 4 && ip.len != 16 {
			return error('x509: invalid IP SAN length')
		}
	}
}

// san_ext assembles a subjectAlternativeName extension for dns_names
// and raw IP addresses (4 or 16 bytes each).
fn san_ext(dns_names []string, ip_names [][]u8) []u8 {
	mut san_content := []u8{}
	for dn in dns_names {
		gn := der_tlv(0x82, dn.bytes())
		for b in gn {
			san_content << b
		}
	}
	for ip in ip_names {
		gn := der_tlv(0x87, ip)
		for b in gn {
			san_content << b
		}
	}
	mut content := []u8{}
	for b in encode_oid(oid_san) {
		content << b
	}
	for b in der_octstr(der_seq(san_content)) {
		content << b
	}
	return der_seq(content)
}

// ku_ext assembles a critical keyUsage extension from an LSB-indexed
// mask (see ku_* constants), packing the minimal BIT STRING.
fn ku_ext(mask u16) []u8 {
	mut nbits := 0
	for i in 0 .. 9 {
		if mask & u16(1 << u16(i)) != 0 {
			nbits = i + 1
		}
	}
	if nbits == 0 {
		nbits = 1
	}
	nbytes := (nbits + 7) / 8
	unused := nbytes * 8 - nbits
	mut val := []u8{}
	val << u8(unused)
	for i in 0 .. nbytes {
		mut byte := u8(0)
		for j in 0 .. 8 {
			bit := i * 8 + j
			if bit < 9 && mask & u16(1 << u16(bit)) != 0 {
				byte |= u8(0x80 >> u8(j))
			}
		}
		val << byte
	}
	if unused > 0 {
		val[val.len - 1] &= u8(0xff << unused)
	}
	mut content := []u8{}
	for b in encode_oid(oid_key_usage) {
		content << b
	}
	for b in der_tlv(0x01, [u8(0xff)]) {
		content << b
	}
	for b in der_octstr(der_tlv(0x03, val)) {
		content << b
	}
	return der_seq(content)
}

// eku_ext assembles a (non-critical) extendedKeyUsage extension.
fn eku_ext(purposes []string) []u8 {
	mut seq := []u8{}
	for p in purposes {
		for b in encode_oid(p) {
			seq << b
		}
	}
	mut content := []u8{}
	for b in encode_oid(oid_ext_key_usage) {
		content << b
	}
	for b in der_octstr(der_seq(seq)) {
		content << b
	}
	return der_seq(content)
}

// nc_subtrees encodes permitted/excluded GeneralSubtrees for one side.
fn nc_subtrees(dns []string, ips [][]u8) []u8 {
	mut trees := []u8{}
	for d in dns {
		mut sub := []u8{}
		sub << der_tlv(0x82, d.bytes())
		trees << der_seq(sub)
	}
	for ip in ips {
		mut sub := []u8{}
		sub << der_tlv(0x87, ip)
		trees << der_seq(sub)
	}
	return der_seq(trees)
}

// nc_ext assembles a critical nameConstraints extension.
fn nc_ext(permitted_dns []string, permitted_ips [][]u8, excluded_dns []string, excluded_ips [][]u8) []u8 {
	mut seq := []u8{}
	if permitted_dns.len + permitted_ips.len > 0 {
		seq << der_tlv(0xa0, nc_subtrees(permitted_dns, permitted_ips))
	}
	if excluded_dns.len + excluded_ips.len > 0 {
		seq << der_tlv(0xa1, nc_subtrees(excluded_dns, excluded_ips))
	}
	mut content := []u8{}
	for b in encode_oid(oid_name_constraints) {
		content << b
	}
	for b in der_tlv(0x01, [u8(0xff)]) {
		content << b
	}
	for b in der_octstr(der_seq(seq)) {
		content << b
	}
	return der_seq(content)
}

// policy_exts assembles KU/EKU/nameConstraints extensions from issuance
// options (empty when the options request none).
fn policy_exts(opts CertGenOptions) []u8 {
	mut out := []u8{}
	if opts.has_key_usage {
		for b in ku_ext(opts.key_usage) {
			out << b
		}
	}
	if opts.ext_key_usage.len > 0 {
		for b in eku_ext(opts.ext_key_usage) {
			out << b
		}
	}
	if opts.permitted_dns.len + opts.permitted_ips.len + opts.excluded_dns.len + opts.excluded_ips.len > 0 {
		for b in nc_ext(opts.permitted_dns, opts.permitted_ips, opts.excluded_dns,
			opts.excluded_ips) {
			out << b
		}
	}
	return out
}

// ca_ski_of returns the CA key identifier for AKI: the CA cert's
// subjectKeyId when present, else the SHA-1 of its raw public key.
fn ca_ski_of(ca_cert &Certificate) ![]u8 {
	if ca_cert.subject_key_id.len > 0 {
		return ca_cert.subject_key_id.clone()
	}
	pk := ca_cert.public_key()!
	return match pk {
		RsaPublicKey {
			n, e := rsa_pub_numbers(pk.der)!
			sha1.sum(rsa_pubkey_der(n, e)!)
		}
		EcdsaP256PublicKey {
			sha1.sum(pk.point)
		}
		EcdsaP384PublicKey {
			sha1.sum(pk.point)
		}
		EcdsaP521PublicKey {
			sha1.sum(pk.point)
		}
		MlDsa44PublicKey {
			sha1.sum(pk.raw)
		}
		MlDsa65PublicKey {
			sha1.sum(pk.raw)
		}
		MlDsa87PublicKey {
			sha1.sum(pk.raw)
		}
		Ed25519PublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake128sPublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake128fPublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake192sPublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake192fPublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake256sPublicKey {
			sha1.sum(pk.raw)
		}
		SlhDsaShake256fPublicKey {
			sha1.sum(pk.raw)
		}
		else {
			return error('x509: SKI not implemented for this key type')
		}
	}
}

// ecdsa_sig_alg_for_key maps an uncompressed EC point to its SPKI curve OID
// and signature-algorithm OID (hash matched to the curve, per RFC 8446 §4.2.3
// convention: P-256/SHA-256, P-384/SHA-384, P-521/SHA-512).
fn ecdsa_sig_alg_for_key(pub_bytes []u8) !(string, string) {
	return match pub_bytes.len {
		65 { oid_prime256v1, '1.2.840.10045.4.3.2' }
		97 { oid_secp384r1, '1.2.840.10045.4.3.3' }
		133 { oid_secp521r1, '1.2.840.10045.4.3.4' }
		else { return error('x509: unsupported EC key size') }
	}
}

pub fn generate_self_signed(opts CertGenOptions, key ecdsa.PrivateKey) !(Certificate, []u8) {
	check_ip_names(opts.ip_names)!
	pub_bytes := ecdsa_pub_uncompressed(key) or { return err }
	mut serial := rand.bytes(8) or { return err }
	for serial.len > 1 && serial[0] == 0 {
		serial = serial[1..].clone()
	}
	serial[0] = serial[0] & 0x7f

	mut name_content := []u8{}
	if opts.common_name.len > 0 {
		mut cn_attr_content := []u8{}
		for b in encode_oid('2.5.4.3') { cn_attr_content << b }
		for b in der_utf8(opts.common_name) { cn_attr_content << b }
		cn_attr := der_seq(cn_attr_content)
		set_content := der_set(cn_attr)
		for b in set_content { name_content << b }
	}
	name_der := der_seq(name_content)

	mut validity_content := []u8{}
	for b in der_utc(opts.not_before) { validity_content << b }
	for b in der_utc(opts.not_after) { validity_content << b }
	validity := der_seq(validity_content)

	mut sig_alg_content := []u8{}
	curve_oid, sigalg_oid := ecdsa_sig_alg_for_key(pub_bytes) or { return err }
	for b in encode_oid(sigalg_oid) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	mut spki_alg_content := []u8{}
	for b in encode_oid(oid_ec_public_key) { spki_alg_content << b }
	for b in encode_oid(curve_oid) { spki_alg_content << b }
	spki_alg := der_seq(spki_alg_content)
	spki_inner := spki_alg
	mut spki_content := []u8{}
	for b in spki_inner { spki_content << b }
	bs := der_bitstr(pub_bytes)
	for b in bs { spki_content << b }
	spki := der_seq(spki_content)

	version := der_ctx(0, der_int([u8(0x02)]))

	mut tbs := []u8{}
	for b in version { tbs << b }
	tbs << der_int(serial)
	for b in sig_alg_id { tbs << b }
	for b in name_der { tbs << b }
	for b in validity { tbs << b }
	for b in name_der { tbs << b }
	for b in spki { tbs << b }

	mut exts_inner := []u8{}
	if opts.dns_names.len + opts.ip_names.len > 0 {
		for b in san_ext(opts.dns_names, opts.ip_names) {
			exts_inner << b
		}
	}
	// Every issued certificate carries subjectKeyIdentifier and
	// authorityKeyIdentifier (self for self-signed) so openssl verify
	// and CRL AKI matching work out of the box; CAs additionally
	// carry critical basicConstraints. (An empty CA:FALSE BC SEQUENCE
	// is rejected by OpenSSL's extension cache, so leaves omit it.)
	ski := ski_for_raw(pub_bytes)
	if opts.is_ca {
		for b in bc_ext(opts.is_ca) {
			exts_inner << b
		}
	}
	for b in policy_exts(opts) {
		exts_inner << b
	}
	for b in ski_ext(ski) {
		exts_inner << b
	}
	for b in aki_ext(ski) {
		exts_inner << b
	}
	if exts_inner.len > 0 {
		exts := der_seq(exts_inner)
		ctx3 := der_ctx(3, exts)
		for b in ctx3 { tbs << b }
	}

	tbs_der := der_seq(tbs)

	// Sign the TBS bytes directly: ecdsa signs with the curve's recommended
	// hash internally (SHA-256/384/512 for P-256/384/521), matching sig_alg_id.
	sig := key.sign(tbs_der, ecdsa.SignerOpts{}) or { return err }
	bsig := der_bitstr(sig)

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
