module x509

import crypto.ecdsa
import crypto.rand
import crypto.sha256
import time

pub struct CertGenOptions {
pub:
	common_name string
	dns_names   []string
	not_before  time.Time
	not_after   time.Time
	is_ca       bool
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

fn encode_oid(oid string) []u8 {
	parts := oid.split('.')
	if parts.len < 2 { return []u8{} }
	mut content := []u8{}
	a := parts[0].int()
	b := parts[1].int()
	content << u8(a * 40 + b)
	for i := 2; i < parts.len; i++ {
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
	mut ss := ''
	ss += two(t.year % 100)
	ss += two(t.month)
	ss += two(t.day)
	ss += two(t.hour)
	ss += two(t.minute)
	ss += two(t.second)
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

pub fn generate_self_signed(opts CertGenOptions, key ecdsa.PrivateKey) !(Certificate, []u8) {
	pub_bytes := ecdsa_pub_uncompressed(key) or { return err }
	mut serial := rand.bytes(8) or { return err }
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
	for b in encode_oid('1.2.840.10045.4.3.2') { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	mut spki_alg_content := []u8{}
	for b in encode_oid(oid_ec_public_key) { spki_alg_content << b }
	for b in encode_oid(oid_prime256v1) { spki_alg_content << b }
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

	if opts.dns_names.len > 0 {
		mut san_content := []u8{}
		for dn in opts.dns_names {
			gn := der_tlv(0x82, dn.bytes())
			for b in gn { san_content << b }
		}
		san_ext_value := der_seq(san_content)
		mut ext_content := []u8{}
		for b in encode_oid('2.5.29.17') { ext_content << b }
		for b in der_octstr(san_ext_value) { ext_content << b }
		ext := der_seq(ext_content)
		exts := der_seq(ext)
		ctx3 := der_ctx(3, exts)
		for b in ctx3 { tbs << b }
	}

	tbs_der := der_seq(tbs)

	hash := sha256.sum256(tbs_der)
	sig := key.sign(hash, ecdsa.SignerOpts{}) or { return err }
	bsig := der_bitstr(sig)

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
