module x509

import crypto.pem
import time

// Certificate is a parsed X.509 certificate: identity (issuer/subject),
// validity window, and DNS names from the subject alternative names. It is
// the V-idiomatic equivalent of the parsing half of Go's crypto/x509.
// Chain building and full verification are follow-ups (they need the trust
// store design of #28405); check_host covers the hostname step.
pub struct Certificate {
pub:
	version    int
	serial     []u8
	issuer     map[string]string // OID string -> value, e.g. '2.5.4.3'
	subject    map[string]string
	not_before time.Time
	not_after  time.Time
	dns_names  []string
	raw        []u8
}

// Well-known attribute OIDs.
pub const oid_common_name = '2.5.4.3'
pub const oid_organization = '2.5.4.10'
pub const oid_san = '2.5.29.17'

// DER tag numbers used below.
const tag_seq = 0x30
const tag_set = 0x31
const tag_integer = 0x02
const tag_bitstring = 0x03
const tag_octet_string = 0x04
const tag_oid = 0x06
const tag_utf8string = 0x0C
const tag_printable = 0x13
const tag_ia5 = 0x16
const tag_utctime = 0x17
const tag_gentime = 0x18
const tag_ctx0 = 0xA0 // [0] EXPLICIT, constructed (version wrapper)
// tag_dnsname is GeneralName dNSName [2]: primitive, so WITHOUT the
// constructed bit (0x82, not 0xA2)
const tag_dnsname = 0x82
const tag_ctx3 = 0xA3

// parse_pem parses the first CERTIFICATE block of PEM text.
pub fn parse_pem(pem_text string) !Certificate {
	block, _ := pem.decode(pem_text) or { return error('x509: no PEM block found') }
	if block.block_type != 'CERTIFICATE' {
		return error('x509: expected CERTIFICATE block, got `${block.block_type}`')
	}
	return parse_der(block.data)
}

// parse_der parses one DER-encoded certificate.
pub fn parse_der(der []u8) !Certificate {
	cert_body := expect_tag(der, 0, tag_seq, 'certificate')!
	tbs, _ := split_seq(cert_body)
	mut pos := 0
	mut version := 1
	if tbs.len > 0 && tbs[0] == tag_ctx0 {
		inner := expect_tag(tbs, 0, tag_ctx0, 'version')!
		vbytes := expect_tag(inner, 0, tag_integer, 'version number')!
		if vbytes.len != 1 {
			return error('x509: bad version length')
		}
		version = int(vbytes[0]) + 1
		pos = tlv_total_len(tbs, 0)!
	}
	serial := expect_tag(tbs, pos, tag_integer, 'serial')!.clone()
	pos = skip_tlv(tbs, pos)!
	// signature algorithm: skip
	pos = skip_tlv(tbs, pos)!
	// issuer
	issuer_body, after_issuer := split_tlv(tbs, pos)
	if issuer_body.len == 0 || issuer_body[0] != tag_seq {
		return error('x509: expected issuer SEQUENCE')
	}
	issuer := parse_name(expect_tag(issuer_body, 0, tag_seq, 'issuer')!)!
	pos = after_issuer
	// validity
	validity_body, after_validity := split_tlv(tbs, pos)
	if validity_body.len == 0 || validity_body[0] != tag_seq {
		return error('x509: expected validity SEQUENCE')
	}
	validity := expect_tag(validity_body, 0, tag_seq, 'validity')!
	not_before, not_after := parse_validity(validity)!
	pos = after_validity
	// subject
	subject_body, after_subject := split_tlv(tbs, pos)
	if subject_body.len == 0 || subject_body[0] != tag_seq {
		return error('x509: expected subject SEQUENCE')
	}
	subject := parse_name(expect_tag(subject_body, 0, tag_seq, 'subject')!)!
	pos = after_subject
	// subjectPublicKeyInfo: skip
	pos = skip_tlv(tbs, pos)!
	// extensions [3] EXPLICIT, optional
	mut dns_names := []string{}
	if pos < tbs.len && tbs[pos] == tag_ctx3 {
		ext_outer := expect_tag(tbs, pos, tag_ctx3, 'extensions')!
		exts := expect_tag(ext_outer, 0, tag_seq, 'extensions list')!
		dns_names = parse_extensions(exts)!
	}
	return Certificate{
		version:    version
		serial:     serial
		issuer:     issuer
		subject:    subject
		not_before: not_before
		not_after:  not_after
		dns_names:  dns_names
		raw:        der.clone()
	}
}

// common_name returns the subject CN, or '' if absent.
pub fn (c &Certificate) common_name() string {
	return c.subject[oid_common_name] or { '' }
}

// organization returns the subject O, or '' if absent.
pub fn (c &Certificate) organization() string {
	return c.subject[oid_organization] or { '' }
}

// check_host verifies that name is covered: SAN dNSNames first (a present
// but non-matching SAN list fails closed with no CN fallback, per RFC 6125);
// the subject CN only when no SAN dNSNames exist. Wildcards match exactly
// one leftmost label, case-insensitively.
pub fn (c &Certificate) check_host(name string) ! {
	if c.dns_names.len > 0 {
		for san in c.dns_names {
			if dns_match(san, name) {
				return
			}
		}
		return error('x509: `${name}` is not covered by SAN ${c.dns_names}')
	}
	if cn := c.subject[oid_common_name] {
		if dns_match(cn, name) {
			return
		}
	}
	return error('x509: `${name}` does not match certificate identity')
}

// dns_match reports whether pattern (possibly wildcard) covers name.
fn dns_match(pattern string, name string) bool {
	p := pattern.to_lower()
	n := name.to_lower()
	if p.starts_with('*.') {
		suffix := p[1..]
		if !n.ends_with(suffix) {
			return false
		}
		// exactly one label may replace the star
		rest := n[..n.len - suffix.len]
		return rest.len > 0 && !rest.contains('.')
	}
	return p == n
}

// --- DER plumbing ---

// tlv_total_len returns tag+length+content bytes starting at pos.
fn tlv_total_len(data []u8, pos int) !int {
	if pos + 1 >= data.len {
		return error('x509: truncated TLV header')
	}
	len_len, content_len := der_length(data, pos + 1)!
	return 1 + len_len + content_len
}

// split_tlv splits one TLV at pos into (full-tlv, next-pos).
fn split_tlv(data []u8, pos int) ([]u8, int) {
	total := tlv_total_len(data, pos) or { return data[pos..], data.len }
	end := pos + total
	if end > data.len {
		return data[pos..], data.len
	}
	return data[pos..end], end
}

// skip_tlv advances past one TLV.
fn skip_tlv(data []u8, pos int) !int {
	_, next := split_tlv(data, pos)
	if next <= pos {
		return error('x509: cannot skip TLV')
	}
	return next
}

// expect_tag returns the content bytes of the TLV at pos, checking its tag.
fn expect_tag(data []u8, pos int, tag u8, what string) ![]u8 {
	if pos >= data.len || data[pos] != tag {
		return error('x509: expected ${what}')
	}
	len_len, content_len := der_length(data, pos + 1)!
	start := pos + 1 + len_len
	end := start + content_len
	if end > data.len {
		return error('x509: truncated ${what}')
	}
	return data[start..end]
}

// split_seq returns the content and end position of the SEQUENCE at the
// start of data. (split_tlv borrows its error default, so this cannot fail,
// but callers still validate the tag.)
fn split_seq(data []u8) ([]u8, int) {
	if data.len == 0 || data[0] != tag_seq {
		return []u8{}, 0
	}
	content := expect_tag(data, 0, tag_seq, 'sequence') or { return []u8{}, 0 }
	total := tlv_total_len(data, 0) or { return content, data.len }
	return content, total
}

// der_length decodes short/long-form lengths (indefinite form rejected).
// Returns (length-of-length, content-length).
fn der_length(data []u8, pos int) !(int, int) {
	if pos >= data.len {
		return error('x509: truncated length')
	}
	first := data[pos]
	if first < 0x80 {
		return 1, int(first)
	}
	n := int(first & 0x7F)
	if n == 0 || n > 4 || pos + n >= data.len {
		return error('x509: bad length encoding')
	}
	mut len := 0
	for i in 1 .. n + 1 {
		len = len * 256 + int(data[pos + i])
	}
	return 1 + n, len
}

// parse_name reads an RDNSequence into an OID->value map, decoding the
// standard string types (anything else is skipped, not failed).
fn parse_name(body []u8) !map[string]string {
	mut out := map[string]string{}
	mut pos := 0
	for pos < body.len {
		set_tlv, next := split_tlv(body, pos)
		pos = next
		if set_tlv.len == 0 || set_tlv[0] != tag_set {
			return error('x509: expected RDN SET')
		}
		inner := expect_tag(set_tlv, 0, tag_set, 'RDN')!
		seq := expect_tag(inner, 0, tag_seq, 'attribute')!
		oid_tlv, after_oid := split_tlv(seq, 0)
		if oid_tlv.len == 0 || oid_tlv[0] != tag_oid {
			return error('x509: expected attribute OID')
		}
		oid := oid_string(expect_tag(oid_tlv, 0, tag_oid, 'OID')!)!
		val_tlv, _ := split_tlv(seq, after_oid)
		if s := der_string(val_tlv) {
			out[oid] = s
		}
	}
	return out
}

// der_string decodes PrintableString/UTF8String/IA5String/TeletexString;
// other string types return none (skipped, per parse_name contract).
fn der_string(tlv []u8) ?string {
	if tlv.len < 2 {
		return none
	}
	tag := tlv[0]
	if tag != tag_printable && tag != tag_utf8string && tag != tag_ia5 && tag != 0x14
		&& tag != 0x1E {
		return none
	}
	content := expect_tag(tlv, 0, tag, 'string') or { return none }
	return content.bytestr()
}

// oid_string renders DER OID bytes in dotted form.
fn oid_string(bytes []u8) !string {
	if bytes.len == 0 {
		return error('x509: empty OID')
	}
	first := int(bytes[0])
	mut parts := if first < 40 {
		['0', first.str()]
	} else if first < 80 {
		['1', (first - 40).str()]
	} else {
		['2', (first - 80).str()]
	}
	mut val := 0
	mut have := false
	for b in bytes[1..] {
		val = val * 128 + int(b & 0x7F)
		if b & 0x80 == 0 {
			parts << val.str()
			val = 0
			have = false
		} else {
			have = true
		}
	}
	if have {
		return error('x509: truncated OID')
	}
	return parts.join('.')
}

// parse_validity reads the {notBefore, notAfter} SEQUENCE.
fn parse_validity(body []u8) !(time.Time, time.Time) {
	first, after_first := split_tlv(body, 0)
	second, _ := split_tlv(body, after_first)
	return parse_time(first)!, parse_time(second)!
}

// parse_time reads a UTCTime or GeneralizedTime (Zulu only).
fn parse_time(tlv []u8) !time.Time {
	if tlv.len < 2 {
		return error('x509: bad time')
	}
	tag := tlv[0]
	content := expect_tag(tlv, 0, tag, 'time')!
	s := content.bytestr()
	if tag == tag_utctime {
		if s.len < 13 || s[s.len - 1] != `Z` {
			return error('x509: bad UTCTime `${s}`')
		}
		yy := s[..2].int()
		year := if yy < 50 { 2000 + yy } else { 1900 + yy }
		return time.new(time.Time{
			year:   year
			month:  s[2..4].int()
			day:    s[4..6].int()
			hour:   s[6..8].int()
			minute: s[8..10].int()
			second: s[10..12].int()
		})
	}
	if tag == tag_gentime {
		if s.len < 15 || s[s.len - 1] != `Z` {
			return error('x509: bad GeneralizedTime `${s}`')
		}
		return time.new(time.Time{
			year:   s[..4].int()
			month:  s[4..6].int()
			day:    s[6..8].int()
			hour:   s[8..10].int()
			minute: s[10..12].int()
			second: s[12..14].int()
		})
	}
	return error('x509: unsupported time tag ${tag}')
}

// parse_extensions walks the extensions list, collecting SAN dNSNames.
// Unknown extensions are skipped (criticality ignored: verification itself
// is a follow-up).
fn parse_extensions(body []u8) ![]string {
	mut dns := []string{}
	mut pos := 0
	for pos < body.len {
		ext, next := split_tlv(body, pos)
		pos = next
		inner := expect_tag(ext, 0, tag_seq, 'extension')!
		mut ip := 0
		oid_tlv, after_oid := split_tlv(inner, ip)
		if oid_tlv.len == 0 || oid_tlv[0] != tag_oid {
			return error('x509: expected extension OID')
		}
		oid := oid_string(expect_tag(oid_tlv, 0, tag_oid, 'OID')!)!
		ip = after_oid
		// optional critical BOOLEAN
		val_tlv, _ := split_tlv(inner, ip)
		if val_tlv.len > 0 && val_tlv[0] == 0x01 {
			_, after_bool := split_tlv(inner, ip)
			ip = after_bool
		}
		val_tlv2, _ := split_tlv(inner, ip)
		if val_tlv2.len == 0 || val_tlv2[0] != tag_octet_string {
			return error('x509: expected extension value')
		}
		octets := expect_tag(val_tlv2, 0, tag_octet_string, 'extension value')!
		if oid == oid_san {
			dns << parse_san(octets)!
		}
	}
	return dns
}

// parse_san reads the GeneralNames SEQUENCE, keeping dNSName ([2]) entries.
fn parse_san(body []u8) ![]string {
	names_body := expect_tag(body, 0, tag_seq, 'SAN list')!
	mut dns := []string{}
	mut pos := 0
	for pos < names_body.len {
		gn, next := split_tlv(names_body, pos)
		pos = next
		if gn.len > 0 && gn[0] == tag_dnsname {
			content := expect_tag(gn, 0, tag_dnsname, 'dNSName')!
			dns << content.bytestr()
		}
	}
	return dns
}
