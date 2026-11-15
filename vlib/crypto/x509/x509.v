module x509

import crypto.pem
import time
import encoding.utf8

// Certificate is a parsed X.509 certificate: identity (issuer/subject),
// validity window, and DNS names from the subject alternative names. It is
// the V-idiomatic equivalent of the parsing half of Go's crypto/x509.
// Chain building and full verification live in verify.v (build_chain,
// verify_chain); check_host covers the hostname step.
pub struct Certificate {
pub:
	version    int
	serial     []u8
	issuer     map[string]string // OID string -> value, e.g. '2.5.4.3'
	subject    map[string]string
	not_before time.Time
	not_after  time.Time
	dns_names  []string
	// ip_addresses holds raw IP SANs (4 bytes IPv4 or 16 bytes IPv6).
	ip_addresses [][]u8
	raw          []u8
	has_san      bool
	// tbs_der is the full TBS TLV (tag+length+value) for signature checks.
	tbs_der []u8
	// issuer_der and subject_der are the full Name TLVs for chain matching.
	issuer_der  []u8
	subject_der []u8
	// sig_alg_oid is the dotted OID of the outer signature algorithm.
	sig_alg_oid string
	// sig_alg_val holds the signatureAlgorithm SEQUENCE value bytes so
	// verifiers can enforce single-OID (no-parameters) encodings.
	sig_alg_val []u8
	// signature holds the raw signature bytes (BIT STRING minus pad byte).
	signature []u8
	// key_usage is an RFC 5280 bitmask (bit 0 = digitalSignature from the
	// LSB side); only meaningful when has_key_usage is set.
	key_usage     u16
	has_key_usage bool
	// ext_key_usage holds EKU dotted OIDs (e.g. '1.3.6.1.5.5.7.3.1').
	ext_key_usage []string
	// is_ca reports basicConstraints cA; has_basic_constraints tracks
	// whether the extension was present at all.
	is_ca                 bool
	has_basic_constraints bool
	// path_len caps following CA certs; only set when has_path_len.
	path_len     int
	has_path_len bool
	// Name-constraint subtrees (dNSName strings, iP raw addr++mask).
	permitted_dns        []string
	permitted_ips        [][]u8
	excluded_dns         []string
	excluded_ips         [][]u8
	has_name_constraints bool
	// policies holds certificatePolicies OIDs (anyPolicy passthrough).
	policies               []string
	has_policies           bool
	has_policy_constraints bool
	// authority_key_id / subject_key_id are raw KeyIdentifiers.
	authority_key_id []u8
	subject_key_id   []u8
	// crl_distribution_points and ocsp_urls hold extracted URIs.
	crl_distribution_points []string
	ocsp_urls               []string
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
	mut rest := pem_text
	mut saw_block := false
	for {
		block, next := pem.decode(rest) or {
			return error(if saw_block {
				'x509: no CERTIFICATE PEM block found'
			} else {
				'x509: no PEM block found'
			})
		}
		saw_block = true
		if block.block_type == 'CERTIFICATE' {
			return parse_der(block.data)
		}
		rest = next
	}
	return error('x509: no CERTIFICATE PEM block found')
}

// parse_der parses one complete DER-encoded certificate without verifying its signature.
pub fn parse_der(der []u8) !Certificate {
	cert_body := expect_tag(der, 0, tag_seq, 'certificate')!
	if tlv_total_len(der, 0)! != der.len {
		return error('x509: trailing certificate data')
	}
	tbs := expect_tag(cert_body, 0, tag_seq, 'TBSCertificate')!
	outer_pos := skip_tlv(cert_body, 0)!
	outer_algorithm := expect_tag(cert_body, outer_pos, tag_seq, 'signature algorithm')!
	parse_algorithm(outer_algorithm)!
	signature_pos := skip_tlv(cert_body, outer_pos)!
	signature := expect_tag(cert_body, signature_pos, tag_bitstring, 'signature')!
	validate_bitstring(signature)!
	if signature.len < 2 || signature[0] != 0 || skip_tlv(cert_body, signature_pos)! != cert_body.len {
		return error('x509: invalid signature encoding')
	}
	mut pos := 0
	mut version := 1
	if tbs.len > 0 && tbs[0] == tag_ctx0 {
		inner := expect_tag(tbs, 0, tag_ctx0, 'version')!
		vbytes := expect_tag(inner, 0, tag_integer, 'version number')!
		if vbytes.len != 1 || vbytes[0] > 2 || skip_tlv(inner, 0)! != inner.len {
			return error('x509: bad version')
		}
		version = int(vbytes[0]) + 1
		pos = skip_tlv(tbs, 0)!
	}
	serial := expect_tag(tbs, pos, tag_integer, 'serial')!.clone()
	if serial.len == 0 || serial[0] & 0x80 != 0
		|| (serial.len > 1 && serial[0] == 0 && serial[1] & 0x80 == 0) {
		return error('x509: invalid serial integer')
	}
	pos = skip_tlv(tbs, pos)!
	algorithm := expect_tag(tbs, pos, tag_seq, 'TBS signature algorithm')!
	parse_algorithm(algorithm)!
	if algorithm != outer_algorithm {
		return error('x509: signature algorithms differ')
	}
	pos = skip_tlv(tbs, pos)!
	issuer := parse_name(expect_tag(tbs, pos, tag_seq, 'issuer')!)!
	issuer_der, _ := split_tlv(tbs, pos)!
	pos = skip_tlv(tbs, pos)!
	not_before, not_after := parse_validity(expect_tag(tbs, pos, tag_seq, 'validity')!)!
	pos = skip_tlv(tbs, pos)!
	subject := parse_name(expect_tag(tbs, pos, tag_seq, 'subject')!)!
	subject_der, _ := split_tlv(tbs, pos)!
	pos = skip_tlv(tbs, pos)!
	spki := expect_tag(tbs, pos, tag_seq, 'subjectPublicKeyInfo')!
	parse_algorithm(expect_tag(spki, 0, tag_seq, 'public key algorithm')!)!
	key_pos := skip_tlv(spki, 0)!
	validate_bitstring(expect_tag(spki, key_pos, tag_bitstring, 'public key')!)!
	if skip_tlv(spki, key_pos)! != spki.len {
		return error('x509: trailing public key data')
	}
	pos = skip_tlv(tbs, pos)!
	// Optional IMPLICIT unique identifiers precede the EXPLICIT extensions.
	for tag in [u8(0x81), 0x82] {
		if pos < tbs.len && tbs[pos] == tag {
			if version < 2 {
				return error('x509: unique identifiers require version 2 or 3')
			}
			validate_bitstring(expect_tag(tbs, pos, tag, 'unique identifier')!)!
			pos = skip_tlv(tbs, pos)!
		}
	}
	mut parsed_exts := ParsedExtensions{}
	if pos < tbs.len && tbs[pos] == tag_ctx3 {
		if version != 3 {
			return error('x509: extensions require version 3')
		}
		ext_outer := expect_tag(tbs, pos, tag_ctx3, 'extensions')!
		exts := expect_tag(ext_outer, 0, tag_seq, 'extensions list')!
		if skip_tlv(ext_outer, 0)! != ext_outer.len {
			return error('x509: trailing extensions data')
		}
		parsed_exts = parse_extensions(exts)!
		pos = skip_tlv(tbs, pos)!
	}
	if pos != tbs.len {
		return error('x509: unexpected TBSCertificate field')
	}
	tbs_full, _ := split_tlv(cert_body, 0)!
	sig_oid := oid_string(expect_tag(outer_algorithm, 0, tag_oid, 'algorithm OID')!)!
	sig_bytes := signature[1..].clone()
	return Certificate{
		version:                 version
		serial:                  serial
		issuer:                  issuer
		subject:                 subject
		not_before:              not_before
		not_after:               not_after
		dns_names:               parsed_exts.dns_names
		ip_addresses:            parsed_exts.ip_addresses
		raw:                     der.clone()
		has_san:                 parsed_exts.has_san
		tbs_der:                 tbs_full.clone()
		issuer_der:              issuer_der.clone()
		subject_der:             subject_der.clone()
		sig_alg_oid:             sig_oid
		sig_alg_val:             outer_algorithm.clone()
		signature:               sig_bytes
		key_usage:               parsed_exts.key_usage
		has_key_usage:           parsed_exts.has_key_usage
		ext_key_usage:           parsed_exts.ext_key_usage
		is_ca:                   parsed_exts.is_ca
		has_basic_constraints:   parsed_exts.has_basic_constraints
		path_len:                parsed_exts.path_len
		has_path_len:            parsed_exts.has_path_len
		permitted_dns:           parsed_exts.permitted_dns
		permitted_ips:           parsed_exts.permitted_ips
		excluded_dns:            parsed_exts.excluded_dns
		excluded_ips:            parsed_exts.excluded_ips
		has_name_constraints:    parsed_exts.has_name_constraints
		policies:                parsed_exts.policies
		has_policies:            parsed_exts.has_policies
		has_policy_constraints:  parsed_exts.has_policy_constraints
		authority_key_id:        parsed_exts.authority_key_id
		subject_key_id:          parsed_exts.subject_key_id
		crl_distribution_points: parsed_exts.crl_distribution_points
		ocsp_urls:               parsed_exts.ocsp_urls
	}
}

fn parse_algorithm(body []u8) ! {
	_ := oid_string(expect_tag(body, 0, tag_oid, 'algorithm OID')!)!
	mut pos := skip_tlv(body, 0)!
	if pos < body.len {
		pos = skip_tlv(body, pos)!
	}
	if pos != body.len {
		return error('x509: trailing algorithm data')
	}
}

fn validate_bitstring(body []u8) ! {
	if body.len == 0 || body[0] > 7 || (body.len == 1 && body[0] != 0) {
		return error('x509: invalid BIT STRING')
	}
	if body.len > 1 && body[0] > 0 && body.last() & u8((1 << body[0]) - 1) != 0 {
		return error('x509: nonzero BIT STRING padding')
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

// check_host checks a DNS hostname against SAN identities, with legacy CN fallback
// only when the SAN extension is absent. Wildcards match one leftmost label.
// It does not verify signatures, trust, validity periods, or IP address identities.
pub fn (c &Certificate) check_host(name string) ! {
	if !valid_dns_name(name, false) || is_ip_name(name) {
		return error('x509: invalid DNS hostname `${name}`')
	}
	if c.has_san || c.dns_names.len > 0 {
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

fn is_ip_name(name string) bool {
	if name.contains(':') {
		return true
	}
	parts := name.trim_right('.').split('.')
	if parts.len != 4 {
		return false
	}
	for p in parts {
		if p.len == 0 || p.len > 3 {
			return false
		}
		for c in p.bytes() {
			if c < `0` || c > `9` {
				return false
			}
		}
		if p.int() > 255 {
			return false
		}
	}
	return true
}

fn valid_dns_name(name string, wildcard bool) bool {
	mut n := name.to_lower()
	if n.ends_with('.') {
		n = n[..n.len - 1]
	}
	if n.len == 0 || n.len > 253 {
		return false
	}
	labels := n.split('.')
	for i, label in labels {
		if wildcard && i == 0 && label == '*' && labels.len >= 3 {
			continue
		}
		if label.len == 0 || label.len > 63 || label[0] == `-` || label[label.len - 1] == `-` {
			return false
		}
		for c in label.bytes() {
			if !(c >= `a` && c <= `z`) && !(c >= `0` && c <= `9`) && c != `-` {
				return false
			}
		}
	}
	return true
}

// dns_match reports whether a valid DNS pattern covers name.
fn dns_match(pattern string, name string) bool {
	if !valid_dns_name(pattern, true) || !valid_dns_name(name, false) || is_ip_name(name) {
		return false
	}
	p := pattern.to_lower().trim_right('.')
	n := name.to_lower().trim_right('.')
	if p.starts_with('*.') {
		suffix := p[1..]
		if !n.ends_with(suffix) {
			return false
		}
		rest := n[..n.len - suffix.len]
		return rest.len > 0 && !rest.contains('.')
	}
	return p == n
}

// --- DER plumbing ---

// tlv_total_len returns tag+length+content bytes after checking all bounds.
fn tlv_total_len(data []u8, pos int) !int {
	if pos < 0 || pos >= data.len || data.len - pos < 2 {
		return error('x509: truncated TLV header')
	}
	if data[pos] & 0x1f == 0x1f {
		return error('x509: unsupported high-tag-number encoding')
	}
	len_len, content_len := der_length(data, pos + 1)!
	header_len := 1 + len_len
	if header_len > data.len - pos || content_len > data.len - pos - header_len {
		return error('x509: truncated TLV content')
	}
	return header_len + content_len
}

// split_tlv splits a checked TLV into its full bytes and the next position.
fn split_tlv(data []u8, pos int) !([]u8, int) {
	total := tlv_total_len(data, pos)!
	return data[pos..pos + total], pos + total
}

fn skip_tlv(data []u8, pos int) !int {
	return pos + tlv_total_len(data, pos)!
}

fn expect_tag(data []u8, pos int, tag u8, what string) ![]u8 {
	if pos < 0 || pos >= data.len || data[pos] != tag {
		return error('x509: expected ${what}')
	}
	total := tlv_total_len(data, pos)!
	len_len, _ := der_length(data, pos + 1)!
	return data[pos + 1 + len_len..pos + total]
}

// der_length decodes a canonical definite DER length bounded by the input.
fn der_length(data []u8, pos int) !(int, int) {
	if pos < 0 || pos >= data.len {
		return error('x509: truncated length')
	}
	first := data[pos]
	if first < 0x80 {
		return 1, int(first)
	}
	n := int(first & 0x7F)
	if n == 0 || n > 4 || n > data.len - pos - 1 || data[pos + 1] == 0 {
		return error('x509: bad length encoding')
	}
	mut length := u64(0)
	for i in 1 .. n + 1 {
		length = length * 256 + u64(data[pos + i])
	}
	if length < 128 || length > u64(data.len - pos - 1 - n) {
		return error('x509: invalid DER length')
	}
	return 1 + n, int(length)
}

// parse_name reads every attribute of every multi-valued RDN SET.
fn parse_name(body []u8) !map[string]string {
	mut out := map[string]string{}
	mut pos := 0
	for pos < body.len {
		inner := expect_tag(body, pos, tag_set, 'RDN SET')!
		pos = skip_tlv(body, pos)!
		if inner.len == 0 {
			return error('x509: empty RDN SET')
		}
		mut attr_pos := 0
		for attr_pos < inner.len {
			seq := expect_tag(inner, attr_pos, tag_seq, 'attribute')!
			attr_pos = skip_tlv(inner, attr_pos)!
			oid := oid_string(expect_tag(seq, 0, tag_oid, 'attribute OID')!)!
			value_pos := skip_tlv(seq, 0)!
			val_tlv, after_value := split_tlv(seq, value_pos)!
			if after_value != seq.len {
				return error('x509: trailing attribute data')
			}
			value, decoded := der_string(val_tlv)!
			if decoded {
				out[oid] = value
			}
		}
	}
	return out
}

// der_string decodes common directory string types, skipping unsupported tags.
fn der_string(tlv []u8) !(string, bool) {
	tag := tlv[0]
	content := expect_tag(tlv, 0, tag, 'string')!
	if tag == 0x1E { // BMPString is big-endian UCS-2, not raw UTF-8.
		if content.len % 2 != 0 {
			return error('x509: invalid BMPString')
		}
		mut out := ''
		for i := 0; i < content.len; i += 2 {
			value := u16(content[i]) << 8 | u16(content[i + 1])
			if value >= 0xd800 && value <= 0xdfff {
				return error('x509: invalid BMPString surrogate')
			}
			out += rune(value).str()
		}
		return out, true
	}
	if tag != tag_printable && tag != tag_utf8string && tag != tag_ia5 {
		return '', false
	}
	value := content.bytestr()
	if tag == tag_utf8string && !utf8.validate_str(value) {
		return error('x509: invalid UTF8String')
	}
	if tag == tag_ia5 || tag == tag_printable {
		for c in content {
			if c > 127 {
				return error('x509: invalid ASCII string')
			}
			if tag == tag_printable && !(c >= `a` && c <= `z`) && !(c >= `A` && c <= `Z`)
				&& !(c >= `0` && c <= `9`) && !" '()+,-./:=?".contains_u8(c) {
				return error('x509: invalid PrintableString')
			}
		}
	}
	return value, true
}

// oid_string decodes checked base-128 subidentifiers, including the first pair.
fn oid_string(bytes []u8) !string {
	if bytes.len == 0 {
		return error('x509: empty OID')
	}
	mut parts := []string{}
	mut value := u64(0)
	mut beginning := true
	for b in bytes {
		if beginning && b == 0x80 {
			return error('x509: nonminimal OID arc')
		}
		if value > (u64(0xffffffffffffffff) - u64(b & 0x7f)) / 128 {
			return error('x509: OID arc overflow')
		}
		value = value * 128 + u64(b & 0x7f)
		beginning = b & 0x80 == 0
		if beginning {
			if parts.len == 0 {
				if value < 40 {
					parts << '0'
					parts << value.str()
				} else if value < 80 {
					parts << '1'
					parts << (value - 40).str()
				} else {
					parts << '2'
					parts << (value - 80).str()
				}
			} else {
				parts << value.str()
			}
			value = 0
		}
	}
	if !beginning {
		return error('x509: truncated OID')
	}
	return parts.join('.')
}

fn parse_validity(body []u8) !(time.Time, time.Time) {
	first, after_first := split_tlv(body, 0)!
	second, after_second := split_tlv(body, after_first)!
	if after_second != body.len {
		return error('x509: trailing validity data')
	}
	return parse_time(first)!, parse_time(second)!
}

// parse_time accepts the exact X.509 Zulu time shape and valid calendar fields.
fn parse_time(tlv []u8) !time.Time {
	if tlv.len < 2 {
		return error('x509: bad time')
	}
	tag := tlv[0]
	if tag != tag_utctime && tag != tag_gentime {
		return error('x509: unsupported time tag ${tag}')
	}
	s := expect_tag(tlv, 0, tag, 'time')!.bytestr()
	expected_len := if tag == tag_utctime { 13 } else { 15 }
	if s.len != expected_len || s[s.len - 1] != `Z` || skip_tlv(tlv, 0)! != tlv.len {
		return error('x509: bad time `${s}`')
	}
	for c in s[..s.len - 1].bytes() {
		if c < `0` || c > `9` {
			return error('x509: bad time `${s}`')
		}
	}
	year_len := if tag == tag_utctime { 2 } else { 4 }
	mut year := s[..year_len].int()
	if tag == tag_utctime {
		year += if year < 50 { 2000 } else { 1900 }
	}
	month := s[year_len..year_len + 2].int()
	day := s[year_len + 2..year_len + 4].int()
	hour := s[year_len + 4..year_len + 6].int()
	minute := s[year_len + 6..year_len + 8].int()
	second := s[year_len + 8..year_len + 10].int()
	if year == 0 || month < 1 || month > 12 || day < 1 || hour > 23 || minute > 59 || second > 59 {
		return error('x509: bad time `${s}`')
	}
	if day > time.days_in_month(month, year)! {
		return error('x509: bad time `${s}`')
	}
	return time.new(time.Time{ year: year, month: month, day: day, hour: hour, minute: minute, second: second })
}

// parse_extensions lives in extensions.v: it enforces unknown-critical
// rejection and fills ParsedExtensions for chain validation.
