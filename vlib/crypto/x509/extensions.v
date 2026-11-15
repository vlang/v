module x509

// Extension OIDs enforced or collected during parsing. Unknown
// critical extensions are rejected (fail-closed); unknown
// non-critical extensions are skipped.
pub const oid_basic_constraints = '2.5.29.19'
pub const oid_key_usage = '2.5.29.15'
pub const oid_ext_key_usage = '2.5.29.37'
pub const oid_name_constraints = '2.5.29.30'
pub const oid_certificate_policies = '2.5.29.32'
pub const oid_policy_constraints = '2.5.29.36'
pub const oid_subject_key_id = '2.5.29.14'
pub const oid_authority_key_id = '2.5.29.35'
pub const oid_crl_distribution_points = '2.5.29.31'
pub const oid_authority_info_access = '1.3.6.1.5.5.1.1'

// EKU purpose OIDs.
pub const oid_eku_server_auth = '1.3.6.1.5.5.7.3.1'
pub const oid_eku_client_auth = '1.3.6.1.5.5.7.3.2'
pub const oid_eku_any = '2.5.29.37.0'

// AIA access-method OIDs.
const oid_ad_ocsp = '1.3.6.1.5.5.7.48.1'

// Key-usage bit positions (RFC 5280 §4.2.1.3), LSB-indexed.
pub const ku_digital_signature = u16(1 << 0)
pub const ku_non_repudiation = u16(1 << 1)
pub const ku_key_encipherment = u16(1 << 2)
pub const ku_data_encipherment = u16(1 << 3)
pub const ku_key_agreement = u16(1 << 4)
pub const ku_key_cert_sign = u16(1 << 5)
pub const ku_crl_sign = u16(1 << 6)
pub const ku_encipher_only = u16(1 << 7)
pub const ku_decipher_only = u16(1 << 8)

// Length/arity guards for every new parse loop.
const max_extensions = 64
const max_san_names = 256
const max_eku_oids = 32
const max_policy_oids = 32
const max_nc_subtrees = 64
const max_distribution_points = 32
const max_access_descriptions = 32
const max_key_id_len = 64

// ParsedExtensions is the full extension view of one certificate.
struct ParsedExtensions {
mut:
	dns_names               []string
	ip_addresses            [][]u8
	has_san                 bool
	key_usage               u16
	has_key_usage           bool
	ext_key_usage           []string
	is_ca                   bool
	has_basic_constraints   bool
	path_len                int
	has_path_len            bool
	permitted_dns           []string
	permitted_ips           [][]u8
	excluded_dns            []string
	excluded_ips            [][]u8
	has_name_constraints    bool
	policies                []string
	has_policies            bool
	has_policy_constraints  bool
	authority_key_id        []u8
	subject_key_id          []u8
	crl_distribution_points []string
	ocsp_urls               []string
}

// parse_extensions reads every extension, rejects duplicates, enforces
// unknown-critical rejection, and fills ParsedExtensions. Every loop is
// length-guarded by the input bounds plus an arity cap.
fn parse_extensions(body []u8) !ParsedExtensions {
	mut out := ParsedExtensions{}
	mut seen := map[string]bool{}
	mut pos := 0
	mut count := 0
	for pos < body.len {
		if count >= max_extensions {
			return error('x509: too many extensions')
		}
		count++
		inner := expect_tag(body, pos, tag_seq, 'extension')!
		pos = skip_tlv(body, pos)!
		oid := oid_string(expect_tag(inner, 0, tag_oid, 'extension OID')!)!
		if oid in seen {
			return error('x509: duplicate extension `${oid}`')
		}
		seen[oid] = true
		mut ip := skip_tlv(inner, 0)!
		mut critical := false
		if ip < inner.len && inner[ip] == 0x01 {
			crit := expect_tag(inner, ip, 0x01, 'critical BOOLEAN')!
			if crit.len != 1 || crit[0] != 0xff {
				return error('x509: invalid critical BOOLEAN')
			}
			critical = true
			ip = skip_tlv(inner, ip)!
		}
		octets := expect_tag(inner, ip, tag_octet_string, 'extension value')!
		if skip_tlv(inner, ip)! != inner.len {
			return error('x509: trailing extension data')
		}
		match oid {
			oid_san {
				out.dns_names, out.ip_addresses = parse_san(octets)!
			}
			oid_basic_constraints {
				out.is_ca, out.path_len, out.has_path_len = parse_basic_constraints(octets)!
				out.has_basic_constraints = true
			}
			oid_key_usage {
				out.key_usage = parse_key_usage(octets)!
				out.has_key_usage = true
			}
			oid_ext_key_usage {
				out.ext_key_usage = parse_eku(octets)!
			}
			oid_name_constraints {
				parse_name_constraints(octets, mut out)!
				out.has_name_constraints = true
			}
			oid_certificate_policies {
				out.policies = parse_certificate_policies(octets)!
				out.has_policies = true
			}
			oid_policy_constraints {
				parse_policy_constraints(octets)!
				out.has_policy_constraints = true
			}
			oid_subject_key_id {
				out.subject_key_id = parse_key_identifier(octets)!
			}
			oid_authority_key_id {
				out.authority_key_id = parse_authority_key_id(octets)!
			}
			oid_crl_distribution_points {
				out.crl_distribution_points = parse_crl_distribution_points(octets)!
			}
			oid_authority_info_access {
				out.ocsp_urls = parse_authority_info_access(octets)!
			}
			else {
				if critical {
					return error('x509: unsupported critical extension `${oid}`')
				}
			}
		}
	}
	out.has_san = oid_san in seen
	return out
}

// parse_san collects DNS names and raw IP addresses, tracking SAN
// presence to avoid CN fallback. Unknown GeneralName tags are rejected.
fn parse_san(body []u8) !([]string, [][]u8) {
	names_body := expect_tag(body, 0, tag_seq, 'SAN list')!
	if skip_tlv(body, 0)! != body.len || names_body.len == 0 {
		return error('x509: invalid SAN list')
	}
	mut dns := []string{}
	mut ips := [][]u8{}
	mut pos := 0
	for pos < names_body.len {
		if dns.len + ips.len >= max_san_names {
			return error('x509: too many SAN names')
		}
		gn, next := split_tlv(names_body, pos)!
		pos = next
		if gn[0] !in [u8(0xa0), 0x81, 0x82, 0xa3, 0xa4, 0xa5, 0x86, 0x87, 0x88] {
			return error('x509: invalid GeneralName tag')
		}
		if gn[0] == tag_dnsname {
			content := expect_tag(gn, 0, tag_dnsname, 'dNSName')!
			name := content.bytestr()
			if !valid_dns_name(name, true) {
				return error('x509: invalid dNSName')
			}
			dns << name
		} else if gn[0] == 0x87 {
			content := expect_tag(gn, 0, 0x87, 'iPAddress')!
			if content.len != 4 && content.len != 16 {
				return error('x509: invalid iPAddress')
			}
			ips << content.clone()
		}
	}
	return dns, ips
}

// parse_basic_constraints reads cA and optional pathLenConstraint.
fn parse_basic_constraints(body []u8) !(bool, int, bool) {
	seq := expect_tag(body, 0, tag_seq, 'basicConstraints')!
	if skip_tlv(body, 0)! != body.len {
		return error('x509: trailing basicConstraints data')
	}
	if seq.len == 0 {
		return false, 0, false
	}
	mut pos := 0
	mut is_ca := false
	mut path_len := 0
	mut has_path := false
	if pos < seq.len && seq[pos] == 0x01 {
		flag := expect_tag(seq, pos, 0x01, 'cA BOOLEAN')!
		if flag.len != 1 {
			return error('x509: invalid cA BOOLEAN')
		}
		is_ca = flag[0] == 0xff
		if flag[0] != 0x00 && flag[0] != 0xff {
			return error('x509: invalid cA BOOLEAN value')
		}
		pos = skip_tlv(seq, pos)!
	}
	if pos < seq.len {
		raw := expect_tag(seq, pos, tag_integer, 'pathLenConstraint')!
		pos = skip_tlv(seq, pos)!
		if raw.len == 0 || raw.len > 4 || (raw.len > 1 && raw[0] == 0) {
			return error('x509: invalid pathLenConstraint')
		}
		if raw[0] & 0x80 != 0 {
			return error('x509: negative pathLenConstraint')
		}
		for b in raw {
			path_len = path_len * 256 + int(b)
		}
		has_path = true
	}
	if pos != seq.len {
		return error('x509: trailing basicConstraints data')
	}
	return is_ca, path_len, has_path
}

// parse_key_usage decodes up to 9 usage bits into an LSB-indexed mask.
fn parse_key_usage(body []u8) !u16 {
	bs := expect_tag(body, 0, tag_bitstring, 'keyUsage')!
	if skip_tlv(body, 0)! != body.len {
		return error('x509: trailing keyUsage data')
	}
	if bs.len < 1 || bs.len > 3 || bs[0] > 7 {
		return error('x509: invalid keyUsage BIT STRING')
	}
	if bs.len == 1 {
		if bs[0] != 0 {
			return error('x509: invalid keyUsage padding')
		}
		return u16(0)
	}
	content := bs[1..]
	nbits := content.len * 8 - int(bs[0])
	if nbits <= 0 || nbits > 9 {
		return error('x509: invalid keyUsage length')
	}
	if bs[0] > 0 && content.last() & u8((1 << bs[0]) - 1) != 0 {
		return error('x509: nonzero keyUsage padding')
	}
	mut mask := u16(0)
	for i in 0 .. nbits {
		if content[i / 8] & u8(0x80 >> u8(i % 8)) != 0 {
			mask |= u16(1 << u16(i))
		}
	}
	return mask
}

// parse_eku collects extended-key-usage purpose OIDs.
fn parse_eku(body []u8) ![]string {
	seq := expect_tag(body, 0, tag_seq, 'extKeyUsage')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid extKeyUsage')
	}
	mut out := []string{}
	mut pos := 0
	for pos < seq.len {
		if out.len >= max_eku_oids {
			return error('x509: too many EKU purposes')
		}
		out << oid_string(expect_tag(seq, pos, tag_oid, 'EKU purpose')!)!
		pos = skip_tlv(seq, pos)!
	}
	return out
}

// parse_name_constraints fills permitted/excluded dNSName and iP subtrees.
// Other GeneralName bases cannot be evaluated, so they fail closed.
// minimum must be absent/zero and maximum absent.
fn parse_name_constraints(body []u8, mut out ParsedExtensions) ! {
	seq := expect_tag(body, 0, tag_seq, 'nameConstraints')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid nameConstraints')
	}
	mut pos := 0
	mut seen_permitted := false
	mut seen_excluded := false
	for pos < seq.len {
		tag := seq[pos]
		if tag != 0xa0 && tag != 0xa1 {
			return error('x509: invalid nameConstraints tag')
		}
		if tag == 0xa0 && seen_permitted {
			return error('x509: duplicate permittedSubtrees')
		}
		if tag == 0xa1 && seen_excluded {
			return error('x509: duplicate excludedSubtrees')
		}
		if tag == 0xa0 {
			seen_permitted = true
		} else {
			seen_excluded = true
		}
		outer := expect_tag(seq, pos, tag, 'subtrees')!
		pos = skip_tlv(seq, pos)!
		trees := expect_tag(outer, 0, tag_seq, 'GeneralSubtrees')!
		if skip_tlv(outer, 0)! != outer.len || trees.len == 0 {
			return error('x509: invalid GeneralSubtrees')
		}
		mut tpos := 0
		for tpos < trees.len {
			dns, ip := parse_general_subtree(trees, tpos)!
			tpos = skip_tlv(trees, tpos)!
			if tag == 0xa0 {
				if out.permitted_dns.len + out.permitted_ips.len >= max_nc_subtrees {
					return error('x509: too many name-constraint subtrees')
				}
				if d := dns {
					out.permitted_dns << d
				} else {
					out.permitted_ips << ip
				}
			} else {
				if out.excluded_dns.len + out.excluded_ips.len >= max_nc_subtrees {
					return error('x509: too many name-constraint subtrees')
				}
				if d := dns {
					out.excluded_dns << d
				} else {
					out.excluded_ips << ip
				}
			}
		}
	}
}

// parse_general_subtree reads one GeneralSubtree, returning either a DNS
// constraint string or a raw IP constraint (address++mask).
fn parse_general_subtree(trees []u8, pos int) !(?string, []u8) {
	sub := expect_tag(trees, pos, tag_seq, 'GeneralSubtree')!
	mut spos := 0
	gn, after_gn := split_tlv(sub, spos)!
	spos = after_gn
	if gn[0] == tag_dnsname {
		content := expect_tag(gn, 0, tag_dnsname, 'constraint dNSName')!
		name := content.bytestr()
		if !valid_dns_name(name, true) {
			return error('x509: invalid constraint dNSName')
		}
		check_subtree_bounds(sub, spos)!
		return name, []u8{}
	}
	if gn[0] == 0x87 {
		content := expect_tag(gn, 0, 0x87, 'constraint iPAddress')!
		if content.len != 8 && content.len != 32 {
			return error('x509: invalid constraint iPAddress')
		}
		check_subtree_bounds(sub, spos)!
		return none, content.clone()
	}
	return error('x509: unsupported name-constraint base')
}

// check_subtree_bounds requires minimum absent/zero and maximum absent.
fn check_subtree_bounds(sub []u8, pos int) ! {
	mut p := pos
	for p < sub.len {
		tag := sub[p]
		if tag != 0x80 && tag != 0x81 {
			return error('x509: invalid GeneralSubtree field')
		}
		val := expect_tag(sub, p, tag, 'subtree bound')!
		if tag == 0x80 {
			if val.len != 1 || val[0] != 0 {
				return error('x509: unsupported subtree minimum')
			}
		} else {
			return error('x509: unsupported subtree maximum')
		}
		p = skip_tlv(sub, p)!
	}
}

// parse_certificate_policies collects policy OIDs (anyPolicy passthrough:
// parsed but not enforced, see README).
fn parse_certificate_policies(body []u8) ![]string {
	seq := expect_tag(body, 0, tag_seq, 'certificatePolicies')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid certificatePolicies')
	}
	mut out := []string{}
	mut pos := 0
	for pos < seq.len {
		if out.len >= max_policy_oids {
			return error('x509: too many policies')
		}
		pi := expect_tag(seq, pos, tag_seq, 'PolicyInformation')!
		pos = skip_tlv(seq, pos)!
		out << oid_string(expect_tag(pi, 0, tag_oid, 'policyIdentifier')!)!
		// policyQualifiers, if present, are skipped without interpretation.
		mut qpos := skip_tlv(pi, 0)!
		for qpos < pi.len {
			qpos = skip_tlv(pi, qpos)!
		}
	}
	return out
}

// parse_policy_constraints validates structure only (passthrough, see README).
fn parse_policy_constraints(body []u8) ! {
	seq := expect_tag(body, 0, tag_seq, 'policyConstraints')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid policyConstraints')
	}
	mut pos := 0
	for pos < seq.len {
		tag := seq[pos]
		if tag != 0x80 && tag != 0x81 {
			return error('x509: invalid policyConstraints field')
		}
		val := expect_tag(seq, pos, tag, 'policyConstraints value')!
		if val.len == 0 || val.len > 4 {
			return error('x509: invalid policyConstraints value')
		}
		pos = skip_tlv(seq, pos)!
	}
}

// parse_key_identifier reads a KeyIdentifier value: the extension value
// holds the DER-encoded KeyIdentifier OCTET STRING (RFC 5280 §4.2.1.2).
fn parse_key_identifier(body []u8) ![]u8 {
	id := expect_tag(body, 0, tag_octet_string, 'KeyIdentifier')!
	if skip_tlv(body, 0)! != body.len || id.len == 0 || id.len > max_key_id_len {
		return error('x509: invalid KeyIdentifier')
	}
	return id.clone()
}

// parse_authority_key_id extracts keyIdentifier, skipping the other
// optional fields after structural validation.
fn parse_authority_key_id(body []u8) ![]u8 {
	seq := expect_tag(body, 0, tag_seq, 'authorityKeyIdentifier')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid authorityKeyIdentifier')
	}
	mut pos := 0
	mut kid := []u8{}
	for pos < seq.len {
		tag := seq[pos]
		if tag != 0x80 && tag != 0xa1 && tag != 0x82 {
			return error('x509: invalid authorityKeyIdentifier field')
		}
		val := expect_tag(seq, pos, tag, 'authorityKeyIdentifier field')!
		pos = skip_tlv(seq, pos)!
		if tag == 0x80 {
			if kid.len > 0 || val.len == 0 || val.len > max_key_id_len {
				return error('x509: invalid authority KeyIdentifier')
			}
			kid = val.clone()
		}
	}
	if kid.len == 0 {
		return error('x509: missing authority KeyIdentifier')
	}
	return kid
}

// parse_crl_distribution_points collects fullName URIs from every
// DistributionPoint, skipping reasons/cRLIssuer fields.
fn parse_crl_distribution_points(body []u8) ![]string {
	seq := expect_tag(body, 0, tag_seq, 'cRLDistributionPoints')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid cRLDistributionPoints')
	}
	mut out := []string{}
	mut pos := 0
	for pos < seq.len {
		if out.len >= max_distribution_points {
			return error('x509: too many distribution points')
		}
		dp := expect_tag(seq, pos, tag_seq, 'DistributionPoint')!
		pos = skip_tlv(seq, pos)!
		mut dpos := 0
		for dpos < dp.len {
			tag := dp[dpos]
			val := expect_tag(dp, dpos, tag, 'DistributionPoint field')!
			dpos = skip_tlv(dp, dpos)!
			if tag == 0xa0 {
				// distributionPoint [0]: only fullName [0] URIs are collected.
				if val.len > 0 && val[0] == 0x80 {
					names := expect_tag(val, 0, 0x80, 'fullName')!
					mut npos := 0
					for npos < names.len {
						ntag := names[npos]
						nval := expect_tag(names, npos, ntag, 'DistributionPoint name')!
						npos = skip_tlv(names, npos)!
						if ntag == 0x86 {
							uri := nval.bytestr()
							if uri.len == 0 || uri.len > 1024 {
								return error('x509: invalid distributionPoint URI')
							}
							out << uri
						}
					}
				}
			}
		}
	}
	return out
}

// parse_authority_info_access collects OCSP (method 1.3.6.1.5.5.7.48.1)
// accessLocation URIs, skipping all other methods.
fn parse_authority_info_access(body []u8) ![]string {
	seq := expect_tag(body, 0, tag_seq, 'authorityInfoAccess')!
	if skip_tlv(body, 0)! != body.len || seq.len == 0 {
		return error('x509: invalid authorityInfoAccess')
	}
	mut out := []string{}
	mut pos := 0
	for pos < seq.len {
		if out.len >= max_access_descriptions {
			return error('x509: too many access descriptions')
		}
		ad := expect_tag(seq, pos, tag_seq, 'AccessDescription')!
		pos = skip_tlv(seq, pos)!
		method := oid_string(expect_tag(ad, 0, tag_oid, 'accessMethod')!)!
		lpos := skip_tlv(ad, 0)!
		gn, _ := split_tlv(ad, lpos)!
		if skip_tlv(ad, lpos)! != ad.len {
			return error('x509: trailing AccessDescription data')
		}
		if method == oid_ad_ocsp && gn[0] == 0x86 {
			uri := expect_tag(gn, 0, 0x86, 'OCSP URI')!.bytestr()
			if uri.len == 0 || uri.len > 1024 {
				return error('x509: invalid OCSP URI')
			}
			out << uri
		}
	}
	return out
}

// dns_constraint_matches reports whether host satisfies a dNSName
// constraint: exact match or any subdomain below it (RFC 5280 §4.2.1.10).
fn dns_constraint_matches(constraint string, host string) bool {
	c := constraint.to_lower().trim_right('.')
	h := host.to_lower().trim_right('.')
	if h == c {
		return true
	}
	return h.ends_with('.' + c)
}

// ip_in_range reports whether ip (4 or 16 raw bytes) falls in the
// constraint range (address++mask, 8 or 32 raw bytes).
fn ip_in_range(ip []u8, constraint []u8) bool {
	if constraint.len != 8 && constraint.len != 32 {
		return false
	}
	half := constraint.len / 2
	addr := constraint[..half]
	mask := constraint[half..]
	if ip.len != half {
		return false
	}
	for i in 0 .. half {
		if (ip[i] ^ addr[i]) & mask[i] != 0 {
			return false
		}
	}
	return true
}

// serial_equal compares DER INTEGER serial contents, tolerating one
// optional leading zero sign byte on either side.
fn serial_equal(a []u8, b []u8) bool {
	na := if a.len > 1 && a[0] == 0 && a[1] & 0x80 != 0 { a[1..] } else { a }
	nb := if b.len > 1 && b[0] == 0 && b[1] & 0x80 != 0 { b[1..] } else { b }
	if na.len != nb.len {
		return false
	}
	for i in 0 .. na.len {
		if na[i] != nb[i] {
			return false
		}
	}
	return true
}
