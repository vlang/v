module x509

import crypto.sha1
import crypto.sha256
import time

// ocsp.v implements OCSP stapling verification (RFC 6960) on opaque
// response bytes: the TLS transport supplies the staple (another agent
// owns the status_request wire); this module parses the response,
// verifies the signature and freshness window, and checks certStatus.
// Only the CA-as-responder model is supported: responderID must name
// the issuer and the signature must verify under the issuer key.

// BasicOCSPResponse responseType OID (id-pkix-ocsp-basic, RFC 6960).
pub const oid_ocsp_basic = '1.3.6.1.5.5.7.48.1.1'
// OCSP certID hash algorithm OIDs.
const oid_hash_sha1 = '1.3.14.3.2.26'
const oid_hash_sha256 = '2.16.840.1.101.3.4.2.1'

// ocsp_skew_sec bounds clock skew for producedAt/thisUpdate/nextUpdate.
const ocsp_skew_sec = i64(900)

// max_ocsp_responses caps SingleResponse entries per response.
const max_ocsp_responses = 64

// OcspCertStatus mirrors the certStatus CHOICE.
pub enum OcspCertStatus {
	good
	revoked
	unknown
}

// OcspSingle is one verified SingleResponse entry.
pub struct OcspSingle {
pub:
	serial           []u8
	status           OcspCertStatus
	this_update      time.Time
	next_update      time.Time
	has_next_update  bool
	revoked_at       time.Time
	hash_oid         string
	issuer_name_hash []u8
	issuer_key_hash  []u8
}

// OcspResponse is a parsed OCSPResponse (basic response only).
pub struct OcspResponse {
pub:
	raw           []u8
	produced_at   time.Time
	responder_der []u8
	sig_alg_oid   string
	sig_alg_val   []u8
	signature     []u8
	tbs_der       []u8
	responses     []OcspSingle
}

// OcspGenOptions tunes make_ocsp_response (V-generated fixtures/tests).
pub struct OcspGenOptions {
pub:
	produced_at     time.Time
	this_update     time.Time
	next_update     time.Time
	has_next_update bool           = true
	status          OcspCertStatus = .good
	revoked_at      time.Time
}

// parse_ocsp_response parses one DER OCSPResponse, requiring
// responseStatus successful and the id-pkix-ocsp-basic response type.
pub fn parse_ocsp_response(der []u8) !OcspResponse {
	if der.len > 65536 {
		return error('x509: OCSP response too large')
	}
	body := expect_tag(der, 0, tag_seq, 'OCSPResponse')!
	if tlv_total_len(der, 0)! != der.len {
		return error('x509: trailing OCSP data')
	}
	status_raw := expect_tag(body, 0, 0x0a, 'responseStatus')!
	mut pos := skip_tlv(body, 0)!
	if status_raw.len != 1 || status_raw[0] != 0 {
		return error('x509: OCSP response not successful')
	}
	outer := expect_tag(body, pos, 0xa0, 'responseBytes')!
	if skip_tlv(body, pos)! != body.len {
		return error('x509: trailing OCSPResponse data')
	}
	inner := expect_tag(outer, 0, tag_seq, 'ResponseBytes')!
	if skip_tlv(outer, 0)! != outer.len {
		return error('x509: trailing ResponseBytes data')
	}
	oid := oid_string(expect_tag(inner, 0, tag_oid, 'responseType')!)!
	if oid != oid_ocsp_basic {
		return error('x509: unsupported OCSP response type')
	}
	vpos := skip_tlv(inner, 0)!
	value := expect_tag(inner, vpos, tag_octet_string, 'responseValue')!
	if skip_tlv(inner, vpos)! != inner.len {
		return error('x509: trailing ResponseBytes data')
	}
	return parse_basic_ocsp_response(value, der)
}

// parse_basic_ocsp_response parses the BasicOCSPResponse bytes.
fn parse_basic_ocsp_response(basic []u8, full_der []u8) !OcspResponse {
	body := expect_tag(basic, 0, tag_seq, 'BasicOCSPResponse')!
	if tlv_total_len(basic, 0)! != basic.len {
		return error('x509: trailing BasicOCSPResponse data')
	}
	tbs := expect_tag(body, 0, tag_seq, 'tbsResponseData')!
	tbs_pos := skip_tlv(body, 0)!
	sig_alg := expect_tag(body, tbs_pos, tag_seq, 'OCSP signatureAlgorithm')!
	parse_algorithm(sig_alg)!
	sig_pos := skip_tlv(body, tbs_pos)!
	sig := expect_tag(body, sig_pos, tag_bitstring, 'OCSP signature')!
	validate_bitstring(sig)!
	if sig.len < 2 || sig[0] != 0 {
		return error('x509: invalid OCSP signature encoding')
	}
	sig_end := skip_tlv(body, sig_pos)!
	if sig_end < body.len {
		// certs [0] EXPLICIT: responder chain, skipped (the issuer key
		// authenticates the staple, not bundled certificates).
		if body[sig_end] != 0xa0 {
			return error('x509: unexpected BasicOCSPResponse field')
		}
		if skip_tlv(body, sig_end)! != body.len {
			return error('x509: trailing BasicOCSPResponse data')
		}
	}
	tbs_full, _ := split_tlv(body, 0)!
	sig_oid := oid_string(expect_tag(sig_alg, 0, tag_oid, 'algorithm OID')!)!
	produced_at, responder_der, responses := parse_tbs_response_data(tbs)!
	return OcspResponse{
		raw:           full_der.clone()
		produced_at:   produced_at
		responder_der: responder_der
		sig_alg_oid:   sig_oid
		sig_alg_val:   sig_alg.clone()
		signature:     sig[1..].clone()
		tbs_der:       tbs_full.clone()
		responses:     responses
	}
}

// parse_tbs_response_data reads responderID (byName only), producedAt
// and the SingleResponse list.
fn parse_tbs_response_data(tbs []u8) !(time.Time, []u8, []OcspSingle) {
	mut pos := 0
	if pos < tbs.len && tbs[pos] == 0xa0 {
		// version [0] EXPLICIT; must be v1 when present.
		ver_outer := expect_tag(tbs, pos, 0xa0, 'OCSP version')!
		pos = skip_tlv(tbs, pos)!
		ver := expect_tag(ver_outer, 0, tag_integer, 'OCSP version number')!
		if skip_tlv(ver_outer, 0)! != ver_outer.len || ver.len != 1 || ver[0] != 0 {
			return error('x509: unsupported OCSP version')
		}
	}
	if pos >= tbs.len {
		return error('x509: truncated tbsResponseData')
	}
	rid, after_rid := split_tlv(tbs, pos)!
	pos = after_rid
	if rid[0] != 0xa1 {
		return error('x509: unsupported OCSP responderID (need byName)')
	}
	responder_der := expect_tag(rid, 0, 0xa1, 'responder byName')!.clone()
	pat, _ := split_tlv(tbs, pos)!
	produced_at := parse_time(pat)!
	pos = skip_tlv(tbs, pos)!
	resp_seq := expect_tag(tbs, pos, tag_seq, 'OCSP responses')!
	pos = skip_tlv(tbs, pos)!
	if pos < tbs.len {
		// responseExtensions [1] EXPLICIT, skipped without interpretation.
		if tbs[pos] != 0xa1 {
			return error('x509: unexpected tbsResponseData field')
		}
		pos = skip_tlv(tbs, pos)!
	}
	if pos != tbs.len {
		return error('x509: trailing tbsResponseData data')
	}
	mut responses := []OcspSingle{}
	mut rpos := 0
	for rpos < resp_seq.len {
		if responses.len >= max_ocsp_responses {
			return error('x509: too many OCSP responses')
		}
		single := expect_tag(resp_seq, rpos, tag_seq, 'SingleResponse')!
		rpos = skip_tlv(resp_seq, rpos)!
		responses << parse_single_response(single)!
	}
	return produced_at, responder_der, responses
}

// parse_single_response reads one SingleResponse entry.
fn parse_single_response(single []u8) !OcspSingle {
	mut pos := 0
	cert_id := expect_tag(single, pos, tag_seq, 'certID')!
	pos = skip_tlv(single, pos)!
	hash_oid, name_hash, key_hash, serial := parse_cert_id(cert_id)!
	st, after_st := split_tlv(single, pos)!
	pos = after_st
	mut status := OcspCertStatus.unknown
	mut revoked_at := time.Time{}
	if st[0] == 0x80 {
		if st.len != 2 {
			return error('x509: invalid OCSP good status')
		}
		status = .good
	} else if st[0] == 0x82 {
		if st.len != 2 {
			return error('x509: invalid OCSP unknown status')
		}
		status = .unknown
	} else if st[0] == 0xa1 {
		status = .revoked
		rest := st[2..]
		rev_tlv, after_rev := split_tlv(rest, 0)!
		revoked_at = parse_time(rev_tlv)!
		if after_rev < rest.len {
			// Optional revocationReason [0] is the only allowed trailer.
			reason := expect_tag(rest, after_rev, 0x80, 'revocationReason')!
			if skip_tlv(rest, after_rev)! != rest.len || reason.len != 1 {
				return error('x509: invalid revocationReason')
			}
		}
	} else {
		return error('x509: invalid OCSP certStatus')
	}
	tat, _ := split_tlv(single, pos)!
	this_update := parse_time(tat)!
	pos = skip_tlv(single, pos)!
	mut next_update := time.Time{}
	mut has_next := false
	for pos < single.len {
		tag := single[pos]
		if tag == 0xa0 {
			inner := expect_tag(single, pos, 0xa0, 'nextUpdate')!
			nt, _ := split_tlv(inner, 0)!
			next_update = parse_time(nt)!
			if skip_tlv(inner, 0)! != inner.len {
				return error('x509: trailing nextUpdate data')
			}
			has_next = true
			pos = skip_tlv(single, pos)!
		} else if tag == 0xa1 {
			// singleExtensions: structural skip only.
			pos = skip_tlv(single, pos)!
		} else {
			return error('x509: unexpected SingleResponse field')
		}
	}
	return OcspSingle{
		serial:           serial
		status:           status
		this_update:      this_update
		next_update:      next_update
		has_next_update:  has_next
		revoked_at:       revoked_at
		hash_oid:         hash_oid
		issuer_name_hash: name_hash
		issuer_key_hash:  key_hash
	}
}

// parse_cert_id reads a CertID, returning the hash OID, issuer hashes
// and serial number.
fn parse_cert_id(cert_id []u8) !(string, []u8, []u8, []u8) {
	alg := expect_tag(cert_id, 0, tag_seq, 'certID hashAlgorithm')!
	mut pos := skip_tlv(cert_id, 0)!
	parse_algorithm(alg)!
	hash_oid := oid_string(expect_tag(alg, 0, tag_oid, 'hash OID')!)!
	if hash_oid != oid_hash_sha1 && hash_oid != oid_hash_sha256 {
		return error('x509: unsupported OCSP hash algorithm')
	}
	name_hash := expect_tag(cert_id, pos, tag_octet_string, 'issuerNameHash')!.clone()
	pos = skip_tlv(cert_id, pos)!
	key_hash := expect_tag(cert_id, pos, tag_octet_string, 'issuerKeyHash')!.clone()
	pos = skip_tlv(cert_id, pos)!
	serial := expect_tag(cert_id, pos, tag_integer, 'certID serial')!.clone()
	pos = skip_tlv(cert_id, pos)!
	if pos != cert_id.len || serial.len == 0 || serial.len > 20 {
		return error('x509: invalid certID')
	}
	want := if hash_oid == oid_hash_sha1 { 20 } else { 32 }
	if name_hash.len != want || key_hash.len != want {
		return error('x509: invalid OCSP issuer hash length')
	}
	return hash_oid, name_hash, key_hash, serial
}

// spki_contents extracts the BIT STRING value (minus pad byte) of a
// certificate's subjectPublicKeyInfo for issuerKeyHash computation.
fn spki_contents(cert_der []u8) ![]u8 {
	cert_tlv, _ := split_tlv(cert_der, 0)!
	hl, _ := der_length(cert_der, 1)!
	tbs_full := cert_tlv[1 + hl..]
	tbs := expect_tag(tbs_full, 0, tag_seq, 'tbsCertificate')!
	mut pos := 0
	if pos < tbs.len && tbs[pos] == tag_ctx0 {
		pos = skip_tlv(tbs, pos)!
	}
	for _ in 0 .. 5 {
		pos = skip_tlv(tbs, pos)!
	}
	spki := expect_tag(tbs[pos..], 0, tag_seq, 'subjectPublicKeyInfo')!
	mut sp := 0
	sp += tlv_total_len(spki, sp)!
	bs := expect_tag(spki[sp..], 0, tag_bitstring, 'subjectPublicKey')!
	if bs.len < 1 || bs[0] != 0 {
		return error('x509: bad SPKI bit string padding')
	}
	return bs[1..].clone()
}

// der_gentime encodes t as a DER GeneralizedTime (YYYYMMDDHHMMSSZ),
// normalizing to UTC like der_utc.
fn der_gentime(t time.Time) []u8 {
	u := t.local_to_utc()
	mut ss := u.year.str()
	if ss.len < 4 {
		ss = '0000'[..4 - ss.len] + ss
	}
	ss += two(u.month)
	ss += two(u.day)
	ss += two(u.hour)
	ss += two(u.minute)
	ss += two(u.second)
	ss += 'Z'
	return der_tlv(tag_gentime, ss.bytes())
}

// cert_id_for builds a SHA-256 CertID for leaf under issuer.
fn cert_id_for(leaf &Certificate, issuer &Certificate) ![]u8 {
	name_hash := sha256.sum256(issuer.subject_der)
	key_hash := sha256.sum256(spki_contents(issuer.raw)!)
	mut alg := []u8{}
	for b in encode_oid(oid_hash_sha256) {
		alg << b
	}
	alg_id := der_seq(alg)
	mut content := []u8{}
	for b in alg_id {
		content << b
	}
	for b in der_octstr(name_hash) {
		content << b
	}
	for b in der_octstr(key_hash) {
		content << b
	}
	content << der_integer_bytes(leaf.serial)
	return der_seq(content)
}

// make_ocsp_response builds and signs a single-entry BasicOCSPResponse
// for leaf.serial under issuer (the CA-as-responder model), returning
// the parsed response and its DER for stapling tests and fixtures.
pub fn make_ocsp_response(leaf &Certificate, issuer &Certificate, opts OcspGenOptions, signer CertSigner) !(OcspResponse, []u8) {
	cert_id := cert_id_for(leaf, issuer)!
	mut status_tlv := []u8{}
	if opts.status == .good {
		status_tlv = [u8(0x80), 0x00]
	} else if opts.status == .unknown {
		status_tlv = [u8(0x82), 0x00]
	} else {
		mut rev_info := []u8{}
		for b in der_gentime(opts.revoked_at) {
			rev_info << b
		}
		status_tlv = der_tlv(0xa1, rev_info)
	}
	mut single := []u8{}
	for b in cert_id {
		single << b
	}
	for b in status_tlv {
		single << b
	}
	for b in der_gentime(opts.this_update) {
		single << b
	}
	if opts.has_next_update {
		for b in der_tlv(0xa0, der_gentime(opts.next_update)) {
			single << b
		}
	}
	single_der := der_seq(single)
	issuer_name := extract_subject_der(issuer.raw)!
	mut tbs := []u8{}
	mut rid := []u8{}
	for b in issuer_name {
		rid << b
	}
	tbs << der_tlv(0xa1, rid)
	for b in der_gentime(opts.produced_at) {
		tbs << b
	}
	tbs << der_seq(single_der)
	tbs_der := der_seq(tbs)
	sig, _ := sign_tbs(signer, tbs_der)!
	sig_oid := signer_sigalg_oid(signer)!
	sig_alg_id := crl_sig_alg_id(sig_oid, signer.kind)!
	mut basic_body := []u8{}
	for b in tbs_der {
		basic_body << b
	}
	for b in sig_alg_id {
		basic_body << b
	}
	for b in der_bitstr(sig) {
		basic_body << b
	}
	basic_der := der_seq(basic_body)
	mut resp_value := []u8{}
	for b in encode_oid(oid_ocsp_basic) {
		resp_value << b
	}
	for b in der_octstr(basic_der) {
		resp_value << b
	}
	mut resp := []u8{}
	resp << der_tlv(0x0a, [u8(0)])
	resp << der_tlv(0xa0, der_seq(resp_value))
	resp_der := der_seq(resp)
	parsed := parse_ocsp_response(resp_der)!
	return parsed, resp_der
}

// verify_ocsp_staple verifies opaque OCSP response bytes against a leaf
// and its issuer: successful status, CA-as-responder identity, issuer
// signature, producedAt/thisUpdate/nextUpdate freshness, certID hash
// binding, and a good certStatus for the leaf serial.
pub fn verify_ocsp_staple(resp_der []u8, leaf_der []u8, issuer_der []u8) ! {
	resp := parse_ocsp_response(resp_der)!
	leaf := parse_der(leaf_der)!
	issuer := parse_der(issuer_der)!
	now := time.now()
	if resp.produced_at.unix() < now.unix() - ocsp_skew_sec
		|| resp.produced_at.unix() > now.unix() + ocsp_skew_sec {
		return error('x509: OCSP producedAt outside freshness window')
	}
	// The CA itself is the responder: byName must be the issuer subject.
	if resp.responder_der != issuer.subject_der {
		return error('x509: OCSP responder mismatch')
	}
	// The staple binds the issuer key and the leaf serial: recompute the
	// certID hashes and require a matching good entry.
	spki := spki_contents(issuer.raw)!
	mut found := false
	for s in resp.responses {
		if !serial_equal(s.serial, leaf.serial) {
			continue
		}
		mut want_name := []u8{}
		mut want_key := []u8{}
		if s.hash_oid == oid_hash_sha256 {
			want_name = sha256.sum256(issuer.subject_der)
			want_key = sha256.sum256(spki)
		} else {
			want_name = sha1.sum(issuer.subject_der)
			want_key = sha1.sum(spki)
		}
		if want_name != s.issuer_name_hash || want_key != s.issuer_key_hash {
			return error('x509: OCSP issuer hash mismatch')
		}
		found = true
		if s.status == .revoked {
			return error('x509: certificate revoked')
		}
		if s.status != .good {
			return error('x509: OCSP status unknown')
		}
		if s.this_update.unix() > now.unix() + ocsp_skew_sec {
			return error('x509: OCSP thisUpdate in the future')
		}
		if s.has_next_update && s.next_update.unix() < now.unix() - ocsp_skew_sec {
			return error('x509: OCSP response stale')
		}
	}
	if !found {
		return error('x509: OCSP response missing leaf serial')
	}
	// Signature over tbsResponseData under the issuer key.
	probe := Certificate{
		tbs_der:     resp.tbs_der
		sig_alg_oid: resp.sig_alg_oid
		sig_alg_val: resp.sig_alg_val
		signature:   resp.signature
	}
	pk := issuer.public_key()!
	verify_child_signature(&probe, pk)!
}
