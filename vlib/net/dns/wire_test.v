module dns

import os

// ---------- Fixture loading helpers ----------

const testdata_dir = os.join_path(os.dir(@FILE), 'testdata')
const synth_dir = os.join_path(testdata_dir, 'synth')

fn load_req(basename string) ![]u8 {
	return os.read_bytes(os.join_path(testdata_dir, '${basename}.req.bin'))!
}

fn load_resp(basename string) ![]u8 {
	return os.read_bytes(os.join_path(testdata_dir, '${basename}.resp.bin'))!
}

fn load_synth(name string) ![]u8 {
	return os.read_bytes(os.join_path(synth_dir, '${name}.bin'))!
}

// ---------- Captured-response decode tests ----------
//
// Each captured fixture exercises a full live response from 1.1.1.1 for
// a different record type. Expectations are derived from the JSON
// sidecars committed under testdata/ (themselves cross-checked by hand
// against `dig`). They are inlined here on purpose: a failure points
// directly to the offending field.

fn test_decode_a_example_com() {
	buf := load_resp('a_example_com')!
	m := decode(buf)!
	assert m.header.id == 0x1001
	assert m.header.qr
	assert m.header.rd
	assert m.header.ra
	assert !m.header.tc
	assert m.header.rcode == .no_error
	assert m.header.ancount == 2

	assert m.questions.len == 1
	assert m.questions[0].name == 'example.com'
	assert m.questions[0].typ == .a
	assert m.questions[0].class == .in_

	assert m.answers.len == 2
	assert m.answers[0].name == 'example.com'
	assert m.answers[0].typ == .a
	assert m.answers[0].ttl == 9
	rd0 := m.answers[0].rdata as AddrRdata
	assert rd0.addr == '172.66.147.243'
	rd1 := m.answers[1].rdata as AddrRdata
	assert rd1.addr == '104.20.23.154'
}

fn test_decode_aaaa_example_com() {
	buf := load_resp('aaaa_example_com')!
	m := decode(buf)!
	assert m.header.id == 0x1002
	assert m.header.ancount == 2
	assert m.questions[0].typ == .aaaa
	assert m.answers.len == 2
	assert m.answers[0].typ == .aaaa
	a0 := m.answers[0].rdata as AddrRdata
	a1 := m.answers[1].rdata as AddrRdata
	// RFC 5952 canonical: longest zero run replaced by "::", lowercase.
	assert a0.addr == '2606:4700:10::6814:179a'
	assert a1.addr == '2606:4700:10::ac42:93f3'
}

fn test_decode_mx_gmail_com() {
	buf := load_resp('mx_gmail_com')!
	m := decode(buf)!
	assert m.header.id == 0x1003
	assert m.header.ancount == 5
	assert m.answers.len == 5
	for ans in m.answers {
		assert ans.name == 'gmail.com'
		assert ans.typ == .mx
		assert ans.ttl == 1659
	}
	prefs := m.answers.map((it.rdata as MxRdata).preference)
	exchanges := m.answers.map((it.rdata as MxRdata).exchange)
	assert prefs == [u16(30), 40, 5, 20, 10]
	assert exchanges == [
		'alt3.gmail-smtp-in.l.google.com',
		'alt4.gmail-smtp-in.l.google.com',
		'gmail-smtp-in.l.google.com',
		'alt2.gmail-smtp-in.l.google.com',
		'alt1.gmail-smtp-in.l.google.com',
	]
}

fn test_decode_txt_dmarc_org() {
	buf := load_resp('txt_dmarc_org')!
	m := decode(buf)!
	assert m.header.ancount == 1
	assert m.questions[0].typ == .txt
	assert m.answers.len == 1
	t := m.answers[0].rdata as TxtRdata
	assert t.strings == ['v=spf1 a mx -all']
}

fn test_decode_txt_empty_cloudflare_com() {
	// Cloudflare returns NOERROR with zero answers when EDNS(0) is
	// absent and the full TXT set would exceed 512 bytes. Decoder must
	// produce an empty answer slice, not error.
	buf := load_resp('txt_empty_cloudflare_com')!
	m := decode(buf)!
	assert m.header.rcode == .no_error
	assert m.header.ancount == 0
	assert m.answers.len == 0
	assert m.questions[0].name == 'cloudflare.com'
	assert m.questions[0].typ == .txt
}

fn test_decode_ns_example_com() {
	buf := load_resp('ns_example_com')!
	m := decode(buf)!
	assert m.questions[0].typ == .ns
	assert m.answers.len >= 1
	for ans in m.answers {
		assert ans.typ == .ns
		_ := (ans.rdata as NameRdata).name
	}
}

fn test_decode_soa_example_com() {
	buf := load_resp('soa_example_com')!
	m := decode(buf)!
	assert m.questions[0].typ == .soa
	assert m.answers.len == 1
	soa := m.answers[0].rdata as SoaRdata
	// 1.1.1.1 returns Cloudflare's view of the example.com SOA.
	assert soa.mname == 'elliott.ns.cloudflare.com'
	assert soa.rname == 'dns.cloudflare.com'
	assert soa.refresh > 0
	assert soa.minimum > 0
}

fn test_decode_cname_www_github_com() {
	buf := load_resp('cname_www_github_com')!
	m := decode(buf)!
	assert m.questions[0].name == 'www.github.com'
	assert m.questions[0].typ == .cname
	assert m.answers.len >= 1
	cn := m.answers[0].rdata as NameRdata
	assert cn.name.len > 0
	assert cn.name.contains('.')
}

fn test_decode_ptr_1_1_1_1() {
	buf := load_resp('ptr_1_1_1_1')!
	m := decode(buf)!
	assert m.questions[0].name == '1.1.1.1.in-addr.arpa'
	assert m.questions[0].typ == .ptr
	assert m.answers.len >= 1
	p := m.answers[0].rdata as NameRdata
	assert p.name == 'one.one.one.one'
}

fn test_decode_srv_caldavs_google_com() {
	buf := load_resp('srv_caldavs_google_com')!
	m := decode(buf)!
	assert m.questions[0].typ == .srv
	assert m.answers.len == 1
	s := m.answers[0].rdata as SrvRdata
	assert s.priority == 5
	assert s.weight == 0
	assert s.port == 443
	assert s.target == 'calendar.google.com'
}

fn test_decode_caa_google_com() {
	buf := load_resp('caa_google_com')!
	m := decode(buf)!
	assert m.questions[0].typ == .caa
	assert m.answers.len >= 1
	c := m.answers[0].rdata as CaaRdata
	assert c.tag in ['issue', 'issuewild', 'iodef']
	assert c.value.len > 0
}

fn test_decode_a_nxdomain() {
	buf := load_resp('a_nxdomain')!
	m := decode(buf)!
	assert m.header.rcode == .nx_domain
	assert m.answers.len == 0
	assert m.authorities.len == 1
	assert m.authorities[0].typ == .soa
	auth := m.authorities[0].rdata as SoaRdata
	assert auth.mname.ends_with('root-servers.net')
}

fn test_decode_a_root_servers_net() {
	buf := load_resp('a_root_servers_net')!
	m := decode(buf)!
	assert m.questions[0].name == 'a.root-servers.net'
	assert m.answers.len == 1
	a := m.answers[0].rdata as AddrRdata
	assert a.addr == '198.41.0.4'
}

// ---------- Synthetic decode tests (edge cases) ----------

fn test_synth_short_header_rejected() {
	buf := load_synth('short_header')!
	if _ := decode(buf) {
		assert false, 'short header must be rejected'
	}
}

fn test_synth_truncated_qname_rejected() {
	buf := load_synth('truncated_qname')!
	if _ := decode(buf) {
		assert false, 'truncated qname must be rejected'
	}
}

fn test_synth_label_top_bits_01_rejected() {
	buf := load_synth('label_top_bits_01')!
	if _ := decode(buf) {
		assert false, 'reserved label flags 0x40 must be rejected'
	}
}

fn test_synth_reserved_label_flags_rejected() {
	buf := load_synth('reserved_label_flags')!
	if _ := decode(buf) {
		assert false, 'reserved label flags 0x80 must be rejected'
	}
}

fn test_synth_pointer_cycle_rejected() {
	buf := load_synth('pointer_cycle')!
	if _ := decode(buf) {
		assert false, 'pointer cycle must be rejected'
	}
}

fn test_synth_pointer_out_of_bounds_rejected() {
	buf := load_synth('pointer_out_of_bounds')!
	if _ := decode(buf) {
		assert false, 'out-of-bounds pointer must be rejected'
	}
}

fn test_synth_truncated_rdata_rejected() {
	buf := load_synth('truncated_rdata')!
	if _ := decode(buf) {
		assert false, 'truncated rdata must be rejected'
	}
}

fn test_synth_tc_set_decodes_with_tc_true() {
	buf := load_synth('tc_set')!
	m := decode(buf)!
	assert m.header.tc
	assert m.header.qr
	assert m.header.rd
	assert m.header.ancount == 0
	assert m.questions.len == 1
	assert m.questions[0].name == 'example.com'
}

fn test_synth_unknown_record_type_decodes_as_opaque() {
	buf := load_synth('unknown_type')!
	m := decode(buf)!
	assert m.answers.len == 1
	op := m.answers[0].rdata as OpaqueRdata
	assert op.bytes == [u8(0xDE), 0xAD, 0xBE, 0xEF]
	assert m.answers[0].name == 'opaque.test'
}

fn test_synth_deep_compression_chain_resolves_full_name() {
	// Three-deep pointer chain: answer.name -> aux2 -> aux1 -> question.
	// A correct decoder yields "a.b.example.com".
	buf := load_synth('deep_compression_chain')!
	m := decode(buf)!
	assert m.questions[0].name == 'example.com'
	assert m.answers.len == 1
	assert m.answers[0].name == 'a.b.example.com'
	a := m.answers[0].rdata as AddrRdata
	assert a.addr == '1.2.3.4'
}

fn test_synth_empty_message_decodes() {
	buf := load_synth('empty')!
	m := decode(buf)!
	assert m.header.qr
	assert m.questions.len == 0
	assert m.answers.len == 0
}

// ---------- Encode tests ----------
//
// capture.py used a tiny canonical encoder (no compression, RD=1, all
// other flags zero, txid fixed). Our encoder must produce bytes equal
// to those .req.bin files when given a Message that mirrors the
// question section.

const captured_cases = [
	'a_example_com',
	'aaaa_example_com',
	'mx_gmail_com',
	'txt_dmarc_org',
	'txt_empty_cloudflare_com',
	'ns_example_com',
	'soa_example_com',
	'cname_www_github_com',
	'ptr_1_1_1_1',
	'srv_caldavs_google_com',
	'caa_google_com',
	'a_nxdomain',
	'a_root_servers_net',
]

fn test_encode_request_byte_equal_to_capture() {
	for case_ in captured_cases {
		req_bytes := load_req(case_)!
		// Decoding the captured request gives the canonical Message
		// shape (txid, single question, RD=1). Re-encoding it must
		// reproduce exactly the bytes capture.py originally sent.
		m := decode(req_bytes)!
		assert m.header.qdcount == 1
		assert m.questions.len == 1
		again := encode(m)!
		assert again == req_bytes, 'encode mismatch for ${case_}'
	}
}

fn test_encode_decode_roundtrip_for_constructed_message() {
	original := Message{
		header:      Header{
			id:      0xCAFE
			rd:      true
			qdcount: 1
		}
		questions:   [
			Question{
				name:  'example.com'
				typ:   .a
				class: .in_
			},
		]
		answers:     []
		authorities: []
		additionals: []
	}
	bytes := encode(original)!
	round := decode(bytes)!
	assert round.header.id == original.header.id
	assert round.header.rd == original.header.rd
	assert round.questions.len == original.questions.len
	for i, q in round.questions {
		assert q.name == original.questions[i].name
		assert q.typ == original.questions[i].typ
		assert q.class == original.questions[i].class
		assert q.type_code == u16(original.questions[i].typ)
		assert q.class_code == u16(original.questions[i].class)
	}
}

fn test_encode_rejects_label_too_long() {
	long_label := 'a'.repeat(64)
	msg := Message{
		header:    Header{
			id:      1
			qdcount: 1
		}
		questions: [
			Question{
				name: '${long_label}.com'
				typ:  .a
			},
		]
	}
	if _ := encode(msg) {
		assert false, 'encoder must reject 64-byte label'
	}
}

fn test_encode_rejects_total_name_too_long() {
	// Build a name whose on-wire encoding exceeds 255 octets.
	mut parts := []string{}
	for _ in 0 .. 50 {
		parts << 'abcdef'
	}
	long := parts.join('.')
	msg := Message{
		header:    Header{
			id:      1
			qdcount: 1
		}
		questions: [
			Question{
				name: long
				typ:  .a
			},
		]
	}
	if _ := encode(msg) {
		assert false, 'encoder must reject names > 255 octets'
	}
}

fn test_encode_root_name_is_single_zero_byte() {
	msg := Message{
		header:    Header{
			id:      0xAAAA
			rd:      true
			qdcount: 1
		}
		questions: [
			Question{
				name: ''
				typ:  .ns
			},
		]
	}
	bytes := encode(msg)!
	// 12-byte header, then 0x00 (root), then QTYPE+QCLASS.
	assert bytes.len == 17
	assert bytes[12] == 0
}

fn test_encode_trailing_dot_equivalent_to_no_dot() {
	msg_a := Message{
		header:    Header{
			id:      1
			qdcount: 1
		}
		questions: [
			Question{
				name: 'example.com'
				typ:  .a
			},
		]
	}
	msg_b := Message{
		header:    Header{
			id:      1
			qdcount: 1
		}
		questions: [
			Question{
				name: 'example.com.'
				typ:  .a
			},
		]
	}
	assert encode(msg_a)! == encode(msg_b)!
}

// response_header returns a response header with the given section counts.
fn response_header(qd int, an int) []u8 {
	return [u8(0x12), 0x34, 0x81, 0x80, 0, u8(qd), 0, u8(an), 0, 0, 0, 0]
}

fn test_a_dot_inside_a_label_is_escaped() {
	mut buf := response_header(1, 0)
	// One label "a.b" then "example": read naively, it would look like three labels.
	buf << [u8(3), `a`, `.`, `b`, 7]
	buf << 'example'.bytes()
	buf << [u8(0), 0, 1, 0, 1]
	m := decode(buf)!
	assert m.questions[0].name == r'a\.b.example'
}

fn test_non_printable_bytes_in_a_label_are_escaped() {
	mut buf := response_header(1, 0)
	buf << [u8(2), 0x00, `\\`, 0]
	buf << [u8(0), 1, 0, 1]
	m := decode(buf)!
	assert m.questions[0].name == r'\000\\'
}

fn test_a_name_running_past_its_record_data_is_rejected() {
	mut buf := response_header(0, 1)
	// An NS record whose rdlength (2) is shorter than the name it holds.
	buf << [u8(0), 0, 2, 0, 1, 0, 0, 0, 60, 0, 2]
	buf << [u8(3), `n`, `s`, `1`, 0]
	if _ := decode(buf) {
		assert false, 'a name longer than its rdata must be rejected'
	}
}

fn test_presentation_names_round_trip_through_encode() {
	q := Message{
		header:    Header{
			id:      7
			qdcount: 1
		}
		questions: [Question{
			name: r'a\.b.ex\\am\032ple.'
			typ:  .txt
		}]
	}
	bytes := encode(q)!
	// The first label holds "a.b": one length octet, three bytes.
	assert bytes[header_size] == 3
	assert bytes[header_size + 1..header_size + 4] == 'a.b'.bytes()
	back := decode(bytes)!
	assert back.questions[0].name == r'a\.b.ex\\am\032ple'
	assert names_equal(back.questions[0].name, q.questions[0].name)
}

fn test_names_compare_case_insensitively_and_ignore_the_root_dot() {
	assert names_equal('Example.COM.', 'example.com')
	assert names_equal('', '.')
	assert !names_equal('example.com', 'example.org')
	assert !names_equal(r'a\.b', 'a.b')
}

fn test_empty_labels_are_rejected_when_encoding() {
	for bad in ['a..b', '.a', 'a.b..'] {
		q := Message{
			header:    Header{
				qdcount: 1
			}
			questions: [Question{
				name: bad
				typ:  .a
			}]
		}
		if _ := encode(q) {
			assert false, '`${bad}` must be rejected'
		}
	}
}

fn test_unknown_codes_are_kept_as_received() {
	mut buf := response_header(1, 0)
	buf[2] = 0x80 | (6 << 3) // QR, opcode 6
	buf[3] = 0x09 // rcode 9
	buf << [u8(0), 0, 99, 0, 5]
	m := decode(buf)!
	assert m.header.opcode == .unknown && m.header.opcode_code == 6
	assert m.header.rcode == .unknown && m.header.rcode_code == 9
	assert m.questions[0].typ == .unknown && m.questions[0].type_code == 99
	assert m.questions[0].class == .unknown && m.questions[0].class_code == 5
}

fn test_record_data_must_be_consumed_exactly() {
	mut buf := response_header(0, 1)
	// An NS record whose rdlength (6) is longer than the name it holds (5).
	buf << [u8(0), 0, 2, 0, 1, 0, 0, 0, 60, 0, 6]
	buf << [u8(3), `n`, `s`, `1`, 0, 0xff]
	if _ := decode(buf) {
		assert false, 'trailing bytes in a record must be rejected'
	}
}

fn test_an_empty_txt_record_is_rejected() {
	mut buf := response_header(0, 1)
	buf << [u8(0), 0, 16, 0, 1, 0, 0, 0, 60, 0, 0]
	if _ := decode(buf) {
		assert false, 'a TXT record needs at least one string'
	}
}

fn test_a_caa_record_with_an_invalid_tag_is_rejected() {
	for tag in ['', 'is-sue'] {
		mut buf := response_header(0, 1)
		rdlen := 2 + tag.len + 3
		buf << [u8(0), 1, 1, 0, 1, 0, 0, 0, 60, 0, u8(rdlen), 0, u8(tag.len)]
		buf << tag.bytes()
		buf << 'v.x'.bytes()
		if _ := decode(buf) {
			assert false, 'CAA tag `${tag}` must be rejected'
		}
	}
}

fn test_huge_section_counts_do_not_allocate_ahead() {
	// 65535 answers announced in a 12-byte message: the decoder must fail on
	// the missing data, not preallocate for the count.
	buf := [u8(0x12), 0x34, 0x81, 0x80, 0, 0, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
	if _ := decode(buf) {
		assert false, 'missing records must be an error'
	}
}
