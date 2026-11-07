module dns

import net
import os
import time

// V's test runner compiles each _test.v file in isolation against the
// non-test module sources, so const declarations from other _test.v
// files are not visible here. We redeclare the small fixture loader
// rather than promote it into the production module.

const resolver_testdata_dir = os.join_path(os.dir(@FILE), 'testdata')
const resolver_synth_dir = os.join_path(resolver_testdata_dir, 'synth')

// ---------- FakeTransport ----------
//
// Records each call and returns canned responses. The first 2 bytes of
// each response are rewritten to echo the txid the resolver sent, so
// captured-fixture .resp.bin files (with their original txid) can be
// reused as canned payloads regardless of how the resolver allocates ids.

struct FakeCall {
	server   string
	query    []u8
	protocol string // 'udp' or 'tcp'
}

struct FakeTransport {
mut:
	udp_response []u8
	tcp_response []u8
	udp_error    string
	tcp_error    string
	calls        []FakeCall
}

fn (mut t FakeTransport) exchange_udp(server string, query []u8, timeout time.Duration) ![]u8 {
	t.calls << FakeCall{
		server:   server
		query:    query.clone()
		protocol: 'udp'
	}
	if t.udp_error != '' {
		return error(t.udp_error)
	}
	return echo_txid(t.udp_response, query)
}

fn (mut t FakeTransport) exchange_tcp(server string, query []u8, timeout time.Duration) ![]u8 {
	t.calls << FakeCall{
		server:   server
		query:    query.clone()
		protocol: 'tcp'
	}
	if t.tcp_error != '' {
		return error(t.tcp_error)
	}
	return echo_txid(t.tcp_response, query)
}

fn echo_txid(template []u8, query []u8) []u8 {
	if template.len < 2 || query.len < 2 {
		return template.clone()
	}
	mut out := template.clone()
	out[0] = query[0]
	out[1] = query[1]
	return out
}

fn fixture(name string) ![]u8 {
	return os.read_bytes(os.join_path(resolver_testdata_dir, '${name}.resp.bin'))!
}

fn synth_fixture(name string) ![]u8 {
	return os.read_bytes(os.join_path(resolver_synth_dir, '${name}.bin'))!
}

// ---------- resolv.conf parsing ----------

fn write_temp_resolv(content string) !string {
	path := os.join_path(os.temp_dir(), 'vdns_resolv_${time.now().unix_milli()}_${rand_token()}.conf')
	os.write_file(path, content)!
	return path
}

fn rand_token() string {
	// Deterministic-enough tag for parallel test runs; not security-critical.
	return time.now().unix_nano().str()
}

fn test_parse_resolv_conf_single_nameserver() {
	path := write_temp_resolv('nameserver 1.2.3.4\n')!
	defer {
		os.rm(path) or {}
	}
	servers := parse_resolv_conf(path)!.servers
	assert servers == ['1.2.3.4:53']
}

fn test_parse_resolv_conf_multiple_nameservers_in_order() {
	path := write_temp_resolv('nameserver 1.1.1.1\nnameserver 9.9.9.9\nnameserver 8.8.8.8\n')!
	defer {
		os.rm(path) or {}
	}
	servers := parse_resolv_conf(path)!.servers
	assert servers == ['1.1.1.1:53', '9.9.9.9:53', '8.8.8.8:53']
}

fn test_parse_resolv_conf_ignores_comments_and_blank_lines() {
	path := write_temp_resolv('# comment\n; semicolon comment\n\nnameserver 1.1.1.1\n# another\n')!
	defer {
		os.rm(path) or {}
	}
	servers := parse_resolv_conf(path)!.servers
	assert servers == ['1.1.1.1:53']
}

fn test_parse_resolv_conf_reads_options_and_ignores_search_domain() {
	path := write_temp_resolv('search example.com lan.local\noptions timeout:2 attempts:3\nnameserver 1.1.1.1\ndomain example.com\n')!
	defer {
		os.rm(path) or {}
	}
	conf := parse_resolv_conf(path)!
	assert conf.servers == ['1.1.1.1:53']
	assert conf.timeout == 2 * time.second
	assert conf.attempts == 3
}

fn test_parse_resolv_conf_missing_file_returns_error() {
	if _ := parse_resolv_conf(os.join_path(os.vtmp_dir(), 'net_dns_no_such_resolv.conf')) {
		assert false, 'a missing file must be an error'
	}
}

fn test_parse_resolv_conf_handles_ipv6_nameserver() {
	path := write_temp_resolv('nameserver 2606:4700:4700::1111\nnameserver 8.8.8.8\n')!
	defer {
		os.rm(path) or {}
	}
	servers := parse_resolv_conf(path)!.servers
	// IPv6 nameservers are bracketed when the port is appended so they
	// can be passed straight to net.dial_*.
	assert servers == ['[2606:4700:4700::1111]:53', '8.8.8.8:53']
}

fn test_parse_resolv_conf_empty_returns_error() {
	path := write_temp_resolv('# only comments\n')!
	defer {
		os.rm(path) or {}
	}
	if _ := parse_resolv_conf(path) {
		assert false, 'must error when no nameserver line is present'
	}
}

// ---------- query() over FakeTransport ----------

fn test_query_uses_first_server_and_returns_decoded_message() {
	resp := fixture('a_example_com')!
	mut ft := &FakeTransport{
		udp_response: resp
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53']
		timeout:   100 * time.millisecond
		attempts:  1
	}
	m := r.query('example.com', .a)!

	assert ft.calls.len == 1
	assert ft.calls[0].server == '198.51.100.1:53'
	assert ft.calls[0].protocol == 'udp'
	// The query the resolver built must round-trip through our decoder
	// to the same question we asked.
	q := decode(ft.calls[0].query)!
	assert q.questions.len == 1
	assert q.questions[0].name == 'example.com'
	assert q.questions[0].typ == .a
	assert q.header.qr == false
	assert q.header.rd == true

	// And the response must be parsed:
	assert m.header.ancount == 2
	assert m.answers.len == 2
}

fn test_query_retries_on_transport_error() {
	// First attempt fails (transport error simulated through udp_error),
	// then the second server succeeds. The fake records both calls so
	// we can assert the failover order.
	mut ft := &FakeTransport{
		udp_error: 'simulated timeout'
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53', '198.51.100.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	if _ := r.query('example.com', .a) {
		assert false, 'expected error when all servers fail'
	}
	// attempts=1 means: try each server once, in order.
	assert ft.calls.len == 2
	assert ft.calls[0].server == '198.51.100.1:53'
	assert ft.calls[1].server == '198.51.100.2:53'
}

fn test_query_falls_back_to_tcp_on_truncation() {
	// UDP returns a TC=1 response (synth_tc_set), TCP returns the full
	// real answer. Resolver must use the TCP body for the result.
	tc := synth_fixture('tc_set')!
	full := fixture('a_example_com')!
	mut ft := &FakeTransport{
		udp_response: tc
		tcp_response: full
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53']
		timeout:   100 * time.millisecond
		attempts:  1
	}
	m := r.query('example.com', .a)!
	assert m.answers.len == 2
	assert ft.calls.len == 2
	assert ft.calls[0].protocol == 'udp'
	assert ft.calls[1].protocol == 'tcp'
	assert ft.calls[1].server == '198.51.100.1:53'
}

fn test_query_drops_response_with_mismatched_txid() {
	// Build a response template whose txid will NOT be echoed: we use a
	// FakeTransport that ignores the query and returns a fixed buffer.
	// The resolver must reject the mismatched txid and surface an error
	// (no other server configured).
	mut resp := fixture('a_example_com')!.clone()
	// Force a known wrong txid (capture has 0x1001; we keep that).
	mut ft := &MismatchingTransport{
		response: resp
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	if _ := r.query('example.com', .a) {
		assert false, 'must reject txid mismatch'
	}
}

// MismatchingTransport returns a response whose txid is guaranteed to
// differ from the query's: we rewrite bytes 0..1 to (query_txid ^ 0xFFFF),
// which makes the test deterministic regardless of how the resolver
// allocates ids.
struct MismatchingTransport {
mut:
	response []u8
}

fn (mut t MismatchingTransport) exchange_udp(server string, query []u8, timeout time.Duration) ![]u8 {
	return flip_txid(t.response, query)
}

fn (mut t MismatchingTransport) exchange_tcp(server string, query []u8, timeout time.Duration) ![]u8 {
	return flip_txid(t.response, query)
}

fn flip_txid(template []u8, query []u8) []u8 {
	mut out := template.clone()
	if out.len >= 2 && query.len >= 2 {
		out[0] = query[0] ^ 0xFF
		out[1] = query[1] ^ 0xFF
	}
	return out
}

// ---------- lookup_* helpers ----------

fn test_lookup_a_returns_addrs_in_answer_order() {
	mut ft := &FakeTransport{
		udp_response: fixture('a_example_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	addrs := r.lookup_a('example.com')!
	assert addrs == ['172.66.147.243', '104.20.23.154']
}

fn test_lookup_aaaa_returns_v6_addrs() {
	mut ft := &FakeTransport{
		udp_response: fixture('aaaa_example_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	addrs := r.lookup_aaaa('example.com')!
	assert addrs.len == 2
	for a in addrs {
		assert a.contains(':')
	}
}

fn test_lookup_mx_returns_mx_records_sorted_by_preference() {
	mut ft := &FakeTransport{
		udp_response: fixture('mx_gmail_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	mxs := r.lookup_mx('gmail.com')!
	// Sorted ascending by preference (5, 10, 20, 30, 40).
	assert mxs.map(it.preference) == [u16(5), 10, 20, 30, 40]
	assert mxs[0].exchange == 'gmail-smtp-in.l.google.com'
	assert mxs[4].exchange == 'alt4.gmail-smtp-in.l.google.com'
}

fn test_lookup_txt_returns_strings_grouped_by_record() {
	mut ft := &FakeTransport{
		udp_response: fixture('txt_dmarc_org')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	txts := r.lookup_txt('dmarc.org')!
	assert txts == [['v=spf1 a mx -all']]
}

fn test_lookup_txt_empty_when_zero_answers() {
	// NOERROR with no answer is a valid empty result, not an error. The capture
	// is truncated (TC=1), which the TCP fallback test covers; TC is cleared
	// here to get the non-truncated, zero-answer case.
	mut resp := fixture('txt_empty_cloudflare_com')!.clone()
	resp[2] &= ~u8(0x02)
	mut ft := &FakeTransport{
		udp_response: resp
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	txts := r.lookup_txt('cloudflare.com')!
	assert txts == [][]string{}
}

fn test_query_rejects_a_response_without_question() {
	mut ft := &FakeTransport{
		udp_response: synth_fixture('empty')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		attempts:  1
	}
	if _ := r.query('example.com', .txt) {
		assert false, 'a response that does not repeat the question must be rejected'
	}
}

fn test_lookup_ns_returns_authoritative_nameservers() {
	mut ft := &FakeTransport{
		udp_response: fixture('ns_example_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	ns := r.lookup_ns('example.com')!
	assert ns.len >= 1
	for n in ns {
		assert n.ends_with('.com') || n.ends_with('.net') || n.ends_with('.org')
	}
}

fn test_lookup_ptr_takes_ip_and_returns_names() {
	mut ft := &FakeTransport{
		udp_response: fixture('ptr_1_1_1_1')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	names := r.lookup_ptr('1.1.1.1')!
	assert names == ['one.one.one.one']
	// And the resolver formed the right reverse arpa name in the query:
	q := decode(ft.calls[0].query)!
	assert q.questions[0].name == '1.1.1.1.in-addr.arpa'
}

fn test_lookup_srv_returns_records() {
	mut ft := &FakeTransport{
		udp_response: fixture('srv_caldavs_google_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	srvs := r.lookup_srv('_caldavs._tcp.google.com')!
	assert srvs.len == 1
	assert srvs[0].port == 443
	assert srvs[0].target == 'calendar.google.com'
}

fn test_lookup_soa_returns_single_soa() {
	mut ft := &FakeTransport{
		udp_response: fixture('soa_example_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	soa := r.lookup_soa('example.com')!
	assert soa.mname == 'elliott.ns.cloudflare.com'
	assert soa.rname == 'dns.cloudflare.com'
}

fn test_lookup_caa_returns_records() {
	mut ft := &FakeTransport{
		udp_response: fixture('caa_google_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	caas := r.lookup_caa('google.com')!
	assert caas.len >= 1
	assert caas[0].tag in ['issue', 'issuewild', 'iodef']
}

fn test_lookup_a_propagates_nxdomain_as_error() {
	mut ft := &FakeTransport{
		udp_response: fixture('a_nxdomain')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['127.0.0.2:53']
		timeout:   50 * time.millisecond
		attempts:  1
	}
	if _ := r.lookup_a('this-domain-must-not-exist.example.') {
		assert false, 'NXDOMAIN must surface as error'
	}
}

// ---------- response validation ----------

fn test_query_rejects_a_message_without_the_qr_bit() {
	mut resp := fixture('a_example_com')!.clone()
	resp[2] &= 0x7f
	mut ft := &FakeTransport{
		udp_response: resp
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53']
		attempts:  1
	}
	if _ := r.query('example.com', .a) {
		assert false, 'a message that is not a response must be rejected'
	}
}

fn test_query_rejects_a_response_to_another_question() {
	mut ft := &FakeTransport{
		udp_response: fixture('a_example_com')!
	}
	mut r := Resolver{
		transport: ft
		servers:   ['198.51.100.1:53']
		attempts:  1
	}
	if _ := r.query('example.org', .a) {
		assert false, 'a response for example.com must not answer example.org'
	}
	if _ := r.query('example.com', .aaaa) {
		assert false, 'an A response must not answer an AAAA query'
	}
	// Names compare case-insensitively, with or without the root dot.
	m := r.query('EXAMPLE.com.', .a)!
	assert m.answers.len > 0
}

fn answer_with_noise(mut server net.UdpConn) {
	mut buf := []u8{len: 512}
	n, from := server.read(mut buf) or { return }
	query := buf[..n].clone()
	// A response that would be valid, sent from another address: the client
	// must only accept datagrams from the server it queried.
	mut spoofer := net.listen_udp('127.0.0.1:0') or { return }
	mut spoofed := query.clone()
	spoofed[2] |= 0x80
	spoofed << [u8(0xde), 0xad]
	spoofer.write_to(from, spoofed) or {}
	spoofer.close() or {}
	// A forged datagram with another id, then one with the right id but no QR
	// bit, then the real response.
	mut forged := query.clone()
	forged[0] ^= 0xff
	forged[2] |= 0x80
	server.write_to(from, forged) or {}
	server.write_to(from, query) or {}
	mut real := query.clone()
	real[2] |= 0x80
	server.write_to(from, real) or {}
}

fn test_net_transport_ignores_datagrams_that_do_not_answer_the_query() {
	mut server := net.listen_udp('127.0.0.1:0')!
	defer {
		server.close() or {}
	}
	addr := server.sock.address()!.str()
	responder := spawn answer_with_noise(mut server)
	mut t := NetTransport{}
	query := [u8(0x12), 0x34, 0x01, 0x00, 0, 1, 0, 0, 0, 0, 0, 0]
	resp := t.exchange_udp(addr, query, 2 * time.second)!
	responder.wait()
	assert resp[0] == 0x12 && resp[1] == 0x34
	assert resp[2] & 0x80 != 0
	// The spoofed datagram, two bytes longer, was not the one returned.
	assert resp.len == query.len
}

// ---------- synthetic responses ----------

fn wire_name(name string) []u8 {
	mut out := []u8{}
	for label in name.split('.') {
		if label.len > 0 {
			out << u8(label.len)
			out << label.bytes()
		}
	}
	out << u8(0)
	return out
}

fn rr(owner string, typ u16, class u16, rdata []u8) []u8 {
	mut out := wire_name(owner)
	out << [u8(typ >> 8), u8(typ), u8(class >> 8), u8(class), 0, 0, 0, 60, u8(rdata.len >> 8),
		u8(rdata.len)]
	out << rdata
	return out
}

// response builds a NOERROR response to `qname`/`qtype` with `records` in the
// answer section; FakeTransport rewrites its id.
fn response(qname string, qtype u16, flags u16, records [][]u8) []u8 {
	mut out := [u8(0), 0, u8(flags >> 8), u8(flags), 0, 1, 0, u8(records.len), 0, 0, 0, 0]
	out << wire_name(qname)
	out << [u8(qtype >> 8), u8(qtype), 0, 1]
	for r in records {
		out << r
	}
	return out
}

fn fake_resolver(udp []u8, tcp []u8) Resolver {
	return Resolver{
		transport: &FakeTransport{
			udp_response: udp
			tcp_response: tcp
		}
		servers:   ['127.0.0.2:53']
		attempts:  1
	}
}

fn test_query_rejects_another_class_or_opcode() {
	mut ch := response('example.com', 1, 0x8180, [][]u8{})
	ch[ch.len - 1] = 3 // class CH
	mut r := fake_resolver(ch, []u8{})
	if _ := r.query('example.com', .a) {
		assert false, 'a CH response must not answer an IN query'
	}
	mut op := response('example.com', 1, 0x8180 | (6 << 11), [][]u8{})
	r = fake_resolver(op, []u8{})
	if _ := r.query('example.com', .a) {
		assert false, 'an unknown opcode must be rejected'
	}
}

fn test_a_truncated_response_falls_back_to_tcp_before_decoding_its_answers() {
	// TC set and one answer announced but absent: only the header and the
	// question are read before switching to TCP.
	mut udp := response('example.com', 1, 0x8380, [][]u8{})
	udp[7] = 1
	tcp := response('example.com', 1, 0x8180, [rr('example.com', 1, 1, [u8(192), 0, 2, 1])])
	mut r := fake_resolver(udp, tcp)
	assert r.lookup_a('example.com')! == ['192.0.2.1']
}

fn test_a_truncated_tcp_response_is_an_error() {
	udp := response('example.com', 1, 0x8380, [][]u8{})
	tcp := response('example.com', 1, 0x8380, [rr('example.com', 1, 1, [u8(192), 0, 2, 1])])
	mut r := fake_resolver(udp, tcp)
	if _ := r.lookup_a('example.com') {
		assert false, 'a truncated TCP response must not be returned'
	}
}

fn test_lookups_keep_the_records_of_the_name_and_its_aliases() {
	answers := [
		rr('www.example.com', 5, 1, wire_name('web.example.net')),
		rr('web.example.net', 1, 1, [u8(192), 0, 2, 10]),
		rr('other.example.com', 1, 1, [u8(192), 0, 2, 99]),
		rr('web.example.net', 1, 3, [u8(192), 0, 2, 98]),
	]
	mut r := fake_resolver(response('www.example.com', 1, 0x8180, answers), []u8{})
	assert r.lookup_a('www.example.com')! == ['192.0.2.10']
	// An alias whose target is not in the answer gives no address.
	mut lone := fake_resolver(response('www.example.com', 1, 0x8180, answers[..1]), []u8{})
	assert lone.lookup_a('www.example.com')! == []string{}
}

fn test_reverse_arpa_validates_ipv4_addresses() {
	assert reverse_arpa('192.0.2.1')! == '1.2.0.192.in-addr.arpa'
	assert reverse_arpa('1.2.0.192.IN-ADDR.ARPA.')! == '1.2.0.192.IN-ADDR.ARPA.'
	for bad in ['256.1.1.1', '1.2.3', '1.2.3.4.5', 'a.b.c.d', '1..2.3', '-1.2.3.4'] {
		if _ := reverse_arpa(bad) {
			assert false, '`${bad}` must be rejected'
		}
	}
}

fn test_parse_resolv_conf_strips_inline_comments_and_bounds_options() {
	path := write_temp_resolv('nameserver 10.0.0.1 # office\nnameserver 10.0.0.2;backup\noptions timeout:100 attempts:0 # options timeout:1\n')!
	defer {
		os.rm(path) or {}
	}
	conf := parse_resolv_conf(path)!
	assert conf.servers == ['10.0.0.1:53', '10.0.0.2:53']
	assert conf.timeout == 30 * time.second
	assert conf.attempts == default_attempts
}
