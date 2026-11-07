module dns

import crypto.rand
import os
import time

// default_timeout is how long one attempt waits for a response.
pub const default_timeout = 5 * time.second
// default_attempts is how many times each server is tried, as in resolv.conf.
pub const default_attempts = 2
// max_udp_payload is the largest response read over UDP. Without EDNS0, a server
// truncates longer responses and sets TC, and the query is retried over TCP.
pub const max_udp_payload = 512

// Transport abstracts the network exchange so tests can swap in a fake
// without touching encode/decode. RFC 1035 §4.2.1/§4.2.2 specify two
// transports with the same message body and different framing: UDP has
// no length prefix; TCP prepends a 2-byte big-endian length.
//
// exchange_udp returns the first datagram that answers the query, i.e. that
// carries its transaction id with the QR bit set; other datagrams are ignored
// until `timeout`.
pub interface Transport {
mut:
	exchange_udp(server string, query []u8, timeout time.Duration) ![]u8
	exchange_tcp(server string, query []u8, timeout time.Duration) ![]u8
}

// Resolver sends queries to `servers` (`host:port`, IPv6 hosts in brackets),
// which `new_resolver` reads from the system configuration or the caller sets.
pub struct Resolver {
pub mut:
	transport Transport = &NetTransport{}
	servers   []string
	timeout   time.Duration = default_timeout
	attempts  int           = default_attempts
}

// ResolvConf is what `parse_resolv_conf` reads from a resolv.conf file.
pub struct ResolvConf {
pub:
	servers  []string
	timeout  time.Duration = default_timeout
	attempts int           = default_attempts
}

// new_resolver builds a Resolver from the servers and options of
// /etc/resolv.conf, and returns an error when the file lists no nameserver.
// `Resolver.servers` sets the servers directly.
pub fn new_resolver() !Resolver {
	conf := parse_resolv_conf('/etc/resolv.conf') or {
		return error('net.dns: no system DNS configuration (${err.msg()}); set Resolver.servers')
	}
	return Resolver{
		servers:  conf.servers
		timeout:  conf.timeout
		attempts: conf.attempts
	}
}

// parse_resolv_conf reads the resolv.conf file at `path`: each `nameserver`
// becomes a `host:port` server (IPv6 hosts in brackets), and the `timeout:n`
// and `attempts:n` options set the timeout and the attempts. Names are queried
// as given. A file that lists no nameserver gives an error.
pub fn parse_resolv_conf(path string) !ResolvConf {
	content := os.read_file(path)!
	mut servers := []string{}
	mut timeout := default_timeout
	mut attempts := default_attempts
	for raw in content.split('\n') {
		// A `#` or `;` starts a comment, at the start of a line or after a value.
		comment := raw.index_any('#;')
		line := if comment >= 0 { raw[..comment] } else { raw }
		fields := line.fields()
		if fields.len < 2 {
			continue
		}
		match fields[0] {
			'nameserver' {
				servers << format_server(fields[1])
			}
			'options' {
				for opt in fields[1..] {
					// The values are capped as glibc does: 30 seconds, 5 attempts.
					if opt.starts_with('timeout:') {
						if n := option_value(opt, 30) {
							timeout = n * time.second
						}
					} else if opt.starts_with('attempts:') {
						if n := option_value(opt, 5) {
							attempts = n
						}
					}
				}
			}
			else {}
		}
	}
	if servers.len == 0 {
		return error('no nameserver directive in ${path}')
	}
	return ResolvConf{
		servers:  servers
		timeout:  timeout
		attempts: attempts
	}
}

// option_value reads the number of a `name:n` option, capped at `max`. It is
// none unless the value is a positive decimal number.
fn option_value(opt string, max int) ?int {
	value := opt.all_after(':')
	if value.len == 0 || value.len > 9 || !value.bytes().all(it.is_digit()) {
		return none
	}
	n := value.int()
	if n < 1 {
		return none
	}
	return if n > max { max } else { n }
}

// format_server appends the default DNS port if absent and brackets
// IPv6 addresses, producing the form "host:53" or "[v6]:53".
fn format_server(addr string) string {
	// Already has a port (and possibly brackets)? Leave it alone.
	if addr.starts_with('[') && addr.contains(']:') {
		return addr
	}
	// IPv6 literals contain multiple colons; brackets disambiguate the
	// port separator.
	if addr.count(':') > 1 {
		return '[${addr}]:53'
	}
	if addr.contains(':') {
		return addr
	}
	return '${addr}:53'
}

// query sends one question for `name` and `t`, and returns the whole response,
// so that a caller can read the authority and additional sections or tell
// NXDOMAIN from an empty answer. Each server is tried in order, `attempts`
// times over; a truncated UDP response is retried over TCP.
pub fn (mut r Resolver) query(name string, t RecordType) !Message {
	if r.servers.len == 0 {
		return error('net.dns: no servers configured')
	}
	// The transaction id is the only thing an off-path attacker has to guess,
	// with the source port, to forge a response: it comes from the system CSPRNG.
	id_bytes := rand.bytes(2)!
	txid := (u16(id_bytes[0]) << 8) | u16(id_bytes[1])
	question := Question{
		name:  name
		typ:   t
		class: .in_
	}
	q := Message{
		header:    Header{
			id:      txid
			rd:      true
			qdcount: 1
		}
		questions: [question]
	}
	query_bytes := encode(q)!

	mut last_err := error('net.dns: no attempts made')
	attempts := if r.attempts < 1 { 1 } else { r.attempts }
	for _ in 0 .. attempts {
		for server in r.servers {
			result := r.attempt_one(server, txid, question, query_bytes) or {
				last_err = err
				continue
			}
			return result
		}
	}
	return last_err
}

// attempt_one performs a single UDP exchange with the given server and,
// when the response has TC=1, a TCP follow-up. Only the header and question of
// a UDP response are decoded before TC is checked: a truncated response may
// not hold complete sections, and is replaced by the TCP one (RFC 2181 §9).
fn (mut r Resolver) attempt_one(server string, txid u16, question Question, query_bytes []u8) !Message {
	udp_resp := r.transport.exchange_udp(server, query_bytes, r.timeout)!
	head := decode_head(udp_resp)!
	check_response(head, txid, question)!
	if !head.header.tc {
		return decode(udp_resp)!
	}
	tcp_resp := r.transport.exchange_tcp(server, query_bytes, r.timeout)!
	msg := decode(tcp_resp)!
	check_response(msg, txid, question)!
	if msg.header.tc {
		return error('net.dns: the TCP response from ${server} is truncated too')
	}
	return msg
}

// check_response accepts a message only if it answers the query that was sent:
// same transaction id, QR set, a standard query, and the same single question.
// The values compared are the ones received, not their enum mapping.
fn check_response(msg Message, txid u16, question Question) ! {
	if msg.header.id != txid {
		return error('net.dns: response id 0x${msg.header.id.hex()} does not match query 0x${txid.hex()}')
	}
	if !msg.header.qr || msg.header.opcode_code != 0 {
		return error('net.dns: message is not a response to a standard query')
	}
	if msg.questions.len != 1 || msg.questions[0].type_code != u16(question.typ)
		|| msg.questions[0].class_code != u16(question.class)
		|| !names_equal(msg.questions[0].name, question.name) {
		return error('net.dns: response question does not match the query')
	}
}

// require_no_error promotes the response code into the V error channel
// so lookup_* helpers can be a single straight-line happy path. NOERROR
// with zero answers (e.g. a TXT query whose response was stripped by
// the resolver) is *not* an error: it is a valid empty result.
fn require_no_error(m Message) ! {
	match m.header.rcode {
		.no_error { return }
		.nx_domain { return error('NXDOMAIN') }
		.serv_fail { return error('SERVFAIL') }
		.refused { return error('REFUSED') }
		.form_err { return error('FORMERR') }
		.not_imp { return error('NOTIMP') }
		.unknown { return error('net.dns: response code ${m.header.rcode_code}') }
	}
}

// max_cname_hops bounds how many aliases are followed within one answer.
const max_cname_hops = 16

// answers_for returns the records of type `want` and class IN that the answer
// section holds for `name`, following the CNAME chain the section contains from
// `name`. Records for other owners are ignored. A chain that leaves the answer
// section gives no record: following it would take another query, which the
// lookup functions do not make.
fn answers_for(answers []ResourceRecord, name string, want RecordType) []ResourceRecord {
	mut owner := name
	mut out := []ResourceRecord{}
	for _ in 0 .. max_cname_hops {
		mut next := ''
		for rr in answers {
			if rr.class_code != u16(RecordClass.in_) || !names_equal(rr.name, owner) {
				continue
			}
			if rr.type_code == u16(want) {
				out << rr
			} else if rr.typ == .cname && want != .cname && next == '' {
				if rr.rdata is NameRdata {
					next = rr.rdata.name
				}
			}
		}
		if out.len > 0 || next == '' {
			break
		}
		owner = next
	}
	return out
}

// lookup_a returns the IPv4 addresses of `name`, in answer order, following the
// aliases that the answer contains.
pub fn (mut r Resolver) lookup_a(name string) ![]string {
	m := r.query(name, .a)!
	require_no_error(m)!
	return addrs_of(answers_for(m.answers, name, .a))
}

// lookup_aaaa returns the IPv6 addresses of `name`, in RFC 5952 form, following
// the aliases that the answer contains.
pub fn (mut r Resolver) lookup_aaaa(name string) ![]string {
	m := r.query(name, .aaaa)!
	require_no_error(m)!
	return addrs_of(answers_for(m.answers, name, .aaaa))
}

fn addrs_of(records []ResourceRecord) []string {
	mut out := []string{}
	for rr in records {
		if rr.rdata is AddrRdata {
			out << rr.rdata.addr
		}
	}
	return out
}

fn names_of(records []ResourceRecord) []string {
	mut out := []string{}
	for rr in records {
		if rr.rdata is NameRdata {
			out << rr.rdata.name
		}
	}
	return out
}

// lookup_mx returns the mail exchangers of `name`, by increasing preference.
pub fn (mut r Resolver) lookup_mx(name string) ![]MxRdata {
	m := r.query(name, .mx)!
	require_no_error(m)!
	mut out := []MxRdata{}
	for rr in answers_for(m.answers, name, .mx) {
		if rr.rdata is MxRdata {
			out << rr.rdata
		}
	}
	out.sort(a.preference < b.preference)
	return out
}

// lookup_txt returns the TXT records of `name`, each as its list of strings.
pub fn (mut r Resolver) lookup_txt(name string) ![][]string {
	m := r.query(name, .txt)!
	require_no_error(m)!
	mut out := [][]string{}
	for rr in answers_for(m.answers, name, .txt) {
		if rr.rdata is TxtRdata {
			out << rr.rdata.strings
		}
	}
	return out
}

// lookup_ns returns the name servers of `name`.
pub fn (mut r Resolver) lookup_ns(name string) ![]string {
	m := r.query(name, .ns)!
	require_no_error(m)!
	return names_of(answers_for(m.answers, name, .ns))
}

// lookup_cname returns the canonical name `name` is an alias of.
pub fn (mut r Resolver) lookup_cname(name string) !string {
	m := r.query(name, .cname)!
	require_no_error(m)!
	names := names_of(answers_for(m.answers, name, .cname))
	if names.len == 0 {
		return error('net.dns: no CNAME record for ${name}')
	}
	return names[0]
}

// lookup_ptr returns the names of the IPv4 address `addr`, or of an
// `in-addr.arpa` / `ip6.arpa` name.
pub fn (mut r Resolver) lookup_ptr(addr string) ![]string {
	arpa := reverse_arpa(addr)!
	m := r.query(arpa, .ptr)!
	require_no_error(m)!
	return names_of(answers_for(m.answers, arpa, .ptr))
}

// reverse_arpa builds the in-addr.arpa name of an IPv4 address. A name that is
// already under in-addr.arpa or ip6.arpa is returned as is.
fn reverse_arpa(addr string) !string {
	lower := addr.to_lower().trim_right('.')
	if lower.ends_with('.in-addr.arpa') || lower.ends_with('.ip6.arpa') {
		return addr
	}
	if addr.contains(':') {
		return error('net.dns: reverse lookup of IPv6 addresses is not supported; pass an ip6.arpa name')
	}
	parts := addr.split('.')
	if parts.len != 4 {
		return error('net.dns: ${addr} is not an IPv4 address')
	}
	for part in parts {
		if part.len == 0 || part.len > 3 || !part.bytes().all(it.is_digit()) || part.int() > 255 {
			return error('net.dns: ${addr} is not an IPv4 address')
		}
	}
	return '${parts[3].int()}.${parts[2].int()}.${parts[1].int()}.${parts[0].int()}.in-addr.arpa'
}

// lookup_srv returns the SRV records of `name`, such as `_imaps._tcp.example.com`.
pub fn (mut r Resolver) lookup_srv(name string) ![]SrvRdata {
	m := r.query(name, .srv)!
	require_no_error(m)!
	mut out := []SrvRdata{}
	for rr in answers_for(m.answers, name, .srv) {
		if rr.rdata is SrvRdata {
			out << rr.rdata
		}
	}
	return out
}

// lookup_soa returns the start of authority record of the zone `name`.
pub fn (mut r Resolver) lookup_soa(name string) !SoaRdata {
	m := r.query(name, .soa)!
	require_no_error(m)!
	for rr in answers_for(m.answers, name, .soa) {
		if rr.rdata is SoaRdata {
			return rr.rdata
		}
	}
	return error('net.dns: no SOA record for ${name}')
}

// lookup_caa returns the certification authority authorization records of `name`.
pub fn (mut r Resolver) lookup_caa(name string) ![]CaaRdata {
	m := r.query(name, .caa)!
	require_no_error(m)!
	mut out := []CaaRdata{}
	for rr in answers_for(m.answers, name, .caa) {
		if rr.rdata is CaaRdata {
			out << rr.rdata
		}
	}
	return out
}
