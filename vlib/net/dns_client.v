module net

import os
import time

// DnsClient queries a DNS server at `server` ("ip" or "ip:port").
pub struct DnsClient {
pub:
	server string
}

// new_dns_client returns a client for the given server address.
pub fn new_dns_client(server string) DnsClient {
	return DnsClient{ server: server }
}

// system_dns_client returns a client for the host resolver. On Unix it
// reads the first nameserver from /etc/resolv.conf; elsewhere it
// reports an error and the caller should use new_dns_client.
pub fn system_dns_client() !DnsClient {
	$if windows {
		return error('net: no system resolver support on Windows, use new_dns_client')
	} $else {
		data := os.read_file('/etc/resolv.conf') or {
			return error('net: cannot read /etc/resolv.conf: ${err.msg()}')
		}
		for line in data.split_into_lines() {
			trimmed := line.trim_space()
			if trimmed.starts_with('nameserver') {
				mut parts := []string{}
				for p in trimmed.split(' ') {
					if p != '' {
						parts << p
					}
				}
				if parts.len >= 2 {
					return DnsClient{ server: parts[1] }
				}
			}
		}
		return error('net: no nameserver in /etc/resolv.conf')
	}
}

// query sends one query and decodes the response.
pub fn (c DnsClient) query(name string, qtype u16) !DnsResponse {
	if name.len == 0 {
		return error('net: empty DNS name')
	}
	if name.contains('\x00') {
		return error('net: DNS name contains a NUL byte')
	}
	server := if c.server.contains(':') { c.server } else { '${c.server}:53' }
	// Query id from time; collisions across concurrent clients are
	// harmless because the id is checked on the way back.
	id := u16(time.now().unix_nano() & 0xffff)
	req := build_dns_query(id, name, qtype)!
	mut conn := dial_udp(server)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(2 * time.second)
	conn.write(req)!
	mut buf := []u8{len: 4096}
	n, _ := conn.read(mut buf)!
	if n >= 2 && buf[2] & 0x02 != 0 {
		return error('net: truncated DNS response, TCP retry not supported')
	}
	return parse_dns_response(buf[..n], id)!
}

// lookup_mx returns the MX records for `name`, sorted by preference.
pub fn (c DnsClient) lookup_mx(name string) ![]DnsMx {
	resp := c.query(name, dns_type_mx)!
	mut out := []DnsMx{}
	for rr in resp.answers {
		if rr.typ != dns_type_mx {
			continue
		}
		match rr.data {
			DnsMx {
				out << rr.data
			}
			else {}
		}
	}
	out.sort(a.pref < b.pref)
	return out
}

// lookup_txt returns the TXT strings for `name`.
pub fn (c DnsClient) lookup_txt(name string) ![]string {
	resp := c.query(name, dns_type_txt)!
	mut out := []string{}
	for rr in resp.answers {
		if rr.typ != dns_type_txt {
			continue
		}
		match rr.data {
			DnsTxt {
				out << rr.data.strings
			}
			else {}
		}
	}
	return out
}

// lookup_srv returns the SRV records for service/proto/name,
// e.g. service "xmpp-server", proto "tcp". The queried name is
// "_service._proto.name" per RFC 2782.
pub fn (c DnsClient) lookup_srv(service string, proto string, name string) ![]DnsSrv {
	resp := c.query('_${service}._${proto}.${name}', dns_type_srv)!
	mut out := []DnsSrv{}
	for rr in resp.answers {
		if rr.typ != dns_type_srv {
			continue
		}
		match rr.data {
			DnsSrv {
				out << rr.data
			}
			else {}
		}
	}
	return out
}

// lookup_mx resolves MX records with the system resolver.
pub fn lookup_mx(name string) ![]DnsMx {
	return system_dns_client()!.lookup_mx(name)
}

// lookup_txt resolves TXT records with the system resolver.
pub fn lookup_txt(name string) ![]string {
	return system_dns_client()!.lookup_txt(name)
}

// lookup_srv resolves SRV records with the system resolver.
pub fn lookup_srv(service string, proto string, name string) ![]DnsSrv {
	return system_dns_client()!.lookup_srv(service, proto, name)
}
