module net

import encoding.binary

// DNS type codes.
pub const dns_type_a = u16(1)
pub const dns_type_ns = u16(2)
pub const dns_type_cname = u16(5)
pub const dns_type_ptr = u16(12)
pub const dns_type_mx = u16(15)
pub const dns_type_txt = u16(16)
pub const dns_type_aaaa = u16(28)
pub const dns_type_srv = u16(33)

// DNS class IN (internet).
pub const dns_class_in = u16(1)

// DnsA is an A record address.
pub struct DnsA {
pub:
	ip string
}

// DnsAaaa is an AAAA record address.
pub struct DnsAaaa {
pub:
	ip string
}

// DnsName is a single-domain-name record (CNAME, NS, PTR).
pub struct DnsName {
pub:
	name string
}

// DnsMx is an MX record.
pub struct DnsMx {
pub:
	pref u16
	host string
}

// DnsTxt is a TXT record; strings are the record's character-strings.
pub struct DnsTxt {
pub:
	strings []string
}

// DnsSrv is an SRV record.
pub struct DnsSrv {
pub:
	priority u16
	weight   u16
	port     u16
	target   string
}

// DnsRData is the decoded body of a resource record.
pub type DnsRData = DnsA | DnsAaaa | DnsMx | DnsName | DnsSrv | DnsTxt

// DnsRecord is one decoded resource record.
pub struct DnsRecord {
pub:
	name string
	typ  u16
	ttl  u32
	data DnsRData
}

// DnsResponse is a decoded DNS message with its answer records.
pub struct DnsResponse {
pub:
	id      u16
	answers []DnsRecord
}

// encode_dns_name encodes `name` as DNS labels. A trailing dot is
// optional; the root is encoded as a single zero byte.
pub fn encode_dns_name(name string) ![]u8 {
	trimmed := name.trim_space().trim_right('.')
	if trimmed == '' {
		return [u8(0)]
	}
	mut out := []u8{}
	for label in trimmed.split('.') {
		if label.len == 0 || label.len > 63 {
			return error('net: invalid DNS label in `${name}`')
		}
		if out.len + 1 + label.len + 1 > 255 {
			return error('net: DNS name too long `${name}`')
		}
		out << u8(label.len)
		out << label.bytes()
	}
	out << u8(0)
	return out
}

// build_dns_query builds a minimal recursive DNS query for `name`/`qtype`.
pub fn build_dns_query(id u16, name string, qtype u16) ![]u8 {
	qname := encode_dns_name(name)!
	mut pkt := []u8{cap: 12 + qname.len + 4}
	pkt << binary.big_endian_get_u16(id)
	// flags: recursion desired
	pkt << binary.big_endian_get_u16(0x0100)
	// qdcount = 1, ancount = nscount = arcount = 0
	pkt << binary.big_endian_get_u16(1)
	pkt << binary.big_endian_get_u16(0)
	pkt << binary.big_endian_get_u16(0)
	pkt << binary.big_endian_get_u16(0)
	pkt << qname
	pkt << binary.big_endian_get_u16(qtype)
	pkt << binary.big_endian_get_u16(dns_class_in)
	return pkt
}

// decode_dns_name decodes a possibly compressed domain name at `pos`.
// It returns the name and the offset just past the name (or pointer).
pub fn decode_dns_name(pkt []u8, pos int) !(string, int) {
	mut parts := []string{}
	mut cur := pos
	mut jumped := false
	mut end := pos
	mut depth := 0
	for {
		if depth > 128 {
			return error('net: DNS name pointer loop')
		}
		depth++
		if cur >= pkt.len {
			return error('net: truncated DNS name')
		}
		b := pkt[cur]
		if b & 0xc0 == 0xc0 {
			if cur + 1 >= pkt.len {
				return error('net: truncated DNS pointer')
			}
			target := int(u16(b & 0x3f) << 8 | u16(pkt[cur + 1]))
			if target >= pkt.len {
				return error('net: DNS pointer out of range')
			}
			if !jumped {
				end = cur + 2
			}
			cur = target
			jumped = true
			continue
		}
		if b & 0xc0 != 0 {
			return error('net: bad DNS label bits')
		}
		if b == 0 {
			if !jumped {
				end = cur + 1
			}
			break
		}
		if cur + 1 + int(b) > pkt.len {
			return error('net: truncated DNS label')
		}
		parts << pkt[cur + 1..cur + 1 + int(b)].bytestr()
		cur += 1 + int(b)
		if !jumped {
			end = cur
		}
	}
	return parts.join('.'), end
}

// parse_dns_response decodes a DNS response message, checking the id,
// the response and error bits, and the answer count.
pub fn parse_dns_response(pkt []u8, want_id u16) !DnsResponse {
	if pkt.len < 12 {
		return error('net: truncated DNS header')
	}
	id := binary.big_endian_u16(pkt[..2])
	if id != want_id {
		return error('net: DNS id mismatch')
	}
	flags := binary.big_endian_u16(pkt[2..4])
	if flags & 0x8000 == 0 {
		return error('net: not a DNS response')
	}
	rcode := flags & 0x000f
	if rcode != 0 {
		return error('net: DNS rcode ${rcode}')
	}
	qdcount := int(binary.big_endian_u16(pkt[4..6]))
	ancount := int(binary.big_endian_u16(pkt[6..8]))
	mut pos := 12
	// Skip the question section.
	for _ in 0 .. qdcount {
		_, next := decode_dns_name(pkt, pos)!
		pos = next
		if pos + 4 > pkt.len {
			return error('net: truncated DNS question')
		}
		pos += 4
	}
	mut answers := []DnsRecord{}
	for _ in 0 .. ancount {
		name, next := decode_dns_name(pkt, pos)!
		pos = next
		if pos + 10 > pkt.len {
			return error('net: truncated DNS answer')
		}
		typ := binary.big_endian_u16(pkt[pos..pos + 2])
		ttl := binary.big_endian_u32(pkt[pos + 4..pos + 8])
		rdlen := int(binary.big_endian_u16(pkt[pos + 8..pos + 10]))
		pos += 10
		if pos + rdlen > pkt.len {
			return error('net: truncated DNS rdata')
		}
		data := decode_dns_rdata(pkt, pos, rdlen, typ)!
		answers << DnsRecord{ name: name, typ: typ, ttl: ttl, data: data }
		pos += rdlen
	}
	return DnsResponse{ id: id, answers: answers }
}

fn decode_dns_rdata(pkt []u8, off int, rdlen int, typ u16) !DnsRData {
	rdata := pkt[off..off + rdlen]
	match typ {
		dns_type_a {
			if rdata.len != 4 {
				return error('net: bad A rdata length')
			}
			return DnsRData(DnsA{ ip: '${rdata[0]}.${rdata[1]}.${rdata[2]}.${rdata[3]}' })
		}
		dns_type_aaaa {
			if rdata.len != 16 {
				return error('net: bad AAAA rdata length')
			}
			ip := canonical_ipv6_from_bytes(rdata)!
			return DnsRData(DnsAaaa{ ip: ip })
		}
		dns_type_cname, dns_type_ns, dns_type_ptr {
			// Names in rdata may use compression against the packet.
			name, _ := decode_dns_name(pkt, off)!
			return DnsRData(DnsName{ name: name })
		}
		dns_type_mx {
			if rdata.len < 3 {
				return error('net: bad MX rdata length')
			}
			pref := binary.big_endian_u16(rdata[..2])
			host, _ := decode_dns_name(pkt, off + 2)!
			return DnsRData(DnsMx{ pref: pref, host: host })
		}
		dns_type_txt {
			mut strings_ := []string{}
			mut p := 0
			for p < rdata.len {
				n := int(rdata[p])
				p++
				if p + n > rdata.len {
					return error('net: truncated TXT rdata')
				}
				strings_ << rdata[p..p + n].bytestr()
				p += n
			}
			return DnsRData(DnsTxt{ strings: strings_ })
		}
		dns_type_srv {
			if rdata.len < 7 {
				return error('net: bad SRV rdata length')
			}
			priority := binary.big_endian_u16(rdata[..2])
			weight := binary.big_endian_u16(rdata[2..4])
			port := binary.big_endian_u16(rdata[4..6])
			target, _ := decode_dns_name(pkt, off + 6)!
			return DnsRData(DnsSrv{ priority: priority, weight: weight, port: port, target: target })
		}
		else {
			return error('net: unsupported DNS type ${typ}')
		}
	}
}
