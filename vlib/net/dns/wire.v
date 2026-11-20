module dns

import net
import strings

// RFC 1035 wire-format encoder and decoder.
//
// The encoder produces queries only: a header and a question section, which
// is all a resolver sends.
//
// The decoder walks the buffer forward with a structural cursor (`pos`) and,
// for each name, a second cursor that may follow compression pointers
// elsewhere in the buffer (RFC 1035 §4.1.4). Visited offsets stop pointer
// loops; the 255-octet name limit and a cap of 127 jumps, chosen as a resource
// limit, bound the work per name.

const max_label_len = 63
const max_name_len = 255
const header_size = 12

// ---------- Encode ----------

// encode serializes a query: the header and the question section.
pub fn encode(msg Message) ![]u8 {
	if msg.answers.len > 0 || msg.authorities.len > 0 || msg.additionals.len > 0 {
		return error('net.dns: only questions can be encoded')
	}
	mut out := []u8{cap: header_size + 32}
	encode_header(mut out, msg.header, u16(msg.questions.len), 0, 0, 0)
	for q in msg.questions {
		encode_name(mut out, q.name)!
		write_u16(mut out, u16(q.typ))
		write_u16(mut out, u16(q.class))
	}
	return out
}

fn encode_header(mut out []u8, h Header, qd u16, an u16, ns u16, ar u16) {
	flags := pack_flags(h)
	write_u16(mut out, h.id)
	write_u16(mut out, flags)
	write_u16(mut out, qd)
	write_u16(mut out, an)
	write_u16(mut out, ns)
	write_u16(mut out, ar)
}

fn pack_flags(h Header) u16 {
	mut f := u16(0)
	if h.qr {
		f |= 0x8000
	}
	f |= (u16(h.opcode) & 0x0F) << 11
	if h.aa {
		f |= 0x0400
	}
	if h.tc {
		f |= 0x0200
	}
	if h.rd {
		f |= 0x0100
	}
	if h.ra {
		f |= 0x0080
	}
	f |= (u16(h.z) & 0x07) << 4
	f |= u16(h.rcode) & 0x0F
	return f
}

fn encode_name(mut out []u8, name string) ! {
	labels := name_labels(name)!
	for label in labels {
		out << u8(label.len)
		out << label
	}
	out << u8(0)
}

// name_labels splits a name in presentation form into its labels, undoing the
// `\.`, `\\` and `\DDD` escapes that `decode` produces. The root is `.` or the
// empty string; a final `.` is optional. Empty labels, labels longer than 63
// octets and names longer than 255 octets are rejected.
fn name_labels(name string) ![][]u8 {
	if name == '' || name == '.' {
		return [][]u8{}
	}
	mut labels := [][]u8{}
	mut label := []u8{}
	mut total := 1 // the root octet
	mut i := 0
	for i < name.len {
		c := name[i]
		if c == `.` {
			if label.len == 0 {
				return error('net.dns: empty label in `${name}`')
			}
			labels << label
			total += 1 + label.len
			label = []u8{}
			i++
			if i == name.len {
				break
			}
			continue
		}
		mut b := c
		if c == `\\` {
			if i + 1 >= name.len {
				return error('net.dns: dangling escape in `${name}`')
			}
			if name[i + 1].is_digit() {
				if i + 3 >= name.len || !name[i + 2].is_digit() || !name[i + 3].is_digit() {
					return error('net.dns: invalid \\DDD escape in `${name}`')
				}
				v := name[i + 1..i + 4].int()
				if v > 255 {
					return error('net.dns: invalid \\DDD escape in `${name}`')
				}
				b = u8(v)
				i += 3
			} else {
				b = name[i + 1]
				i++
			}
		}
		label << b
		if label.len > max_label_len {
			return error('net.dns: label exceeds 63 octets in `${name}`')
		}
		i++
	}
	if label.len > 0 {
		labels << label
		total += 1 + label.len
	}
	if total > max_name_len {
		return error('net.dns: name exceeds 255 octets')
	}
	return labels
}

// names_equal compares two names in presentation form the way DNS does: label
// by label, ASCII case-insensitively, with or without the final root dot.
fn names_equal(a string, b string) bool {
	la := name_labels(a) or { return false }
	lb := name_labels(b) or { return false }
	if la.len != lb.len {
		return false
	}
	for i, x in la {
		y := lb[i]
		if x.len != y.len {
			return false
		}
		for k in 0 .. x.len {
			if ascii_lower(x[k]) != ascii_lower(y[k]) {
				return false
			}
		}
	}
	return true
}

fn ascii_lower(b u8) u8 {
	return if b >= `A` && b <= `Z` { b + 32 } else { b }
}

fn write_u16(mut out []u8, v u16) {
	out << u8(v >> 8)
	out << u8(v & 0xFF)
}

// ---------- Decode ----------

struct Decoder {
	buf []u8
mut:
	pos int
}

// decode parses a whole DNS message. Names are returned in presentation form:
// labels joined by `.`, with a `.` or `\` inside a label escaped as `\.` or
// `\\`, and non-printable bytes as `\DDD`, as in zone files.
pub fn decode(buf []u8) !Message {
	if buf.len < header_size {
		return error('net.dns: short header (${buf.len} < ${header_size})')
	}
	mut d := Decoder{
		buf: buf
		pos: 0
	}
	header := d.read_header()!
	questions := d.read_questions(int(header.qdcount))!
	answers := d.read_rrs(int(header.ancount))!
	authorities := d.read_rrs(int(header.nscount))!
	additionals := d.read_rrs(int(header.arcount))!
	return Message{
		header:      header
		questions:   questions
		answers:     answers
		authorities: authorities
		additionals: additionals
	}
}

// decode_head parses only the header and the question section of a message,
// which is enough to tell whether it answers a query, and whether it was
// truncated, before the rest is decoded.
pub fn decode_head(buf []u8) !Message {
	if buf.len < header_size {
		return error('net.dns: short header (${buf.len} < ${header_size})')
	}
	mut d := Decoder{
		buf: buf
	}
	header := d.read_header()!
	return Message{
		header:    header
		questions: d.read_questions(int(header.qdcount))!
	}
}

// capacity bounds a preallocation by what the rest of the buffer can hold, so
// that a count in the header cannot make the decoder allocate on its own.
fn (d &Decoder) capacity(count int, min_size int) int {
	fits := (d.buf.len - d.pos) / min_size
	return if count < fits { count } else { fits }
}

fn (mut d Decoder) read_questions(n int) ![]Question {
	// A question takes at least 5 octets: the root name, a type and a class.
	mut out := []Question{cap: d.capacity(n, 5)}
	for _ in 0 .. n {
		out << d.read_question()!
	}
	return out
}

fn (mut d Decoder) read_header() !Header {
	id := d.read_u16()!
	flags := d.read_u16()!
	qd := d.read_u16()!
	an := d.read_u16()!
	ns := d.read_u16()!
	ar := d.read_u16()!
	return Header{
		id:          id
		qr:          flags & 0x8000 != 0
		opcode:      opcode_from_u16((flags >> 11) & 0x0F)
		opcode_code: u8((flags >> 11) & 0x0F)
		rcode_code:  u8(flags & 0x0F)
		aa:          flags & 0x0400 != 0
		tc:          flags & 0x0200 != 0
		rd:          flags & 0x0100 != 0
		ra:          flags & 0x0080 != 0
		z:           u8((flags >> 4) & 0x07)
		rcode:       rcode_from_u16(flags & 0x0F)
		qdcount:     qd
		ancount:     an
		nscount:     ns
		arcount:     ar
	}
}

fn (mut d Decoder) read_question() !Question {
	name := d.read_name()!
	qtype := d.read_u16()!
	qclass := d.read_u16()!
	return Question{
		name:       name
		typ:        record_type_from_u16(qtype)
		class:      record_class_from_u16(qclass)
		type_code:  qtype
		class_code: qclass
	}
}

fn (mut d Decoder) read_rrs(n int) ![]ResourceRecord {
	// A record takes at least 11 octets: the root name, then type, class, TTL
	// and data length.
	mut out := []ResourceRecord{cap: d.capacity(n, 11)}
	for _ in 0 .. n {
		out << d.read_rr()!
	}
	return out
}

fn (mut d Decoder) read_rr() !ResourceRecord {
	name := d.read_name()!
	rtype := d.read_u16()!
	rclass := d.read_u16()!
	ttl := d.read_u32()!
	rdlen := int(d.read_u16()!)
	if d.pos + rdlen > d.buf.len {
		return error('net.dns: rdata truncated (need ${rdlen} bytes at offset ${d.pos})')
	}
	rd_start := d.pos
	rdata := d.read_rdata(int(rtype), rd_start, rdlen)!
	// Make sure we are positioned right after the rdata even when the
	// rdata reader did not consume the whole region (e.g. unknown types).
	d.pos = rd_start + rdlen
	return ResourceRecord{
		name:       name
		typ:        record_type_from_u16(rtype)
		class:      record_class_from_u16(rclass)
		ttl:        ttl
		rdata:      rdata
		type_code:  rtype
		class_code: rclass
	}
}

fn (mut d Decoder) read_rdata(rtype int, off int, rdlen int) !RData {
	match rtype {
		1 {
			return decode_a(d.buf, off, rdlen)!
		}
		28 {
			return decode_aaaa(d.buf, off, rdlen)!
		}
		2, 5, 12 {
			// NS, CNAME, PTR — single name field.
			mut sub := Decoder{
				buf: d.buf
				pos: off
			}
			n := sub.read_name()!
			check_consumed(sub.pos, off, rdlen)!
			return NameRdata{
				name: n
			}
		}
		15 {
			return decode_mx(d.buf, off, rdlen)!
		}
		16 {
			return decode_txt(d.buf, off, rdlen)!
		}
		6 {
			return decode_soa(d.buf, off, rdlen)!
		}
		33 {
			return decode_srv(d.buf, off, rdlen)!
		}
		257 {
			return decode_caa(d.buf, off, rdlen)!
		}
		else {
			return OpaqueRdata{
				bytes: d.buf[off..off + rdlen].clone()
			}
		}
	}
}

fn decode_a(buf []u8, off int, rdlen int) !RData {
	if rdlen != 4 {
		return error('net.dns: A rdlength != 4 (got ${rdlen})')
	}
	return AddrRdata{
		addr: '${buf[off]}.${buf[off + 1]}.${buf[off + 2]}.${buf[off + 3]}'
	}
}

fn decode_aaaa(buf []u8, off int, rdlen int) !RData {
	if rdlen != 16 {
		return error('net.dns: AAAA rdlength != 16 (got ${rdlen})')
	}
	addr := net.canonical_ipv6_from_bytes(buf[off..off + 16])!
	return AddrRdata{
		addr: addr
	}
}

fn decode_mx(buf []u8, off int, rdlen int) !RData {
	if rdlen < 3 {
		return error('net.dns: MX rdlength < 3')
	}
	pref := (u16(buf[off]) << 8) | u16(buf[off + 1])
	mut sub := Decoder{
		buf: buf
		pos: off + 2
	}
	exch := sub.read_name()!
	check_consumed(sub.pos, off, rdlen)!
	return MxRdata{
		preference: pref
		exchange:   exch
	}
}

fn decode_txt(buf []u8, off int, rdlen int) !RData {
	// RFC 1035 §3.3.14: one or more character strings.
	if rdlen == 0 {
		return error('net.dns: TXT record without any string')
	}
	mut parts := []string{}
	mut i := 0
	for i < rdlen {
		ln := int(buf[off + i])
		i++
		if i + ln > rdlen {
			return error('net.dns: TXT string overruns rdata')
		}
		parts << buf[off + i..off + i + ln].bytestr()
		i += ln
	}
	return TxtRdata{
		strings: parts
	}
}

fn decode_soa(buf []u8, off int, rdlen int) !RData {
	if rdlen < 22 {
		return error('net.dns: SOA rdlength < 22')
	}
	mut sub := Decoder{
		buf: buf
		pos: off
	}
	mname := sub.read_name()!
	rname := sub.read_name()!
	if sub.pos + 20 != off + rdlen {
		return error('net.dns: SOA record data has the wrong length')
	}
	serial := sub.read_u32()!
	refresh := i32(sub.read_u32()!)
	retry := i32(sub.read_u32()!)
	expire := i32(sub.read_u32()!)
	minimum := sub.read_u32()!
	return SoaRdata{
		mname:   mname
		rname:   rname
		serial:  serial
		refresh: refresh
		retry:   retry
		expire:  expire
		minimum: minimum
	}
}

fn decode_srv(buf []u8, off int, rdlen int) !RData {
	if rdlen < 7 {
		return error('net.dns: SRV rdlength < 7')
	}
	prio := (u16(buf[off]) << 8) | u16(buf[off + 1])
	weight := (u16(buf[off + 2]) << 8) | u16(buf[off + 3])
	port := (u16(buf[off + 4]) << 8) | u16(buf[off + 5])
	mut sub := Decoder{
		buf: buf
		pos: off + 6
	}
	// RFC 2782 forbids compressing the target, but compressed targets are
	// accepted, as other resolvers do.
	target := sub.read_name()!
	check_consumed(sub.pos, off, rdlen)!
	return SrvRdata{
		priority: prio
		weight:   weight
		port:     port
		target:   target
	}
}

fn decode_caa(buf []u8, off int, rdlen int) !RData {
	if rdlen < 2 {
		return error('net.dns: CAA rdlength < 2')
	}
	flags := buf[off]
	tag_len := int(buf[off + 1])
	if 2 + tag_len > rdlen {
		return error('net.dns: CAA tag overruns rdata')
	}
	if tag_len == 0 {
		return error('net.dns: CAA record with an empty tag')
	}
	for b in buf[off + 2..off + 2 + tag_len] {
		if !b.is_alnum() {
			return error('net.dns: CAA tag is not alphanumeric (RFC 8659 §4.1)')
		}
	}
	tag := buf[off + 2..off + 2 + tag_len].bytestr()
	value := buf[off + 2 + tag_len..off + rdlen].bytestr()
	return CaaRdata{
		flags: flags
		tag:   tag
		value: value
	}
}

// check_consumed fails unless the fields read from the record data at `off`
// ended exactly at the end of the `rdlen` octets the record declares.
fn check_consumed(end int, off int, rdlen int) ! {
	if end != off + rdlen {
		return error('net.dns: record data length does not match its content')
	}
}

fn (mut d Decoder) read_u16() !u16 {
	if d.pos + 2 > d.buf.len {
		return error('net.dns: short read at offset ${d.pos}')
	}
	v := (u16(d.buf[d.pos]) << 8) | u16(d.buf[d.pos + 1])
	d.pos += 2
	return v
}

fn (mut d Decoder) read_u32() !u32 {
	if d.pos + 4 > d.buf.len {
		return error('net.dns: short read at offset ${d.pos}')
	}
	v := (u32(d.buf[d.pos]) << 24) | (u32(d.buf[d.pos + 1]) << 16) | (u32(d.buf[d.pos + 2]) << 8) | u32(d.buf[d.pos + 3])
	d.pos += 4
	return v
}

fn (mut d Decoder) read_name() !string {
	// Pointer following: we walk the buffer accumulating labels until
	// we hit a zero terminator. The first time we follow a pointer the
	// "structural" cursor (d.pos) is fixed past the pointer bytes; from
	// then on `cur` roams freely through the buffer to resolve the
	// chain. This matches the wire layout (a name's *encoding* ends at
	// the first pointer or the terminator, whichever comes first).
	mut labels := []string{}
	mut cur := d.pos
	mut followed := false
	mut visited := map[int]bool{}
	mut total_len := 1 // the root octet
	mut jumps := 0
	for {
		if cur >= d.buf.len {
			return error('net.dns: name overflows buffer at offset ${cur}')
		}
		ln := d.buf[cur]
		if ln == 0 {
			cur++
			if !followed {
				d.pos = cur
			}
			break
		}
		match ln & 0xC0 {
			0xC0 {
				if cur + 1 >= d.buf.len {
					return error('net.dns: pointer truncated at offset ${cur}')
				}
				ptr := int(((u16(ln) & 0x3F) << 8) | u16(d.buf[cur + 1]))
				if !followed {
					d.pos = cur + 2
					followed = true
				}
				// A resource limit rather than a format one: a pointer may lead to
				// another pointer without adding a label, so the name length alone
				// does not bound the number of jumps.
				jumps++
				if jumps > 127 {
					return error('net.dns: too many compression pointers at offset ${cur}')
				}
				if ptr in visited {
					return error('net.dns: pointer cycle at offset ${cur} -> ${ptr}')
				}
				visited[ptr] = true
				if ptr >= d.buf.len {
					return error('net.dns: pointer ${ptr} out of bounds')
				}
				cur = ptr
			}
			0 {
				cur++
				if cur + int(ln) > d.buf.len {
					return error('net.dns: label overruns buffer at offset ${cur}')
				}
				total_len += int(ln) + 1
				if total_len > max_name_len {
					return error('net.dns: name exceeds 255 octets')
				}
				labels << presentation_label(d.buf[cur..cur + int(ln)])
				cur += int(ln)
			}
			else {
				return error('net.dns: reserved label flags 0x${(ln & 0xC0).hex()} at offset ${cur}')
			}
		}
	}
	return labels.join('.')
}

// presentation_label renders one label so that the joined name is unambiguous:
// a `.` inside a label would otherwise read as a label separator.
fn presentation_label(raw []u8) string {
	mut plain := true
	for b in raw {
		if b == `.` || b == `\\` || b < 0x21 || b > 0x7e {
			plain = false
			break
		}
	}
	if plain {
		return raw.bytestr()
	}
	mut sb := strings.new_builder(raw.len + 8)
	for b in raw {
		if b == `.` || b == `\\` {
			sb.write_u8(`\\`)
			sb.write_u8(b)
		} else if b < 0x21 || b > 0x7e {
			sb.write_string('\\${int(b):03d}')
		} else {
			sb.write_u8(b)
		}
	}
	return sb.str()
}
