module dns

// RecordType lists the record types this module models (RFC 1035 and later).
// `unknown` stands for the other types, whose data is kept as OpaqueRdata.
pub enum RecordType {
	unknown = 0
	a       = 1
	ns      = 2
	cname   = 5
	soa     = 6
	ptr     = 12
	mx      = 15
	txt     = 16
	aaaa    = 28
	srv     = 33
	caa     = 257
}

// record_type_from_u16 maps a wire value to a RecordType, falling back
// to `.unknown` rather than producing an undefined enum value.
fn record_type_from_u16(v u16) RecordType {
	return match v {
		1 { RecordType.a }
		2 { RecordType.ns }
		5 { RecordType.cname }
		6 { RecordType.soa }
		12 { RecordType.ptr }
		15 { RecordType.mx }
		16 { RecordType.txt }
		28 { RecordType.aaaa }
		33 { RecordType.srv }
		257 { RecordType.caa }
		else { RecordType.unknown }
	}
}

fn record_class_from_u16(v u16) RecordClass {
	return match v {
		1 { RecordClass.in_ }
		2 { RecordClass.cs }
		3 { RecordClass.ch }
		4 { RecordClass.hs }
		else { RecordClass.unknown }
	}
}

fn opcode_from_u16(v u16) Opcode {
	return match v {
		0 { Opcode.query }
		1 { Opcode.iquery }
		2 { Opcode.status }
		4 { Opcode.notify }
		5 { Opcode.update }
		else { Opcode.unknown }
	}
}

fn rcode_from_u16(v u16) ResponseCode {
	return match v {
		0 { ResponseCode.no_error }
		1 { ResponseCode.form_err }
		2 { ResponseCode.serv_fail }
		3 { ResponseCode.nx_domain }
		4 { ResponseCode.not_imp }
		5 { ResponseCode.refused }
		else { ResponseCode.unknown }
	}
}

// RecordClass is the class of a record (RFC 1035 §3.2.4). Only IN is used in practice; others are
// historical (CSNET, CHAOS, Hesiod) but kept for completeness so that
// an unexpected class on the wire is still representable rather than
// silently coerced.
pub enum RecordClass {
	unknown = 0
	in_     = 1
	cs      = 2
	ch      = 3
	hs      = 4
}

// ResponseCode is the RCODE field of a response (RFC 1035 §4.1.1). Wider DNSSEC/EDNS rcodes are out of scope.
pub enum ResponseCode {
	no_error  = 0
	form_err  = 1
	serv_fail = 2
	nx_domain = 3
	not_imp   = 4
	refused   = 5
	// unknown stands for the other codes; `Header.rcode_code` holds the value.
	unknown = 16
}

// Opcode is the kind of query (RFC 1035 §4.1.1), with NOTIFY (RFC 1996) and
// UPDATE (RFC 2136), which are decoded as received.
pub enum Opcode {
	query  = 0
	iquery = 1
	status = 2
	notify = 4
	update = 5
	// unknown stands for the other opcodes; `Header.opcode_code` holds the value.
	unknown = 16
}

// Header is the fixed 12-byte header of a message (RFC 1035 §4.1.1).
pub struct Header {
pub:
	id     u16
	qr     bool
	opcode Opcode
	aa     bool
	tc     bool
	rd     bool
	ra     bool
	z      u8
	rcode  ResponseCode
	// opcode_code and rcode_code are the values as received, which give the
	// actual code when `opcode` or `rcode` is `.unknown`.
	opcode_code u8
	rcode_code  u8
	qdcount     u16
	ancount     u16
	nscount     u16
	arcount     u16
}

// Question is one entry of the question section: a name, a type and a class.
pub struct Question {
pub:
	name  string
	typ   RecordType
	class RecordClass = .in_
	// type_code and class_code are the values as received; they are set by
	// `decode`, and `typ` and `class` are what `encode` writes.
	type_code  u16
	class_code u16
}

// AddrRdata is the address of an A or AAAA record.
pub struct AddrRdata {
pub:
	addr string // dotted IPv4, or IPv6 in RFC 5952 form
}

// NameRdata is the domain name of an NS, CNAME or PTR record.
pub struct NameRdata {
pub:
	name string
}

// TxtRdata holds the character strings of a TXT record, in order.
pub struct TxtRdata {
pub:
	strings []string
}

// MxRdata is a mail exchanger and its preference; lower is preferred.
pub struct MxRdata {
pub:
	preference u16
	exchange   string
}

// SoaRdata is the start of authority record of a zone (RFC 1035 §3.3.13).
pub struct SoaRdata {
pub:
	mname   string
	rname   string
	serial  u32
	refresh i32
	retry   i32
	expire  i32
	minimum u32
}

// SrvRdata is a service location record (RFC 2782).
pub struct SrvRdata {
pub:
	priority u16
	weight   u16
	port     u16
	target   string
}

// CaaRdata is a certification authority authorization record (RFC 8659).
pub struct CaaRdata {
pub:
	flags u8
	tag   string
	value string
}

// OpaqueRdata holds the raw data of the other record types.
pub struct OpaqueRdata {
pub:
	bytes []u8
}

// RData is the data of a resource record, one variant per record shape.
pub type RData = AddrRdata
	| CaaRdata
	| MxRdata
	| NameRdata
	| OpaqueRdata
	| SoaRdata
	| SrvRdata
	| TxtRdata

// ResourceRecord is one record of the answer, authority or additional section.
pub struct ResourceRecord {
pub:
	name  string
	typ   RecordType
	class RecordClass
	ttl   u32
	rdata RData
	// type_code and class_code are the values as received, which give the
	// actual code when `typ` or `class` is `.unknown`.
	type_code  u16
	class_code u16
}

// Message is a whole DNS message: its header and its four sections.
pub struct Message {
pub:
	header      Header
	questions   []Question
	answers     []ResourceRecord
	authorities []ResourceRecord
	additionals []ResourceRecord
}
