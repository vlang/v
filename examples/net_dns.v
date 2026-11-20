// A small `dig`: it queries one record type for a name with `net.dns`, and prints the records
// of the answer section.
//
// v run examples/net_dns.v example.com
// v run examples/net_dns.v mx gmail.com
// v run examples/net_dns.v -s 192.0.2.53 txt example.com
import net.dns
import os

fn main() {
	mut args := os.args[1..].clone()
	mut servers := []string{}
	for args.len > 0 && args[0] == '-s' {
		if args.len < 2 {
			usage()
		}
		servers << server_address(args[1])
		args = args[2..].clone()
	}
	if args.len == 0 || args.len > 2 {
		usage()
	}
	name := args.last()
	typ := if args.len == 2 { record_type(args[0]) or { usage() } } else { dns.RecordType.a }
	mut r := if servers.len > 0 {
		dns.Resolver{
			servers: servers
		}
	} else {
		dns.new_resolver() or {
			eprintln('${err}; pass a server with -s')
			exit(1)
		}
	}
	if typ == .ptr {
		// lookup_ptr turns an IPv4 address into its in-addr.arpa name.
		for host in r.lookup_ptr(name) or {
			eprintln(err)
			exit(1)
		} {
			println(host)
		}
		return
	}
	msg := r.query(name, typ) or {
		eprintln(err)
		exit(1)
	}
	println('${msg.header.rcode}, ${msg.answers.len} answer(s)')
	for rr in msg.answers {
		println('${rr.name}\t${rr.ttl}\t${rr.typ}\t${rdata_text(rr.rdata)}')
	}
}

@[noreturn]
fn usage() {
	eprintln('usage: net_dns [-s server] [a|aaaa|ns|cname|soa|ptr|mx|txt|srv|caa] name')
	exit(1)
}

// server_address adds the DNS port to a server given without one, and brackets
// a bare IPv6 address: `192.0.2.53`, `2001:db8::53` and `[2001:db8::53]:5353`
// all work.
fn server_address(s string) string {
	if s.starts_with('[') {
		return if s.contains(']:') { s } else { '${s}:53' }
	}
	if s.count(':') > 1 {
		return '[${s}]:53'
	}
	return if s.contains(':') { s } else { '${s}:53' }
}

fn record_type(s string) ?dns.RecordType {
	return match s.to_lower() {
		'a' { .a }
		'aaaa' { .aaaa }
		'ns' { .ns }
		'cname' { .cname }
		'soa' { .soa }
		'ptr' { .ptr }
		'mx' { .mx }
		'txt' { .txt }
		'srv' { .srv }
		'caa' { .caa }
		else { none }
	}
}

fn rdata_text(rdata dns.RData) string {
	return match rdata {
		dns.AddrRdata { rdata.addr }
		dns.NameRdata { rdata.name }
		dns.MxRdata { '${rdata.preference} ${rdata.exchange}' }
		dns.TxtRdata { rdata.strings.map('"${it}"').join(' ') }
		dns.SrvRdata { '${rdata.priority} ${rdata.weight} ${rdata.port} ${rdata.target}' }
		dns.SoaRdata {
			'${rdata.mname} ${rdata.rname} ${rdata.serial} ${rdata.refresh} ${rdata.retry} ${rdata.expire} ${rdata.minimum}'
		}
		dns.CaaRdata { '${rdata.flags} ${rdata.tag} "${rdata.value}"' }
		dns.OpaqueRdata { '${rdata.bytes.len} bytes' }
	}
}
