## Description

`net.dns` is a DNS client written in V. It builds queries and parses responses in the RFC 1035
wire format, sends them over UDP and retries over TCP when a response is truncated, and returns
typed records. It complements `net.lookup_host`, which returns addresses, with the other record
types.

Supported lookups: A, AAAA, NS, CNAME, SOA, PTR (IPv4 addresses, `in-addr.arpa` and `ip6.arpa`
names), MX, TXT, SRV and CAA. They return the records of class IN that belong to the name asked
for, following the aliases (CNAME) that the answer contains. `query` returns the whole response,
with its authority and additional sections. Records of other types are returned as
`OpaqueRdata`, and the type, class, opcode and response code values received are kept as
numbers alongside their enums.

```v
import net.dns

fn main() {
	mut r := dns.new_resolver() or {
		dns.Resolver{
			servers: ['192.0.2.53:53']
		}
	}
	for mx in r.lookup_mx('example.com') or { [] } {
		println('${mx.preference} ${mx.exchange}')
	}
}
```

## Configuration

`new_resolver` reads `/etc/resolv.conf`: its `nameserver` entries, and the `timeout:n` and
`attempts:n` options. Names are queried exactly as given. When the file lists no nameserver,
`new_resolver` returns an error, and the servers are set through `Resolver.servers`.

## Security

Each query uses a transaction id from the system CSPRNG. The UDP transport checks the source
address and port, the transaction id and the QR bit of each datagram it receives, and returns
the first one that matches within the timeout. A response is then accepted when it is a standard
query response that repeats the question asked, with the same name, type and class; otherwise
the next server is tried. A truncated response is replaced by the TCP one before its sections
are read. The timeout bounds the reception of a UDP response, and the reads of each TCP response
share one timeout budget.

The decoder bounds every name to 255 octets, rejects compression loops, follows at most 127
pointers per name, checks that each record's data is consumed exactly, and caps preallocations
to what the message can hold. Names are returned in presentation form, as in zone files: a `.`
or `\` inside a label is escaped as `\.` or `\\`, and other non-printable bytes as `\DDD`; the
root is the empty string. `query` accepts the same form.

## Testing

`v test vlib/net/dns/` runs the tests against captured responses and local sockets. Tests
against real servers run when `DNS_NETWORK_TESTS=1` is set.
