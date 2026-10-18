## Description

`net` provides networking functions. It is mostly a wrapper to BSD sockets,
so you can listen on a port, connect to remote TCP/UDP services, and
communicate with them.

Blocking TCP reads on POSIX sockets use `SO_RCVTIMEO` to enforce the configured
read timeout directly in `recv`, avoiding a readiness poll before each read.
`set_read_timeout` updates this socket timeout; `no_timeout` and `infinite_timeout`
disable it. Deadline-only reads and nonblocking reads continue to use readiness
polling, as do sockets that do not support the receive-timeout option.

A deadline set with `set_read_deadline`, `set_write_deadline` or `set_accept_deadline` takes
precedence over the matching timeout for as long as it is set: a UDP read with a 400 ms deadline
waits 400 ms even under the 100 ms default timeout, and a TCP accept, whose timeout is infinite by
default, ends at its deadline. Set the deadline back to `time.Time{}` or `time.unix(0)` to return
to the timeout. These are the only unset deadline values; other timestamps, including those
within the first second of 1970, expire when they are in the past. The same rules apply to
`net.unix` waits. With an unset deadline and `no_timeout` or `infinite_timeout`, a wait has no
time limit.

A deadline applies where an operation waits before its system call: the
`wait_for_read`/`wait_for_write`/`wait_for_accept` methods, and in the default build TCP reads and
accepts and UDP reads. In the default build, writes (TCP, UDP and Unix) and Unix stream reads and
accepts call `send`/`recv`/`accept` first and block there, where no deadline reaches them. Builds
with `-d net_nonblocking_sockets` or `-use-coroutines` take other paths.

Networking APIs support `-autofree`. Invalid-port and timeout results own their error values,
while preserving the messages and codes of the corresponding `net.err_*` constants.

UDP multicast example:

```v
import net

mut socket := net.listen_udp('0.0.0.0:9999')!
socket.join_multicast_group('224.0.0.1', '0.0.0.0')!
socket.set_multicast_ttl(2)!
socket.set_multicast_loop(true)!
```

Addresses print as `host:port`, with IPv6 bracketed and rendered per RFC 5952:
`127.0.0.1:8080`, `[2001:db8::1]:8080`. A scoped (link-local) address also
carries its RFC 4007 zone, as the numeric interface index the kernel reported:
`[fe80::1%3]:8080`. Both `split_address` and `dial_tcp` accept that form, so an
address read off a socket with `peer_addr()` stays usable as a dial target.

`lookup_host(name)` resolves a host name to IPv4 and IPv6 address strings in resolver order.
`resolve_ip(name)` returns the first address. Numeric literals are accepted, including bare or
bracketed IPv6, and results omit brackets and ports. Pass only a host name: `host:port` inputs,
malformed brackets, empty names, and names containing NUL bytes return errors. DNS results depend
on the host's resolver configuration and can contain more than one address.

## IPv4 subnets

`parse_cidr('192.168.1.77/24')!` creates an `IPNet` retaining the original address and
computing its masked `network()`, `broadcast()`, and `size()`. `contains('192.168.1.2')`
checks membership and returns false for malformed queries. Prefixes range from 0 to 32.
`parse_ip` and `format_ip` convert between dotted IPv4 addresses and `u32` values. Parsing
rejects invalid octets, leading zeroes, and numeric overflow. IPv6 CIDR is unsupported.

`resolve_ipaddrs` rejects an empty address with an error. Use `:port` for the IPv4 or IPv6
wildcard address, according to the selected address family.
