## Description

`net` provides networking functions. It is mostly a wrapper to BSD sockets,
so you can listen on a port, connect to remote TCP/UDP services, and
communicate with them.

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
