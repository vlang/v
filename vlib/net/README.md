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
