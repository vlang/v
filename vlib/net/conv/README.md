## Description

`net.conv` provides a convenient way to convert number values *to*,
and *from* the network byte order format (which is always big endian).

When communicating across a network, it is possible that the machines
use different byte orders, since the host format of each system can
vary, depending on the CPU, and on most systems, is usually
little endian.

To avoid mismatches due to that, the network byte order is used by
convention to send network data in a manner that will be received
coherently, regardless of the endianness of the sender system and
the receiver system.
