# net.ssl

`net.ssl` selects the default Mbed TLS backend, or OpenSSL when built with `-d use_openssl`.

## Thread safety

`SSLConn` does not synchronize access to its TLS context. Serialize all calls on the same
connection, including `read`, `write`, timeout changes, and `close`/`shutdown`. This applies
with both the default Mbed TLS backend and OpenSSL (`-d use_openssl`): one thread reading
while another writes to the same connection is unsupported.

A mutex around individual calls prevents overlapping TLS operations, but a blocking read
can then prevent a writer from making progress. It does not by itself provide independent
reader and writer threads. TLS reads can need to write, writes can need to read, and the
TLS engine may already have buffered data that socket readiness checks cannot see.
A full duplex adapter must also preserve backend retry rules, enforce deadlines, handle
fatal errors, and coordinate shutdown before releasing the connection. `net.ssl` currently
does not provide such a public adapter.

Keep a connected `SSLConn` at a stable address: do not copy it after a handshake, because
backend callbacks can retain pointers into the connection. Stop all users of a connection
before closing it or releasing its storage. After a failed write, bytes may already have
reached the peer; blindly replaying the buffer can duplicate application data.

See the backend documentation on [Mbed TLS thread safety][mbedtls-threads] and
[OpenSSL thread safety][openssl-threads].

[mbedtls-threads]:
  https://mbed-tls.readthedocs.io/en/latest/kb/development/thread-safety-and-multi-threading/
[openssl-threads]: https://docs.openssl.org/3.5/man7/openssl-threads/
