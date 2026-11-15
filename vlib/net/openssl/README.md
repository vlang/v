On Linux, TLS socket writes suppress SIGPIPE for each send. A closed peer produces
an I/O error without changing the application's SIGPIPE handler. This also applies
to writes performed during TLS handshakes, reads and shutdown.

## Upgrade an accepted TCP connection to TLS

`new_ssl_server_conn(mut tcp_conn, config)` performs a server TLS handshake on an
existing `net.TcpConn`, using the certificate and key in `SSLConnectConfig`.
Use it for protocols such as SMTP STARTTLS that begin as plain TCP connections.
The returned `SSLConn` borrows the TCP socket: keep `tcp_conn` open while using TLS,
shut down the TLS connection, and then close `tcp_conn`.

Set `validate: true` and supply trusted client CA certificates in `verify` to
require a valid client certificate. Without `validate`, client authentication is
disabled. `in_memory_verification` makes `cert`, `cert_key`, and `verify` contain
PEM data instead of file paths. Missing or invalid trust roots fail the upgrade
when validation is enabled. Set `alpn_protocols` to the protocols the server may
select during the handshake; each protocol name must contain 1 to 255 bytes.

## Thread safety

`SSLConn` does not synchronize access to its TLS context. Serialize calls on the same
connection, including reads, writes, timeout changes, and shutdown. Concurrent calls from
one reader thread and one writer thread are unsupported. A blocking read held under a
mutex can prevent the writer from progressing; the mutex alone is not a duplex adapter.

Keep a connected `SSLConn` at a stable address and stop all users before shutdown.
See [`net.ssl` thread safety](../ssl/README.md#thread-safety) for retry and lifetime guidance.
