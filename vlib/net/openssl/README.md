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
