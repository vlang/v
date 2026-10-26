## Upgrade an accepted TCP connection to TLS

`new_ssl_server_conn(mut tcp_conn, config)` performs a server TLS handshake on an
existing `net.TcpConn`, using the certificate and key in `SSLConnectConfig`.
Use it for protocols such as SMTP STARTTLS that begin as plain TCP connections.
The returned `SSLConn` borrows the TCP socket: keep `tcp_conn` open while using TLS,
shut down the TLS connection, and then close `tcp_conn`.
