# net.mbedtls

`SSLConn.shutdown()` also releases contexts created by `new_ssl_conn` before a successful
connection, including their ALPN configuration. Calling it again after cleanup returns the
existing `connection was not open` error without releasing resources a second time.
