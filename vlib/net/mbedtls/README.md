# net.mbedtls

With `validate: true` and an empty `verify`, SSL clients load trusted CA certificates from
`SSL_CERT_FILE` when it names an existing file, or from the first available common system CA
bundle. Initialization fails if no bundle is found or the selected bundle cannot be loaded.
Set `verify` explicitly to select a CA file or in-memory CA certificates instead.
Loading a CA bundle does not configure a client certificate. Servers that request optional
client authentication can accept clients without `cert` and `cert_key`.

`SSLConn.shutdown()` also releases contexts created by `new_ssl_conn` before a successful
connection, including their ALPN configuration. Calling it again after cleanup returns the
existing `connection was not open` error without releasing resources a second time.

On Linux, TLS sends suppress SIGPIPE for each socket operation. A peer that closes
the connection produces a connection-reset error without terminating the process
or changing its SIGPIPE handler. Nonblocking backpressure remains retryable.

## Thread safety

`SSLConn` does not synchronize access to its TLS context. Serialize calls on the same
connection, including reads, writes, timeout changes, and shutdown. Concurrent calls from
one reader thread and one writer thread are unsupported. A blocking read held under a
mutex can prevent the writer from progressing; the mutex alone is not a duplex adapter.

Keep a connected `SSLConn` at a stable address and stop all users before shutdown.
See [`net.ssl` thread safety](../ssl/README.md#thread-safety) for retry and lifetime guidance.
