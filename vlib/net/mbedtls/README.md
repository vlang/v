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
