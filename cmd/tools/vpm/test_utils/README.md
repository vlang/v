VPM fixtures use `cmd_ok_args(location, args)` and `cmd_fail_args(location, args)`
to execute literal arguments and check the expected exit status. The command string
versions are deprecated; paths and values belong in separate array elements.

`start_mock_http_server(handler)` serves an `http.Handler` on a private loopback port.
Send requests to its `base_url` and call `close()` in a `defer` block. The serving thread owns
its sockets; `close()` signals it and joins it before returning, including when no request
was sent. Shared state observed by a test, such as a request log, still needs synchronization.
