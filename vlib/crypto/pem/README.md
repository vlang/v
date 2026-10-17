# crypto.pem

`crypto.pem` encodes and decodes PEM blocks containing headers and a base64 body.

`Block.encode` uses LF line endings by default. Set `line_ending: '\r\n'` for CRLF output.
The `line_length` setting also controls the wrapping of header values.

Both `decode` and `decode_only` accept LF and CRLF header separators, including folded
header values whose continuation lines are indented, or follow a header value that is empty
(as `Block.encode` wraps long values) or ends with a comma (as in the RFC 1421 examples).
Invalid base64 bodies return `none`. `decode` also returns the remaining input after the
block without changing its line endings.
