# mime.multipart

`mime.multipart` splits an in-memory MIME multipart body into individual parts.
It preserves their wire order, repeated form field names, and raw body bytes,
including binary data and trailing whitespace.

```v
import mime.multipart

body := '--example\r\nContent-Disposition: form-data; name="message"\r\n\r\nhello\r\n--example--\r\n'
parts := multipart.parse(body, 'example')
assert parts.len == 1
assert parts[0].field_name == 'message'
assert parts[0].data == 'hello'
```

Pass the unquoted `boundary` parameter value from the outer Content-Type header.
Boundaries follow the MIME character restrictions and have a maximum length of
70 bytes. Delimiters must occur at the start of a line and use CRLF line endings.
Optional spaces and tabs after delimiters are accepted. Preamble and epilogue
are ignored. The closing delimiter can end at EOF without a final CRLF.

Each `Part` contains `field_name` and `file_name` from Content-Disposition,
`content_type` from Content-Type, and `data` without the delimiter's preceding
CRLF. Missing metadata is an empty string. Header names are case insensitive;
header order, folded headers, and quoted disposition parameters are supported.
Parts without Content-Disposition are also returned, with empty names.

The `parse` API returns an array rather than an error. An invalid boundary or a
body without a delimiter returns an empty array. A malformed header block or
incomplete part stops parsing; complete preceding parts remain in the result.
Do not use the returned part count alone to validate an entire message.

This module does not decode transfer encodings, charsets, encoded filename
parameters, or nested multipart bodies. Filenames are metadata; callers choose
how to validate them before using them as filesystem paths.

The framing follows [RFC 2046 section 5.1.1][mime]. Ordered form fields and
duplicate names follow [RFC 7578 section 5.2][forms].

[mime]: https://www.rfc-editor.org/rfc/rfc2046.html#section-5.1.1
[forms]: https://www.rfc-editor.org/rfc/rfc7578.html#section-5.2
