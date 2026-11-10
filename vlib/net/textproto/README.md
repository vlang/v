# textproto

`textproto` provides helpers for MIME-style text protocol headers, similar to
Go's `net/textproto`.

`canonical_mime_header_key` changes only valid ASCII field names. Empty names or names
containing spaces, control bytes, or other invalid field-name bytes are returned unchanged.

`read_mime_header` accepts CRLF or LF line endings and stops at the first empty line.
Repeated names share a canonical key and retain their value order. A line beginning with
a space or tab continues the previous valid field. Only spaces and tabs are trimmed from
each value line; other non-ASCII bytes are preserved. Folding an empty initial value does
not add leading whitespace.

Parsing is lenient: malformed fields and orphan continuations are skipped, and a malformed
line ends the previous field's continuation. Field names must be nonempty ASCII tokens;
values may contain tabs but no other control bytes. Unlike Go's `ReadMIMEHeader`, this helper
returns only a map and does not report parsing errors.

```v
import net.textproto

assert textproto.canonical_mime_header_key('content-type') == 'Content-Type'

h := textproto.read_mime_header('Content-Type: text/plain\r\nX-Custom: 1\r\n')
assert h['Content-Type'][0] == 'text/plain'
```
