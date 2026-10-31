# textproto

`textproto` provides helpers for MIME-style text protocol headers, similar to
Go's `net/textproto`.

```v
import net.textproto

assert textproto.canonical_mime_header_key('content-type') == 'Content-Type'

h := textproto.read_mime_header('Content-Type: text/plain\r\nX-Custom: 1\r\n')
assert h['Content-Type'][0] == 'text/plain'
```
