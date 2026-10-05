## Description

`net.urllib` parses and assembles URLs and provides path and query escaping.

`URL.str()`, `URL.escaped_path()` and `URL.request_uri()` escape characters
that are invalid in a URL path, including backslashes. Parsing accepts a raw
backslash in the path; serializing that URL emits `%5C`.

```v
import net.urllib

u := urllib.parse(r'http://example.com/\/x')!
assert u.path == r'/\/x'
assert u.str() == 'http://example.com/%5C/x'
```

Valid existing percent encodings retain their spelling, such as `%5c`.
