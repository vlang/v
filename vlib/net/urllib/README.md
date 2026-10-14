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

Parsing preserves the distinction between an omitted password and an explicitly empty password.
For `http://user:@example.com/`, `URL.user.password_set` is true and `URL.str()` retains `user:@`.
The same applies to an empty username: `http://:@example.com/` retains `:@` when serialized.
A percent-encoded colon in a username, such as `user%3Aname`, does not set a password unless a
literal colon follows it.

A URL with a single trailing `?` retains its empty query marker. Additional question marks
inside a nonempty query remain part of `raw_query`, including a question mark at its end.
