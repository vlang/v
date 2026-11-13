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

## Joining hosts and ports

`urllib.join_host_port(host, port)` constructs a `host:port` address. IPv6 literals and
scoped IPv6 literals are bracketed; existing brackets are preserved. The separator remains
when the port is empty, so an empty host and port produce `:`. Host spelling and zone
identifiers are unchanged.

```v
import net.urllib

assert urllib.join_host_port('example.com', '443') == 'example.com:443'
assert urllib.join_host_port('::1', '8080') == '[::1]:8080'
assert urllib.join_host_port('fe80::1%eth0', '') == '[fe80::1%eth0]:'
assert urllib.join_host_port('', '80') == ':80'
```

The helper only joins strings: it does not validate the host or resolve a service name such
as `https`. `split_host_port` can split the result when the port is numeric or empty.
