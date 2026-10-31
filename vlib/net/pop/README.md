`net.pop` provides a synchronous POP3 client over plain TCP. It does not provide TLS; use only
connections where cleartext credentials and messages are appropriate.

Call `dial(Config{server: ..., username: ..., password: ...})`, then `login()` and `stat()`,
`list_all()`, `retr(number)`, or `dele(number)`. Message numbers must be positive. `retr` returns
message text with LF endings and removes POP3 dot stuffing. Transport and incomplete multiline
responses return errors and close the connection. Credentials cannot contain CR, LF, or NUL.
`quit()` sends QUIT and closes even if the server rejects it; `close()` aborts without committing
deletions. Calling methods on a closed client returns an error. Clients are not safe for concurrent
commands. TCP's standard read/write timeouts apply.

`server` must be a nonempty host name or IP literal; IPv6 may be bare or bracketed. `port` must be
between 1 and 65535. `stat()` rejects malformed numbers and values outside V's `int` range.
