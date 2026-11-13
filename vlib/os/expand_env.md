# Environment expansion

`os.expand_env(text)` replaces `$NAME` and `${NAME}` with their values in the
current process environment. Undefined names and empty values both produce an
empty string. Inserted values are not expanded again.

```v
import os

os.setenv('V_EXPAND_EXAMPLE', 'world', true)
assert os.expand_env('hello \${V_EXPAND_EXAMPLE}') == 'hello world'
```

Unbraced names use ASCII letters, digits, and underscores. A digit at the start,
or one of `*`, `#`, `$`, `@`, `!`, `?`, and `-`, names a single-character variable:
`$10` expands variable `1` followed by literal `0`; `${10}` expands variable `10`.
These names are read from the environment, without invoking a shell.

Braces allow names with other characters, including non-ASCII names. Names containing
NUL bytes expand to an empty string instead of looking up a truncated name. A lone `$`
or a `$` followed by an unrecognized character stays literal. An empty `${}` is
removed. An unterminated `${name` loses the `${` prefix and processes the remaining
text normally, matching Go's `os.ExpandEnv` syntax.

This function does not evaluate shell quotes, commands, or default-value expressions
such as `${NAME:-fallback}`.
