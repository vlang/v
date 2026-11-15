# Clearing the environment

`os.clearenv()` removes the current process's environment variables, including
variables with empty values. Repeated calls on an empty environment are harmless.
The environment can be populated again with `os.setenv()`.

```v
import os

os.clearenv()
assert os.environ().len == 0
os.setenv('V_CLEAR_EXAMPLE', 'fresh value', true)
assert os.getenv('V_CLEAR_EXAMPLE') == 'fresh value'
```

The change affects the calling process and the environment inherited by children
started afterward. It does not change the parent process's environment. To customize
only one child, use `os.Process.set_environment()` instead.

On Windows, clearing covers the live Win32 environment and the C runtime's cached
environment. It also removes hidden per-drive entries such as `=C:`, which
`os.environ()` deliberately omits. Variables created directly through Win32 are
removed even when the C runtime does not know about them.

The JavaScript backend deletes environment properties rather than assigning empty
values, so `os.environ()` no longer lists the removed names.
