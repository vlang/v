# Accessing the C environment

On POSIX targets, `C.environ` provides access to the process environment as a null-terminated
array of C strings. The C backend declares it when generating code with or without system
headers, so adding a `#include` does not change its availability.
Portable `-os cross` output keeps the declaration behind a C `_WIN32` guard, allowing
the same generated source to compile for POSIX or Windows.

Use `os.environ()` when a V map of environment variables is sufficient.
