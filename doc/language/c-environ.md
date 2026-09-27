# Accessing the C environment

On POSIX targets, `C.environ` provides access to the process environment as a null-terminated
array of C strings. The C backend declares it when generating code with or without system
headers, so adding a `#include` does not change its availability.

Use `os.environ()` when a V map of environment variables is sufficient.
