# Tool arguments

The `diff`, `download`, `repeat`, `time`, and `timeout` tools accept the same arguments when
run through the V launcher or as standalone executables. The launcher removes the tool command
name before forwarding arguments, preserving the first option or positional argument.

For example, `./v timeout --version` and a compiled `timeout --version` both print the version.
Likewise, `./v time echo hello` and a compiled `time echo hello` run the same command.
