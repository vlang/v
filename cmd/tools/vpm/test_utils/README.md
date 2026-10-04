VPM fixtures use `cmd_ok_args(location, args)` and `cmd_fail_args(location, args)`
to execute literal arguments and check the expected exit status. The command string
versions are deprecated; paths and values belong in separate array elements.
