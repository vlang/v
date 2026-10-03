The scripting helpers accept literal argument arrays through `exec_args`, `run_args`,
`frun_args`, and `exit_0_status_args`. Pass each path or value as a separate element,
without shell escaping. The string versions are deprecated because they interpret
command strings as shell code.

`exec_args` returns the process result or an error. `frun_args` returns its output or
an error; `run_args` returns trimmed output on success and an empty string on failure.
`exit_0_status_args` reports whether the exit code was zero.
