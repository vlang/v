Tool subprocess helpers accept literal argument arrays through `TestSession.exec`,
`TestSession.system_args`, and `build_v_args_failed`. Pass each path and option as
a separate element. The command string versions are deprecated with migration hints.

Each selected test gets its own temporary folder. If the runner cannot create that folder,
it reports a failed test with the path and filesystem error, and skips compilation for that file.
