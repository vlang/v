# Environment reports with `v doctor`

Run `v doctor` to collect V, operating system, and C toolchain details for a bug report.
Version rows show `N/A` when an optional compiler or tool is unavailable. This includes
Windows launch errors caused by a missing executable or directory, regardless of the
language used by Windows for its error message.

On Windows, exit codes 2 and 3 show `Error: ...` when the tool ran and reported a failure.
Other launch failures, such as access denied, also remain visible as errors.
