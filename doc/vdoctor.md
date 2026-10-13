# Environment reports with `v doctor`

Run `v doctor` to collect V, operating system, and C toolchain details for a bug report.
Version rows show `N/A` when an optional compiler or tool is unavailable. This includes
Windows launch errors caused by a missing executable or directory, regardless of the
language used by Windows for its error message.

On Windows, exit codes 2 and 3 show `Error: ...` when the tool ran and reported a failure.
Other launch failures, such as access denied, also remain visible as errors.

## The `vlib` that the compiler uses

The compiler can use the `vlib` of the checkout containing the compiled sources or working
folder, or the root recorded when the compiler was built. The macOS compiler dispatcher
keeps the invoking compiler's checkout when its executable is in a valid V root; otherwise
it uses the recorded root. A copied executable or an updated checkout can therefore pair a
compiler with a `vlib` from another commit without reporting this during compilation.

`v doctor` reports the `vlib` that the compiler used for the `v doctor` tool itself:

* `V vlib dir` is `OK` when that `vlib` is in the folder of the V executable (`V home dir`).
  `NOT in the folder of the V executable` means that the tool used a different checkout's
  `vlib`, for example after the executable was copied out of its original checkout.
* `V vlib commit` is `OK` when that checkout is at the commit that the compiler was built
  from. Uncommitted changes do not count. `MISMATCH: V was built from commit X, but this vlib
  is at commit Y` means that the checkout was updated without rebuilding V, or that the
  executable comes from another checkout. Rebuild V there with `make` or `v self`.
  `N/A` means that there is no Git checkout to read a commit from.

When `v doctor` is run inside another V checkout, it also adds `cwd vlib dir` and
`cwd vlib commit` rows describing that checkout. Those rows identify a possible mismatch
for compilers that resolve modules from the sources or working folder; they do not imply
that every compiler mode selects that checkout. Run the tool in the folder that you work
in. For a single V3 compilation, pass `-v` to see its resolved root in the
`v.pref.lookup_path` line.
