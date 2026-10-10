# Environment reports with `v doctor`

Run `v doctor` to collect V, operating system, and C toolchain details for a bug report.
Version rows show `N/A` when an optional compiler or tool is unavailable. This includes
Windows launch errors caused by a missing executable or directory, regardless of the
language used by Windows for its error message.

On Windows, exit codes 2 and 3 show `Error: ...` when the tool ran and reported a failure.
Other launch failures, such as access denied, also remain visible as errors.

## The `vlib` that the compiler uses

A V executable does not look for `vlib` next to itself. It uses the `vlib` of the V checkout
that contains the compiled sources, then the one that contains the working folder, and
otherwise the one of the checkout that it was built in. So a compiler can be paired with a
`vlib` that is older or newer than itself. It still runs then, but it can behave differently,
and a compilation does not say so.

`v doctor` reports the `vlib` that the compiler used for the `v doctor` tool itself:

* `V vlib dir` is `OK` when that `vlib` is in the folder of the V executable (`V home dir`).
  `NOT in the folder of the V executable` means that the executable was copied or moved out
  of the checkout that it was built in. It keeps using the `vlib` there.
* `V vlib commit` is `OK` when that checkout is at the commit that the compiler was built
  from. Uncommitted changes do not count. `MISMATCH: V was built from commit X, but this vlib
  is at commit Y` means that the checkout was updated without rebuilding V, or that the
  executable comes from another checkout. Rebuild V there with `make` or `v self`.
  `N/A` means that there is no Git checkout to read a commit from.

Sources inside another V checkout are compiled with the `vlib` of that checkout, whichever
compiler is used. When `v doctor` is run in a folder of such a checkout, it adds the rows
`cwd vlib dir` and `cwd vlib commit`, that tell the same about the `vlib` there. Run it in
the folder that you work in. For a single compilation, pass `-v` to see its `vlib`, in the
`v.pref.lookup_path` line.
