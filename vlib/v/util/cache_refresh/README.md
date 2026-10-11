# Module cache refresh

This compiler utility records how cached modules from a checkout's vlib were built,
so `v up` can refresh them after replacing the compiler or updating their sources.

- `root_dir(vroot, vexe)` locates the checkout's cache and respects `V3CACHE` and test isolation.
  Isolated cache lookup uses the supplied V compiler executable, rather than the helper process.
- `record` saves a separate atomic inventory entry for each module and build configuration.
  Project modules outside the checkout's vlib are excluded.
- `collect(vroot, vexe)` snapshots the compiler's recorded configurations before an update.
  For older caches, it identifies vlib modules from their headers and chooses the
  available C compilers in both normal and production mode.
- `refresh` compiles a temporary importer for each entry using the updated compiler.
  It preserves build modes, resolved compiler commands, and absolute search paths.
  Each recorded working directory keeps relative C compilation inputs valid, and
  effective `-run-only` selectors preserve caches created through `v test`.
  It excludes linker-only flags, including operands embedded in `-cflags`.
  It validates dependencies, reuses unchanged objects, and
  reports newly cached modules on stdout. It never runs the generated programs.
  Removed modules are skipped, and a compilation failure is returned to `v up`.

Inventory records use length-prefixed arguments and working directories to preserve
quoting, spaces, and newlines. Records without a working directory remain readable.
