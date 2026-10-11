# Module cache refresh

This compiler utility records how cached modules from a checkout's vlib were built,
so `v up` can refresh them after replacing the compiler or updating their sources.

- `root_dir` locates the checkout's cache and respects `V3CACHE` and test isolation.
- `record` saves a separate atomic inventory entry for each module and build configuration.
  Project modules outside the checkout's vlib are excluded.
- `collect` snapshots the recorded configurations before an update. For older caches,
  it identifies vlib modules from their headers and chooses the available C compilers
  in both normal and production mode.
- `refresh` compiles a temporary importer for each entry using the updated compiler.
  It preserves build modes, resolved compiler commands, and absolute search paths.
  It excludes linker-only flags, including operands embedded in `-cflags`.
  It validates dependencies, reuses unchanged objects, and
  reports newly cached modules on stdout. It never runs the generated programs.
  Removed modules are skipped, and a compilation failure is returned to `v up`.

Inventory records use length-prefixed arguments to preserve quoting, spaces, and newlines.
