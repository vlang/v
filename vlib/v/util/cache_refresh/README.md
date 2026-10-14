# Module cache refresh

This compiler utility records how cached modules from a checkout's vlib were built,
so `v up` can refresh them after replacing the compiler or updating their sources.
`v up` selects only TinyCC configurations from this inventory, including its recorded
normal/production and program/test modes. Other compilers refresh their caches on the next build.
Only changed generated code or compilation inputs require another C compilation;
unchanged module objects are reused without cache creation messages.

- `root_dir(vroot, vexe)` locates the checkout's cache and respects `V3CACHE` and test isolation.
  Isolated cache lookup uses the supplied V compiler executable, rather than the helper process.
  Delegated ownership compilers retain `V3_PRIMARY_VEXE` for the same root,
  even when their launcher resets `VEXE` to the helper executable.
- `record` saves a separate atomic inventory entry for each module and build configuration.
  Project modules outside the checkout's vlib are excluded.
- `collect(vroot, vexe)` snapshots the compiler's recorded configurations before an update.
  Unreadable, truncated, and unsupported inventory files are skipped.
  For older caches, it identifies vlib modules from their headers and chooses the
  available C compilers in both normal and production mode, for programs and tests.
  Successful migrations mark the old headers, so later updates use the new inventory
  without repeating the fallback matrix. Failed or skipped migrations remain retryable.
- `refresh` uses the updated compiler to rebuild cached objects from the original
  program input when project interface implementers require it and it still exists.
  It stops before linking or running the program, and refreshes each entry/configuration once.
  Live requests sharing a command, working directory, entry and test display mode run once,
  even when historical project directives or imports differ.
  Source identities bind live entries to their recorded configurations. Changed project
  inputs retire old records until a successful build records the current source/configuration pair.
  Unchanged entries retain all recorded compiler, production, and test variants.
  Absolute `-file-list` files and directories accompany live entries. Directory operands
  recursively include their current sources, so newly added files are included during refresh.
  Otherwise it prepares a temporary importer with the recorded project directives.
  Both paths stop after validating and rebuilding module objects, before compiling
  program objects, building development libraries, or linking an executable.
  Refreshed objects are validated against the published module interfaces before returning.
  It uses the compiler's default caching mode and preserves build modes,
  resolved compiler commands, and absolute search paths.
  A recorded working directory keeps relative C compilation inputs valid.
  Builds without relative inputs or project interface scopes share records across projects.
  Effective `-run-only` selectors and assertion display modes preserve test configurations.
  It excludes linker-only flags, including operands embedded in `-cflags`.
  It validates dependencies, reuses unchanged objects, and
  reports modules compiled into new cached objects on stdout. It never runs the generated programs.
  Different configurations can compile the same module separately.
  By default, a build announces its compiler flags, production mode, and program/test mode
  only before its first output. Successful cache checks that produce no output remain quiet.
  A final summary counts the distinct builds checked, including reused configurations,
  only when a child produced output. The optional `RefreshOptions{verbose: true}` argument
  announces every checked build's record index and total and prints the summary for silent builds.
  `v up -v` enables this verbose progress. Indices can jump as equivalent builds and
  unavailable inputs are skipped.
  Removed modules and records with vanished working directories, required compile inputs,
  listed files or directories, or unavailable C compilers are skipped.
  A compilation failure is returned to `v up`, including TinyCC module object failures.
  Refresh does not hide an object failure by retrying a whole program without caching.
  Child output is streamed while refresh runs. A disk-full error stops the remaining builds
  immediately and asks for free space before another update.

After a compiler update, module interfaces are regenerated and validated with the new compiler.
Matching compiled objects can be shared across compiler configurations when their generated C,
C compiler, compilation flags, working directory, and native dependency fingerprints match.
The C compiler identity is independent of the V build, and unused generated string literals
are removed from module C before comparison. Referenced version literals still require a rebuild.
The shared object and C source are checked before reuse and linked into the new configuration
without another C compilation. Changed generated code, flags, or native inputs require a new object.
This preserves compiler validation while avoiding repeated compilation and duplicate object data.
Older caches populate the shared index when their objects are next compiled.

Inventory records use length-prefixed arguments and working directories to preserve
quoting, spaces, and newlines. Records without a working directory remain readable.

`FlagGroup` retains directive ordering and the unfiltered group identity used for
deduplication. Linker operands are removed from its replayed flags. Module directives
come from the updated sources, including changes to macros and forced headers.
`encode_project_flags` and `decode_project_flags` preserve these groups in records
and the refresh child's environment, including identities containing NUL separators.

The C generator's `cache_project_flag_groups` separates project directives from
vlib directives. `cache_directive_flags_with_project` and
`FlatGen.set_cache_refresh_project_flags` merge them using the usual directive ordering.
CLI C flags retain their position after source directives. Original vlib imports
preserve flags contributed by modules outside the refreshed module's dependencies.

Inventory version 7 adds live-entry source identities, including project imports and
directory contents. Older live records without these identities are skipped until rebuilt;
their source/configuration pairs cannot be validated. Older synthetic records remain usable.
Version 6 adds effective assertion display modes and required compile inputs.
Absolute `-file-list` operands are saved as arguments and checked before refresh.
Explicit files are tracked as required inputs; directory operands retain directory semantics.
Legacy header provenance is temporary collection metadata and is not part of record identity.
Version 5 adds the original entry path to version 4's project groups and imports.
Older inventories remain readable. Version 3 did not distinguish project and module flags;
its full flag vector is discarded and its CLI options are retained to avoid replaying
stale directives.
