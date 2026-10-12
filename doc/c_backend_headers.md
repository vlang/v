# Headers in generated C

On Unix, compiling `examples/hello_world.v` generates C without `#include` directives.
On macOS, `v -keepc self` also retains compiler C with no include directives.
The compiler's string literals can contain the text of includes for other generated programs.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Explicit user-provided headers remain included.
Portable `-os cross` C output keeps conditional platform headers for its target ABIs.

On macOS, vlib's common POSIX bindings also use their V declarations in programs
that import networking and SQLite. SQLite links the library supplied by macOS.
Header ownership is tracked per module so an unrelated SDK cannot hide a headerless
module's declarations or struct layouts. Incremental builds inline their cached
declarations and remove duplicate literal storage, preserving initialization and cleanup.

On macOS, bundled graphics, image, audio, compression, and TLS implementations are
compiled into cached native objects. Their V bindings declare C functions without
bodies and supply the layouts and constants needed by generated C. Native SDK
checks verify storage sizes, alignment, and graphics field offsets. AppKit helpers
use explicit native interfaces rather than depending on generated V struct layouts.
PostgreSQL and OpenSSL compatibility helpers also live in native objects.

For native objects with types declared completely in V, `@[c_extern; typedef]`
on a `struct C.Name` makes its V declaration supply the C layout. Unrelated
headers in the same module do not suppress it. Apply `@[c_extern]` to bodyless
C function bindings when their declarations must also survive unrelated headers.
Keep SDK-owned types header-backed instead of declaring a second layout.

A binding file that directly links a native source, object, or library retains
its bodyless C prototypes unless that file also includes their header. A sibling
file's headers still cover bindings that only share the module's library linkage.

Darwin synchronization, jump buffers, time, and filesystem records use the SDK's
field widths, sizes, and alignment. Recovery functions retain their C control-flow
attributes, including `returns_twice`.
Coverage and profiling also use bodyless V bindings, including the Darwin clock functions.

Cached interfaces retain their original source ownership and exported callback records.
Scripts and programs with an explicit `main` share guarded callback initialization,
preserving program arguments and initializing modules once when cached objects are reused.

For a single-file POSIX program containing only literal `print`, `println`, `eprint`,
and `eprintln` calls, the compiler proves that no V allocations are needed. It emits
only `main` and the required libc declarations, without linking a collector or native
runtime helpers. A single `println` uses `puts`; other output preserves embedded NUL
bytes and handles partial writes. Programs with allocations, initialization, or other
runtime requirements retain their normal runtime. Instrumented, library, freestanding,
and `-file-list` builds also retain their runtime support.

The GC, signal, process, and compiler helpers are linked from native objects. Their platform headers
are parsed when those objects are built. V caches the objects using the C compiler, target,
flags, and source and header dependencies. `-nocache` disables the V module cache; native
objects can still be reused. The collector's thread registration and signal handling stay
in the native helpers.

TinyCC delegates Objective-C helper objects to the native compiler. On macOS,
programs linking native Mach-O objects use the platform C compiler for their
generated C as well; valid SDK objects remain cached across those builds.
Native helpers keep their source language: C++ standards for generated C do not
override the standard of a separately compiled C helper, and conversely for C++ helpers.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.

Autofree uses a monolithic build so cleanup retains complete local-variable metadata.

Older bootstrap compilers use compatibility headers for their first compiler build.
The current compiler supplies an internal capability define and omits those headers.
