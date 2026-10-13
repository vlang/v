# Headers in generated C

On Unix, compiling `examples/hello_world.v` generates C without `#include` directives.
On macOS, `v -keepc self` also retains compiler C with no include directives.
The compiler's string literals can contain the text of includes for other generated programs.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Native backends retain C declarations without rooting same-named
V functions.
Explicit headers for other C libraries remain included.
The headerless requirement covers hello world and compiler self builds, rather than every
library binding. Modules such as `compress.brotli` retain their C headers and cached
implementations.
Coverage and profiling retain file I/O and timer declarations even when the application
imports no standard library modules.
Mach task APIs declared by the application also keep their prototypes in headerless C.
An explicit Mach SDK header remains authoritative when supplied by the application.

For a single-file POSIX program containing only literal `print`, `println`, `eprint`,
and `eprintln` calls, the compiler proves that no V allocations are needed. It emits
only `main` and the required libc declarations, without linking a collector or native
runtime helpers. A single `println` uses `puts`; other output preserves embedded NUL
bytes and handles partial writes. Programs with allocations, initialization, or other
runtime requirements retain their normal runtime. Instrumented, library, freestanding,
and `-file-list` builds also retain their runtime support. Portable `-os cross` and `-cross`
snapshots retain the complete runtime, pointer-width checks, and custom-define metadata even
for literal output.

The GC, signal, process, and compiler helpers are linked from native objects. Their platform headers
are parsed when those objects are built. V caches the objects using the C compiler, target,
flags, and source and header dependencies. `-nocache` disables the V module cache; native
objects can still be reused. The collector's thread registration and signal handling stay
in the native helpers.
Missing shipped C objects are built with the selected compiler, including TinyCC.
On macOS, existing or external object inputs still select the platform C compiler.
Wrappers that expand Boehm allocation macros include the collector header explicitly.
Warm incremental builds share the cached declaration prefix's string storage and retain
new program literals and cleanup hooks after source edits.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.

Autofree uses a monolithic build so cleanup retains complete local-variable metadata.

Older bootstrap compilers use compatibility headers for their first compiler build.
The current compiler supplies an internal capability define and omits those headers.

Portable `-os cross` snapshots and `-is_o` objects embed their V-owned native runtime helpers,
so standalone C consumers do not need helper paths from the compiler checkout. These modes
and `-no-builtin` use compatibility headers when complete V bindings are unavailable.

Malformed embedded compatibility headers are diagnosed before C compilation in portable,
object, and `-no-builtin` modes. Parallel builds can reuse a native object cache directory
created by another compiler process; filesystem failures remain errors.
