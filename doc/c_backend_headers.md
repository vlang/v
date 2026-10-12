# Headers in generated C

On Unix, compiling `examples/hello_world.v` generates C without `#include` directives.
On macOS, `v -keepc self` also retains compiler C with no include directives.
The compiler's string literals can contain the text of includes for other generated programs.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Explicit headers for other C libraries remain included.

For a single-file POSIX program containing only literal `print`, `println`, `eprint`,
and `eprintln` calls, the compiler proves that no V allocations are needed. It emits
only `main` and the required libc declarations, without linking a collector or native
runtime helpers. A single `println` uses `puts`; other output preserves embedded NUL
bytes and handles partial writes. Programs with allocations, initialization, or other
runtime requirements retain their normal runtime. Instrumented and library builds
also retain their runtime support.

The GC, signal, process, and compiler helpers are linked from native objects. Their platform headers
are parsed when those objects are built. V caches the objects using the C compiler, target,
flags, and source and header dependencies. `-nocache` disables the V module cache; native
objects can still be reused. The collector's thread registration and signal handling stay
in the native helpers.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.

Autofree uses a monolithic build so cleanup retains complete local-variable metadata.

Older bootstrap compilers use compatibility headers for their first compiler build.
The current compiler supplies an internal capability define and omits those headers.
