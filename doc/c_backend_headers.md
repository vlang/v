# Headers in generated C

On Unix, compiling `examples/hello_world.v` generates C without `#include` directives.
On macOS, `v -keepc self` also retains compiler C with no include directives.
The compiler's string literals can contain the text of includes for other generated programs.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Native backends retain C declarations without rooting same-named
V functions.
Explicit headers for other C libraries remain included.
Coverage and profiling retain file I/O and timer declarations even when the application
imports no standard library modules.
Mach task APIs declared by the application also keep their prototypes in headerless C.
An explicit Mach SDK header remains authoritative when supplied by the application.

The GC, signal, process, and compiler helpers are linked from native objects. Their platform headers
are parsed when those objects are built. V caches the objects using the C compiler, target,
flags, and source and header dependencies. `-nocache` disables the V module cache; native
objects can still be reused. The collector's thread registration and signal handling stay
in the native helpers.
Missing shipped C objects are built with the selected compiler, including TinyCC.
On macOS, existing or external object inputs still select the platform C compiler.
Wrappers that expand Boehm allocation macros include the collector header explicitly.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.

Autofree uses a monolithic build so cleanup retains complete local-variable metadata.

Older bootstrap compilers use compatibility headers for their first compiler build.
The current compiler supplies an internal capability define and omits those headers.

Portable `-os cross` snapshots and `-is_o` objects embed their V-owned native runtime helpers,
so standalone C consumers do not need helper paths from the compiler checkout. These modes
and `-no-builtin` use compatibility headers when complete V bindings are unavailable.
