# Headers in generated C

On Unix, compiling `examples/hello_world.v` generates C without `#include` directives.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Explicit headers for other C libraries remain included.

The builtin GC and signal helpers are linked from native objects. Their platform headers
are parsed when those objects are built. V caches the objects using the C compiler, target,
flags, and source and header dependencies. `-nocache` disables the V module cache; native
objects can still be reused. The collector's thread registration and signal handling stay
in the native helpers.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.
