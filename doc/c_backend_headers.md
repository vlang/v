# Headers in generated C

Literal hello world programs generate C without `#include` directives on supported hosted Unix,
Windows, Android and iOS targets. Windows keeps its CRT output mode; mobile targets retain
their platform logging APIs. Compiler self builds use standalone core declarations on
macOS, Linux, Windows and the BSD targets with a described libc ABI.

NetBSD, Solaris, Haiku, QNX, Serenity, Vinix and Emscripten retain the SDK preamble for
compiler and runtime builds. Their symbol versions, storage or constants require native
declarations that the standalone preamble does not yet describe completely. Literal
hello output still avoids those headers on the other hosted targets. Vinix retains the complete
runtime even for literal output. Native execution must be validated on each
platform; generating C for a target alone does not verify its runtime ABI.
The compiler's string literals can contain the text of includes for other generated programs.
The C backend emits libc prototypes from bodyless V declarations such as
`fn C.strlen(const_s &char) usize`. Runtime bindings are marked used so cached modules
retain their prototypes. Native backends retain C declarations without rooting same-named
V functions.
Explicit headers for other C libraries remain included. Core SDK headers are also
restored when another binding requests headers, so native declarations stay authoritative
regardless of source collection order.
The headerless requirement covers hello world and compiler self builds, rather than every
library binding. Modules such as `compress.brotli` retain their C headers and cached
implementations.
Coverage and profiling retain file I/O and timer declarations even when the application
imports no standard library modules.
On macOS, standalone `statvfs` storage retains the native 32-bit block and inode counters;
its field offsets and widths are checked against the installed SDK.
Mach task APIs declared by the application also keep their prototypes in headerless C.
An explicit Mach SDK header remains authoritative when supplied by the application.

On FreeBSD and OpenBSD, builds with system headers include `<sys/sysctl.h>` for
`os.executable()` and retain its native `sysctl` declaration. Headerless builds supply
the sysctl constants and prototype directly.

Win32 API names in headerless C declarations receive `WINAPI` and their wide export
names only for Windows targets. On other targets, names such as `Sleep` and `CreateFile`
keep the spelling and calling convention of the program's own C functions.

For a single-file hosted program containing only literal `print`, `println`, `eprint`,
and `eprintln` calls, the compiler proves that no V allocations are needed. It emits
only `main` and the required native declarations, without linking a collector or initializing
the V runtime. Platform logging adapters remain linked when required.
On POSIX targets a single `println` uses `puts`; other output preserves
embedded NUL bytes and handles partial writes. Windows ASCII output uses the CRT in binary mode,
with bounded write counts and partial-write retries. Unicode console output retains
the normal Windows runtime. Android retains log priorities, formatting and configurable
tags. iOS retains the native logger; allocations within platform loggers belong to those
libraries. iOS format literals containing `%` retain the normal runtime. Programs with
allocations, initialization, or other runtime requirements retain their normal runtime.
Instrumented, library, freestanding,
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
Warm builds using `os` also retain system-header ownership, so platform stat macros do not
conflict with compatibility declarations. Windows standalone and custom-libc builds keep
their required native layouts.
Windows file and console APIs preserve `C.DWORD` output pointer types at SDK call sites.
This preserves the SDK's pointer types when a C compiler checks calls against Windows headers.
Core file mappings retain standalone declarations on verified 64-bit targets, including native
mapping flags and the synchronization function signature. Other targets and programs using
explicit C headers or custom libc headers keep platform-header ownership.

Regular generated C declares interned strings with `_SL(_str_N, "...", byte_length)`.
The macro expands to a `static const string` with a plain brace initializer. V supplies the byte
length, including embedded NUL bytes, so the C compiler needs no compound literal or `sizeof`.
Literal IDs remain ordinary identifiers so they do not expand C struct member names. This form also
works in portable snapshots and MSVC output. Cached modules retain explicit literal initializers
for sharing and rewriting through their declaration prefix.

Generated C still needs the native objects and library flags when linked. Use V to build
executables, or `-generate-c-project` to export a build command with the native sources
and library flags.

Autofree uses a monolithic build so cleanup retains complete local-variable metadata.

Scalar array appends can use typed stores into spare capacity in library modules as well as
`main`. Growth and slice handling retain the runtime append helper. User functions and callbacks
named `array_push` keep their own calls.

Older bootstrap compilers use compatibility headers for their first compiler build.
The BSD constants used by `os.executable()` are defined in V. The current compiler
supplies an internal capability define and omits the compatibility headers.
FreeBSD resident memory lookup also uses V-owned sysctl identifiers without `libprocstat`.

Portable `-os cross` snapshots and `-is_o` objects embed their V-owned native runtime helpers,
so standalone C consumers do not need helper paths from the compiler checkout. These modes
and `-no-builtin` use compatibility headers when complete V bindings are unavailable.

Portable compiler snapshots generated with `-cross cmd/v` or `-os cross cmd/v` supply
their runtime declarations and helper definitions from V without include directives on
the supported hosted targets.
They retain the complete compiler runtime and disable the platform signal handler.
The text of includes inside compiler string literals is still present for other generated programs.

Malformed embedded compatibility headers are diagnosed before C compilation in portable,
object, and `-no-builtin` modes. Parallel builds can reuse a native object cache directory
created by another compiler process; filesystem failures remain errors.

Negated integer literals above `INT_MAX` and within the signed 64-bit range receive an `LL`
suffix in C output, including parenthesized operands and constant initializers. This preserves
their sign with MSVC.
