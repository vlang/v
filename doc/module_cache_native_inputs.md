# Native inputs in the module cache

V leaves C headers to the C toolchain. It does not parse header declarations, expand nested
includes, or project native interfaces for TCC. C bindings declare their types and functions in V.
The header-parsing bridge introduced by commit `4563cf06ba` has been removed.

Outside the native owner path described below, user native inputs and bundled native files
without an explicit ownership contract bypass module and program caches. Forced headers and macro
files use the same conservative fallback. The
compiler no longer scans bundled mbedTLS or other headers to decide whether their contents can be
replicated into cached objects. Embedded resources retain their ordinary dependency records.

On macOS, cached development builds with Objective-C sources can compile the application unit
with TCC and link a native development dylib. Clang compiles the native owner and external V
modules. Application-directory modules remain in the TCC unit; other modules retain cached
objects and V declaration interfaces.

Native function pointer declarations come solely from explicit `fn C...` bindings. Their V types
must match the native ABI. C headers remain includes for the selected C compiler, including
headers with guarded include cycles. Types and macros private to a native source are not inferred:
expose them through a C-compatible header or explicit native helpers. Objective-C implementations
must use `.m` or `.mm` source files. Native source includes belong to the owner; their code is not
copied into every cached object. For a header with private implementation code, keep its public C
types outside a `V3CACHE_NATIVE_INTERFACE` guard and place implementation under
`!defined(V3CACHE_NATIVE_INTERFACE) || defined(V3CACHE_NATIVE_OWNER)`. The C compiler selects the
interface or implementation; V does not inspect either branch. Imported and explicitly exported V
functions retain their ordinary symbols when called through a C binding.
Mixed Objective-C and Objective-C++ source contexts retain the fallback; `-no-retry-compilation`
rejects unsupported contexts instead of changing compilers.

The C compiler supplies a fresh dependency manifest before cache reuse. Editing a nested header
invalidates dependent artifacts. The native compiler rejects date/time macros before publishing
the owner and V module objects. Embedded resources remain dependencies of their owning V modules,
including resources omitted from warm V declaration interfaces. Use `-showcc` to confirm the final
TCC invocation and `V3_CACHE_TRACE=1` to inspect module reuse. The consumer's compiler-generated
dependency manifest also selects framework link flags for nested C includes.

Parallel C builds outside this explicit owner path keep quoted native includes in one translation
unit when their ownership is unknown. The preprocessed TCC prelude cache and its readers have
been removed. Possible date/time dependencies disable executable reuse without preprocessing
the unit to inspect header output.

C compiler dependency manifests can still name headers for dependency reporting and opaque file
identity checks. A dependency list alone cannot establish missing include candidates or time macro
context, so executable reuse is bypassed when that information would require reading headers.

`-cross -o output.c` preserves local header and native source includes. Supply those files and
their include paths on the machine compiling the C output; V does not recursively embed them.

Set `V3_CACHE_TRACE=1` to inspect cache decisions and `-showcc` to inspect the native
compiler command.
`-print-watched-files` and `-dump-files` parse V source bodies to include embedded resources,
including resources omitted from warm V declaration headers.
