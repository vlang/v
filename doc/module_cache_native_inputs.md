# Native inputs in the module cache

V leaves C headers to the C toolchain. It does not parse header declarations, expand nested
includes, or project native interfaces for TCC. C bindings declare their types and functions in V.
The native bridge introduced by commit `4563cf06ba` has been removed.

User native inputs and bundled native files without an explicit ownership contract bypass module
and program caches. Forced headers and macro files use the same conservative fallback. The
compiler no longer scans bundled mbedTLS or other headers to decide whether their contents can be
replicated into cached objects. Embedded resources retain their ordinary dependency records.

Parallel C builds keep quoted native includes in one translation unit when their ownership is
unknown. Headers remain includes for the C compiler. TCC builds needing Objective-C or incompatible
native objects follow the existing native compiler fallback, without a projected header interface.
Native `.m` and `.mm` inputs select the platform compiler before C generation when retries are
enabled, including when TinyCC was requested explicitly. The preprocessed TCC prelude cache is
removed because its validation required header inspection.

C compiler dependency manifests can still name headers for dependency reporting and opaque file
identity checks. A dependency list alone cannot establish missing include candidates or time macro
context, so executable reuse is bypassed when that information would require reading headers.

`-cross -o output.c` preserves local header and native source includes. Supply those files and
their include paths on the machine compiling the C output; V does not recursively embed them.

Set `V3_CACHE_TRACE=1` to inspect cache decisions and `-showcc` to inspect the native
compiler command.
`-print-watched-files` and `-dump-files` parse V source bodies to include embedded resources,
including
resources omitted from warm V declaration headers.
