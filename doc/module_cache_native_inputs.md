# Native inputs in the module cache

With `-new-compiler -cc cc`, applications importing `compress.zstd`, including `veb`
applications, can use the module and program caches. Cached module objects share one
copy of V's bundled zstd implementation in the program translation unit. Other units
use the public declarations embedded in the bundled amalgamation. Changes to the
native source remain cache inputs and invalidate affected entries. Embedded resources in omitted
cached function bodies retain their dependency records after their current content and metadata
are verified. Editing or removing a resource invalidates reuse. Bundled mbedTLS headers are checked
with the selected C preprocessor, using the generated units' C
or Objective-C language, and their active dependencies are tracked. C++ contexts,
custom native configuration directives, SDK macro overrides, renamed SDK bindings, and forced
headers keep the conservative fallback when their original context cannot be reconstructed.
Compiler response files and config/spec files also retain the fallback.
Production, shared, and GNU89 inline modes retain this fallback for bundled mbedTLS headers.

Module object cache keys distinguish system libc headers from V's fallback declarations.
A program import or `#include` can select system headers for the whole program, changing native
type layouts even in modules whose V source is unchanged. Objects from these two header modes
are kept separate, including when reusing a cache populated by an older compiler.

Set `V3_CACHE_TRACE=1` to inspect cache decisions, and use `-show-timings` to see
`monomorphize (cached)` on an unchanged build. External native inputs without a safe
ownership or replication policy still fall back to an uncached build.

On macOS, development builds that need Objective-C or external native objects can use
the bundled TCC for the program. The platform compiler builds the native owner and cached
dependency modules into a development dylib, which TCC links with the application's C unit.
Modules inside the application directory remain part of the application unit; modules outside
it, including the standard library, retain their module objects and interfaces in the cache.
The C compiler supplies the dependency manifest for project headers and their nested includes.
Changing those inputs invalidates the affected cached artifacts.
The V program keeps TCC's compile-time conditions. Native calls, including functions from static
archives, resolve through the native owner. Loop conditions containing `or { break }` or
`or { continue }` run inside the loop body so TCC preserves their control flow and short circuiting.

Caching is enabled by default. Use `-showcc` to inspect the final TCC invocation and
`V3_CACHE_TRACE=1` to inspect module reuse. `-nocache`, production builds, and builds requiring
complete retained C output keep their existing compilation paths.

`-print-watched-files` and `-dump-files` parse source bodies to include every embedded resource,
including resources omitted from warm declaration headers.
