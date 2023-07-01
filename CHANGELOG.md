## V 0.3.5
*29 June 2023*

### Improvements in the language
- **Coroutines with a scheduler**. Only Linux/macOS for now, requires `-use-coroutines` and
  `coroutines.sleep()` instead of `time.sleep()`. They work with IO and net, but not with GC
  for now.
- `spawn` now spawns system threads, `go` spawns coroutines.
- Static type methods: `Foo.new()` to replace factory functions like `new_foo()`.
- Smartcasting with complex conditions:`if sum_type is Foo && !sum_type.is_info && get_name(sum_type.info.name) == 'foo' `.
- Functions can now return fixed size arrays.
- Enum values now can have attributes.
- Generic functions as function parameters are now supported: `fn f[T](x T, i int, f_ Fn[T]) T { `.
- Anonymous structs can no longer have attributes.
- Allow fixed array returns.

### Breaking changes
- `byte` deprecated in favor of `u8` (`byte` is automatically converted to `u8` by vfmt).

### Checker improvements/fixes
- Disallow `Result` type aliases (`type Foo = !Bar`) and `Result` in maps (`map[key]!Type`).
- Add a missing check for taking address of literal value member.
- Fixed map initialization for maps with option values.
- Allow `a << none`, where `a` is `[]?&int`.
- Disallow multiple `else` branches in a match.
- Fix index expression with sumtype of array types.
- Remove hardcoded check for function calls for `C.stat`, `C.sigaction`, etc.
- Fix multiple embedded external module interface.
- Fix missing check for diff type on map value declaration.
- Simplify error handling in the checker (#18507).
- Option alias fixes.
- Fix alias to struct ptr on struct init.
- Sumtypes can no longer hold references.
- Fix a bug checking generic closures.
- A hard to reach limit of 1 million iterations for resolving all generics.
- Fix missing check for unwrapped shift operation.
- Fix enum max value validation.
- Add a missing mutability check for `array.delete` calls.
- Disallow `_ = <- quit`.
- Disallow type matching with primitive vars.
- Warning instead of error for unnecessary brackets in if/match.
- Include import aliases when checking for import duplicates.
- Fix infering generic array type in nested call.
- Allow casted `enum val` and `const` as fixed array size.
- Disallow multiple return values in const declarations.
- Fix contains() with array of interfaces.
- Disallow mut for blank idents.

### Standard library
- json: Enum value string serialization supports `[json:'alias']` to change its string values.
- Struct fields can now be skipped during JSON/ORM serialization via `[json:'-']` and `[sql:'-']`,
  in addition to `[skip]`. This allows having custom behavior for different serialization methods.
- builtin: heap usage API (gc_memory_use() and gc_heap_usage())
- math.big: refactoring, gcd fixes/improvements, overflow fixes, `mod_inverse`.
- flag: fix finalize with multiple shortargs.
- runtime: add new functions total_memory/0 and free_memory/0.
- time: small cleanup of parse_iso8601 comments, make the C.strftime declaration forwards compatible
- stbi: allow customization of number of channels in `stbi.load()`.
- stbi: add a `resize_uint8` function for resizing images in memory.
- time, x.json2: improve iso8601 time decoding.
- builtin: zero out internal map/array pointers on m.free(), to reduce the work for the GC
  mark phase for non escaping maps/arrays, used in hot loops.
- time: add more detailed error descriptions, add custom format parsing with time.parse_format.
- sync: add Mutex.destroy and RwMutex.destroy methods.
- datatypes: add Bloom filter.
- rand: add missing rand.u16(), update doc comments, add test.
- builtin: speed up string methods with vmemcpy instead of `for` loop for copying data.

### Web
- The builtin websocket library is now thread safe.
- Enhanced builtin csrf protection in vweb.
- vweb: ability to set and get values on vweb.Context.
- vweb: support for host specific static files.
- vweb: host option added to controller, and a new host attribute.
- vweb: middleware docs improved; same with docs for `[vweb_global]` and `shared`.
- vweb: return 404 on file not found.
- net.http: copy IANA's list of methods to the http.Method enum.
- net.conv: use a pure V implementation instead of C.hton etc.
- net.html: `get_tag()` methods to find first tag occurrence.
- net.html: fixed text parsing for inline tags.
- net.html: fix parsing of nested quoted strings in code tags.
- picoev: FreeBSD support.

### ORM
- Fixed a foreign key bug that could result in an extra insert.
- Comptime bug with `[skip]` and `[sql:'-']` fixed.
- Checker error for unsupported field types.
- Allow structs without the id field, more flexible primary keys.
- Improved docs and examples.
- Uninitialized structs are no longer inserted into related tables.

### Database drivers
- mysql: TIMESTAMP support.
- mysql: allocate memory for each string and blob dynamically depending on its value length.
- mysql: add the ability to commit transactions.

### Native backend
- Refactoring, splitting large files into multiple.

### C backend
- Fix code generation for generic unions.
- Fix `[N]chan` (fixed arrays of channels).
- Fix nested fixed array instantiation.
- Fix fixed array of map.
- Fix stringification of usize struct fields (before, they were treated as 32 bit *signed* numbers).

### Comptime
- A new `$res` comptime function to get returned value in defer block (#18382).
- Fix comptimeselector option propagation.
- A mutability check for comptime assignments.
- Fix comptime assigning to sumtype or indexexpr.
- Make comptime calls work with or-block.

### Tools
- A new VPM site: vpm.vlang.io. A better design, discoverability of packages, descriptions, most downloaded packages etc.
- vpm: installation of mixed modules.
- `v ls --install -p D:\path\vls.exe` to install a local vls executable.
- vdoc: highlight comments with gray color.
- vet: allow vetting files with global variables.
- Make util.launch_tool/3 more robust, by recompiling V tools always in a known current working folder.



## V 0.3.4

*30 Apr 2023*

### Breaking Changes

The following changes may break compilation of existing code or change behavior at runtime:

- `json`: enums are serialized as strings by default, `[json_as_number]` attribute can be used for
  the old behavior.

  If you are serializing enums to JSON in your application, then you will need to add the
  `[json_as_number]` attribute to keep the old behavior!

- Variable shadowing has been completely banned (previously variable names could conflict with
  module names).

### Web

- vweb now supports live page reloading.
  The web app is instantly updated in the browser (no need to refresh the page)
  every time a **.v** or a **.html** file is changed.
- vweb is now significantly faster and more stable under load, due to a new multithreaded worker
  pool, which is much more efficient at spreading the workload among all threads equally.
- vweb now supports middleware.
- vweb now supports controllers.
  It's now possible to have multiple app structs to better separate logic.
- vweb now supports overridable `.not_found()` method for custom 404 pages in vweb.
- vweb now uses database pool.
- Fixed multipart form parsing in vweb.

### Backends

- A new pure WASM backend, based on binaryen, a WASM `builtin` module, and a pure V WASM
  serialization library.
- Lots of fixes and new features in the native backend, including making codegen logic
  platform independent.
- Now code generated by the С backend, can be compiled by a C++20 compiler.
- C backend does not generate unused interface functions now.

### Compiler CLI

- `v share file.v` for sharing code via the playground.
- `v up` speed up for when it hasn't been run for a long time (**vc/** bootstrapping has been
  optimized).
- `v init` no longer overwrites existing `src/main.v`.
- `v self` now uses a faster TCC backend on macOS (Intel/Apple Silicon), just like on Windows/Linux.
- A new command line flag `-e` for running short V programs on command line: `v -e "println(2+5)"` (
  works just like in Perl).
- A new `-ldflags` option, in addition to `-cflags`. Works just like LDFLAGS in C.

### ORM

- All ORM queries now return `![]` (`Result` of an array).
  This allows handling/propagating DB errors and simplifies working with ORM (one way).
- Many ORM improvements: type checks for `limit/offset/order by/where`; support of reference objects
  in `insert`; struct fields can be used with `limit/offset`; `Connection` interface.
- ORM now supports the `like` operator:
  ```v
  users := sql db {
      select from User where name like 'Bob%'
  }
  ```
- A new `-d trace_orm` option to see all SQL queries generated and used by V ORM and
  `-d trace_pg_error` to trace PG errors.

### Standard Library

- Added new `termios` module.
- `net.ssl`: types using ssl contexts can now be converted to strings via `.str()`/printed
  via `println()`.
- `v.reflection`: added type symbol info metadata.
- `crypto` and `math` modules have been updated to use `Result` instead of `Option`.
- `datatypes.LinkedList[map]` now works correctly.
- `urllib.Values.get()` now returns an Option.
- `strconv`: `v_printf()` was made private, `v_sprintf()` was deprecated. String interpolation
  should be used instead.
- `net.http`: mime types have been updated to include all official types.
- `gg`: `create_image()` now returns `!Image` instead of `Image`, allowing to handle errors.
- `sokol`: errors during image creation no longer result in a panic, but can be handled by the
  programmer.
- `sokol`: macOS apps can now be quit using **Cmd+Q**.
- `os.hostname()` and `os.loginname()` now return `Result`.
- `strconv.atoi` optimizations.
- `println()` now supports arrays with recursive references.
- `termux`: support for cross-compilation from termux to other platforms.
- `readline` module now works much better on macOS: key navigation, history, etc (now on par with
  Linux).
- `os`: fixed a memleak in `getline()`.
- `os.Process` now has a `create_no_window` option (Windows only).
- `os.Process` now has a `set_work_folder()` method to set the initial working folder of the new
  child process.

### Option as a first class type

Final steps in making the Option type a first class type:

- If guards now work with struct fields which are `Option` functions.
  Such fields can now also be assigned to other fields/variables.
- Option receivers can no longer have methods.
- `none` can now be cast to all `Option` types, including aliases.
- Option references are now supported: `?&Type`.
- Arrays of `Option`s are now allowed.
- Allow `foo := Foo{}`, when `Foo` has an Option field, that is a struct, that has a `[required]`
  tag on its fields.

### Compile-time Reflection

- Compile-time interface fields evaluation.
- Compile-time enum evaluation:
  ```v
  $for item in MyEnum.fields {
      println(item.value)
      println(item.name)
  }
  ```
- Added `$option` as a compile-time reflection type representing an any Option type.
- All special compile-time reflection types are now lowercase (`$int`, `$enum`, `$option`, etc).

### Checker Improvements/Fixes

- Enums can no longer be initialized like structs.
- Capture variables can no longer shadow anonymous function params.
- Mixing multi-return results with other types in return statements is no longer allowed.
- Assigning anonymous structs to named structs is no longer allowed.
- `[required]` fields are now checked for embedded structs.
- Fixed a bug with closures with fixed array variables.
- Builtin methods `first/last/repeat` can now be used in custom user types (previously they only
  worked in builtin arrays).
- Generic struct initialization no longer needs explicit types to be provided:
  ```v
  struct Foo[T, U] {
  	a T
  	b U
  }

  foo := Foo{
  	a: 2
  	b: 'x'
  }

  println(foo)
  ```
- unsafe: dereferencing nil references is no longer allowed in the following case:
  ```v
  a := unsafe { nil }
  println(*a)
  ```

### OSes

- Added basic QNX support.

### Other changes

- Lots of documentation/readme improvements.
- Lots of playground improvements: [play.vlang.io](https://play.vlang.io), including a really cool
  feature: "Show generated C code".
- A new `[spawn_stack: 131072]` function attribute for controlling the max size of the stack of the
  spawned threads.
- Channel pop now works with an `or` block: `ch := <-self.item or { return none }`
- `it` has been renamed to `index` in array inits.
- "Is V still fast?" web-page has been sped up by splitting the result table into multiple years.

### Development

- GitHub Copilot summaries in PRs.

## V 0.3.3
*30 Jan 2023*
### Improvements in the language
- String interpolation simplified to just '${name}', enforced by vfmt, and updated in the entire code base.
- `go foo()` has been replaced with `spawn foo()` (launches an OS thread, `go` will be used for
  upcoming coroutines instead).
- vfmt now supports `// vfmt off` and `// vfmt on` for turning off the formatting locally for short snippets of code.
  Useful for keeping your carefully arranged matrices intact.
- Match branch range expressions with consts: `match x { const1...const2 {} }`
- Hot code reloading via `[live]` is now supported in imported modules, not just the main module.
- Syntax sugar for map inits without needing explicit casts for interfaces: `all.children := { "abc": rect, "def": ui.rectangle()}`.
- `$embed_file()` fixes, including variable args support.
- `none` fixes: no longer allowed to be used as a separate type, `dump()` support, not allowed inside `unsafe`.
- Const functions: `const y = term.yellow`, then `println(y('abc'))`.
- Builtin type names can no longer be used as identifiers.
- Generic `typeof[T]()`, `sizeof[T]()`, `isreftype[T]()` functions.
- Deprecated `-error-limit` in favour of the documented `-message-limit` option.
- Maps now support aliased keys.
- Operator overloading now works with reference types.
- Generic struct inits with nested generic structs and generic optional types are now allowed.
- During array creation, `len:` is required when using default values for the array.
- Optimized one byte `[]u8` arrays creation.
- Recursive aliasing is no longer allowed (e.g. `type Alias = map[string]Alias`).

### Breaking changes
- `[]` is now used for generics instead of `<>`.
- Accessing a pointer map value requires an `or {}` block outside `unsafe`.

### Checker improvements/fixes
- Lots of fixes in the type checker.
- Int signedness mismatch is now checked: `cannot use literal signed integer as u8`.

### Standard library
- `math.vec` module for generic vector math including 2D, 3D, and 4D vector operations.
- Builtin stb_image.h used by gg has been updated to the latest v2.28.
- All of vlib has been updated to use separate Option/Result types.
- To avoid confusion, all references in the code and documentation to `Optional` have been replaced with `Option`.
- `gg.Context` pipeline has more effects, including the `additive` effect.
- Much cleaner eof checks in `os`: refactor `err == IError(os.Eof{})` to `err is os.Eof`.
- Lots of work on `x.json2`, the pure V json encoder, soon to become official.
- New `v.reflection` module for runtime reflection.
- Improved `os.mv()`, which now works consistently even across different windows drives/mount points.
- `string.trim_indent()`, useful with multi line strings, that start/end with new lines and indentation.
- Reduced memory consumption in the `crypto` modules.
- Official V UI library is now licensed under MIT.
- Deprecated `math.util` and `math.mathutil` have been removed.
- New time format support: `time.format_rfc3339()`.
- `encoding.html.escape()`.
- All public functions in the `hash` and `encoding.base32` modules have been documented.
- New `crypto.pem` module.
- New `map.reserve()` method.

### Web
- Improved vweb stability under load.

### ORM
- Various ORM fixes and improvements, including string interpolation support, type checks, fn calls in `where`.

### Database drivers
- VFS support in the builtin `sqlite` module; `sqlite.get_affected_rows_count()`.
- Improved `pg` compatibility with older PostgreSQL versions before 2014.
- `sqlite`, `pg`, `mysql` have been moved to `db.sqlite`, `db.pg`, `db.mysql`.

### Native backend
- Operator support for floats, multi return.

### Comptime
- Improved compile time checks, like `$if x is Type {`; `$if T in [$Array, $Struct, $Alias, $Function] {`.
- `$for in` works with alias types.
- New comptime features for fields: `field.is_<field>`, `field.is_alias`, `field.is_enum`.

### OS support
- Installation instructions for using V on NixOS.
- Better `make` support for OpenBSD.
- Much improved experience for `v install pcre` on Windows (it now bundles its own .c files, so it compiles cleanly, even if the platform does not have another pcre package installed).
- V can now be compiled with tcc on latest macOS and Apple Silicon.

### Tools
- fast.vlang.io fixes & improvements, new server.
- New official IntelliJ plugin: https://intellij-v.github.io.
- Lots of new language documentation, a nicer table of contents.
- Improved documentation for most of the vlib modules
- `make.bat` & `v up` improvements on Windows.
- TeamCity test runner support via `v -test-runner teamcity foo_test.v`.
- CI optimizations for faster runs.
- New official AdventOfCode repo with AOC solutions, also added to CI.
- More detailed timings in `v -show-timings`.
- `v new <name> web` for quickly scaffolding new web projects.


## V 0.3.2
*31 Oct 2022*

### Improvements in the language
- New simplified string interpolation: `println("Hello, {name}!")`. It will be the only way, old syntax (`${name}` and `$name`)
  will be deprecated.
- Easier custom error creation: `return MyCustomErr{}` instead of `return IError(MyCustomErr)`.
- All floats outputs now have `.0` conditionally appended to them to improve clarity.
- Custom integer enum types: `enum Xyz as u64 {`.
- AST transformer fixes and optimizations.
- Stylistic improvements and bug fixes in vfmt.
- Casting integers to enums now requires `unsafe{}`.
- Improved error and warning messages.
- Parallel compilation now uses `sync.Pool`.
- `-skip-unused` fixes, soon to be made the default.

### Breaking changes
*No breaking changes*

### Checker improvements/fixes
- Improved type checker: lots of new type checks and fixed checker bugs.
- Unused last expression in `if` is now checked.
- Anonymous structs visibility issues fixed.

### Standard library
- `net.ssl` has been migrated from a dynamically linked OpenSSL to a statically linked Mbed TLS. This means that V binaries will no
  longer have an OpenSSL dependency. OpenSSL can still be enabled via `-d use_openssl`.
- msgpack module for decoding/encoding msgpack. (`v install msgpack`)
- Most of vlib has been updated to use the new Option/Result types.
- net, net.http, vweb bugs and fixes.
- QuadTree and RingBuffer types in `datatypes`.
- Forward iterator for `datatypes.LinkedList<T>`, forward and backward iterators for `datatypes.DoublyLinkedList<T>`.
- A new `maps` module, similar to existing `arrays`. It has generic `filter`, `flatten`, `invert`, `to_map`, `to_array`, `from_array`
  functions.
- `utf8.is_number()`, `utf8.is_space()` functions.
- New `encoding.base32` module.
- `gg.TouchPoint` to differentiate between different types of touch input.
- `str.int()` conversion speedup (without -prod).

### Web
- `vweb.csrf` module.

### ORM
- Support parenthesized expressions like `select from User where (name == 'Sam' && is_customer == true) || id == 1`.

### Native backend
- Lots of native backend improvements, including library calls, comptime conditionals, enums, method definitions/calls, structs.

### V interpreter
- Some further interpreter work.

### C backend
- cgen cleanups.

### OS support
- Removed the need for the `[console]` attribute in Windows GUI apps.
- More precise WINAPI declarations for easier integration on Windows.
- More CI tests on FreeBSD.

### Tools
- New stunning playground with an improved look and feel, a much better and more responsive editor,
  code sharing by link, more convenient keyboard control, reusability for potential embedding:
  https://play.vlang.io.
- Improved call tracing via `-trace-calls`.
- Lots of documentation improvements, including a better documentation of the recent Option/Result split.
- V REPL: Home/End keys support. Lots of clean up.




## V 0.3.1
*31 Aug 2022*

### Improvements in the language
- Anonymous structs.
- Lots of bug fixes: 90% of all bugs ever submitted are closed.
- New keyword/type: `nil`. Only to be used inside `unsafe`. Replaces `voidptr(0)`.
- V can now find code in the `src/` directory. This allows making V repos much cleaner.
- Support `assert condition, extra_message`, where the `extra_message` will be evaluated and shown if the assertion fails.
- Operator overloading now works with aliases and generics.
- Scanner optimizations.
- Using C's #define is no longer allowed in normal V code, only in `.c.v` files.

### Breaking changes
- Anonymous sumtypes have been removed (deprecated for now) due to complicating the language and the compiler too much.

### Checker improvements/fixes
- More type checks.
- Lots of fixes in `shared` types.

### Standard library
- `os.mkdir()` now has an optional `mode` parameter.
- `encoding.csv` is now generic, supports bools, accepts a custom delimiter, and is compatible with io.Reader/io.Writer.
- `datatypes` module now uses operator overloading.
- All `datatypes` types can be converted to V arrays.
- `smtp` improvements including multiple recipients and base64/utf8 support.
- `arrays.carray_to_varray<T>()` for converting C arrays to V arrays.
- `strconv.v_sprintf()` has been deprecated in favor of string interpolation.
- TOML module now supports `[toml:...]` attributes, just like the JSON module.
- `os.walk()` is no longer recursive (still works the same).
- `io` has been migrated to `Result`.
- Third party window control in Sokol.
- `string.replace_char()`, `math.round_sig()`.
- Improved multiplication performance in `math.big`.

### Web
- `net.urllib` ipv6 support.
- `net.Http.Response.text` renamed to `body`.
- `net.websocket` timeout is now configurable.

### ORM
- ORM functions now return `Result`, so the errors can be handled.

### Database drivers

### Native backend
- Major improvements to the fast native backend including linking support on Linux. The goal is to be able to self host V soon.

### V interpreter
- V interpreter improvements.

### C backend
- Parallelized cc step. Speeds up -prod and clang/gcc compilation by 300-500% (depending on
  the number of cores). Experimental and hidden behind a -parallel-cc flag, soon to be the default.
- Intel C compiler support.
- Go backend fixes.
- `#preinclude` for low level C interop.

### OS support
- Full termux support via `$if termux {`, more predictable logging on Android.
- Older macOS support (<10.12).
- Windows code has been removed from `v.c` distributed on non-Windows systems. (`v_windows.c` is used on Windows.)

### Tools
- DOOM is now translated/compiled and launched on CI servers. A screenshot of the running game
  is made via `vgret` and is compared to the expected result.
- VLS performance improvements, especially on Windows.
- `v ls` tool for installing, for updating, and for launching VLS (V Language Server).
- `v doc` now has syntax highlighting.




## V 0.3
*30 Jun 2022*
- C to V translation via C2V: `v translate file.c`. (Demo video: [Translating DOOM from C to V, building it in under a second and running it!](https://www.youtube.com/watch?v=6oXrz3oRoEg))
- Lots of bug fixes in V, cgen, and C interop to allow running translated DOOM.v.
- Programs built with the V compiler no longer leak memory by default.
- Closures. All operating systems are supported. ([Demo](https://twitter.com/v_language/status/1528710491882852352))
- `Option` and `Result` are now separate types: `?Foo` and `!Foo` respectively. Old code will continue working for 1 year and will result in a warning/hint.
- Hundreds of new checks in the type checker.
- All V's backends have been split up into separate processes.  As the result, building V got 26% faster.
- Maps and arrays can now return options: `m[bad_key] or { ... }`, `if x := arr[key] { ... }`.
- `ustring` has been replaced with `[]rune` (works just like in Go).
- Maps can now have non-string keys.
- A new compiler pass for transforming the AST (doesn't slow the compiler too much, adds about 25ms to `v self`). It eliminates unreachable branches and performs other simple optimizations and transformations.
- C backend is now parallel (just the cgen part for now).
- Lots of compiler source code clean up and minor optimizations. The compiler got ~30% faster according to fast.vlang.io.
- Better compiler source code organization (absolutely necessary as it's surpassed 100k loc).
- The naming of V's integer types is now more consistent: `byte` has been renamed to `u8`.  Old code will continue working for 1 year and will result in a warning/hint.
- The typo detector now highlights the suggested name so that it's more visible.
- `datatypes` module now has `Heap, Queue, Stack, BSTree, LinkedList`.
- Interfaces can now be embedded (like structs).
- vlib now has a TOML parser, fully compatible with TOML 1.0.
- Lots of work done on the V.js backend, including the graphics library, which has been ported to V.js.
- JS promises, await (V.js).
- It's now possible to do more complex array initialization by using each individual element of the array (`[]int{init: it}`).
- Unsigned right shift operators `>>>` and `>>>=` have been added to V. (They work exactly like in Java.)
- `-nofloat` option, which is useful for writing kernels and for embedded systems without an FPU (used in Vinix).
- Generic interfaces.
- TCC is now bundled with the language, this allows building V programs without an external C compiler dependency.
- Null can be used in `unsafe` only (for example, for C interop).
- Pointer arithmetic and comparing pointers to numbers is now also only allowed in `unsafe`.
- Inline sumtypes.
- New module `compress.gzip`.
- Lots of `net`/`net.http`/`vweb` fixes (also used for the upcoming Gitly launch).
- IPv6 support.
- `net.http` headers are now enum fields instead of strings. This allows to avoid typos and offers autocomplete.
- Struct field deprecation.
- Static GC (no longer a dynamic lib dependency).
- New various algorithms for random number generation: MT19937RNG, etc  (module `rand`).
- Fix immutability bugs that allowed to bypass compiler immutability checks and modify const/immutable values.
- Lots of fixes in the JSON serializer.
- Heap allocated only structs marked with `[heap]`.
- Significantly improve lots of error messages, make them more clear, suggest hints.
- Bug fixes and new features in the pure V `regex` module.
- Lots of new drawing functions in the graphics module (like `gg.draw_polygon_filled(), gg.draw_arc_empty()` etc)
- Builtin FPS display in `gg`.
- Latest Sokol backend in `gg`.
- Advanced CI tests for the graphics module. Graphical apps are run on GitHub Actions instances, their output is saved to an image, uploaded, and compared to the expected result.
- More bug fixes in generics.
- Bug fixes in aliases. They can now fully replace the types they alias.
- `[minify]` struct attribute for struct minification.
- `for in` now works with fixed arrays.
- The parser was made a bit faster by skipping `vfmt` code when not in `vfmt` mode (by using `-d vfmt`).
- Lots of vfmt improvements, especially with comments.
- Experimental `#[index]` syntax for negative indexing (like in Python, but needs special syntax instead of being used by default).
- Visibility bug fixes in modules (`pub`).
- Error propagation in complex expressions (e.g. `foo(bar()?)`).
- Optionals can now by used in consts (`const x := opt() or {}`).
- Lots of new documentation, including vlib modules documentation and the official V Documentation.
- vpm improvements (including a new vpm mirror).
- `sync` improvements including `sync.thread_id()`, `sync.Once`..
- V can now be used to generate object files (`foo.o`) that can be used in existing C projects.
- `-usecache` and `-skip-unused` fixes, they are close to being on by default.
- Lots of Windows issues fixed.
- Amazon Linux support.
- Fixes in shared maps and arrays.
- `term.ui` improvements, including multi byte/UTF-8 events.
- New `crypto` modules, including `crypto.des, crypto.cipher, crypto.blowfish`.
- Comptime fixes.
- 4 byte bool option (`-d 4bytebool`) for compatibility with some C software.
- `strconv` (pure V formatting module used in string interpolation) fixes and performance improvements.
- ORM fixes (pg, mysql, sqlite). Tables are now created automatically based on the V structs, no more need in sql files to create tables for apps.
- `volatile` keyword.
- `"stringliteral".len` optimization (replaced by the actual number by the new `transform` pass).
- Lots of inline assembler improvements (it's used a lot in Vinix).
- Many new functions in the `math` module.
- Separators in number literals: `1_000_000`.
- `strings.Builder` optimizations and new methods.
- Autofree fixes (still not production ready, hidden behind the `-autofree` flag).
- Lots of Android fixes in V and in vab.
- Lots of commits to the native backend (amd64/arm64).
- V interpreter fixes. (Still at an early stage.)
- Go2V translator has been started by the community, and can already translate simple programs.
- An early version of the Go backend (`v -b go -o file.go file.v`).

## V 0.2.4
*30 Aug 2021*
- Introduce `isize` and `usize` types, deprecate `size_t` in favor of `usize`.
- Add `datatypes` and `datatypes.fsm` modules.
- Add `compile_error` and `compile_warn` comptime functions.
- Bare metal support. Vinix OS kernel is now being developed in V.
- Builtin web framework vweb is now multithreaded, all CPU cores are used.
- String interpolation and struct stringers are now implemented in pure V
with a much cleaner and faster implementation. Previously libc's `sprintf`
was used.
- Improved `unused variable` warning. Assigning to a variable no longer marks it as used.
*... lots of missing changelog for this version, sorry (will update a bit later)*

## V 0.2.2 - 0.2.3
*22 Jan 2021*
- Allow interfaces to define fields, not just methods.
- `vweb` now uses struct embedding: `app.vweb.text('hello') => app.text('hello')`.
- Consts can now be declared outside of `const()` blocks: `const x = 0`.
- Overloading of  `>`, `<`, `!=`, `==`, `<=` and `>=` operators.
- New struct updating syntax: `User{ ...u, name: 'new' }` to replace `{ u | name: 'new' }`.
- `byte.str()` has been fixed and works like all other numbers. `byte.ascii_str()` has been added.
- Smart cast in for loops: `for mut x is string {}`.
- `[noinit]` struct attribute to disallow direct struct initialization with `Foo{}`.
- Array decompose: `[1, 2, 3]...` is now `...[1, 2, 3]`
- Treating `enum` as `int` and operations on `enum` except `==` and `!=` are removed for strict type checking.
- Support `[manualfree] fn f1(){}` and `[manualfree] module m1`, for functions doing their own memory management.
- Allow usage of `<` and `>` operators for struct in `.sort` method for arrays, i.e. `arr.sort(a < b)`.
- Auto generate assignment operators like `+=`, `-=`, `*=`, `/=` and `%=` if the operators are defined.
- Colorize and improve failing tests output.
- Fix `go` with a generic function: `go test<string>(c, 'abcd')`.
- Add comptime `x := $embed_file('v.png') println(x.len) println(ptr_str(x.data()))`, for embedding files into binaries.
- Advanced vdoc search on mobile layout.
- string's `left()`/`right` were removed in favor of slicing syntax: `str[..pos]`.
- gg: native graphics mode on macOS/iOS (using Cocoa Drawing API).
- Full path to consts must be specified everywhere. This makes it easy to distinguish them
from local variables.
- `__offsetof` for low level needs (works like `offsetof` in C).
- vfmt now preserves empty lines, like gofmt.
- Support for compile time environment variables via `$env('ENV_VAR')`.
- Allow method declaration of `==` and `<` operators and auto generate `!=`, `>`, `<=` and `>=`.
- support `dump(expr)`, i.e. tracing of both the location, name and value of an expression
- deprecate os.exec in favour of os.executable() which does *NOT* return an option, when the command was not found

## V 0.2.1
*30 Dec 2020*
- Hashmap bootstrapping fixes.
- Array decomposition to varargs: `fn sum(i ...int) int` => `a := [2,3,4] println(sum(a...))`
- HTML module docs generated by vdoc now have global search.

## V 0.2
*22 Dec 2020*
- Compile-time memory management via `-autofree` (not production ready yet). [Video demonstration](https://www.youtube.com/watch?v=gmB8ea8uLsM).
- Channels and locks.
- Thread safe typed arrays via keyword `shared`.
- Struct embedding.
- IO streams (`io.Reader`, `io.Writer` etc).
- A powerful websocket module that conforms to RFC 6455 and passes the Autobahn test suite (498 client tests and 249 server tests).
- The `net` module is now non blocking and is more feature complete providing similar API to Go.
- V's graphics module now uses Metal/DirectX/OpenGL instead of just OpenGL.
- V can now run in the browser via WASM and execute V code by translating it to JavaScript:
https://v-wasm.now.sh
- V binaries for Linux/Windows/macOS are now built and deployed automatically via GitHub Actions.
- Smart casting for sumtypes and interfaces, including complex expressions: `if x.expr is int { println(x.expr + 1) }`.
- Clean and easy way to sort arrays: `users.sort(a.name > b.name)`.
- A huge amount of `vfmt` fixes and improvements. It has now reached a point where it can be safely used on any V source file.
- A new CI job that runs `v fmt -verify` on the entire code base, a new command that makes sure the file/directory
has been vfmt'ed. This ensures that all code submitted to the V project is formatted.
- A new tool `v vet` for analyzing the project and finding potential bugs and errors.
- A new `term.ui` module for building dynamic terminal UIs with an example editor written in it.
- Early iOS and Android support.
- All missing ORM features from the old backend were brought back.
- Magic `it` variable has been replaced with smart casts (the change is completely handled by vfmt).
- Cross-compiling to Windows and Linux brought back.
- C2V can now generate wrappers. Example: https://github.com/medvednikov/libsodium. (C2V will be released by 0.3)
- C++ compiler support: code, generated by the C backend can now by compiled by C++ compilers.
- Short generics syntax: `foo(5)` instead of `foo<int>(5)`.
- Cached modules via `-usecache`. Faster compilation due to not needing to rebuild the entire vlib for
each program. Will be enabled by default in 0.2.1.
- New improved sum types implementation.
- Lots of errors that happen often during the development cycle were turned into warnings to increase
  development speed. They are still errors in production builds.
- Labeled `break` and `continue`.
- Lots of documentation. The official language documentation grew 3 times in size.
- `modules.vlang.io` is now generated automatically on every commit.
- Builtin compile-time JSON serializer now supports `time.Time`.
- Fixes in type aliases, to make them behave just like the types they alias.
- `array.contains(element)` is now generic.
- Lots of improvements in the JS backend and its type system.
- Simpler and more constinent function arg syntax: `foo(a int, b int, c string)` instead of `foo(a, b int, c string)`
- Lots of fixes and optimizations in the hashmap.
- Lots of missing checks in the type checker were added (for example, checking the correct usage of public struct fields).
- Mutability bug fixes
- Taking the address of a map value is no longer allowed, like in Go.
- Matrix multiplication.
- A new `#pkgconfig` flag to provide platform independent way to get compilation flags for C libraries/packages.
- Explicit parentheses requirement in complex boolean expressions.
- `println` was made even smarter, and can now handle complex types.
- Precompiled text templates can now be used outside of vweb via `$tmpl()`.
- Gitly, a big web application written in vweb has been released: https://github.com/vlang/gitly
- `['/:arg1/:arg2/action']` vweb action attribute for easily getting query parameters assigned to method arguments.
- Improved performance of text rendering, `gg.text_width()`.
- Webview module in V UI.
- Binary enum flags.
- `[export]` attribute to change exported function name (for example for calling from a C library).
- `unsafe` fixes and improvements.
- Improvements to rand: `rand.ulid()`, `rand.uuid()`, a unified customizable PRNG API.
- Hundreds of other fixes, features, and tests (from now on the changelog will be updated
right away as the feature/bug fix lands).


## V 0.1.27
*5 May 2020*

- vfmt has been re-written from scratch using the new AST parser.
    It's much faster, cleaner, and can format
files with compilation errors.
- `strconv`, `sprintf`, and `printf` in native V, without any libc calls.
- Interfaces are now a lot more stable and have all expected features.
- Lots of x64 backend improvements: function calls, if expressions, for loops, local variables.
- `map()` and `filter()` methods can now be chained.
- New `[]int{cap:cap, len:len}` syntax for initializing array length and capacity.
- New `is` keyword for checking the type of sum types and interfaces.
- `as` can now be used to cast interfaces and sum types.
- Profiling with `-profile`. Prints a nice table with details about every single function call:
    number of calls, average time per call, total time per function
- `import(xxx)` syntax has been removed in favor of `import xxx` for simplicity and greppability.
- Lots of fixes and improvements in the type checker.
- `time.StopWatch`
- `dl` module for dynamic loading.
- Automatic `str()` method generation for every single type, including all arrays.
- Short struct initialization syntax for imitating named function args: `foo(bar:0, baz:1)`.
- New operator `!in`.
- Performance improvements in critical parts of the builtin data structures (array, map).
- High order functions improvements (functions can now be returned etc).
- Anonymous functions that can be defined inside other functions.
- Built-in JSON module is back.
- Lots and lots of new tests added, including output tests that test error messages.
- Multiple errors are now printed, the compiler no longer stops after the first error.
- The new JS backend using the AST parser (almost complete).
- Variadic functions.
- `net.websocket` module (early stage).
- `vlib` is now memory leak free, lots of `autofree` improvements.
- Simplified and cleaned up `cmd/v`, `v.builder`.
- V UI was updated to work with the new backend.


## V 0.1.25
*1 Apr 2020*

- The entire compiler has been re-written with an AST parser.
    The code is now a lot cleaner and more maintainable.
    ~15k lines of old compiler code were removed.

## V 0.1.24
*31 Dec 2019*

- A new parser/generator built on top of an AST that simplifies code greatly
    and allows to implement new backends much faster.
- Sum types (`type Expr = IfExpr | MatchExpr | IntegerLiteral`).
- B-tree map (sped up the V compiler by ~10%).
- `v fmt -w`.
- The entire code base has been formatted with vfmt.
- Generic structs.
- SDL module.
- Arrays of pointers.
- os: `is_link()`, `is_dir()`, `exists()`.
- Ranging through fixed size arrays.
- Lots of fixes in ORM and vweb.
- The first tutorial: [building a simple web application with vweb](https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_vweb/README.md)
- Match expressions now must be exhaustive.
- freestanding: `malloc()`/`free()`.
- `++` is now required instead of `+= 1` for consistency.
- Interpolated strings now allow function calls: `println('val = $get_val()')`.
- `string.replace_each([])` for an efficient replacement of multiple values.
- More utf8 helper functions.
- `-prealloc` option for block allocations.
- `type` aliases.
- Running `v` with an unknown command will result in an error.
- `atof` implementation in pure V.
- Enums can now have negative values.
- New `filepath` module.
- `math.factorial`.
- `ftp` module.
- New syntax for casting: `val as Type`.
- Fewer libc functions used (soon V will have no dependency on libc).


## V 0.1.23
*30 Nov 2019*

- [Direct x64 machine code generation](https://github.com/vlang/v/issues/2849).
    Hello world being built in 3 milliseconds.
- Bare metal support via the `-freestanding` flag, to build programs without linking to libc.
- Prebuilt V packages for Linux, macOS, and Windows.
- `string.index()` now returns `?int` instead of `int/-1`.
- Lots of fixes in Generics.
- vweb framework for developing web applications is back.
- Vorum, the forum/blogging software written in vweb, can now be compiled and has been added to CI.
- REPL, `v up` have been split up into separate applications to keep the core V compiler small.
- V now enforces short enum syntax (`.green` instead of `Color.green`) when it's enough.
- V UI for macOS.
- Interfaces have been rewritten. `[]interface` support.
- `os.cp()` for copying files and directories.
- Additional compile-time flags: `$if clang, msvc, mingw, x32, x64, big_endian, little_endian {`.
- All C functions now have to be declared, all missing C functions have been defined.
- Global variables (only with the `-enable-globals` flag)
    for low level applications like kernels and drivers.
- Nothing can be cast to bool (previously code like `if bool(1) {` worked).
- `<<` and `>>` now work with all integer types.
- V detects Cygwin and shows an error (V supports Windows natively).
- Improved type checking of some operators (`%, |, &` etc).
- Windows 7 support.
- `println(true)` now prints `true` instead of `1`.
- `os.exec()` now uses `CreateProcess` on Windows.
- fast.vlang.io website for monitoring the performance of V after every commit.
- On Windows Visual Studio is now used automatically if GCC is not installed.
- vfmt!
- Lots of cleaning up in the compiler code.
- Multi-level pointers in unsafe code (`****int`).
- MSVC backtrace.
- `$if os {` blocks are now skipped on a different OS.
- C string literals (`c'hello'`).
- AlpineLinux/musl fixes + added to CI.
- Inline assembly.
- Clipboard module (Windows, macOS, X).
- `foo()?` syntax for error propagation.
- Docs have been migrated from HTML to `doc/docs.md`.
- `eventbus` module.
- Haiku OS support.
- `malloc/free` on bare metal.
- `utf8` helper functions (`to_lower()`, `to_upper()`, etc).
- Optimization of `for c in str {`.
- `string/array.left/right/slice/substr` were removed (use `[a..b]` slicing syntax instead).



## V 0.1.22
*28 Oct 2019*

- Generic functions (`fn foo<T>(bar T) T {`) with varargs support.
- `array[start..end]` and `string[start..end]` slicing syntax.
- Optimized `array.filter()` and `array.map()`.
- `sqlite` module.
- Cached modules for faster compilation.
- Dramatic compilation optimizations: [V now compiles itself in 0.10 - 0.30 seconds](https://github.com/vlang/v/wiki/The-V-language-now-compiles-itself-in-0.09-seconds)
- V scripts (simpler and cross-platform alternative to Bash).
- Infinite multi-dimensional arrays (`[][][]int`).
- `unsafe`.
- `[deprecated]` attribute.
- `[if]` function attributes for compile time function exclusion for performance.
- `switch` has been completely removed from the language and replaced by
`match` everywhere.
- `pub struct` and `pub const`, previously all structs and consts were public
by default.
- `musl` support (V can now run on, for example, Alpine Linux).
- Module header generation. V now supports closed source modules, which are still
used in some industries.
- Constants were added to typo suggestions.
- `color in [.green, .red, .blue]` now works without specifying `Color.green`.
- V compiler is now a module that can be used by other programs.
- Backtraces now have source lines on Linux.
- `runtime.nr_cpus()`.
- `fn init()` for module initialization.
- `a in [1, 2, 3]` optimization: no array gets allocated.
- Raw strings: `s := r'hello\nworld'`.
- `if a := func() { }` syntax for handling options.
- f32/f64 comparison now uses machine epsilon by default.


## V 0.1.21
*30 Sep 2019*

- `none` keyword for options.
- Solaris support.
- All table lookup functions now use `none`.
- varargs: `fn foo(bar int, params ...string) {`.
- Double quotes (`"`) can now also be used to denote strings.
- GitHub Actions CI in addition to Travis.
- `-compress` option. The V binary built with `-compress` is only ~90 KB!
- More memory management.
- Unused modules result in an error.
- "Unused variable/module" errors are now warnings in non-production builds.
- Duplicate methods with the same name can no longer be defined.
- Struct names must be capitalized, variable/function names must use snake_case.
- Error messages are now even nicer!
- Lots of fixes in automatic `.str()` method generation for structs and arrays.
- ~30% faster parser (files are no longer parsed separately for each pass).
- `_` is no longer a variable, but an actual syntax construct to skip unused values, like in Go.
- Multiple returns (`fn foo() (int, string) {`).
- `!` can now only be used with booleans.


## V 0.1.20
*17 Sep 2019*

- JavaScript backend!
- Hundreds of C warnings were fixed. `gcc v.c` now builds without
any warnings.
- The mutability check now applies to function args (mutable
receivers that are not modified result in a compilation error).
- V tests now show how long each test took.
- Official Android support (only console applications via Termux for now).
- Typo check. If a variable/function/module etc is misspelled,
V will suggest the correct name.
- Lots of Microsoft C fixes, and a separate Travis instance for
this backend.
- Bitwise operators `|`, `^`, `&` no longer work with booleans.


## V 0.1.19
*12 Sep 2019*

- Lots of refactoring, simplifications, and optimizations in the compiler.
- Experimental memory management at compilation (only for the V compiler itself for now).
- Lots of ORM fixes.
- Functions can now be inlined via the `[inline]` attribute.
- New `mysql` module.
- Better error format that is supported by all major editors (go to error).
- Error messages now point to the actual place where the error happened.
- Custom json field names: `struct User { last_name string [json:lastName] }`.
- Raw json fields via the `[raw]` attribute.
- All C code was removed from the `freetype` module.
- `gg` module can now render all Unicode characters.
- `[typedef]` attribute for imported C struct typedefs.
- Support of Objective C interfaces (primarily for using Cocoa).
- REPL: clear command and custom functions.
- REPL tests (which are also used for testing certain compiler errors).
- Syntax bug fixed: `foo[0] += 10` is now possible.
- http: support plain HTTP protocol and follow redirects.
- http: header data is now processed correctly.
- net: basic UDP support.
- `import const` was removed from the language.
- `array.contains()` was removed from the language (`in` should be used instead).
- `[0; len]` syntax was removed (replaced with a simpler `[0].repeat(len)`)
- Primitive aliases were removed to simplify the language.
- GitHub supports V now!
- Backtraces are now printed on panics.
- A new awesome `readline` module.
- V.c is now regenerated automatically after every commit.
- A bug with struct ordering was fixed, now structs can be declared in any order.
- V modules can now be built with `v build module`.
- `@FILE, @LINE, @FN, @COLUMN` for debugging.


## V 0.1.18
*16 Aug 2019*

- Built-in ORM (`uk_customers = db.select from Customer where country == 'uk' && nr_orders > 0`).
- Map initialization syntax: `m := { ‘foo’: ‘bar’, ‘baz’: ‘foo’ }`.
- `map.delete(key)`.
- `libcurl` dependency was removed from the `http` module.
- All function arguments are now immutable by default (previously they could be
  modified inside the function).
- `http` functions now return options.
- `sync.WaitGroup`.
- `vweb` static files serving.
- `crypto.rand` module.
- `v up` to update V.
- SChannel support on Windows.
- `net.urllib` module.
- vpm package manager, `v install`.
- `()` are now required in complex bool expressions: `(a && b) || c` instead of `a && b || c`.
- All arrays now have a default `.str()` method.
- Bootstrapping V with MSVC.
- Experimental `≠` etc support.
- `encoding.csv` module.
- `$if debug {` for running code in debug mode only.
- Map struct fields are now initialized automatically, just like arrays.
- Maps now support array values.
- `json` functions can no longer be used if the `json` module is not imported.


## V 0.1.17
*29 Jul 2019*
- `vweb` module for developing web apps in V.
- vtalk, open source V forum software.
- Generics (very limited right now, but they will be gradually improved).
- Comptime codegen (`foo.$method()` where `method` is a string).
- @ for escaping keywords (e.g. `struct Foo { @type string }`).
- Windows Unicode fixes (V can now work with non-ASCII paths etc on Windows).
- Fix mutable args bugs + don't allow primitive arguments to be modified.
- Declaring a mutable variable and never modifying it results in a compilation error.
- Interactive debugging support.
- `sync` module for Windows.
- `#!` support on Unix systems (V scripts).
- Lots of Visual Studio fixes.
- `crypto.aes` and `crypto.rc4` modules.
- Internal modules.


## V 0.1.16
*23 Jul 2019*
- V can now be used with Visual Studio!
- Hot code reloading now works with graphical applications (e.g. graph.v, bounce.v).
- Compile time memory management for arrays.
- High order functions.
- `match` expression (replacing `switch`).
- Import cycle detection.
- `crypto/md5`, `crypto/sha256`, and `crypro/sha512` modules.
- `os.executable()` - a cross platform function that returns full path to current executable.
- `~/.vlang` and `VROOT` were removed entirely. The installation is a lot cleaner now.
- V can now be packaged for all Linux distros.
- Arch Linux package.
- `string(bytes_buffer, len)`, `string(bytes_array)` casts.
- Multiple `defer`s.
- `key in map` syntax (replacing `map.exists(key)`).


## V 0.1.15
*15 Jul 2019*
- FreeBSD, OpenBSD, NetBSD, DragonFly support.
- Hot reloading now works with graphical applications: [bounce.v](examples/hot_reload/bounce.v)
- VROOT was removed, the installation process is now much simpler.
- `defer` statement.
- map.v was re-written. It's now much faster.
- `for key, val in map` syntax.
- `flag` module for parsing command line arguments.
- `zip` module.
- `crypto/sha1` module.
- Submodules and module aliases (`import encoding.base64 as b64`).


## V 0.1.14
*12 Jul 2019*
- `gg` module Windows support, V Tetris runs on Windows.
- Compile `glad` and `cJSON` only once. Programs using `gg` or `json` compile a bit faster.
- `v.c` has been cleaned up and minimized (~16k => ~10k lines of code).
- `type` aliases can now have methods.
- Const overflow check during compilation (`byte(1000)` will no longer compile).


## V 0.1.13
*10 Jul 2019*
- New enum syntax (`token == .name`), enum values are no longer global consts.
- Submodules (`import encoding.base64`).
- Hot code reloading.
- Special `err` variable for getting error values.
- Complex numbers.
- `<<` can now append arrays (`numbers << [1, 2, 3]`).
- Lots of Windows fixes (Windows still needs some work).
- Lots of REPL improvements (e.g. `>> 2 + 3` works now, no `println` required).
- The website was made easily translatable, it's now partially available in several languages.


## V 0.1.12
*4 Jul 2019*
- V can finally compile itself on Windows (https://github.com/vlang/v#mingw-w64).
- `os` module now uses options in all functions that return `File`.
- Lots of bugs with options were fixed.
- `println` was optimized. It no longer results in allocations.
    Now it also works correctly with all integer types.
- Lots of `vfmt` fixes, it will be enabled tomorrow.
- New `strings` module.
- Lots of other fixes and improvements, thanks to all the contributors.


## V 0.1.11
*1 Jul 2019*
- Cross compilation for Windows!
- Lots of Windows fixes.
- socket.v.
- maps fixed.


## V 0.1.9 - 0.1.10
*29 Jun 2019*
- Windows support via MinGW-w64. Pre-built Windows binary.
- File structure has been simplified: all vlib modules were moved to the vlib/ directory,
  makefile was moved to the root.
- One single archive with pre-built binaries for all operating systems.
- `mut var := val` was fixed (previously `mut var = val` was allowed as well).


## V 0.1.8
*28 Jun 2019*
- Single file programs without `fn main` now work as expected.
- REPL has been fixed: it now supports imports, consts, function definitions, etc.


## V 0.1.7
*27 Jun 2019*
- All C code in the compiler and vlib has been replaced with V.
- `#` syntax for embedding C code has been removed.
- Exported functions now need to be marked with `pub`, all public vlib functions have been updated.
- CI has been set up (Travis + Azure). On every commit and PR it is made sure that V
  can compile itself, all tests pass, and all examples compile.
- More tests have been uploaded.
- Cleaner bytes to string conversion: `tos2(bytes)` => `string(bytes)`.
- The home page has 3 more examples next to 'hello world' that show the features of the language.
- Lots of bugs and issues fixed.
