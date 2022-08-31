## V 0.3.1
*31 Aug 2022*
- Anonymous structs.
- Lots of bug fixes: 90% of all bugs ever submitted are closed.
- Major improvements to the fast native backend including linking support on Linux. The goal is to be able to self host V soon.
- Parallelized cc step. Speeds up -prod and clang/gcc compilation by 300-500% (depending on
  the number of cores). Experimental and hidden behind a -parallel-cc flag, soon to be the default.
- New keyword/type: `nil`. Only to be used inside `unsafe`. Replaces `voidptr(0)`.
- V can now find code in the `src/` directory. This allows making V repos much cleaner.
- Intel C compiler support.
- Older macOS support (<10.12).
- `os.mkdir()` now has an optional `mode` parameter.
- Full termux support via `$if termux {`, more predictable logging on Android.
- Go backend fixes.
- More type checks.
- DOOM is now translated/compiled and launched on CI servers. A screenshot of the running game
  is made via `vgret` and is compared to the expected result.
- VLS performance improvements, especially on Windows.
- `v ls` tool for installing, for updating, and for launching VLS (V Language Server).
- Support `assert condition, extra_message`, where the `extra_message` will be evaluated and shown if the assertion fails.
- Anonymous sumtypes have been removed (deprecated for now) due to complicating the language and the compiler too much.
- `encoding.csv` is now generic, supports bools, accepts a custom delimiter, and is compatible with io.Reader/io.Writer.
- Operator overloading now works with aliases and generics.
- `datatypes` module now uses operator overloading.
- All `datatypes` types can be converted to V arrays.
- `smtp` improvements including multiple recipients and base64/utf8 support.
- `v doc` now has syntax highlighting.
- `arrays.carray_to_varray<T>()` for converting C arrays to V arrays.
- `strconv.v_sprintf()` has been deprecated in favor of string interpolation.
- TOML module now supports `[toml:...]` attributes, just like the JSON module.
- `os.walk()` is no longer recursive (still works the same).
- Windows code has been removed from `v.c` distributed on non-Windows systems. (`v_windows.c` is used on Windows.)
- ORM functions now return `Result`, so the errors can be handled.
- `#preinclude` for low level C interop.
- `net.urllib` ipv6 support.
- Lots of fixes in `shared` types.
- `io` has been migrated to `Result`.
- Third party window control in Sokol.
- Scanner optimizations.
- `string.replace_char()`, `math.round_sig()`.
- Improved multiplication performance in `math.big`.
- `net.Http.Response.text` renamed to `body`.
- Using C's #define is no longer allowed in normal V code, only in `.c.v` files.
- V interpreter improvements.
- `net.websocket` timeout is now configurable.


## V 0.3
*30 Jun 2022*
- C to V translation via C2V: `v translate file.c`. (Demo video: [Translating DOOM from C to V, building it in under a second and running it!](https://www.youtube.com/watch?v=6oXrz3oRoEg))
- Lots of bug fixes in V, cgen, and C interop to allow running translated DOOM.v.
- Programs built with the V compiler no longer leak memory by default.
- Closures. All operating systems are supported. ([Demo](https://twitter.com/v_language/status/1528710491882852352))
- `Option` and `Result` are now separate types: `?Foo` and `!Foo` respectively. Old code will continue working for 1 year and will result in a warning/hint.
- Hundreds of new checks in the type checker.
- All V's backends have been split up into separate processes.  As the result, building V got 26% faster.
- Maps and arrays can now return optionals: `m[bad_key] or { ... }`, `if x := arr[key] { ... }`.
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
- Pointer arithmetics and comparing pointers to numbers is now also only allowed in `unsafe`.
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
- `os.cp()` for copying files and directores.
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
- `if a := func() { }` syntax for handling optionals.
- f32/f64 comparison now uses machine epsilon by default.


## V 0.1.21
*30 Sep 2019*

- `none` keyword for optionals.
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
  modifed inside the function).
- `http` functions now return optionals.
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
- `os` module now uses optionals in all functions that return `File`.
- Lots of bugs with optionals were fixed.
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
