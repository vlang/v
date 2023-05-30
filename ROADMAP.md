## [Version 0.3]
- [x] gc option
- [x] channels
- [x] lock{}
- [x] thread safe arrays
- [x] rune type
- [x] replace `ustring` with `[]rune`
- [x] fix `byte.str()`
- [x] maps with non-string keys
- [x] iOS/Android support
- [x] parallel cgen
- [x] IO streams
- [x] struct embedding
- [x] interface embedding
- [x] interfaces: allow struct fields (not just methods)
- [x] short generics syntax (`foo(5)` instead of `foo<int>(5)`)
- [x] more advanced errors, not just `error('message')`

## [Version 0.4]

- [x] [Coroutines](https://github.com/vlang/v/discussions/11582)
- [x] vfmt: add missing imports (like goimports)
- [x] Recursive structs via options: `struct Node { next ?Node }`
- [x] First class Option type
- [x] Optional function struct fields
- [ ] Handle function pointers safely, remove `if function == 0 {`
- [x] Bundle OpenSSL like GC
- [x] Anonymous structs
- [x] Improve vweb: allow separation of logic via "controllers", lots of other fixes
- [x] New VPM site

## [Version 0.5]

- [ ] [Thread safe maps](https://github.com/vlang/v/discussions/11729)
- [ ] Parallel parser
- [ ] Parallel checker
- [ ] Parallel C compilation
- [ ] `recover()` from panics
- [ ] -usecache on by default
- [ ] -skip-unused on by default
- [ ] 64/32 bit int depending on arch (will remove array.len limitation on 64 bit systems)
- [ ] `copy()` builtin function (e.g. for easier conversion from `[]Foo` to `[4]Foo`)

## [Version 1.0]

- [ ] Cross compilation of C
- [ ] Big remaining bugs fixed
- [ ] More powerful comptime
- [ ] Constraits for generics
- [ ] Coroutines on Windows
- [ ] Autofree memory management option ready for production
- [ ] C2V supporting entire C99 standard

### Tooling

- [ ] More stable VLS
- [ ] Profiler improvements
  - [ ] Basic interactive shell with search, sort, filter, etc.
- [ ] VPM
  - [x] New VPM site
  - [ ] Package versioning
- [ ] A better documentation platform

### Web

- [ ] Site that brings everything together in a single style
- [ ] Interactive educational platform (learning to program for beginners)
