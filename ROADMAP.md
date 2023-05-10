## [Version 0.4]

- [ ] [Coroutines](https://github.com/vlang/v/discussions/11582)
- [ ] [Thread safe maps](https://github.com/vlang/v/discussions/11729)
- [ ] Parallel parser
- [ ] Parallel checker
- [ ] Parallel C compilation
- [ ] `recover()` from panics
- [x] vfmt: add missing imports (like goimports)
- [ ] Recursive structs via options: `struct Node { next ?Node }`
- [x] First class Option type
- [x] Optional function struct fields
- [ ] Handle function pointers safely, remove `if function == 0 {`
- [x] Bundle OpenSSL like GC
- [x] Anonymous structs
- [ ] -usecache on by default
- [ ] -skip-unused on by default
- [ ] `any` type
- [ ] `copy()` builtin function (e.g. for easier conversion from `[]Foo` to `[4]Foo`)
- [ ] A better documentation platform
- [ ] Improve vweb: allow separation of logic via "controllers", lots of other fixes
- [ ] 64/32 bit int depending on arch (will remove array.len limitation on 64 bit systems)

## [Version 1.0]

- [ ] Cross compilation of C
- [ ] Big remaining bugs fixed
- [ ] More powerful comptime
- [ ] Constraits for generics
- [ ] Autofree memory management option ready for production
- [ ] C2V supporting entire C99 standard

### Tooling

- [ ] More stable VLS
- [ ] Profiler improvements
  - [ ] Basic interactive shell with search, sort, filter, etc.
- [ ] VPM
  - [ ] New VPM site
  - [ ] Package versioning

### Web

- [ ] Site that brings everything together in a single style
- [ ] Interactive educational platform (learning to program for beginners)
