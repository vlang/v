# 0.2
_tbd_

# 0.3
- [x] channels
- [x] lock{}
- [x] thread safe arrays
- [x] iOS/Android support
- [x] short generics syntax (`foo(5)` instead of `foo<int>(5)`)
- [ ] make `-autofree` the default
- [ ] coroutines
- [ ] thread safe maps
- [ ] C2V translator
- [ ] doom.v
- [ ] rune type, replace `ustring` with `[]rune`, fix `byte.str()`
- [ ] maps with non-string keys
- [ ] parallel parser (and maybe checker/gen?)
- [ ] `recover()` from panics
- [ ] IO streams
- [ ] struct embedding (partially)
- [ ] interface embedding
- [ ] interfaces: allow struct fields (not just methods)
- [ ] vfmt: fix common errors automatically to save time (make vars mutable and vice versa, add missing imports etc)
- [ ] method expressions with an explicit receiver as the first argument
- [ ] fix all remaining generics issues
- [ ] merge v.c and v_win.c
- [ ] more advanced errors, not just `error('message')`
- [ ] match expr compiled as C if-else instead of ternary operator
