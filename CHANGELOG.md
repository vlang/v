# V 0.1.12
- V can finally compile itself on Windows. (https://github.com/vlang/v#mingw-w64)
- `os` module now uses optionals in all functions that return `File`. Lots of  bugs with optionals fixed.
- `println` was optimized. It no longer results in allocations. Now it also works correctly with all integer types.
- Lots of `vfmt` fixes, it will be enabled tomorrow. 
- New `strings` module.
- Lots of other fixes and improvements, thanks to all the contributors. 

# V 0.1.11

- Cross compilation for Windows!
- Lots of Windows fixes
- socket.v
- maps fixed


# 0.1.9 - 0.1.10
- Windows support via MinGW-w64. Pre-built Windows binary.
- File structure has been simplified: all vlib modules were moved to the vlib/ directory,
  makefile was moved to the root.
- One single archive with pre-built binaries for all operating systems. 
- `mut var := val` was fixed (previously `mut var = val` was allowed as well).

# 0.1.8
- Single file programs without `fn main` now work as expected.
- REPL has been fixed: it now supports imports, consts, function definitions, etc.

# 0.1.7

- All C code in the compiler and vlib has been replaced with V.
- `#` syntax for embedding C code has been removed.
- Exported functions now need to be marked with `pub`, all public vlib functions have been updated.
- CI has been set up (Travis + Azure). On every commit and PR it is made sure that V
  can compile itself, all tests pass, and all examples compile.
- More tests have been uploaded.
- Cleaner bytes to string conversion: `tos2(bytes)` => `string(bytes)`.
- The home page has 3 more examples next to 'hello world' that show the features of the language.
- Lots of bugs and issues fixed.
