
# V 0.1.14 
*12 Jul 2019* 
- `gg` module Windows support, V Tetris runs on Windows. 
- `glad` and `cJSON` are now compiled only once, this makes compilation of programs using `gg` and `json
  a bit faster. 
- `v.c` has been cleaned up and minimized (~16k => ~10k lines of code). 
- `type` aliases can now have methods.  
- Const overflow check during compilation (`byte(1000)` will no longer compile) 


# V 0.1.13
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


# V 0.1.12
*4 Jul 2019* 
- V can finally compile itself on Windows. (https://github.com/vlang/v#mingw-w64)
- `os` module now uses optionals in all functions that return `File`. Lots of  bugs with optionals fixed.
- `println` was optimized. It no longer results in allocations. Now it also works correctly with all integer types.
- Lots of `vfmt` fixes, it will be enabled tomorrow. 
- New `strings` module.
- Lots of other fixes and improvements, thanks to all the contributors. 


# V 0.1.11
*1 Jul 2019* 
- Cross compilation for Windows!
- Lots of Windows fixes
- socket.v
- maps fixed


# 0.1.9 - 0.1.10
*29 Jun 2019* 
- Windows support via MinGW-w64. Pre-built Windows binary.
- File structure has been simplified: all vlib modules were moved to the vlib/ directory,
  makefile was moved to the root.
- One single archive with pre-built binaries for all operating systems. 
- `mut var := val` was fixed (previously `mut var = val` was allowed as well).

# 0.1.8
*28 Jun 2019* 
- Single file programs without `fn main` now work as expected.
- REPL has been fixed: it now supports imports, consts, function definitions, etc.

# 0.1.7
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
