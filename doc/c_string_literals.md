# Generated C string literals

The C backend emits long string table entries as adjacent C string literals on separate
source lines. Each piece stays below the per-literal and source-line limits used by MSVC.
This includes strings embedded in the compiler itself, such as its C prelude, so they can
be compiled during `makev.bat -msvc` bootstrap stages.

Splitting preserves the string's bytes and byte length, including embedded zero bytes and
escaped characters. It applies to regular, split, and cached string table declarations.
The limits on the total size of concatenated literals still depend on the target C compiler.
