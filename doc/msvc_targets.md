# MSVC targets

MSVC (`-cc msvc`, `-cc cl`, or `-cc cl.exe`) requires a Windows target.
Combining it with `-os linux`, `-os macos`, or another non-Windows target is rejected
before the generated C is compiled, including when requesting a target-specific C file.
Use `-os windows` to generate C for MSVC, or select a compiler for the intended target.
Portable `-os cross` C snapshots retain their existing behavior.
