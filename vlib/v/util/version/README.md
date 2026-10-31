# Compiler version information

`githash(path)` reads the current commit of a Git checkout and returns its first seven
characters. It supports ordinary checkouts and linked worktrees, including detached HEADs.
Branch refs can be stored in loose files or `packed-refs`; loose files take precedence.
Missing refs and malformed packed entries produce an error instead of a hash.
