module os

// The capture helper (execute_capture_nix.o) links libc (pipe, posix_spawn,
// fcntl), so a freestanding build has neither pipe nor spawn and must not link
// it. The `_notd_freestanding` suffix applies the flag for every backend when
// `-freestanding` is not passed; `#flag` directives inside plain `$if` blocks
// are not collected on every backend pipeline, hence this flag file.
#flag @VEXEROOT/vlib/os/execute_capture_nix.o
