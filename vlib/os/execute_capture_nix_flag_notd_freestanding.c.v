module os

// The capture helper uses POSIX libc functions (pipe, posix_spawn, fcntl).
// The define suffix excludes freestanding builds; it does not also select Unix
// targets. Qualified flags exclude Windows even when `-os cross` preserves
// compile-time branches for portable C output.
#flag linux @VEXEROOT/vlib/os/execute_capture_nix.o
#flag darwin @VEXEROOT/vlib/os/execute_capture_nix.o
#flag freebsd @VEXEROOT/vlib/os/execute_capture_nix.o
#flag openbsd @VEXEROOT/vlib/os/execute_capture_nix.o
#flag netbsd @VEXEROOT/vlib/os/execute_capture_nix.o
#flag dragonfly @VEXEROOT/vlib/os/execute_capture_nix.o
#flag android @VEXEROOT/vlib/os/execute_capture_nix.o
#flag termux @VEXEROOT/vlib/os/execute_capture_nix.o
#flag ios @VEXEROOT/vlib/os/execute_capture_nix.o
#flag solaris @VEXEROOT/vlib/os/execute_capture_nix.o
#flag qnx @VEXEROOT/vlib/os/execute_capture_nix.o
#flag haiku @VEXEROOT/vlib/os/execute_capture_nix.o
#flag serenity @VEXEROOT/vlib/os/execute_capture_nix.o
#flag vinix @VEXEROOT/vlib/os/execute_capture_nix.o
#flag wasm32_emscripten @VEXEROOT/vlib/os/execute_capture_nix.o
