# Experimental vlibc runtime

Use `./v -freestanding -d vlibc program.v` to enable the experimental Unix runtime.
Its `vlibc_*` helpers cover files, descriptors, processes, sockets, threads, and synchronization.
The low-level descriptor arrays and wait-status pointers use 32-bit kernel integers (`i32`).
`vlibc_waitpid(pid)` blocks until that child exits and returns its process ID and an error.
Use `vlibc_sys_wait4` on Linux, BSD, and macOS to obtain the raw POSIX child exit status.
File reads retain partial bytes together with a read error; interrupted reads and writes retry.
File helpers report close errors after otherwise successful I/O.
`vlibc_htons`, `vlibc_htonl`, and `vlibc_parse_ipv4` produce network-byte-order values.
`vlibc_parse_ipv4` requires valid dotted-quad input with four decimal octets in 0..255;
it does not validate malformed addresses.

This interface is experimental. Linux supports amd64 and arm64 using raw syscalls.
Linux thread startup uses a separate argument block on each mapped stack; join waits until the
kernel clears that thread's TID before releasing its stack.
macOS uses libSystem for process startup and pthreads for threads.
FreeBSD and DragonFly join through kernel-published exit state. OpenBSD uses `__tfork` and
`__threxit`; current OpenBSD syscall-pinning policies may restrict raw syscall executables.
NetBSD uses shared-memory clone processes and reaps each clone before releasing its stack.
Solaris selects its own amd64 syscall runtime; its thread creation helper returns `enosys`.

Run `./v vlib/v/tests/vlibc_linux_test.v` on Linux or
`./v vlib/v/tests/vlibc_bsd_test.v` on a matching BSD/macOS host for the runtime regressions.
Cross-compilation can check generated assembly but does not validate the target kernel ABI.
