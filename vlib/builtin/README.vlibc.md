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
Its socket helpers use transports registered with the kernel; legacy netconfig fallback is absent.

Run `./v vlib/v/tests/vlibc_linux_test.v` on Linux or
`./v vlib/v/tests/vlibc_bsd_test.v` on a matching BSD/macOS host for the runtime regressions.
Use `./v vlib/v/tests/vlibc_solaris_test.v` on Solaris for files, descriptors, and sockets.
Cross-compilation can check generated assembly but does not validate the target kernel ABI.

## Without `-d vlibc`

Plain `-freestanding` Linux builds (amd64 and arm64) get their runtime from
`builtin/linux_bare` on the C backend. Non-C backends (`-b x64`, `-b arm64`, the SSA
pipeline) skip that directory, because the driver adds it only for `backend == 'c'`;
there, `builtin/freestanding_d_freestanding.c.v` supplies the same surface under
`$if freestanding && native && linux`: the `bare_*` print/input/panic/backtrace hooks,
the `malloc/free/realloc/calloc/memcpy/memmove/memset/strlen/memcmp/getchar/qsort`
exports, and the process `exit` route, all on raw syscalls with mmap-per-block memory.
Its allocator is intentionally simpler than linux_bare's dlmalloc arrangement;
`exit` goes through `exit_group`, matching `linux_bare` behavior.

Inline assembly on the native lanes: the x64 backend (`-b x64`, also spelled `-b amd64`)
lowers the structured template shapes the runtime uses (`syscall`, `mov` between
registers and named operands, `mulq`/`divq` and the add/sub/xor/or/and family, with
one or two outputs), so `-b x64 -freestanding` programs run their syscalls. The
freestanding runtime also exports its libc-shaped functions (`malloc`, `memcpy`,
`strlen`, ...) under the C names, and the native link resolves calls to them
inside the binary. The arm64 backend (`-b arm64 -arch arm64`) lowers the same
shapes (`svc`, `mov`, the add/sub/adc/sbc, mul/umulh/udiv, and/orr/eor family, with
register, named-operand and immediate operands). Its freestanding objects link and
their syscall trampolines contain real `svc` instructions, but a complete arm64
freestanding executable is not linked yet: the object still references libc symbols
that the process layer reaches (`posix_spawn`, `sigaction`, `stat`, ...) and the
`_wyp` data symbol. Use `-freestanding` with the C backend to run freestanding
programs today.

## `-syscalls`

`-syscalls` builds a libc-linked program whose os module uses the raw-syscall routes.
builtin stays on libc, including its allocator, so the link keeps libc, `-lm -ldl -lpthread`
and the usual startup. The driver sets a `syscalls` define and takes only the `bf_*` layer and
the kernel syscall layer from the bare directory: `bf.v`, `syscalls.v`, `linux_syscalls.v` and
`linux_syscalls.i386.v`. The freestanding `_start`, the libc-named exports (`malloc`,
`memcpy`, `__errno_location`, `floor`, ...) and the freestanding allocator stay out, so libc
keeps its own symbols.

The os module takes the syscall routes under `$if freestanding || syscalls` and its libc-only
paths under `$if !freestanding && !syscalls`. `os.exec` and `File.reopen` stay unsupported in
`-syscalls` builds, as they are in `-freestanding` builds.

`-syscalls` supports the C backend only. Native backends, windows targets, and combining it
with `-freestanding` are rejected with an error. It is verified on Linux with gcc and clang.

OpenBSD is rejected for both `-freestanding` and `-syscalls`. Its kernel only accepts `syscall`
instructions from pinned regions (the dynamic linker's and libc's text), and `msyscall` is
obsolete, so the raw-syscall routes abort with `pinsyscalls ... error 78`. Supporting OpenBSD
means routing its bf layer through libc wrappers instead.
