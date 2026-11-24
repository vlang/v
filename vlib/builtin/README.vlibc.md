# Experimental vlibc runtime

Use `./v -freestanding -d vlibc program.v` to enable the experimental Unix runtime.
Its `vlibc_*` helpers cover files, descriptors, processes, sockets, threads, and synchronization.
The low-level descriptor arrays and wait-status pointers use 32-bit kernel integers (`i32`).
`vlibc_waitpid(pid)` blocks until that child exits and returns its process ID and an error.
Use `vlibc_sys_wait4` on Linux, BSD, and macOS to obtain the raw POSIX child exit status.
File reads retain partial bytes together with a read error; interrupted reads and writes retry.
File helpers report close errors after otherwise successful I/O.
The common `bf_read_ready(fd, buffer, count)` helper reads without waiting and restores the
descriptor's original status flags before returning. Its caller must exclusively control the
descriptor and its shared open-file description during the call. Results use the `bf_*`
convention: a byte count and normalized errno, with errno 11 for unavailable input and zero
bytes with no error for EOF. A failure to restore the original flags is reported as an error.
`bf_openat` accepts the Linux `O_CLOEXEC` flag and translates it to the target's atomic open flag.
`bf_tai_now()` returns kernel TAI seconds, nanoseconds, and normalized errno. It returns ENOSYS
on targets without that clock; it does not silently substitute UTC or assume a leap-second offset.
`vlibc_htons`, `vlibc_htonl`, and `vlibc_parse_ipv4` produce network-byte-order values.
`vlibc_parse_ipv4` requires valid dotted-quad input with four decimal octets in 0..255;
it does not validate malformed addresses.

This interface is experimental. Linux supports amd64 and arm64 using raw syscalls.
Linux thread startup uses a separate argument block on each mapped stack; join waits until the
kernel clears that thread's TID before releasing its stack.
Linux freestanding `os.is_main_thread()` uses kernel thread IDs without requiring pthreads.
Its startup uses the raw working-directory syscall and omits the hosted process capture mutex.
GCC and Clang links discard unused hosted helpers from Linux freestanding executables.
Portable C snapshots omit startup calls from the inactive vlibc runtime.
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

## Bare heap allocation

The bare libc adapters share one dlmalloc heap. Initialization and heap mutations
(`malloc`, `realloc`, aligned allocation and `free`) are serialized with an allocation-free
atomic lock, so independently created threads can allocate and release one another's blocks.
The memory-mapping callbacks do not reenter that heap. Like ordinary libc allocation, these
operations are not async-signal-safe.

Zeroed allocation rejects a multiplication that cannot be represented instead of allocating
a wrapped byte count. Aligned requests reuse dlmalloc's aligned allocation support; the
returned base pointer works with the same heap's free and realloc operations. Resizing
preserves the initialized prefix but does not promise to preserve extra alignment.

## Without `-d vlibc`

Plain `-freestanding` Linux builds (amd64 and arm64) get their runtime from
`builtin/linux_bare` on the C backend. Non-C backends (`-b amd64`, `-b arm64`, the SSA
pipeline) skip that directory, because the driver adds it only for `backend == 'c'`;
there, `builtin/freestanding_d_freestanding.c.v` supplies the same surface under
`$if freestanding && native && linux`: the `bare_*` print/input/panic/backtrace hooks,
the `malloc/free/realloc/calloc/memcpy/memmove/memset/strlen/memcmp/getchar/qsort`
exports, and the process `exit` route, all on raw syscalls with mmap-per-block memory.
Its allocator is intentionally simpler than linux_bare's dlmalloc arrangement;
`exit` goes through `exit_group`, matching `linux_bare` behavior.

Clang/GCC freestanding executables linked with `-nostdlib` also include the compiler's arithmetic
runtime archive when the toolchain exposes one through its standard query. V uses the active target
and final compile/link flags when locating it. Drivers that do not expose an archive keep their
normal link policy; supply the target runtime in the link flags when that policy requires it.
This supplies operations such as 128-bit division while retaining the freestanding startup and
allocation paths.

Inline assembly on the native lanes: the amd64 backend (`-b amd64`, also spelled `-b x64`)
lowers the structured template shapes the runtime uses (`syscall`, `mov` between
registers and named operands, `mulq`/`divq` and the add/sub/xor/or/and family, with
one or two outputs), so `-b amd64 -freestanding` programs run their syscalls. The
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

## V threads in freestanding C programs

On Linux amd64/arm64, ordinary `spawn`, `thread.wait()`, arrays of threads and detached
sync workers use the same raw-clone and ELF TLS runtime in plain `-freestanding` builds.
No `-d vlibc` flag or pthread compatibility layer is required. Thread argument/result
containers and control blocks use the ordinary heap, so a parent's scoped arena is not inherited.
Borrowed payloads keep their original lifetime: keep their parent arena alive until the worker
has finished using them.
A joinable thread retains its stack and TLS until the kernel confirms exit. Detaching
transfers exclusive ownership to the worker, which blocks catchable signals, clears its
kernel TID address and unmaps its own TLS and stack before a register-only exit syscall.

Thread handles retain their existing single-owner contract: do not join or detach the same
handle concurrently or reuse a handle after it has been consumed. macOS uses libSystem's
pthread runtime. A Linux shared-library host also supplies pthreads and initializes TLS;
its ordinary V threads use that host runtime. The low-level raw `vlibc_thread_spawn` API
requires standalone TLS initialization and reports `EINVAL` inside a library host, rather
than constructing a partial host-libc thread control block.

## Thread-local storage in freestanding C programs

Linux amd64 and arm64 C builds initialize their ELF `PT_TLS` image before entering `main`.
Startup reads the kernel auxiliary vector and the mapped ELF headers to find the TLS template
and its load bias. It copies initialized bytes, leaves the `.tbss` tail zero-filled, preserves
the segment's alignment and first-byte offset, and installs the architecture's thread pointer.
Each bare clone receives a fresh image through `CLONE_SETTLS`. Its mapping remains alive until
join observes the kernel's exit notification, then is unmapped together with its stack.

macOS uses the native loader and libSystem TLS. Compiler-generated arena, errno, panic,
recursive-printing and closure-capture state uses real C TLS on these supported targets;
threads created by a shared-library host receive their storage from that host's loader.
The standalone Linux TLS bootstrap is not run when a host initializes a shared library.

Plain `-freestanding` selects a native TLS-capable platform C compiler. Explicit TinyCC is
rejected with a diagnostic; its hosted pthread-key fallback does not supply a no-libc runtime.
Other bare targets retain their existing TLS limitations. Vinix's kernel runtime owns its own
per-thread/per-CPU state and does not opt into this user-space ELF TLS setup.

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

The Linux freestanding `write` export matches the POSIX system-header prototype, including
the C descriptor width and read-only buffer. External C callers receive a byte count or `-1`
with native `errno` set on failure; successful writes preserve `errno`. Native C errno has
its own thread-local cell, independent of the normalized `bf_*` error state. V code uses
`sys_write` or `bf_write` to receive the count and error separately.

Freestanding macOS retains libSystem's `write`, C memory/string functions, and native
thread-local `errno`. Internal V memory helpers and the runtime's allocation functions remain
available; `sys_write` and `bf_write` still use raw syscalls.

Linux C-backend freestanding builds also provide POSIX `read`, `clock_gettime`,
`nanosleep`, and `isatty` for V modules and external C callers. These exports use
the system headers' native types and the runtime's existing kernel syscall layer.
Successful calls preserve native `errno`; failures set it to the kernel error.
Interrupted reads and sleeps report `EINTR` directly, allowing the caller to choose
whether to retry. An interrupted `nanosleep` reports the remaining duration when
the caller supplies a non-null remainder pointer.

The Linux C freestanding runtime provides a bounded, allocation-free `memchr` with the
standard C ABI. Aligned heap copies use the same bare allocator and arena hooks as ordinary
allocations; their pointers can be released through V's normal `free` route. Allocation failure
reporting uses raw descriptor output and terminates without allocating a panic message.

Linux C freestanding executables run ELF preinitializers and initializers after native TLS is
ready and before entering V. The original argc, argv and environment are passed to those callbacks,
including constructors supplied by the selected C toolchain.
Finalizers run in reverse order after the program returns normally; an explicit raw process exit
skips them. Shared libraries use the host loader's callback lifecycle.

The Linux `bf_futex_wait` and `bf_futex_wake` helpers provide process-private waits
on aligned, live `u32` words. Wait timeouts are relative nanoseconds (negative means
indefinite). Calls report normalized errors without modifying native or BF errno;
a wait may wake spuriously or return `EINTR`, `EAGAIN`, or `ETIMEDOUT`. The caller
must use atomic accesses and check its protected state again. Wake returns a count and an error,
and accepts a positive limit through `2147483647`. The experimental raw `vlibc_sys_futex`
surface uses the same kernel entry point.

On Linux amd64 and arm64, returning from the freestanding program or calling `exit` terminates
all its threads. A panic in a worker is also a process failure. Internal thread completion uses
the separate thread-only exit syscall so other workers and the main program keep running.

Native C helper objects receive the same freestanding compilation policy as generated C:
no hosted-C assumptions or stack-protector runtime, and individually discardable code/data
sections on Linux. They also receive the runtime's freestanding and shared/object startup
capability defines. These flags participate in the existing native-object cache manifest,
so a hosted helper object cannot be reused as a freestanding helper.

Linux C freestanding also supplies the C11 `aligned_alloc` entry point. The alignment must
be a nonzero power of two and the size a multiple of it; invalid requests return null with
`EINVAL`, and failed allocations set `ENOMEM`. Successful calls preserve native errno.
The result belongs to the ordinary bare heap and accepts `free` and `realloc`.
