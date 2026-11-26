# Experimental vlibc runtime

Use `./v -vlibc program.v` to enable the V `vlibc_*` API with the usual V runtime and collector.
`-d vlibc` is equivalent. Add `-bare` or `-syscalls` to use the bare V runtime.
`-no-system-libc` omits automatic CRT and libc linkage. Use `-shim-libc` to supply the
replacement C ABI while retaining the ordinary V runtime and collector.

The Linux C backend selects the V runtime and C process ABI independently.

| Flags | V runtime | Public `vlibc_*` API | C process ABI |
| --- | --- | --- | --- |
| None | Regular | No | System libc |
| `-vlibc` or `-d vlibc` | Regular | Yes | System libc |
| `-syscalls` or `-bare` | Bare | No | System libc |
| `-syscalls -vlibc` | Bare | Yes | System libc |
| `-no-system-libc` | Regular | No | Caller supplies startup and the C ABI |
| `-vlibc -no-system-libc` or `-shim-libc` | Regular | Yes | vlibc |
| `-freestanding` or `-syscalls -no-system-libc` | Bare | No | Bare builtins |
| `-bare -shim-libc` | Bare | Yes | Bare builtins |

On Linux these flags allow shared C libraries. A library may bring its own dependency on libc.
With `-vlibc` alone, system libc provides functions such as `malloc`, `write` and `pthread_create`.
Use `-ldflags -static` when a fully static executable is required.

The `vlibc_*` helpers cover files, descriptors, processes, sockets, threads, and synchronization.
The low-level descriptor arrays and wait-status pointers use 32-bit kernel integers (`i32`).
`vlibc_waitpid(pid)` blocks until that child exits and returns its process ID and an error.
Use `vlibc_sys_wait4` on Linux, BSD, and macOS to obtain the raw POSIX child exit status.
File reads retain partial bytes together with a read error; interrupted reads and writes retry.
File helpers report close errors after otherwise successful I/O.
For a nonblocking descriptor, `os.fd_read` returns an empty string and -1 when a read would
block. Read `os.last_error().code()` immediately to distinguish EAGAIN from other errors.
An empty string with count zero indicates EOF on a valid descriptor with a positive read size.
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
Linux standalone thread startup uses a separate argument block on each mapped stack; join waits
until the kernel clears that thread's TID before releasing its stack. Libc-enabled `-vlibc` threads
use native pthreads with independent TLS and release their resources through `pthread_join`.
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

Linux freestanding C libraries and `no_main` exports inherit their host's environment even
when initialized without process arguments. V environment changes still use the private
`os.setenv`, `os.unsetenv`, and `os.clearenv` store.
Standalone Darwin/amd64 `os.signal_ignore` uses raw kernel signal masking for worker threads;
the main thread retains its signal-handler behavior.

## Calendar and elapsed clocks

Freestanding C builds on Linux amd64/arm64 and macOS amd64 provide these helpers:

- `bf_realtime_now()` returns Unix seconds, nanoseconds, and normalized errno.
- `bf_monotonic_now()` returns awake-time monotonic nanoseconds and normalized errno.
  Its origin is unspecified; suspend time is excluded.
- `bf_sleep_ns(duration)` waits for a nonnegative relative interval, retrying interruptions
  with the remaining duration. Zero succeeds immediately and a negative interval returns EINVAL.

These helpers preserve both the native and BF errno cells. The macOS backend uses raw kernel
clocks and Mach deadlines; its POSIX `clock_gettime`, `gettimeofday`, `time`, and `nanosleep`
exports use SDK ABI declarations and update native errno only on failure. In
`-d v_bare_no_startup` host embedding, the host retains its native errno accessor.
The macOS arm64 runtime retains libSystem and its existing clock implementation.

## Bare heap allocation

Bare builds default to manual memory management. On native Linux amd64 and arm64 C builds,
`-syscalls -gc boehm` selects Boehm for V allocations, including on spawned V threads.
With `-no-system-libc`, the collector and C dependencies use the replacement C ABI. Standalone
applications share environment changes between the V `os` module and C environment functions,
including when `-syscalls` links the system libc.
In both modes, release C allocations with the allocator that supplied them.

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

Static Linux amd64 and arm64 C builds initialize their ELF `PT_TLS` image before entering `main`.
Startup reads the kernel auxiliary vector and the mapped ELF headers to find the TLS template
and its load bias. It copies initialized bytes, leaves the `.tbss` tail zero-filled, preserves
the segment's alignment and first-byte offset, and installs the architecture's thread pointer.
Each bare clone receives a fresh image through `CLONE_SETTLS`. Its mapping remains alive until
join observes the kernel's exit notification, then is unmapped together with its stack.

Dynamic Linux executables using the replacement C ABI retain GNU ld.so. The selected C
compiler locates its target loader; V does not guess a host path for cross compilation.
The loader owns the initial thread's TLS. V workers initialize all startup modules through
GNU ld.so's `_dl_allocate_tls_init` in V-owned TLS/DTV mappings. Join and detached exit reclaim
those mappings. One loader-owned sizing probe lives for the process lifetime; per-thread
allocations avoid the loader's unreclaimable startup allocator. These are GNU private interfaces
and DTV/TCB ABIs, so support is specific to that loader and requires compatibility testing.
Deferred TLS modules loaded after startup are not supported by this thread runtime.
The preinit callback establishes the environment before shared-library constructors. Main-image
initializers run once, and the loader's process finalizer runs shared-library destructors before
stdio is flushed. Explicit static links retain the independent syscall/TLS bootstrap.

macOS uses the native loader and libSystem TLS. Compiler-generated arena, errno, panic,
recursive-printing and closure-capture state uses real C TLS on these supported targets;
threads created by a shared-library host receive their storage from that host's loader.
The standalone Linux TLS bootstrap is not run when a host initializes a shared library.

Plain `-freestanding` selects a native TLS-capable platform C compiler. Explicit TinyCC is
rejected with a diagnostic; its hosted pthread-key fallback does not supply a no-libc runtime.
Other bare targets retain their existing TLS limitations. Vinix's kernel runtime owns its own
per-thread/per-CPU state and does not opt into this user-space ELF TLS setup.

## `-syscalls`

`-syscalls`, or its alias `-bare`, selects the freestanding V runtime with libc available to C code.
Both `syscalls` and `freestanding` compile-time conditions are true. V printing and OS operations
use the bare kernel routes; `input_character()` and `os.get_raw_line()` read the raw descriptor.
Libc supplies startup, ELF TLS, pthreads, and C stdio. On FreeBSD, the system startup object
provides the executable entry point and the system libc provides `posix_memalign` and `free`.
V and C share the system environment, including changes made after startup.
With the default manual allocator, V and C share an allocation domain. V aligned allocations
accept power-of-two alignments starting at 1 and arbitrary positive sizes. Small alignments use
malloc's guarantee and larger ones use posix_memalign. Both work with ordinary C/V free and realloc.
Freestanding FreeBSD programs can run without closures. Capturing closures require executable
trampoline storage, which the FreeBSD freestanding runtime does not implement.
With explicit Boehm collection, V allocations belong to the collector and C allocations
belong to the selected C runtime.

Explicit `-freestanding` and `-no-system-libc` disable optional libc regardless of flag order.
`-shim-libc` combines `-vlibc` and `-no-system-libc`.
C interoperability stays enabled. C objects, archives, and shared libraries supplied through `#flag`
or `-ldflags` still link. With `-shim-libc`, vlibc supplies their supported C ABI functions.
A library built against system libc can itself request `libc.so.6`; omitting automatic `-lc`
does not remove that dependency. Use libraries built for the replacement runtime when the
whole process must remain libc-free.

V adds `--static` to `#pkgconfig` only for explicitly static links, including private dependencies.
Caller-specified pkg-config modifiers remain intact.
Declared libraries remain in the link, including math, pthread and loader archives;
only the automatic CRT/libc link is suppressed. Archive groups resolve circular static dependencies.

Native dependencies under `~/.vmodules/vlibc/linux-<arch>-shared` are discovered automatically
(`VMODULES` selects a different module root). Explicit static links use the separate
`linux-<arch>-static` installation. A `vlibc-target` marker must match the target.
`VLIBC_PREFIX` selects an explicit installation instead. Prefix headers and library paths, plus
its pkg-config metadata, apply to executables using `-freestanding` or `-no-system-libc`.
Builds using system libc and embedded libraries/objects keep their usual search paths. Prefixes are
not automatically reused for cross targets; an explicit prefix must match that target.
The X11 and vpac installers build each new release in a private store directory and publish it
with one symlink replacement after validation. Previously published directories stay available
to applications using their resolved paths.
The optional builder below can install the X11/font stack once, after which ordinary
`v -shim-libc . -o app` commands need no additional dependency flags.

Failed `-syscalls`, `-bare`, `-vlibc`, `-no-system-libc`, and `-shim-libc` builds report
diagnostics from the compiler that supports those flags.

Shared libraries and objects retain their host's startup and ABI, as described above.
Platform restrictions still apply; macOS arm64 requires libSystem.

The os module takes the syscall routes under `$if freestanding || syscalls` and its libc-only
paths under `$if !freestanding && !syscalls`. On Linux amd64/arm64 and macOS amd64 C targets,
`os.exec` captures merged stdout/stderr using the syscall process runtime and inherits stdin.
Arguments and `argv[0]` are passed literally; a missing `PATH` uses `/bin:/usr/bin` for lookup
without changing the child's environment. Lookup continues to the next directory if the kernel
cannot find an earlier candidate's interpreter. `os.execute` retains its `/bin/sh -c` semantics.
Capture drains output before waiting, reports execution failures with status -1, and returns
the signal number for a signaled child, matching hosted capture. `File.reopen` remains unsupported
in these builds.

`-syscalls` supports the C backend only. Native backends and Windows targets are rejected.
It is verified on Linux with GCC and Clang.
Run `./v vlib/v/compiler_tests/runtime_selection_test.v` for the compiler/mode/flag-order
matrix and `./v vlib/v/tests/syscalls_os_test.v` for raw OS I/O and stdin interoperability.

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
The native shims require platform C headers at build time, including in builds without
`-target-libc-headers`. Their headers also supply the generated C's errno declarations.
Successful calls preserve native `errno`; failures set it to the kernel error.
Interrupted reads and sleeps report `EINTR` directly, allowing the caller to choose
whether to retry. An interrupted `nanosleep` reports the remaining duration when
the caller supplies a non-null remainder pointer.

Linux amd64/arm64 C-backend freestanding builds also provide C ABI adapters for
files (`open`, `close`, `pread`, `lseek`, `stat`, `fstat`, `rename`, `remove`),
descriptor flags and locks through `fcntl`, sockets, `sendfile`, eventfd and epoll.
The adapters use native C headers for declarations and raw Linux syscalls at runtime;
they do not link libc. File metadata uses `statx`, requiring Linux 4.11 or newer.
The variadic `syscall` entry forwards the architecture's argument registers and
translates kernel failures into `-1` and native `errno`.

The compatibility resolver reads `/etc/hosts`, `/etc/services`, and `/etc/resolv.conf`.
It resolves IPv4 and IPv6 addresses over UDP, retries truncated replies over TCP, follows
CNAMEs, and applies configured search domains, `ndots`, retry counts, and timeouts.
It validates reply IDs, questions, record bounds, and compressed names. Address lists grow
dynamically rather than silently dropping records at a fixed address limit.
Numeric addresses include IPv6 interface scopes; null hosts select wildcard or loopback addresses.
Supported flags are `AI_PASSIVE`, `AI_CANONNAME`, `AI_NUMERICHOST`, `AI_NUMERICSERV`,
`AI_ADDRCONFIG`, `AI_V4MAPPED`, and `AI_ALL`. Interface discovery for `AI_ADDRCONFIG` uses
Linux netlink. Resolution failures return `EAI_*`; `EAI_SYSTEM` retains the native errno.
This resolver does not load libc NSS plugins or implement IDN conversion.
`getnameinfo` supplies numeric IPv4/IPv6 text, interface scopes, Unix socket names,
reverse hosts-file and PTR lookup, and TCP/UDP service names. Its V reverse resolver validates
compressed names, follows CNAMEs and retries truncated UDP replies over TCP. `NI_NUMERICHOST`,
`NI_NUMERICSERV`, `NI_NAMEREQD`, `NI_NOFQDN` and `NI_DGRAM` are supported; IDN flags report
`EAI_BADFLAGS`. Bounded output reports `EAI_OVERFLOW`, and `EAI_SYSTEM` preserves native errno.

The runtime provides buffered `fopen`/`fdopen`, `fclose`, `fread`/`fwrite`,
`fseek`/`ftell`, line input, stream error/EOF state, and the three standard streams.
`setbuf` and `setvbuf` support full, line, and unbuffered modes. Stream locks are recursive;
`fflush(NULL)` flushes open output streams without holding the stream registry lock while
waiting for a stream lock. Buffered output is flushed after normal return and by `exit`.
Raw kernel exit calls bypass this cleanup.

Native objects compiled against optimized glibc headers can use `getc_unlocked`,
`putc_unlocked`, `feof_unlocked`, and `ferror_unlocked` on these streams. Their inline
accesses share the flags and buffering used by the out-of-line functions. Wide streams keep
the character encoding selected when they first become wide-oriented.

C process adapters supply `setsid`, `dup2`, `execve`, and architecture-specific `vfork`.
The `wait`, `waitpid`, `wait3`, `wait4` and `waitid` family uses Linux wait syscalls, preserving
native status widths, optional resource usage, nonblocking queries and unreaped-child observation.
The `vfork` entry preserves the shared parent stack across child execution; its child must obey
the usual restrictions and call `execve` or `_exit` without returning to the caller.

The musl-derived formatter supplies `printf`, `fprintf`, `snprintf`, `sprintf`,
`asprintf`, and their `v` variants.
It supports integer conversions including binary (`%b` and `%B`), floating-point conversions
including hexadecimal and long double, `%n`, positional arguments through position 9, field widths,
precision, and flags. Floating-point rounding follows the architecture's rounding mode.
Wide-character conversions follow the selected C or C.UTF-8 locale and report `EILSEQ`
for invalid or unrepresentable input. Growing output uses one formatting pass, including `%n`.
Bounded formatting preserves truncation, the would-have-written length, and output guards,
including large precision values. Results exceeding `INT_MAX` fail with `EOVERFLOW`.
The source and accompanying `linux_bare/COPYRIGHT.musl` retain its MIT license notice.

Additional C adapters provide `posix_memalign`, `strcmp`, `strchr`, `strstr`,
`explicit_bzero`, `getpid`, `sched_getaffinity`, `signal`, signal-set and mask operations,
`time`, and `gmtime_r`.
`sysconf` supports page size, clock ticks, online/configured processor counts,
physical/available pages, the descriptor limit, and minimum and recommended signal stack sizes.
Page and clock values come from `/proc/self/auxv`. Processor counts use `/sys/devices/system/cpu`.
Unsupported queries fail with `EINVAL`. C crypto libraries can use normal, recursive,
and error-checking pthread mutexes, including the standard `PTHREAD_MUTEX_INITIALIZER`.
Mutex attributes select private or process-shared futexes; timed locks support realtime
and monotonic deadlines. Robust mutexes and priority inheritance/protection report `ENOTSUP`.
Native C thread creation uses the V-owned backend described below; ordinary V threads use the
same raw-clone runtime. Unsupported GNU-specific mutex initializers are not accepted.

These C adapters are available with plain `-freestanding`; `-d vlibc` additionally
enables the V `vlibc_*` API. They remain separate from libc-linked `-syscalls` builds.
Host-embedded libraries and objects retain their host's stdio, resolver, and pthread services.
The regression matrix and additional sanitizer and cross-target checks are described in
[`testdata/freestanding_compat/README.md`](testdata/freestanding_compat/README.md).

Linux standalone C adapters also supply directory iteration, symlink/canonical path handling,
memory mappings, vector I/O, descriptor readiness, and the 64-bit file aliases used by native
C libraries. The environment is shared between C and V: changes through either API are visible
to the other. V getters and snapshots copy under the environment lock into the caller's allocator;
owned entries use the durable heap. `putenv` borrows its caller's storage according to the C API.
Host-embedded V libraries retain their private V environment store.

The numeric scanner supports integer bases 2 through 36, decimal and hexadecimal floats,
long doubles, scanf widths and assignment suppression, scansets, and `%m` string allocation.
C23 integer entry points additionally accept binary prefixes and binary scanf conversions;
legacy entry points retain their original prefix rules. `qsort`, GNU `qsort_r`, and `bsearch`
use the supplied C callback ABI; sorting requires no temporary heap allocation.
The `abs`, `labs`, `llabs`, `div`, `ldiv` and `lldiv` entry points use V integer operations;
their C adapters preserve the native integer widths and quotient/remainder structure layouts.
`frexpf`, `frexp` and `frexpl` normalize native representations in V, preserving signed zero,
subnormals and extended precision without floating arithmetic. `fegetround` and `fesetround`
read and update the calling thread's rounding mode; amd64 updates both x87 and SSE controls.
Invalid modes leave the controls unchanged.
On Linux arm64, `scalbnl` scales binary128 values using the active rounding mode and reports
overflow, underflow, and inexact results in the floating-point environment.
The native absolute-value and sign-copy families also preserve floating widths and signed zero.
Floating environment capture/restore, exception flags, hold/update, and GNU exception-mask
controls use native V register operations. GNU default-environment sentinels are supported;
amd64 restoration preserves the live x87 register stack. Raising an enabled exception uses
actual floating-point operations, allowing the kernel to deliver `SIGFPE`.

Locale data currently covers C/POSIX and C.UTF-8, with global and per-thread locale selection,
incremental UTF-8 conversion, locale queries, and narrow/wide C-locale date formatting.
Wide formatting preserves Unicode literals, counts destination capacity in wide characters,
and restores the caller's locale after explicit-locale conversions. The locale-specific
float/double/long-double parsers retain the supported locales' common numeric syntax.
GNU locale aliases forward to the same APIs. Other locale names fail explicitly.
GNU locale data used by native C++ facets includes immutable wide day/month names and date
formats, encoded wide punctuation, category names/codesets and raw monetary unavailable-byte
markers. English wide data is shared for the process lifetime, independently of locale handles.
Reentrant and static password/group lookup reads `/etc/passwd` and
`/etc/group` without loading NSS plugins.
The runtime also supplies nonlocal jumps with optional signal-mask restoration, BSD random-state
APIs, and kernel-backed `arc4random` entropy. C condition variables support private/shared futex
waits, signal/broadcast, and realtime or monotonic absolute deadlines.
Native `pthread_create`, join, detach and exit use the same V-owned thread backend as V workers.
Thread attributes support stack size and detach state. Thread-specific keys run destructors for
up to four passes, key generations prevent stale values after reuse, and `pthread_once` resets
an unfinished initializer when its thread exits. Read/write locks support private/shared futexes,
try/timed acquisition, and the GNU writer preference attribute. Arbitrary host pthread object
layouts are not supported.
Native pthreads support cancellation requests, state/type selection and explicit cancellation
tests. Blocking descriptor/socket I/O, poll/select, condition waits, join and shell waits have
cancellation boundaries. Condition cancellation reacquires the mutex before user cleanup;
canceling a joiner leaves the target joinable. Shell-wait cleanup kills/reaps its owned child
and restores the parent's signal state. C cleanup buffers run before thread-specific destructors.
When a native GCC unwinder is loaded, cancellation and `pthread_exit` also unwind C++ objects,
including mixed C/C++ cleanup frames and asynchronous signal interruption. C++ catch-all handlers
must rethrow this unwind. Ordinary C cleanup buffers also work without a shared unwinder.
The runtime reserves Linux signal 32 for cancellation and keeps it unblocked through the public
signal-mask API. V-created workers retain V's result ownership; canceling their native identity
reports `ENOTSUP`. Stateful stdio and resolver adapters temporarily disable cancellation while
they own internal resources, then restore the caller's state.
Thread-name getters/setters support the current thread and other live native threads, using
kernel names with a 15-byte limit. Scheduling policy/priority, affinity and niceness adapters
use the kernel's validation and permission checks. Pthread queries retain their positive errno
return convention, and joinable controls publish the end of their kernel TID before exit.
Fork refreshes the surviving thread's kernel identity. `nice` clamps full-range increments
without signed overflow; affinity getters initialize unused bytes in the caller's mask.
GNU static recursive/error-checking mutex initializers publish their kind atomically before
locking, including when used by native C++ classes.

The thread, stream, logger, calendar, added string and pattern implementations are V sources.
Their C adapters marshal native types, opaque storage, callbacks and variadic argument lists.
Existing compatibility sources still implement other parts of the C API in C.
GNU checked memory/string, stream, descriptor and printf entry points use V bounds checks.
Overflow checks include element-count multiplication; detected violations terminate with
`SIGABRT` before an out-of-bounds destination access. Checked line input still accepts short
lines when the requested limit exceeds the object size. Fortified printf disallows `%n` in
writable formats
when its checking flag is positive. It checks the complete format against Linux mappings,
including formats spanning mappings with different protections, without changing `errno`.
This protected `%n` check needs readable `/proc/self/maps`; inability to establish read-only
storage is rejected. Other format strings do not need this mapping check.
The checked printf entry points use vlibc's own streams, including during ELF constructors.
Cookie streams support read/write/seek/close callbacks through `fopencookie`; `tmpfile` creates
an unlinked temporary file. `sigaction` translates native masks and handlers into the kernel ABI.
Pending-signal queries, queued signals, synchronous/timed waits and temporary-mask suspension
use Linux signal syscalls. `sigwait` returns an error code while preserving native `errno`.
`statvfs`, descriptor duplication, process execution, extended attributes and terminal attribute
wrappers use Linux syscalls. `getifaddrs` reads link/address snapshots through netlink,
including packet addresses, masks and interface statistics; `if_nametoindex` uses kernel ioctls.
`closefrom` prefers `close_range`, with older-kernel fallbacks.
Directory-relative creation, rename, access checks and ownership operations preserve Linux
flags. Access checks use `faccessat2`; older kernels support the zero-flag fallback and reject
unsupported flag semantics explicitly. Positioned writes retain the descriptor offset.
Temporary file/directory templates use kernel entropy, exclusive creation and private modes;
suffix and close-on-exec variants share the V implementation.
System V semaphores, shared memory and message queues use native Linux syscalls, including timed
semaphore operations, process undo and metadata commands. `ftok` derives keys from native file IDs.
`scandir`/`scandir64` own their copied records, grow the output vector and sort in V with bounded
stack space; native selector/comparator callbacks retain their declared ABI. Alphabetic and version
comparators share the C/POSIX collation and version-order implementation. `strcasestr` uses a
case-folded prefix search, with stack storage for short patterns; `strtok` retains the ordinary
shared token state. Effective access checks, process-group/session queries and microsecond sleeps
use the same syscall/error conversion as their existing native counterparts.
`system`, `popen` and `pclose` launch `/bin/sh` in fork children. The parent retains its runtime;
the external shell and commands use their own runtime. System calls coordinate temporary
SIGINT/SIGQUIT dispositions across concurrent callers and restore the caller's signal mask.
Pipe streams own their child and descriptor, close older popen descriptors in new children,
support close-on-exec modes and retry interrupted child waits.

Credential setters operate in single-threaded programs and fork children. They currently return
`ENOTSUP` while other runtime threads are active, rather than updating only one kernel task.
Thread creation and fork coordinate this check; fork protects the heap and resets the child's
thread count. A process-wide credential rendezvous for live threads remains unimplemented.


The C calendar APIs reuse V's calendar and TZif/POSIX timezone parsers. `tzset` and `localtime`
observe changes to `TZ` and `TZDIR`; cached parsed locations own their allocator scope, and
returned timezone abbreviations remain valid after a cache reload. `syslog` supports local
Unix datagram/stream loggers, severity masks, borrowed identifiers, `%m`, `LOG_PID`,
`LOG_PERROR` and console fallback, while preserving the caller's `errno`.

`iconv` supports UTF-8, UTF-16/32 with explicit endian variants and BOM detection, UCS-2/4,
ASCII, ISO-8859 (except the unassigned 12), Windows-1250 through 1258, CP437/850/852/866,
KOI8-R/U and MacRoman. Invalid, incomplete and output-limited conversions report `EILSEQ`,
`EINVAL` and `E2BIG` with the input/output positions retained. Unknown encodings and
transliteration requests fail at `iconv_open`; `//IGNORE` consumes invalid or unrepresentable
characters and still reports their conversion error. Stateful and CJK legacy encodings are
outside this initial encoding set.

The pattern engine supplies `regcomp`, `regexec`, `regfree`, `regerror` and POSIX `fnmatch`.
It handles basic/extended expressions, captures, repetitions, character classes, anchors,
case folding and basic-expression backreferences. Ordinary expressions use a Thompson
instruction stream with capture tags; backreferences retain distinct capture states.
Backreferences can require more memory and time than ordinary expressions.
Filename patterns support pathname/period restrictions, escaping, case folding and leading
path components. GNU extended glob operators are outside this surface.
`glob`/`glob64` walk matching path components with the same engine. They support escaped
literals, period restrictions, directory marking/filtering, home and nested brace expansion,
native alternate-directory callbacks, error callbacks, offset slots and append ownership.
Each brace alternative sorts independently; append calls retain existing order and reserved
slots. `globfree`/`globfree64` release owned paths and vectors. All expansion, walking, sorting
and ownership policy is V; C adapters preserve native record and callback layouts.
The `pread64`, `pwrite64`, `ftruncate64` and `lseek64` adapters retain native 64-bit offsets.
GNU and POSIX `strerror_r` keep their distinct return contracts. The POSIX entry also supplies
`__xpg_strerror_r`, reports truncation/unknown errors and preserves the caller's errno.

Dynamic lookup uses the loader's public ELF link-map metadata. `dlopen` can acquire an
already loaded startup dependency, including its public library-name symlink; `dlsym` searches
its dependency closure. The main handle and `RTLD_DEFAULT` also see startup preloads.
`RTLD_NEXT` starts after the calling object. `dlclose` releases that acquisition while
startup references retain the mapping. Errors are thread-local. `dladdr` and `dl_iterate_phdr`
expose loaded objects and their public program-header metadata. In static executables,
`dl_iterate_phdr` visits the main image so collectors can find its writable roots.
Loading an additional object
after startup and looking up TLS symbols are not implemented; these requests fail rather than
loading a host libc. This limitation does not force static linking.
`dladdr1` additionally returns native symbol or link-map records. `dlinfo` supports the base
namespace, link-map, origin and program-header requests; other requests report a loader error.
Mapped-file paths preserve origins after working-directory changes, and startup-library symlink
aliases can be acquired through absolute, relative or public basename paths.
`_dl_find_object` reports load bounds and GNU EH/SFrame segments through its native public ABI.
The lookup uses no allocation or locks, preserves errno and loader-error state, and also works
for an explicitly static main image. Failed searches leave the output untouched.

This surface supports static and shared C dependencies such as the X11/Xft/fontconfig stack.
Keeping the entire process libc-free requires dependencies built for the replacement runtime,
including any loader plugins. System shared libraries that depend on libc retain that dependency.
The optional pinned dependency builder and validation commands are documented in
[the compatibility fixture guide](testdata/freestanding_compat/README.md).

The Linux C freestanding runtime provides a bounded, allocation-free `memchr` with the
standard C ABI. Aligned heap copies use the same bare allocator and arena hooks as ordinary
allocations; their pointers can be released through V's normal `free` route. Allocation failure
reporting uses raw descriptor output and terminates without allocating a panic message.

Linux C freestanding executables run ELF preinitializers and initializers after native TLS is
ready and before entering V. The original argc, argv and environment are passed to those callbacks,
including constructors supplied by the selected C toolchain.
Registered `atexit` and C destructor callbacks run in reverse registration order, followed by
ELF finalizers, after normal return or `exit`. Explicit raw process exit skips this cleanup.
Shared libraries use the host loader's callback lifecycle.

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
