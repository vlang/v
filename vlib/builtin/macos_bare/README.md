# macOS kernel services

The V implementation uses raw Mach/BSD inline assembly. SDK-generated MIG request/reply types define
the wire ABI through declarative C interop; V implements the transport and ownership policy.
The runtime links none of the SDK's client routines. Mach RPC transport uses
`mach_msg2` (macOS 13 or newer). Native Mach status values stay distinct from normalized BF errors.
The bridge never reads or changes native/BF errno and needs no pthread registration or Apple TLV.

`bf_mach_task_self` and `bf_mach_thread_self` acquire send-right references. Release each with
`bf_mach_port_deallocate`. `bf_mach_reply_port` creates a receive right; destroy that with
`bf_mach_port_destroy_receive`. Each internal RPC owns and destroys its own reply port, including
failure paths. An interrupted receive retries only the receive, preserving exactly-once requests.

`bf_mach_thread_create_running` copies caller-provided native register state into a kernel thread.
The caller owns its stack and TLS. External termination waits until user execution has stopped;
it leaves the caller's thread-port reference intact. Self-target termination is rejected.

`bf_thread_exit_mapped` uses raw BSD `bsdthread_terminate`, whose kernel handler does not require
`bsdthread_register`. It can unmap the caller's combined stack/TLS mapping before terminating,
signal an optional Mach semaphore after termination, and release one optional thread reference.
All these actions finish in the kernel, so no user code accesses the unmapped stack. The caller
must transfer exactly one owned thread reference when requesting its release. Semaphore creation
returns an owned send reference; after destroying the semaphore, release its dead-name reference.
Use process exit for the initial main thread, whose stack has special kernel handling.

`bf_wait_u32` and `bf_wake_u32` provide wait-equal/wake-one-or-all without claiming Linux futex
woken counts. Timeout values are relative nanoseconds; negative means indefinite, zero requests
immediate expiry. Callers must use atomic predicate accesses and recheck after every return.
XNU's atomic copyin cannot fault in pages, so read the predicate before waiting. Spurious wakeups,
interruptions, and errors remain visible. Raw `bf_ulock_wait`/`bf_ulock_wake` reject `ULF_NO_ERRNO`.

The VM remap helper uses the current task for both maps, mask zero, and shared inheritance.
It supports fixed-overwrite aliasing of signed executable templates without requesting new execute
permission. `bf_page_size` queries the native page size with raw `sysctl`.

The frozen C implementation remains in `testdata/macos_kernel_services` as an independent oracle.
The production runtime compiles no C implementation of these services.

The standalone Intel fixture links with `-nostdlib -static`, has `LC_UNIXTHREAD`, and checks an
empty undefined-symbol list and absence of dynamic-library load commands. It exercises raw Mach
threads, distinct thread IDs, wait/wake/timeouts, shared data and executable remaps, and a worker
that unmaps its own stack and posts completion through the kernel. The ARM64 bridge is compiled
against its native SDK ABI; stock release XNU rejects standalone static ARM64 executables, so that
runtime test requires a separately supported launch environment.

AMD64 C-backend memory exports (`memcpy`, `memmove`, `memset`, `memcmp`, `memchr`, `strlen`,
`bzero`, and `__bzero`) delegate to the V byte operations. This leaf chain does
not allocate or use profiling/tracing, so startup can use it before TLS setup.
The standalone `__error` export returns the native C `int` TLS cell, independent
of normalized BF errors. `v_bare_no_startup` embedding keeps the host native errno
accessor; ARM64 retains libSystem memory and errno services.

## Standalone Intel startup

The C backend's AMD64 `-freestanding` executable path owns `_start`, TLS setup,
initializer calls, `main`, and process exit. Runtime policy lives in
`startup_d_freestanding.c.v`; its assembly is limited to the kernel entry and TLV
register convention. Darwin's SDK declarations supply Mach-O layouts. No C runtime
implementation is linked for startup.

Before calling generated V initialization, startup validates the command/section
geometry, normalizes static TLV descriptors, allocates a per-thread TLS image, and
installs GS. Each TLS storage block contains its data pointer and its own base
pointer, followed by the template copy with its original alignment residue.
Thread creation uses the same storage layout; startup owns the initial thread's
mapping until process termination.

Initializers receive Darwin's five arguments, including mutable CRT references.
Native `main` receives argc, argv, environ, and the apple vector. Normal return and
`exit` run finalizers once in reverse order. `_Exit` and `_exit` terminate
immediately. Early helpers exclude profiling and call tracing because neither
instrumentation nor allocation is safe before TLS is ready.

ARM64 retains the system loader and libSystem. Shared libraries and object files
use their host's startup rather than defining this entry point.

`vlib/v/tests/freestanding_macos_v_startup_test.v` generates the actual helpers,
executes their portable logic with an independent Linux ABI oracle at `-O0` and
`-O3`, and assembles the actual entry templates for Mach-O. Its syscall transport
is a test substitute; this is not evidence of execution on Darwin. Full native
macOS validation is still required for the assembled runtime.

## Threads

Standalone AMD64 V threads use `thread_runtime.c.v` with a combined TLS/guard/stack mapping.
A kernel semaphore gates entry until the creator publishes the control and thread-port ownership.
BSD329 blocks only the caller's signals before creation and until native TLS is active; it also
blocks signals before teardown. Completion and detach transfer one control/result owner through
an atomic state change. BSD361 performs final mapping reclamation and signals a joining waiter;
production needs no polling or reaper. Returned payloads remain on the ordinary heap, so thread
exit does not discard a whole preallocation scope that may contain escaping values.

ARM64, shared libraries, objects, and explicit `v_bare_no_startup` builds keep host-managed threads
and TLS. They neither activate a standalone TLS block nor replace the host's GS base. Raw kernel
services remain explicit APIs in these modes. Native SSA backends are experimental; these SDK
interop declarations target the mature C backend and do not infer native ABI layouts from headers.

The experimental `-d vlibc` thread API follows the same selection. Its callback
context uses the ordinary heap, independently of a caller's current arena, and
successful join releases it after kernel-confirmed exit. A failed create releases
its unpublished context; failed joins retain the caller's handle. Standalone
self-join returns `edeadlk`, and consumed or empty handles return `esrch`.
The legacy `vlibc_mutex_*` and `vlibc_cond_*` compatibility wrappers still require
host pthread services. They are not supported by a standalone AMD64 executable;
use the supported `sync` module for raw-runtime synchronization.
## Exit callbacks

Standalone AMD64 implements the C `atexit` ABI in V, including compiler-generated
cleanup registrations and the public V `at_exit` function. The first 32 registrations
use static storage. Additional registrations use the existing persistent bare heap,
independent of scoped arenas, and return failure if allocation fails.

Callbacks run in reverse registration order outside the registry lock. A callback
can register another callback. Normal exit drains registered callbacks before image
finalizers and drains again afterward for registrations made by those finalizers.
Closing the final empty registry rejects concurrent late registrations rather than
accepting a callback that would never run. `_Exit` and `_exit` skip both callback
and image-finalizer lists. ARM64 and embedding hosts retain their system registration API;
this standalone registry is excluded from those builds.

## Signal handlers

The AMD64 C backend implements the C `signal` function in V with XNU's raw
sigaction syscall. The wrapper preserves the SDK function-pointer signature,
returns the previous handler, and supports the kernel's default and ignored
dispositions. New handlers use `SA_RESTART`. Successful registration preserves
native errno; failures return `SIG_ERR` and set native errno without changing
the separate BF error cell.

The assembly entry calls the ordinary one-argument handler and returns through
XNU's token-validated sigreturn convention. It retains the interrupted context
and validation token across the callback, including nested signal delivery.
Registration and the entry exclude profiling and call tracing. ARM64 keeps its
libSystem signal implementation.

`vlib/v/compiler_tests/macos_v_signal_test.v` checks the generated V policy
through an independent C ABI consumer and a raw-dispatch substitute. Its native
Mac test checks repeated and nested delivery, mask restoration, previous
handlers, inherited ignored dispositions, errno separation, and a blocking
read restarted after a V handler runs on a worker thread. Debug and production
binaries must have no runtime imports.

## POSIX byte I/O and assertion failure

Standalone AMD64 exports `read`, `write`, and `isatty` with Darwin's SDK ABI.
Byte I/O returns the kernel's short count or interruption directly. Failures set
native errno; successful operations preserve it. These wrappers do not retry or
change the independent BF error cell. `isatty` queries descriptor type and falls
back to the SDK terminal-attributes query when that service is unavailable.

`abort` targets the calling kernel thread with SIGABRT, unblocking it even when
the caller inherited a blocked mask. Ignoring the signal or returning from its
handler leads to a default-disposition second delivery. A final instruction trap
preserves abnormal termination if the kernel cannot deliver SIGABRT. Normal exit
callbacks and finalizers do not run. The SDK `__assert_rtn` adapter writes its
diagnostic without allocation, runtime initialization, or native errno access,
then follows this same termination path. It accepts Darwin's null and legacy
minus-one function pointers. These exports exclude profiling and tracing.
ARM64 and embedding hosts keep their system I/O and abnormal-termination ABI.

`vlib/v/compiler_tests/macos_libc_abi_test.v` executes the generated V helpers
against an independent LP64 SDK model at `-O0` and `-O3`, including `-autofree`,
short/interrupted I/O, terminal fallback, protected memory boundaries, and real
Linux signal termination through substituted kernel transport. Its separate
Mac fixture uses SDK-typed calls and a hosted wait-status controller to check
native SIGABRT, worker and simultaneous callers, pre-initialization assertions,
and the absence of libSystem imports. Linux transport tests do not establish
native Darwin execution.
