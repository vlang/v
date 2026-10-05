// segfault_handler_nix.h installs V's SIGSEGV/SIGBUS handler for hosted Unix programs.
//
// A stack overflow leaves no room on the faulting thread's own stack, so the handler
// runs on an alternate signal stack (`sigaltstack`). A fault address inside or just
// below the stack of the faulting thread is reported as a stack overflow. Any other
// fault goes to the handler that was installed before (if any), or for SIGSEGV to V's
// `v_segmentation_fault_handler`. The stack overflow path is
// async-signal-safe: it only uses write(2), sigaltstack(2), backtrace_symbols_fd and
// _exit.
//
// Every alternate signal stack that V registers starts with 4 words: a magic value,
// the lowest and the highest address of the thread stack it belongs to, and the size of
// the alternate stack mapping. The spawned-thread runtime that the C backend emits
// (`__v_thread_signal_stack_enter`) writes the same layout; keep both in sync.
#if !defined(_WIN32) && !defined(V_SEGFAULT_HANDLER_NIX_H)
#define V_SEGFAULT_HANDLER_NIX_H

#include <errno.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/resource.h>

#if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__) || defined(__SANITIZE_HWADDRESS__)
#define V_SEGFAULT_HANDLER_SANITIZED 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer) || __has_feature(thread_sanitizer) || __has_feature(memory_sanitizer) || __has_feature(hwaddress_sanitizer)
#define V_SEGFAULT_HANDLER_SANITIZED 1
#endif
#endif

#if defined(SA_ONSTACK) && defined(SA_SIGINFO) && defined(SS_DISABLE) && (defined(MAP_ANONYMOUS) || defined(MAP_ANON)) \
	&& !defined(V_SEGFAULT_HANDLER_SANITIZED) && !defined(__wasm__) && !defined(__EMSCRIPTEN__) && !defined(__ANDROID__)
#define V_SEGFAULT_HANDLER_ACTIVE 1
#endif

#if defined(V_SEGFAULT_HANDLER_ACTIVE)

#if defined(__APPLE__) || defined(__linux__)
#include <sys/ucontext.h>
#endif
#if defined(__APPLE__) || defined(__GLIBC__)
#include <execinfo.h>
#define V_SEGFAULT_HANDLER_SYMBOLS 1
#endif

#define V_SIGNAL_STACK_MAGIC ((uintptr_t)0x56534f56u)
// Faults up to this far below the lowest stack address still count as a stack
// overflow: a function with a big frame can skip past the guard page.
#define V_STACK_OVERFLOW_SLACK ((uintptr_t)256 * 1024)
#define V_STACK_OVERFLOW_MAX_LINES 48

typedef void (*v_segfault_fallback_fn)(int);
static v_segfault_fallback_fn v_segfault_fallback = 0;
// The actions that were installed before V's handler, for SIGSEGV and SIGBUS.
static struct sigaction v_segfault_previous[2];
// Saved actions stay immutable. Atomically consume one-shot handlers across threads.
static int v_segfault_previous_consumed[2];

static int v_segfault_consume_previous(int index) {
#if defined(__TINYC__)
	extern unsigned int __atomic_exchange_4(unsigned int*, unsigned int, int);
	return __atomic_exchange_4((unsigned int*)&v_segfault_previous_consumed[index], 1, 5) == 0;
#else
	return __atomic_exchange_n(&v_segfault_previous_consumed[index], 1, 5) == 0;
#endif
}

static void v_segfault_write(const char* s, size_t len) {
	while (len > 0) {
		ssize_t written = write(2, s, len);
		if (written <= 0) {
			if (written < 0 && errno == EINTR) {
				continue;
			}
			return;
		}
		s += written;
		len -= (size_t)written;
	}
}

static void v_segfault_write_str(const char* s) {
	v_segfault_write(s, strlen(s));
}

static void v_segfault_write_uint(uintptr_t value, unsigned base) {
	char buf[32];
	size_t i = sizeof(buf);
	do {
		unsigned digit = (unsigned)(value % base);
		buf[--i] = (char)(digit < 10 ? '0' + digit : 'a' + digit - 10);
		value /= base;
	} while (value != 0 && i > 0);
	v_segfault_write(buf + i, sizeof(buf) - i);
}

// v_signal_stack_bounds reads the stack bounds that V stored at the base of the
// alternate signal stack, that the handler currently runs on.
static int v_signal_stack_bounds(uintptr_t* lo, uintptr_t* hi) {
	stack_t ss;
	memset(&ss, 0, sizeof(ss));
	if (sigaltstack(NULL, &ss) != 0 || !(ss.ss_flags & SS_ONSTACK) || ss.ss_sp == NULL
		|| ss.ss_size < 4 * sizeof(uintptr_t)) {
		return 0;
	}
	uintptr_t* header = (uintptr_t*)ss.ss_sp;
	if (header[0] != V_SIGNAL_STACK_MAGIC || header[2] == 0) {
		return 0;
	}
	*lo = header[1];
	*hi = header[2];
	return 1;
}

static uintptr_t v_segfault_code_address(uintptr_t addr) {
#if defined(__aarch64__) || defined(__arm64__)
	// Drop pointer authentication bits from saved return addresses.
	addr &= (uintptr_t)0x0000ffffffffffffULL;
#endif
	return addr;
}

static int v_segfault_context_registers(void* context, uintptr_t* pc, uintptr_t* fp) {
	if (context == NULL) {
		return 0;
	}
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
	ucontext_t* uc = (ucontext_t*)context;
	*pc = (uintptr_t)uc->uc_mcontext->__ss.__pc;
	*fp = (uintptr_t)uc->uc_mcontext->__ss.__fp;
	return 1;
#elif defined(__APPLE__) && defined(__x86_64__)
	ucontext_t* uc = (ucontext_t*)context;
	*pc = (uintptr_t)uc->uc_mcontext->__ss.__rip;
	*fp = (uintptr_t)uc->uc_mcontext->__ss.__rbp;
	return 1;
#elif defined(__linux__) && defined(__x86_64__)
	ucontext_t* uc = (ucontext_t*)context;
	// REG_RIP and REG_RBP, without depending on _GNU_SOURCE:
	*pc = (uintptr_t)uc->uc_mcontext.gregs[16];
	*fp = (uintptr_t)uc->uc_mcontext.gregs[10];
	return 1;
#elif defined(__linux__) && defined(__aarch64__)
	// uc_mcontext is 16-byte aligned. TCC ignores that alignment attribute, which
	// would shift every register by 8 bytes, so align its offset explicitly.
	size_t offset = (offsetof(ucontext_t, uc_mcontext) + 15) & ~(size_t)15;
	mcontext_t* mc = (mcontext_t*)((char*)context + offset);
	*pc = (uintptr_t)mc->pc;
	*fp = (uintptr_t)mc->regs[29];
	return 1;
#else
	(void)pc;
	(void)fp;
	return 0;
#endif
}

static void v_segfault_flush_frames(void** frames, int* len) {
	if (*len == 0) {
		return;
	}
#if defined(V_SEGFAULT_HANDLER_SYMBOLS)
	backtrace_symbols_fd(frames, *len, 2);
#else
	for (int i = 0; i < *len; i++) {
		v_segfault_write_str("0x");
		v_segfault_write_uint((uintptr_t)frames[i], 16);
		v_segfault_write_str("\n");
	}
#endif
	*len = 0;
}

// v_stack_overflow_backtrace walks the frame pointer chain of the overflowed stack,
// starting from the interrupted context. It only reads frame records inside the
// thread's stack bounds, and folds the long runs of identical frames, that a
// runaway recursion leaves, into a single line.
static void v_stack_overflow_backtrace(void* context, uintptr_t lo, uintptr_t hi) {
	uintptr_t pc = 0;
	uintptr_t fp = 0;
	if (!v_segfault_context_registers(context, &pc, &fp)) {
		return;
	}
	// The recorded bounds can be estimates, see v_install_segfault_handler and
	// `__v_thread_signal_stack_enter`, so accept frames in the whole overflow window.
	uintptr_t lowest = lo > V_STACK_OVERFLOW_SLACK ? lo - V_STACK_OVERFLOW_SLACK : 0;
	void* frames[V_STACK_OVERFLOW_MAX_LINES];
	int nframes = 0;
	int lines = 0;
	uintptr_t current = v_segfault_code_address(pc);
	uintptr_t repeats = 0;
	for (;;) {
		uintptr_t next = 0;
		if (fp >= lowest && fp <= hi - 2 * sizeof(uintptr_t) && fp % sizeof(uintptr_t) == 0) {
			uintptr_t* record = (uintptr_t*)fp;
			uintptr_t next_fp = record[0];
			next = v_segfault_code_address(record[1]);
			fp = next_fp > fp ? next_fp : 0;
		}
		if (next != 0 && next == current) {
			repeats++;
			continue;
		}
		if (current != 0) {
			frames[nframes++] = (void*)current;
			lines++;
			if (repeats > 0 || nframes == V_STACK_OVERFLOW_MAX_LINES) {
				v_segfault_flush_frames(frames, &nframes);
			}
			if (repeats > 0) {
				v_segfault_write_str("    ... the frame above repeats ");
				v_segfault_write_uint(repeats, 10);
				v_segfault_write_str(" more times\n");
				lines++;
			}
		}
		if (next == 0) {
			break;
		}
		if (lines >= V_STACK_OVERFLOW_MAX_LINES) {
			v_segfault_flush_frames(frames, &nframes);
			v_segfault_write_str("    ... more frames omitted\n");
			break;
		}
		current = next;
		repeats = 0;
	}
	v_segfault_flush_frames(frames, &nframes);
}

static int v_segfault_is_stack_overflow(siginfo_t* info) {
	uintptr_t lo = 0;
	uintptr_t hi = 0;
	// si_code <= 0 means that the signal was sent by a process, not raised by a fault.
	if (info == NULL || info->si_code <= 0 || !v_signal_stack_bounds(&lo, &hi) || lo == 0) {
		return 0;
	}
	uintptr_t addr = (uintptr_t)info->si_addr;
	uintptr_t lowest = lo > V_STACK_OVERFLOW_SLACK ? lo - V_STACK_OVERFLOW_SLACK : 0;
	return addr >= lowest && addr < hi;
}

static void v_segfault_signal_handler(int sig, siginfo_t* info, void* context) {
	if (v_segfault_is_stack_overflow(info)) {
		v_segfault_write_str("V panic: stack overflow\n");
#if !defined(CUSTOM_DEFINE_no_backtrace)
		uintptr_t lo = 0;
		uintptr_t hi = 0;
		if (v_signal_stack_bounds(&lo, &hi)) {
			v_stack_overflow_backtrace(context, lo, hi);
		}
#endif
		_exit(128 + sig);
	}
	// A handler that was installed earlier (by TCC's `-bt` runtime, or by the GC for
	// its write barrier) keeps handling every other fault.
	int previous_index = sig == SIGBUS ? 1 : 0;
	struct sigaction* previous = &v_segfault_previous[previous_index];
	if (previous->sa_handler != SIG_DFL && previous->sa_handler != SIG_IGN
		&& (!(previous->sa_flags & SA_RESETHAND)
			|| v_segfault_consume_previous(previous_index))) {
		if (previous->sa_flags & SA_SIGINFO) {
			previous->sa_sigaction(sig, info, context);
		} else {
			previous->sa_handler(sig);
		}
		return;
	}
	if (sig == SIGSEGV && v_segfault_fallback != 0) {
#if defined(_VGCBOEHM) || defined(CUSTOM_DEFINE_gcboehm)
		// The fallback allocates, and a collection started from the alternate signal
		// stack would scan the gap between it and the thread's own stack.
		GC_disable();
#endif
		v_segfault_fallback(sig);
	}
	// Restore the default action, so that the process still terminates with `sig`.
	struct sigaction dfl;
	memset(&dfl, 0, sizeof(dfl));
	dfl.sa_handler = SIG_DFL;
	sigemptyset(&dfl.sa_mask);
	sigaction(sig, &dfl, NULL);
	raise(sig);
}

static size_t v_signal_stack_size(void) {
	size_t size = 64 * 1024;
#if defined(_SC_MINSIGSTKSZ)
	long min_size = sysconf(_SC_MINSIGSTKSZ);
	if (min_size > 0 && (size_t)min_size + 32 * 1024 > size) {
		size = (size_t)min_size + 32 * 1024;
	}
#endif
	return size;
}

// v_signal_stack_register gives the calling thread an alternate signal stack, unless
// it already has one.
static void v_signal_stack_register(uintptr_t lo, uintptr_t hi) {
	stack_t old;
	memset(&old, 0, sizeof(old));
	if (sigaltstack(NULL, &old) != 0 || !(old.ss_flags & SS_DISABLE)) {
		return;
	}
	size_t size = v_signal_stack_size();
#if defined(MAP_ANONYMOUS)
	void* base = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#else
	void* base = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#endif
	if (base == MAP_FAILED) {
		return;
	}
	uintptr_t* header = (uintptr_t*)base;
	header[0] = V_SIGNAL_STACK_MAGIC;
	header[1] = lo;
	header[2] = hi;
	header[3] = (uintptr_t)size;
	stack_t ss;
	memset(&ss, 0, sizeof(ss));
	ss.ss_sp = base;
	ss.ss_size = size;
	ss.ss_flags = 0;
	if (sigaltstack(&ss, NULL) != 0) {
		munmap(base, size);
	}
}

// v_segfault_save_previous stores the current action for `sig`. It reports false for
// an ignored signal, that is left alone. sa_handler and sa_sigaction share their
// storage, so SIG_DFL is checked through sa_handler, whatever sa_flags is: macOS keeps
// SA_SIGINFO in sa_flags across exec, while it resets the handler to SIG_DFL.
static int v_segfault_save_previous(int sig, struct sigaction* previous) {
	memset(previous, 0, sizeof(*previous));
	if (sigaction(sig, NULL, previous) != 0 || previous->sa_handler == SIG_IGN) {
		return 0;
	}
#if defined(__APPLE__)
	// Darwin does not return SA_RESETHAND when querying an installed action. Keep
	// existing callbacks under kernel control, so one-shot handlers stay one-shot.
	if (previous->sa_handler != SIG_DFL) {
		return 0;
	}
#endif
	return 1;
}

// v_segfault_install_signal must run after v_segfault_save_previous: V's handler calls
// the previous one directly, so it blocks the signals of the previous action's sa_mask.
// The Boehm GC, for example, blocks its stop-the-world signal in its write fault
// handler, and the heap gets corrupted when a collection interrupts that handler.
static void v_segfault_install_signal(int sig) {
	struct sigaction sa;
	memset(&sa, 0, sizeof(sa));
	sa.sa_sigaction = v_segfault_signal_handler;
	struct sigaction* previous = &v_segfault_previous[sig == SIGBUS ? 1 : 0];
	sa.sa_flags = SA_SIGINFO | SA_ONSTACK | (previous->sa_flags & (SA_NODEFER | SA_RESTART));
	sa.sa_mask = previous->sa_mask;
	sigaction(sig, &sa, NULL);
}

// v_install_segfault_handler is called once, at the start of the main thread, from
// builtin_init. `main_argv` is the argv of C's main, which the startup code
// places above the frames of main on the initial stack.
static void v_install_segfault_handler(void* fallback, void* main_argv) {
	// Sanitizers report stack overflows themselves; they are excluded at compile time.
	int install_segv = v_segfault_save_previous(SIGSEGV, &v_segfault_previous[0]);
	int install_bus = v_segfault_save_previous(SIGBUS, &v_segfault_previous[1]);
	if (!install_segv && !install_bus) {
		return;
	}
	v_segfault_fallback = (v_segfault_fallback_fn)fallback;
	uintptr_t lo = 0;
	uintptr_t hi = 0;
#if defined(__APPLE__)
	pthread_t self = pthread_self();
	hi = (uintptr_t)pthread_get_stackaddr_np(self);
	size_t stack_size = pthread_get_stacksize_np(self);
	lo = hi > stack_size ? hi - stack_size : 0;
	(void)main_argv;
#else
	char marker = 0;
	hi = (uintptr_t)&marker;
	struct rlimit limit;
	memset(&limit, 0, sizeof(limit));
	if (getrlimit(RLIMIT_STACK, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY && (uintptr_t)limit.rlim_cur < hi) {
		uintptr_t argv_addr = (uintptr_t)main_argv;
		if (argv_addr > hi && argv_addr - hi < (uintptr_t)limit.rlim_cur) {
			hi = argv_addr;
		}
		// The main thread stack can grow down to RLIMIT_STACK below its top, that is a
		// bit above `hi`, so this estimate is at most a little too low.
		lo = hi - (uintptr_t)limit.rlim_cur;
	}
#endif
	v_signal_stack_register(lo, hi);
	if (install_segv) {
		v_segfault_install_signal(SIGSEGV);
	}
	if (install_bus) {
		v_segfault_install_signal(SIGBUS);
	}
}

#else

static void v_install_segfault_handler(void* fallback, void* main_argv) {
	(void)fallback;
	(void)main_argv;
}

#endif
#endif
