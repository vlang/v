// race_options.c configures the ThreadSanitizer runtime of programs built with `v -race`.
//
// TSan reads its options from the TSAN_OPTIONS environment variable, on top of the
// defaults that the program returns from `__tsan_default_options`. Go's race detector is
// configured with GORACE in the same syntax; V uses VRACE for that, for example
// `VRACE="halt_on_error=1 log_path=/tmp/race"`. TSAN_OPTIONS still overrides both.

// This file is compiled on its own, not as part of the generated C, so it has to request
// the declarations that a strict ISO C mode (`v -c99`) hides on glibc: syscall(), AT_FDCWD.
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#if defined(__has_feature)
#if __has_feature(thread_sanitizer)
#define V_RACE_TSAN 1
#endif
#endif
#if defined(__SANITIZE_THREAD__)
#define V_RACE_TSAN 1
#endif

#ifdef V_RACE_TSAN

// `abort_on_error=0` makes a program that reported races exit with `exitcode` (66, like
// Go) instead of aborting, which is the default on macOS.
// `report_thread_leaks=0`: like Go, report data races, not threads that are never joined;
// starting a thread with `spawn` and not waiting for it is common in V programs.
// `second_deadlock_stack=1` shows where both mutexes of a lock-order inversion were taken.
// `ignore_interceptors_accesses=0`: on macOS, TSan ignores the memory accesses of its libc
// interceptors by default, and with them those of `memcpy`/`memset`, which the C compiler
// uses for struct, array and fixed array copies. Races on such copies would go unreported.
#define V_RACE_DEFAULT_OPTIONS \
	"abort_on_error=0 exitcode=66 report_thread_leaks=0 second_deadlock_stack=1 " \
	"ignore_interceptors_accesses=0"

// TSan calls `__tsan_default_options` while it initializes itself: before it can track
// instrumented code, and on Linux before its interceptors of libc functions like strlen,
// memcpy or open are ready. So this code is not instrumented, and it calls no libc
// function. GCC's attribute removes all TSan instrumentation of a function; clang needs
// `disable_sanitizer_instrumentation` (clang 14+) to also drop the function entry/exit
// hooks that `no_sanitize("thread")` keeps for functions that make calls.
#if defined(__clang__)
#if defined(__has_attribute)
#if __has_attribute(disable_sanitizer_instrumentation)
#define V_RACE_NO_INSTRUMENTATION __attribute__((disable_sanitizer_instrumentation))
#endif
#endif
#else
#define V_RACE_NO_INSTRUMENTATION __attribute__((no_sanitize_thread))
#endif

// Nothing in the program calls `__tsan_default_options`, only the TSan runtime does, so
// it has to survive link time optimization (`-prod`) and stay visible to the runtime.
#define V_RACE_HOOK __attribute__((used, visibility("default")))

#ifndef V_RACE_NO_INSTRUMENTATION
// Older clang: return the constant defaults, without any call or memory access that
// would be instrumented. VRACE is ignored, TSAN_OPTIONS still works.
V_RACE_HOOK const char *__tsan_default_options(void) {
	return V_RACE_DEFAULT_OPTIONS;
}
#else

#if defined(__linux__)
#include <fcntl.h>
#include <sys/syscall.h>
#include <unistd.h>
#endif

#if defined(__APPLE__)
// A shared library cannot link `environ` on macOS.
#include <crt_externs.h>
#define V_RACE_ENVIRON (*_NSGetEnviron())
#else
extern char **environ;
#define V_RACE_ENVIRON environ
#endif

static char v_race_options[4096];

// The loops below use volatile accesses, so that an optimizing compiler cannot turn
// them back into calls of memcpy or strlen.
typedef const volatile char *v_race_str;

// v_race_env_value returns the value of `name` if `entry` is a `name=value` entry.
V_RACE_NO_INSTRUMENTATION static const char *v_race_env_value(v_race_str entry, const char *name) {
	while (*name != 0) {
		if (*entry != *name) {
			return 0;
		}
		entry++;
		name++;
	}
	return *entry == '=' ? (const char *)entry + 1 : 0;
}

#if defined(__linux__) && defined(SYS_read)
static char v_race_environ[32768];

// v_race_proc_environ looks up `name` in /proc/self/environ. With clang, the TSan runtime
// initializes from `.preinit_array`, before the C library has set up `environ`; TSan
// itself reads /proc/self/environ then too. Raw system calls avoid its interceptors.
V_RACE_NO_INSTRUMENTATION static const char *v_race_proc_environ(const char *name) {
#if defined(SYS_openat)
	long fd = syscall(SYS_openat, AT_FDCWD, "/proc/self/environ", O_RDONLY);
#else
	long fd = syscall(SYS_open, "/proc/self/environ", O_RDONLY);
#endif
	if (fd < 0) {
		return 0;
	}
	long len = 0;
	while (len < (long)sizeof(v_race_environ) - 1) {
		long n = syscall(SYS_read, fd, v_race_environ + len, sizeof(v_race_environ) - 1 - len);
		if (n <= 0) {
			break;
		}
		len += n;
	}
	syscall(SYS_close, fd);
	v_race_environ[len] = 0;
	for (long i = 0; i < len;) {
		const char *value = v_race_env_value(v_race_environ + i, name);
		if (value != 0) {
			return value;
		}
		v_race_str entry = v_race_environ;
		while (i < len && entry[i] != 0) {
			i++;
		}
		i++;
	}
	return 0;
}
#endif

V_RACE_NO_INSTRUMENTATION static const char *v_race_getenv(const char *name) {
	char **env = V_RACE_ENVIRON;
	if (env != 0) {
		for (char **entry = env; *entry != 0; entry++) {
			const char *value = v_race_env_value(*entry, name);
			if (value != 0) {
				return value;
			}
		}
		return 0;
	}
#if defined(__linux__) && defined(SYS_read)
	return v_race_proc_environ(name);
#else
	return 0;
#endif
}

V_RACE_HOOK V_RACE_NO_INSTRUMENTATION const char *__tsan_default_options(void) {
	const char *user_options = v_race_getenv("VRACE");
	if (user_options == 0 || *user_options == 0) {
		return V_RACE_DEFAULT_OPTIONS;
	}
	// TSan applies the options from left to right, so the ones from VRACE win.
	volatile char *options = v_race_options;
	v_race_str defaults = V_RACE_DEFAULT_OPTIONS;
	v_race_str user = user_options;
	unsigned long len = 0;
	while (*defaults != 0) {
		options[len++] = *defaults++;
	}
	options[len++] = ' ';
	while (*user != 0 && len < sizeof(v_race_options) - 1) {
		options[len++] = *user++;
	}
	options[len] = 0;
	return v_race_options;
}

#endif // V_RACE_NO_INSTRUMENTATION
#endif // V_RACE_TSAN
