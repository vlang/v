module c

// headerless_compiler_runtime_helpers supplies the runtime facades needed by a
// relocatable compiler bootstrap without consulting the consumer's SDK headers.
fn (mut g FlatGen) headerless_compiler_runtime_helpers() {
	g.writeln('#ifndef _WIN32')
	g.emit_c_runtime_declarations(['pipe', 'close', 'fork', 'dup2', 'execlp', 'execvp', '_exit',
		'write'])
	g.writeln('int getrusage(int, struct rusage*);')
	g.writeln('#ifdef __APPLE__')
	g.writeln('int pthread_attr_set_qos_class_np(pthread_attr_t*, unsigned int, int);')
	g.writeln('#define QOS_CLASS_USER_INITIATED 0x19')
	g.writeln('#ifndef F_SETNOSIGPIPE')
	g.writeln('#define F_SETNOSIGPIPE 73')
	g.writeln('#endif')
	g.writeln('#endif')
	g.write(headerless_compiler_pthread_helpers)
	// This V-owned process facade already declares posix_spawn's opaque actions
	// and contains no include directives. Its libc dependencies are declared above.
	g.write($embed_file('@VEXEROOT/vlib/os/execute_capture_nix.h').to_string())
	g.writeln('#endif')
	g.write(headerless_compiler_detached_thread_helpers)
	g.collect_preserved_c_fns(['getrusage', 'pthread_attr_set_qos_class_np', 'v3_pthread_zero',
		'v3_pthread_create', 'v3_pthread_is_current', 'v3_bench_peak_rss_kb',
		'v_sync_thread_create_detached', 'v_sync_thread_create_detached_with_stack',
		'v_os_execute_capture_start', 'v_os_exec_capture_start', 'v_os_exec_capture_input_start',
		'v_os_fd_write_all'])
	g.portable_run_signal_helpers(true)
	g.headerless_compiler_platform_helpers()
}

const headerless_compiler_pthread_helpers = r'
static inline pthread_t v3_pthread_zero(void) {
	return (pthread_t)0;
}

static inline int v3_pthread_create(pthread_t* thread, size_t stack_size,
	void* (*start_routine)(void*), void* arg) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) return rc;
#ifdef __APPLE__
	rc = pthread_attr_set_qos_class_np(&attr, QOS_CLASS_USER_INITIATED, 0);
#endif
	rc = pthread_attr_setstacksize(&attr, stack_size);
	if (rc == 0) rc = pthread_create(thread, &attr, (void*)start_routine, arg);
	if (pthread_attr_destroy(&attr) != 0) abort();
	return rc;
}

static inline int v3_pthread_is_current(pthread_t thread) {
	return pthread_equal((pthread_t)pthread_self(), thread);
}

static inline i64 v3_bench_peak_rss_kb(void) {
	struct rusage usage;
	if (getrusage(RUSAGE_SELF, &usage) != 0) return -1;
#ifdef __APPLE__
	return (i64)usage.ru_maxrss / 1024;
#else
	return (i64)usage.ru_maxrss;
#endif
}
'

const headerless_compiler_detached_thread_helpers = r'
#ifdef _WIN32
#ifndef ERROR_NOT_ENOUGH_MEMORY
#define ERROR_NOT_ENOUGH_MEMORY 8
#endif
typedef struct {
	unsigned int (*start)(void*);
	void* arg;
} v_sync_windows_thread_context;

static DWORD WINAPI v_sync_windows_thread_start(void* raw_context) {
	v_sync_windows_thread_context context = *(v_sync_windows_thread_context*)raw_context;
	free(raw_context);
	return (DWORD)context.start(context.arg);
}

static inline int v_sync_thread_create_detached_with_stack(void* start, void* arg,
	size_t stack_size) {
	v_sync_windows_thread_context* context = malloc(sizeof(v_sync_windows_thread_context));
	if (context == NULL) return ERROR_NOT_ENOUGH_MEMORY;
	context->start = (unsigned int (*)(void*))start;
	context->arg = arg;
	HANDLE handle = CreateThread(NULL, stack_size, v_sync_windows_thread_start, context, 0, NULL);
	if (handle == NULL) {
		DWORD error = GetLastError();
		free(context);
		return (int)error;
	}
	CloseHandle(handle);
	return 0;
}
#else
static inline int v_sync_thread_create_detached_with_stack(void* start, void* arg,
	size_t stack_size) {
	pthread_attr_t attr;
	int rc = pthread_attr_init(&attr);
	if (rc != 0) return rc;
	if (stack_size != 0) rc = pthread_attr_setstacksize(&attr, stack_size);
	if (rc == 0) rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	if (rc == 0) {
		pthread_t thread;
		rc = pthread_create(&thread, &attr, start, arg);
	}
	if (pthread_attr_destroy(&attr) != 0) abort();
	return rc;
}
#endif

static inline int v_sync_thread_create_detached(void* start, void* arg) {
	return v_sync_thread_create_detached_with_stack(start, arg, V_THREAD_STACK_SIZE);
}
'

// headerless_cmdexec_poll_helper replaces only the shipped poll header when the
// standalone ABI owns libc declarations; SDK-backed units keep the native helper.
fn (mut g FlatGen) headerless_cmdexec_poll_helper() {
	if g.target.os == 'windows' || !g.core_header_directives.any(it.directive.module == 'cmdexec') {
		return
	}
	g.write(headerless_cmdexec_poll)
	g.collect_preserved_c_fns(['v_cmdexec_poll_revents'])
}

const headerless_cmdexec_poll = r'
#ifndef _WIN32
struct v_cmdexec_pollfd {
	int fd;
	short events;
	short revents;
};
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__ANDROID__)
typedef unsigned int v_cmdexec_nfds_t;
#else
typedef size_t v_cmdexec_nfds_t;
#endif
#ifdef __APPLE__
int v_cmdexec_poll_sys(struct v_cmdexec_pollfd*, v_cmdexec_nfds_t, int) __asm__("_poll");
#else
int v_cmdexec_poll_sys(struct v_cmdexec_pollfd*, v_cmdexec_nfds_t, int) __asm__("poll");
#endif
static inline short v_cmdexec_poll_revents(int fd, short events) {
	struct v_cmdexec_pollfd descriptor = {fd, events, 0};
	if (v_cmdexec_poll_sys(&descriptor, 1, 0) <= 0) {
		return 0;
	}
	return descriptor.revents;
}
#endif
'

// portable_run_signal_helpers keeps compiler snapshots independent of the native object.
fn (mut g FlatGen) portable_run_signal_helpers(headerless bool) {
	mut source := $embed_file('@VEXEROOT/vlib/v/driver/run_signals_nix.c').to_string()
	if headerless {
		g.writeln('#ifndef _WIN32')
		g.emit_c_runtime_declarations(['signal', 'kill'])
		g.writeln('#endif')
		mut lines := []string{}
		for line in source.split_into_lines() {
			if !line.starts_with('#include ') {
				lines << line
			}
		}
		source = lines.join('\n').replace('sig_atomic_t', 'i32')
	}
	g.write(source.replace('int v3_run_signals_begin(', 'static int v3_run_signals_begin(')
		.replace('void v3_run_signals_set_pid(', 'static void v3_run_signals_set_pid(')
		.replace('int v3_run_signals_finish(', 'static int v3_run_signals_finish('))
	g.writeln('')
	g.collect_preserved_c_fns(['v3_run_signals_begin', 'v3_run_signals_set_pid', 'v3_run_signals_finish'])
}
