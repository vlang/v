#ifndef V_BUILTIN_GC_DEBUGGER_LINUX_H
#define V_BUILTIN_GC_DEBUGGER_LINUX_H

#if defined(__linux__)
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#pragma weak __data_start
#pragma weak data_start
#pragma weak _end

extern int __data_start[];
extern int data_start[];
extern int _end[];

static inline int v__gc_can_register_main_data_roots_linux(void) {
	void* data_start_ptr = __data_start != NULL ? (void*)__data_start : (void*)data_start;
	void* data_end_ptr = (void*)_end;
	return data_start_ptr != NULL && data_end_ptr != NULL && data_start_ptr < data_end_ptr;
}

static inline int v__gc_debugger_present_linux(void) {
	int fd = open("/proc/self/status", O_RDONLY);
	if (fd < 0) {
		return 0;
	}
	char buf[4096];
	ssize_t n = read(fd, buf, sizeof(buf) - 1);
	(void)close(fd);
	if (n <= 0) {
		return 0;
	}
	buf[n] = '\0';
	char* tracer_pid = strstr(buf, "TracerPid:");
	if (tracer_pid == NULL) {
		return 0;
	}
	tracer_pid += sizeof("TracerPid:") - 1;
	while (*tracer_pid == ' ' || *tracer_pid == '\t') {
		++tracer_pid;
	}
	return *tracer_pid != '0' && *tracer_pid != '\n' && *tracer_pid != '\0';
}

static inline void v__gc_register_main_data_roots_linux(void) {
	if (!v__gc_can_register_main_data_roots_linux()) {
		return;
	}
	void* data_start_ptr = __data_start != NULL ? (void*)__data_start : (void*)data_start;
	GC_add_roots(data_start_ptr, (void*)_end);
}
#endif

#endif
