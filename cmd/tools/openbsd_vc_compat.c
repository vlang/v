// Bootstrap compatibility for portable vc snapshots generated before the
// OpenBSD entropy and pthread condition-attribute guards were added.
#ifdef __OpenBSD__
#include <stdarg.h>
#include <stddef.h>

int getentropy(void *buffer, size_t length);

long syscall(long number, ...) {
	(void)number;
	va_list arguments;
	va_start(arguments, number);
	unsigned char *buffer = va_arg(arguments, unsigned char *);
	int length = va_arg(arguments, int);
	va_end(arguments);
	if (length < 0) {
		return -1;
	}
	for (size_t offset = 0; offset < (size_t)length;) {
		size_t chunk = (size_t)length - offset;
		if (chunk > 256) {
			chunk = 256;
		}
		if (getentropy(buffer + offset, chunk) != 0) {
			return -1;
		}
		offset += chunk;
	}
	return length;
}

int pthread_condattr_setpshared(void *attribute, int process_shared) {
	(void)attribute;
	(void)process_shared;
	return 0;
}
#endif
