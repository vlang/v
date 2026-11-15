#include <stddef.h>

#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
void __msan_unpoison(const volatile void *data, size_t len);
#endif
#endif

void v_brotli_msan_unpoison(const volatile void *data, size_t len) {
#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
	__msan_unpoison(data, len);
#else
	(void)data;
	(void)len;
#endif
#else
	(void)data;
	(void)len;
#endif
}
