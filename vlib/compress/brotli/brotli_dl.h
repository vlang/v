#ifndef V_COMPRESS_BROTLI_DL_H
#define V_COMPRESS_BROTLI_DL_H

#include <stddef.h>

#if defined(_WIN32)
#include <windows.h>

static void* v_brotli_open(const char* name) {
	return (void*)LoadLibraryA(name);
}

static void* v_brotli_sym(void* handle, const char* name) {
	return (void*)GetProcAddress((HMODULE)handle, name);
}

static int v_brotli_close(void* handle) {
	return FreeLibrary((HMODULE)handle) != 0;
}
#else
#include <dlfcn.h>
#ifdef dlopen
#undef dlopen
#endif
#ifdef dlsym
#undef dlsym
#endif
#ifdef dlclose
#undef dlclose
#endif

static void* v_brotli_open(const char* name) {
	return dlopen(name, RTLD_LAZY);
}

static void* v_brotli_sym(void* handle, const char* name) {
	return dlsym(handle, name);
}

static int v_brotli_close(void* handle) {
	return dlclose(handle) == 0;
}
#endif

#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
void __msan_unpoison(const volatile void* a, size_t size);
#define V_BROTLI_MEMORY_SANITIZER 1
#endif
#endif

static void v_brotli_msan_unpoison(const void* ptr, size_t len) {
#if defined(V_BROTLI_MEMORY_SANITIZER)
	if (len > 0) {
		__msan_unpoison(ptr, len);
	}
#else
	(void)ptr;
	(void)len;
#endif
}

#endif
