#ifndef V_COMPRESS_BROTLI_DL_H
#define V_COMPRESS_BROTLI_DL_H

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

#endif
