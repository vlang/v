#ifndef V_FASTC_LIBTCC_SYSTEM_DARWIN_H
#define V_FASTC_LIBTCC_SYSTEM_DARWIN_H

#if !defined(V_PARALLEL_CC) || defined(V_PARALLEL_CC_OUT_0)

#include <dlfcn.h>
#include <string.h>

typedef int (*v_fastc_system_fn)(const char *);

#if !defined(V_PARALLEL_CC) && defined(__INCLUDE_LEVEL__) &&                \
    __INCLUDE_LEVEL__ > 0
// A compiler built before the single-definition parallel-CC protocol inserts
// this header through out.h in every split translation unit. Weak definitions
// keep that one-generation bootstrap path linkable. A normal generated C file
// has include level zero and retains the strong interposer; current split builds
// emit one strong definition through V_PARALLEL_CC_OUT_0.
#define V_FASTC_SYSTEM_DEFINITION __attribute__((weak, visibility("hidden")))
#else
#define V_FASTC_SYSTEM_DEFINITION
#endif

V_FASTC_SYSTEM_DEFINITION _Thread_local int v_fastc_tcc_skip_codesign;
V_FASTC_SYSTEM_DEFINITION _Thread_local int v_fastc_tcc_skipped_codesigns;
V_FASTC_SYSTEM_DEFINITION _Thread_local v_fastc_system_fn v_fastc_real_system;

V_FASTC_SYSTEM_DEFINITION void v_fastc_tcc_set_error_func(TCCState *state,
                                                          void *opaque,
                                                          void *error_func) {
  tcc_set_error_func(state, opaque, (TCCErrorFunc *)error_func);
}

V_FASTC_SYSTEM_DEFINITION int system(const char *command) {
	static const char codesign_prefix[] = "codesign -f -s - ";
	if (v_fastc_tcc_skip_codesign && command != NULL
		&& strncmp(command, codesign_prefix, sizeof(codesign_prefix) - 1) == 0) {
		v_fastc_tcc_skipped_codesigns++;
		return 0;
	}
	if (v_fastc_real_system == NULL) {
		v_fastc_real_system = (v_fastc_system_fn)dlsym(RTLD_NEXT, "system");
	}
	return v_fastc_real_system == NULL ? -1 : v_fastc_real_system(command);
}

V_FASTC_SYSTEM_DEFINITION void v_fastc_tcc_set_skip_codesign(int skip) {
	v_fastc_tcc_skip_codesign = skip;
}

V_FASTC_SYSTEM_DEFINITION int v_fastc_tcc_skipped_codesign_count(void) {
	return v_fastc_tcc_skipped_codesigns;
}

#undef V_FASTC_SYSTEM_DEFINITION

#else

void v_fastc_tcc_set_error_func(TCCState *state, void *opaque,
                                void *error_func);
void v_fastc_tcc_set_skip_codesign(int skip);
int v_fastc_tcc_skipped_codesign_count(void);

#endif

#endif
