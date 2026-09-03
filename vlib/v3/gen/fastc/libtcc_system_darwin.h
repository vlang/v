#ifndef V_FASTC_LIBTCC_SYSTEM_DARWIN_H
#define V_FASTC_LIBTCC_SYSTEM_DARWIN_H

#include <dlfcn.h>
#include <string.h>

typedef int (*v_fastc_system_fn)(const char *);

static _Thread_local int v_fastc_tcc_skip_codesign;
static _Thread_local int v_fastc_tcc_skipped_codesigns;
static _Thread_local v_fastc_system_fn v_fastc_real_system;

int system(const char *command) {
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

static void v_fastc_tcc_set_skip_codesign(int skip) {
	v_fastc_tcc_skip_codesign = skip;
}

static int v_fastc_tcc_skipped_codesign_count(void) {
	return v_fastc_tcc_skipped_codesigns;
}

#endif
