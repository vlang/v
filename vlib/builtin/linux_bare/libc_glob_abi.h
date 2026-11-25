#ifndef V_BARE_GLOB_ABI_H
#define V_BARE_GLOB_ABI_H
#include <stdint.h>
#include <glob.h>
/* The V walker can test optional flags uniformly. An absent extension
 * contributes no bits to the accepted flag mask. */
#ifdef GLOB_ALTDIRFUNC
#define V_GLOB_HAS_CALLBACKS 1
#endif
#ifndef GLOB_ALTDIRFUNC
#define GLOB_ALTDIRFUNC 0
#endif
#ifndef GLOB_BRACE
#define GLOB_BRACE 0
#endif
#ifndef GLOB_MAGCHAR
#define GLOB_MAGCHAR 0
#endif
#ifndef GLOB_NOMAGIC
#define GLOB_NOMAGIC 0
#endif
#ifndef GLOB_ONLYDIR
#define GLOB_ONLYDIR 0
#endif
void v_glob_view(uintptr_t, uintptr_t, int32_t, int32_t);
void v_glob_assign(uintptr_t, uintptr_t, int32_t);
uintptr_t v_glob_open(uintptr_t, uintptr_t);
uintptr_t v_glob_read(uintptr_t, uintptr_t, int32_t);
void v_glob_close(uintptr_t, uintptr_t);
int32_t v_glob_stat(uintptr_t, uintptr_t, int32_t, int32_t, uintptr_t);
int32_t v_glob_error(uintptr_t, uintptr_t, int32_t);
uintptr_t v_glob_home(uintptr_t);
#endif
