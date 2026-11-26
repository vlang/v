#include "libc_compat.h"
#include "libc_glob_abi.h"
#include <dirent.h>
#include <pwd.h>
#include <sys/stat.h>

/* Native public layouts and callback signatures only. Walking, matching,
 * expansion, ownership and sorting are implemented in V. */
struct v_glob_view {
    uintptr_t count, paths, offset;
    int32_t flags;
    uintptr_t close_fn, read_fn, open_fn, lstat_fn, stat_fn;
};
#ifdef V_GLOB_HAS_CALLBACKS
#define V_GLOB_CALLBACKS(g, v, flags) if (flags & GLOB_ALTDIRFUNC) { \
        v->close_fn = (uintptr_t)g->gl_closedir; v->read_fn = (uintptr_t)g->gl_readdir; \
        v->open_fn = (uintptr_t)g->gl_opendir; v->lstat_fn = (uintptr_t)g->gl_lstat; \
        v->stat_fn = (uintptr_t)g->gl_stat; }
#define V_GLOB_SET_FLAGS(g, flags_) ((g)->gl_flags = (flags_))
#else
#define V_GLOB_CALLBACKS(g, v, flags) ((void)0)
#define V_GLOB_SET_FLAGS(g, flags_) ((void)0)
#endif
#define V_GLOB_READ(type) do { const type *g = (const type *)input; \
    *v = (struct v_glob_view){ \
        .count = flags & GLOB_APPEND ? g->gl_pathc : 0, \
        .paths = flags & GLOB_APPEND ? (uintptr_t)g->gl_pathv : 0, \
        .offset = flags & GLOB_DOOFFS ? g->gl_offs : 0, .flags = flags}; \
    V_GLOB_CALLBACKS(g, v, flags); } while (0)
void v_glob_view(uintptr_t input, uintptr_t output, int32_t large, int32_t flags) {
    struct v_glob_view *v = (void *)output;
    if (large) V_GLOB_READ(glob64_t); else V_GLOB_READ(glob_t);
}
#define V_GLOB_WRITE(type) do { type *g = (type *)output; \
    g->gl_pathc = v->count; g->gl_pathv = (char **)v->paths; \
    g->gl_offs = v->offset; V_GLOB_SET_FLAGS(g, v->flags); } while (0)
void v_glob_assign(uintptr_t output, uintptr_t input, int32_t large) {
    const struct v_glob_view *v = (const void *)input;
    if (large) V_GLOB_WRITE(glob64_t); else V_GLOB_WRITE(glob_t);
}
uintptr_t v_glob_open(uintptr_t callback, uintptr_t path) {
    return callback ? (uintptr_t)((void *(*)(const char *))callback)((const char *)path)
                    : (uintptr_t)opendir((const char *)path);
}
uintptr_t v_glob_read(uintptr_t callback, uintptr_t directory, int32_t large) {
    if (large) {
        const struct dirent64 *e = callback ? ((struct dirent64 *(*)(void *))callback)((void *)directory)
                                          : readdir64((DIR *)directory);
        return e ? (uintptr_t)e->d_name : 0;
    }
    const struct dirent *e = callback ? ((struct dirent *(*)(void *))callback)((void *)directory)
                                    : readdir((DIR *)directory);
    return e ? (uintptr_t)e->d_name : 0;
}
void v_glob_close(uintptr_t callback, uintptr_t directory) {
    if (callback) ((void (*)(void *))callback)((void *)directory);
    else closedir((DIR *)directory);
}
int32_t v_glob_stat(uintptr_t callback, uintptr_t path, int32_t follow, int32_t large, uintptr_t mode) {
    if (large) {
        struct stat64 s;
        int r = callback ? ((int (*)(const char *, struct stat64 *))callback)((const char *)path, &s)
                         : follow ? stat64((const char *)path, &s) : lstat64((const char *)path, &s);
        if (!r) *(uint32_t *)mode = s.st_mode;
        return r;
    }
    struct stat s;
    int r = callback ? ((int (*)(const char *, struct stat *))callback)((const char *)path, &s)
                     : follow ? stat((const char *)path, &s) : lstat((const char *)path, &s);
    if (!r) *(uint32_t *)mode = s.st_mode;
    return r;
}
int32_t v_glob_error(uintptr_t callback, uintptr_t path, int32_t error) {
    return ((int (*)(const char *, int))callback)((const char *)path, error);
}
uintptr_t v_glob_home(uintptr_t record) { return (uintptr_t)((struct passwd *)record)->pw_dir; }
extern int32_t v_pattern_glob(uintptr_t, int32_t, uintptr_t, uintptr_t, int32_t);
extern void v_pattern_globfree(uintptr_t, int32_t);
int glob(const char *pattern, int flags, int (*error)(const char *, int), glob_t *out) {
    return v_pattern_glob((uintptr_t)pattern, flags, (uintptr_t)error, (uintptr_t)out, 0);
}
#if !defined(glob64)
int glob64(const char *pattern, int flags, int (*error)(const char *, int), glob64_t *out) {
    return v_pattern_glob((uintptr_t)pattern, flags, (uintptr_t)error, (uintptr_t)out, 1);
}
#endif
void globfree(glob_t *g) { v_pattern_globfree((uintptr_t)g, 0); }
#if !defined(globfree64)
void globfree64(glob64_t *g) { v_pattern_globfree((uintptr_t)g, 1); }
#endif
