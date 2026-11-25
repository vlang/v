#include "libc_compat.h"
#include "libc_directory_abi.h"
#include <dirent.h>
#include <stdint.h>
#include <string.h>

extern int32_t v_directory_scan(uintptr_t, uintptr_t, uintptr_t, uintptr_t, int32_t);
extern int32_t v_string_collate(uintptr_t, uintptr_t);
extern int32_t v_string_version_compare(uintptr_t, uintptr_t);
uintptr_t v_directory_open(uintptr_t path) { return (uintptr_t)opendir((const char *)path); }
uintptr_t v_directory_read(uintptr_t directory, int32_t large) {
    return large ? (uintptr_t)readdir64((DIR *)directory) : (uintptr_t)readdir((DIR *)directory);
}
int32_t v_directory_close(uintptr_t directory) { return closedir((DIR *)directory); }
uintptr_t v_directory_entry_size(uintptr_t entry, int32_t large) {
    return large ? ((const struct dirent64 *)entry)->d_reclen : ((const struct dirent *)entry)->d_reclen;
}
int32_t v_directory_select(uintptr_t callback, uintptr_t entry, int32_t large) {
    if (large) return ((int (*)(const struct dirent64 *))callback)((const struct dirent64 *)entry);
    return ((int (*)(const struct dirent *))callback)((const struct dirent *)entry);
}
int32_t v_directory_compare(uintptr_t callback, uintptr_t a, uintptr_t b, int32_t large) {
    if (large) return ((int (*)(const struct dirent64 **, const struct dirent64 **))callback)
        ((const struct dirent64 **)a, (const struct dirent64 **)b);
    return ((int (*)(const struct dirent **, const struct dirent **))callback)
        ((const struct dirent **)a, (const struct dirent **)b);
}
int scandir(const char *path, struct dirent ***list, int (*select)(const struct dirent *),
            int (*compare)(const struct dirent **, const struct dirent **)) {
    return v_directory_scan((uintptr_t)path, (uintptr_t)list, (uintptr_t)select, (uintptr_t)compare, 0);
}
int scandir64(const char *path, struct dirent64 ***list, int (*select)(const struct dirent64 *),
              int (*compare)(const struct dirent64 **, const struct dirent64 **)) {
    return v_directory_scan((uintptr_t)path, (uintptr_t)list, (uintptr_t)select, (uintptr_t)compare, 1);
}
int alphasort(const struct dirent **a, const struct dirent **b) {
    return v_string_collate((uintptr_t)(*a)->d_name, (uintptr_t)(*b)->d_name);
}
int alphasort64(const struct dirent64 **a, const struct dirent64 **b) {
    return v_string_collate((uintptr_t)(*a)->d_name, (uintptr_t)(*b)->d_name);
}
int versionsort(const struct dirent **a, const struct dirent **b) {
    return v_string_version_compare((uintptr_t)(*a)->d_name, (uintptr_t)(*b)->d_name);
}
int versionsort64(const struct dirent64 **a, const struct dirent64 **b) {
    return v_string_version_compare((uintptr_t)(*a)->d_name, (uintptr_t)(*b)->d_name);
}
