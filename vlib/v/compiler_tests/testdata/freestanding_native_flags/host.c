#include <dlfcn.h>
#include <stdio.h>
typedef int (*probe_fn)(void);
int main(int argc, char **argv) {
    if (argc != 2) return 1;
    void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
    probe_fn probe = (probe_fn)dlsym(library, "v_native_flags_probe");
    if (!probe || probe() != 3) return 3;
    dlclose(library);
    puts("native library runtime flags passed");
    return 0;
}
