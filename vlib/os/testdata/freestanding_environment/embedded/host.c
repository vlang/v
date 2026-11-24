#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef ENV_OBJECT
extern int v_environment_probe(void);
#else
#include <dlfcn.h>
typedef int (*probe_fn)(void);
#endif

int main(int argc, char **argv) {
    if (argc != 2) return 1;
#ifdef ENV_OBJECT
    (void)argv;
    int result = v_environment_probe();
#else
    void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
    if (!library) { fprintf(stderr, "%s\n", dlerror()); return 2; }
    probe_fn probe = (probe_fn)dlsym(library, "v_environment_probe");
    if (!probe) return 3;
    int result = probe();
    dlclose(library);
#endif
    if (result) { fprintf(stderr, "environment probe: %d\n", result); return result; }
    const char *inherited = getenv("FENV_UNICODE");
    const char *empty = getenv("FENV_EMPTY");
    if (!inherited || strcmp(inherited, "λ🙂") || !empty || *empty) return 19;
    puts("embedded environment passed");
    return 0;
}
