#include "initializers.h"
#include <stddef.h>

typedef void (*v_bare_initializer)(int, char **, char **);
typedef void (*v_bare_finalizer)(void);

/* GNU ELF linkers synthesize these bounds when the corresponding input
 * sections exist. Weak references also support images without callbacks. */
extern v_bare_initializer __preinit_array_start[] __attribute__((weak));
extern v_bare_initializer __preinit_array_end[] __attribute__((weak));
extern v_bare_initializer __init_array_start[] __attribute__((weak));
extern v_bare_initializer __init_array_end[] __attribute__((weak));
extern v_bare_finalizer __fini_array_start[] __attribute__((weak));
extern v_bare_finalizer __fini_array_end[] __attribute__((weak));

void v_bare_run_initializers(int argc, char **argv) {
    char **envp = argv + argc + 1;
    if (__preinit_array_start != NULL && __preinit_array_end != NULL) {
        for (v_bare_initializer *entry = __preinit_array_start;
             entry != __preinit_array_end; ++entry) {
            (*entry)(argc, argv, envp);
        }
    }
    if (__init_array_start != NULL && __init_array_end != NULL) {
        for (v_bare_initializer *entry = __init_array_start;
             entry != __init_array_end; ++entry) {
            (*entry)(argc, argv, envp);
        }
    }
}

void v_bare_run_finalizers(void) {
    if (__fini_array_start != NULL && __fini_array_end != NULL) {
        v_bare_finalizer *entry = __fini_array_end;
        while (entry != __fini_array_start) {
            (*--entry)();
        }
    }
}
