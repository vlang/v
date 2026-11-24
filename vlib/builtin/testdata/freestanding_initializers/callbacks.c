#include <unistd.h>
static __thread int v_initializer_tls = 31;
static int v_initializer_phase;
static int v_initializer_valid = 1;

static int v_test_equal(const char *a, const char *b) {
    while (*a == *b && *a != 0) { ++a; ++b; }
    return *a == *b;
}
static int v_test_arguments(int argc, char **argv, char **envp) {
    if (argc != 3 || argv == NULL || envp != argv + argc + 1) return 0;
    if (argv[argc] != NULL || !v_test_equal(argv[1], "alpha beta") ||
        !v_test_equal(argv[2], "λ")) return 0;
    for (char **entry = envp; *entry != NULL; ++entry) {
        if (v_test_equal(*entry, "V_FREESTANDING_INIT_TEST=ready-λ")) return 1;
    }
    return 0;
}
static void v_test_preinitialize(int argc, char **argv, char **envp) {
    if (!v_test_arguments(argc, argv, envp)) v_initializer_valid = 0;
    if (v_initializer_tls != 31 || v_initializer_phase != 0) v_initializer_valid = 0;
    v_initializer_tls = 41;
    v_initializer_phase = 1;
    (void)write(1, "P", 1);
}
__attribute__((used, section(".preinit_array")))
static void (*v_test_preinit_entry)(int, char **, char **) = v_test_preinitialize;

__attribute__((constructor(101))) static void v_test_initialize_first(int argc, char **argv, char **envp) {
    if (!v_test_arguments(argc, argv, envp)) v_initializer_valid = 0;
    if (v_initializer_tls != 41 || v_initializer_phase != 1) v_initializer_valid = 0;
    v_initializer_tls = 51;
    v_initializer_phase = 2;
    (void)write(1, "A", 1);
}
__attribute__((constructor(200))) static void v_test_initialize_second(int argc, char **argv, char **envp) {
    if (!v_test_arguments(argc, argv, envp)) v_initializer_valid = 0;
    if (v_initializer_tls != 51 || v_initializer_phase != 2) v_initializer_valid = 0;
    v_initializer_phase = 3;
    (void)write(1, "B", 1);
}
int v_test_initializers_ready(void) {
    #if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
    /* x86-64 guarantees SSE2. This value comes from the toolchain's own
     * constructor, not from a test-only call to __builtin_cpu_init. */
    if (!__builtin_cpu_supports("sse2")) return 0;
#endif
    return v_initializer_valid && v_initializer_phase == 3 && v_initializer_tls == 51;
}
__attribute__((destructor(101))) static void v_test_finalize_first(void) {
    (void)write(1, v_initializer_phase == 4 ? "Y\n" : "!\n", 2);
}
__attribute__((destructor(200))) static void v_test_finalize_second(void) {
    (void)write(1, v_initializer_phase == 3 ? "Z" : "!", 1);
    v_initializer_phase = 4;
}
