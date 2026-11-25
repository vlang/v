#include "probe.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

extern void v_freestanding_stdio_flush(void);

static void *condition_worker(void *ignored) { (void)ignored; v_freestanding_condition_worker(); return NULL; }
static void *increment(void *ignored) { (void)ignored; v_freestanding_compat_increment(); return NULL; }
int main(int argc, char **argv) {
    if (argc == 4) return v_freestanding_resolver_check(argv[1], (unsigned short)atoi(argv[2]), atoi(argv[3])) ? 1 : 0;
    if (argc != 2) return 2;
    int status = v_freestanding_compat_check(argv[1]);
    if (!status) status = v_freestanding_stdio_check(argv[1]);
    if (!status) status = v_freestanding_mutex_check();
    if (!status) status = v_freestanding_dns_parser_fuzz();
    if (!status) status = v_freestanding_format_contracts();
    if (!status) status = v_freestanding_fontstack_check(argv[1]);
    char format_path[4096];
    int length = snprintf(format_path, sizeof format_path, "%s/format.bin", argv[1]);
    if (length < 0 || length >= (int)sizeof format_path) return 6;
    if (!status) status = v_freestanding_format_oracle(format_path);
    if (status) { fprintf(stderr, "native ABI probe failed at C line %d\n", status); return 1; }
    pthread_t threads[8];
    for (int i = 0; i < 8; ++i) if (pthread_create(&threads[i], NULL, increment, NULL)) return 3;
    for (int i = 0; i < 8; ++i) if (pthread_join(threads[i], NULL)) return 4;
    if (v_freestanding_compat_total() != 80000) return 5;
    for (int i = 0; i < 8; ++i) if (pthread_create(&threads[i], NULL, condition_worker, NULL)) return 3;
    status = v_freestanding_condition_release();
    if (status) { fprintf(stderr, "condition probe failed at C line %d\n", status); return 1; }
    for (int i = 0; i < 8; ++i) if (pthread_join(threads[i], NULL)) return 4;
    puts("namespaced native ABI checks passed");
    v_freestanding_stdio_flush();
    return 0;
}
