#include "../../macos_bare/kernel_services.h"
#include <stdint.h>

void *v_test_closure_table_a(void);
void *v_test_closure_table_b(void);

static void *(*read_capture)(void);

static int captured_value(void) {
    const int *payload = (const int *)read_capture();
    return *payload;
}

/* Reuse the kernel-services fixture's raw entry, exit, and memory helpers. */
int v_macos_kernel_services_probe(void) {
    void *source = v_test_closure_table_a();
    if (source != v_test_closure_table_b()) return 1;
    uint64_t page_size = 0;
    if (v_macos_kernel_page_size(&page_size) || !page_size || page_size > 16384)
        return 2;
    for (int pass = 0; pass < 4; ++pass) {
        uint64_t base = 0;
        if (v_macos_kernel_vm_allocate(&base, 32768, 1)) return 3;
        uint64_t code = base + 16384;
        int32_t current = 0, maximum = 0;
        if (v_macos_kernel_vm_remap((uintptr_t)source, 16384, &code, 0x4000,
            0, &current, &maximum) || code != base + 16384 || current != 5)
            return 4;
        read_capture = (void *(*)(void))(uintptr_t)code;
        for (int slot = 1; slot < 1024; ++slot) {
            int payload = pass * 1024 + slot;
            void **metadata = (void **)(uintptr_t)(base + slot * 16);
            metadata[0] = &payload;
            metadata[1] = (void *)captured_value;
            int (*callback)(void) = (int (*)(void))(uintptr_t)(code + slot * 16);
            if (callback() != payload) return 5;
        }
#if defined(PROBE_WRITE_CODE)
        const char marker[] = "closure code write probe reached\n";
        if (v_macos_kernel_bsd_call6(4, 1, (uintptr_t)marker, sizeof(marker) - 1,
            0, 0, 0, 0)) return 6;
        *(volatile unsigned char *)(uintptr_t)(code + 16) = 0xc3;
        return 93;
#elif defined(PROBE_EXEC_METADATA)
        *(volatile unsigned char *)(uintptr_t)base = 0xc3;
        const char marker[] = "closure metadata execute probe reached\n";
        if (v_macos_kernel_bsd_call6(4, 1, (uintptr_t)marker, sizeof(marker) - 1,
            0, 0, 0, 0)) return 6;
        ((void (*)(void))(uintptr_t)base)();
        return 93;
#endif
        if (v_macos_kernel_vm_deallocate(base, 32768)) return 7;
    }
    return 0;
}
