#include "probe.h"
#include "../../macos_bare/kernel_services.h"
#include <mach/mach.h>
#include <mach/machine/thread_status.h>

static uint32_t worker_done, worker_park, exiting_done;
static uint32_t exiting_semaphore;
static uint32_t exiting_port, exiting_ready;
static uint64_t exiting_address, exiting_size;
static uint64_t worker_tid;

extern uint64_t v_macos_probe_code_template(void);
#if defined(__x86_64__)
__asm__(".section __TEXT,__text,regular,pure_instructions\n.p2align 14\n"
    ".globl _v_macos_probe_code_template\n_v_macos_probe_code_template:\n"
    "movl $777, %eax\nret\n.space 16384-(.-_v_macos_probe_code_template),0x90\n");
#else
__asm__(".section __TEXT,__text,regular,pure_instructions\n.p2align 14\n"
    ".globl _v_macos_probe_code_template\n_v_macos_probe_code_template:\n"
    "mov x0, #777\nret\n.space 16384-(.-_v_macos_probe_code_template)\n");
#endif

static void worker(void) {
    worker_tid = v_macos_probe_tid();
    __atomic_store_n(&worker_done, 1, __ATOMIC_RELEASE);
    (void)v_macos_kernel_ulock_wake(1 | 0x100, &worker_done, 0);
    for (;;) (void)v_macos_kernel_ulock_wait(1, &worker_park, 0, 0);
}

static void self_exit_worker(void) {
    while (!__atomic_load_n(&exiting_ready, __ATOMIC_ACQUIRE))
        (void)v_macos_kernel_ulock_wait(1, &exiting_ready, 0, 100000);
    __atomic_store_n(&exiting_done, 1, __ATOMIC_RELEASE);
    (void)v_macos_kernel_ulock_wake(1 | 0x100, &exiting_done, 0);
    (void)v_macos_kernel_thread_exit_mapped(exiting_address, exiting_size,
        exiting_port, exiting_semaphore);
    __atomic_store_n(&exiting_done, 2, __ATOMIC_RELEASE);
    for (;;) (void)v_macos_kernel_ulock_wait(1, &worker_park, 0, 0);
}

int v_macos_kernel_services_probe(void) {
    uint64_t failed_result = 99;
    if (v_macos_kernel_bsd_call6(3, UINT64_MAX, 0, 0, 0, 0, 0, &failed_result) != 9
        || failed_result != 0) return 36;
    uint64_t main_tid = v_macos_probe_tid();
    if (!main_tid) return 34;
    uint64_t page_size = 0;
    if (v_macos_kernel_page_size(&page_size) || page_size == 0) return 20;
    uint32_t task = v_macos_kernel_task_self();
    uint32_t thread = v_macos_kernel_thread_self();
    uint32_t reply = v_macos_kernel_reply_port();
    if (!task || !thread || !reply) return 1;
    if (v_macos_kernel_port_deallocate(thread)
        || v_macos_kernel_port_destroy_receive(reply)
        || v_macos_kernel_port_deallocate(task)) return 2;

    uint64_t region = 0;
    if (v_macos_kernel_vm_allocate(&region, 65536, VM_FLAGS_ANYWHERE) || !region) return 3;
    unsigned char *bytes = (unsigned char *)(uintptr_t)region;
    for (unsigned i = 0; i < 65536; ++i) bytes[i] = (unsigned char)(i % 251);
    uint64_t alias = 0;
    int32_t current = 0, maximum = 0;
    if (v_macos_kernel_vm_remap(region, 65536, &alias, VM_FLAGS_ANYWHERE, 0,
        &current, &maximum) || !alias || alias == region) return 4;
    if ((current & (VM_PROT_READ | VM_PROT_WRITE)) != (VM_PROT_READ | VM_PROT_WRITE)) return 5;
    unsigned char *copy = (unsigned char *)(uintptr_t)alias;
    for (unsigned i = 0; i < 65536; ++i) if (copy[i] != (unsigned char)(i % 251)) return 6;
    copy[333] = 179;
    if (bytes[333] != 179) return 7;
    if (v_macos_kernel_vm_deallocate(alias, 65536)) return 8;

    uint64_t code_address = 0;
    if (v_macos_kernel_vm_allocate(&code_address, 16384, VM_FLAGS_ANYWHERE)) return 21;
    uint64_t requested_address = code_address;
    if (v_macos_kernel_vm_remap((uintptr_t)v_macos_probe_code_template, 16384,
        &code_address, VM_FLAGS_FIXED | VM_FLAGS_OVERWRITE, 0, &current, &maximum)) return 22;
    if (code_address != requested_address || !(current & VM_PROT_EXECUTE)
        || !(current & VM_PROT_READ) || (current & VM_PROT_WRITE)) return 23;
    if (((uint64_t (*)(void))(uintptr_t)code_address)() != 777) return 24;
    if (v_macos_kernel_vm_deallocate(code_address, 16384)) return 25;

    uint32_t untouched = 1;
    if (v_macos_kernel_ulock_wait(1, &untouched, 0, 1)) return 9;
    if (v_macos_kernel_ulock_wait(1 | 0x01000000, &untouched, 0, 1) != 22) return 10;
    if (v_macos_kernel_ulock_wake(1 | 0x01000000, &untouched, 0) != 22) return 11;
    /* XNU's atomic copyin deliberately cannot fault in a fresh BSS page. A
     * mutex/condition caller always reads its predicate before entering wait. */
    __atomic_store_n(&worker_done, 0, __ATOMIC_RELEASE);
    __atomic_store_n(&worker_park, 0, __ATOMIC_RELEASE);
    if (v_macos_kernel_ulock_wait(1, &worker_done, 0, 1000) != 60) return 12;
    if (v_macos_kernel_ulock_wait2(1, &worker_done, 0, 1) != 60) return 19;

#if defined(__x86_64__)
    x86_thread_state64_t state = {0};
    state.__rip = (uint64_t)(uintptr_t)worker;
    state.__rsp = region + 65536 - 8;
    state.__rflags = 0x202;
    int32_t flavor = x86_THREAD_STATE64;
    uint32_t count = x86_THREAD_STATE64_COUNT;
#else
    arm_thread_state64_t state = {0};
    arm_thread_state64_set_pc_fptr(state, worker);
    arm_thread_state64_set_sp(state, region + 65536);
    int32_t flavor = ARM_THREAD_STATE64;
    uint32_t count = ARM_THREAD_STATE64_COUNT;
#endif
    uint32_t child = 0;
    if (v_macos_kernel_thread_create_running(flavor, (const uint32_t *)&state,
        count, &child) || !child) return 13;
    unsigned attempts = 0;
    while (!__atomic_load_n(&worker_done, __ATOMIC_ACQUIRE)) {
        int32_t code = v_macos_kernel_ulock_wait(1, &worker_done, 0, 100000);
        if (code != 0 && code != 4 && code != 60) return 14;
        if (++attempts > 20) return 15;
    }
    if (!worker_tid || worker_tid == main_tid) return 35;
    if (v_macos_kernel_thread_terminate(child)) return 16;
    if (v_macos_kernel_port_deallocate(child)) return 17;

    if (v_macos_kernel_semaphore_create(0, 0, &exiting_semaphore) || !exiting_semaphore) return 26;
    if (v_macos_kernel_semaphore_signal(exiting_semaphore)
        || v_macos_kernel_semaphore_wait(exiting_semaphore)) return 27;
    exiting_address = region;
    exiting_size = 65536;

#if defined(__x86_64__)
    state.__rip = (uint64_t)(uintptr_t)self_exit_worker;
#else
    arm_thread_state64_set_pc_fptr(state, self_exit_worker);
#endif
    if (v_macos_kernel_thread_create_running(flavor, (const uint32_t *)&state,
        count, &child) || !child) return 28;
    /* Transfer the single received thread reference to BSD361. The worker
     * cannot consume it before the create RPC has returned its send right. */
    exiting_port = child;
    __atomic_store_n(&exiting_ready, 1, __ATOMIC_RELEASE);
    (void)v_macos_kernel_ulock_wake(1 | 0x100, &exiting_ready, 0);
    int32_t waited;
    do {
        waited = (int32_t)v_macos_kernel_mach_trap6(38, exiting_semaphore, 2, 0, 0, 0, 0);
    } while (waited == KERN_ABORTED);
    if (waited) return 29;
    if (__atomic_load_n(&exiting_done, __ATOMIC_ACQUIRE) != 1) return 29;
    attempts = 0;
    for (;;) {
        uint32_t type = 0;
        int32_t status = v_macos_kernel_port_type(child, &type);
        if (status == KERN_INVALID_NAME) break;
        if (status != 0 || ++attempts > 10000) return 30;
    }
    alias = 0;
    if (v_macos_kernel_vm_remap(region, 65536, &alias, VM_FLAGS_ANYWHERE, 0,
        &current, &maximum) != KERN_INVALID_ADDRESS) return 31;
    if (v_macos_kernel_semaphore_destroy(exiting_semaphore)) return 32;
    if (v_macos_kernel_port_deallocate(exiting_semaphore)) return 33;
    return 0;
}
