#ifndef V_MACOS_BARE_KERNEL_SERVICES_H
#define V_MACOS_BARE_KERNEL_SERVICES_H

#include <stdint.h>

/* Mach errors retain kern_return_t/mach_msg_return_t values. These functions
 * never call libSystem or read/write its errno or TLV state. Each *_self call
 * acquires a send-right reference; release it with port_deallocate. reply_port
 * acquires a receive right, released with port_destroy_receive. */
uint64_t v_macos_kernel_mach_trap6(uint32_t, uint64_t, uint64_t, uint64_t,
    uint64_t, uint64_t, uint64_t);
/* Return native Darwin errno and optionally the primary syscall result (zero
 * on error). A null result pointer is valid; neither errno cell is touched. */
int32_t v_macos_kernel_bsd_call6(uint32_t, uint64_t, uint64_t, uint64_t,
    uint64_t, uint64_t, uint64_t, uint64_t *);
uint32_t v_macos_kernel_task_self(void);
uint32_t v_macos_kernel_thread_self(void);
uint32_t v_macos_kernel_reply_port(void);
int32_t v_macos_kernel_port_deallocate(uint32_t);
int32_t v_macos_kernel_port_destroy_receive(uint32_t);
int32_t v_macos_kernel_port_type(uint32_t, uint32_t *);
int32_t v_macos_kernel_thread_create_running(int32_t, const uint32_t *, uint32_t,
    uint32_t *);
int32_t v_macos_kernel_thread_terminate(uint32_t);
int32_t v_macos_kernel_thread_exit(void);
int32_t v_macos_kernel_thread_exit_mapped(uint64_t, uint64_t, uint32_t, uint32_t);
int32_t v_macos_kernel_semaphore_create(int32_t, int32_t, uint32_t *);
int32_t v_macos_kernel_semaphore_destroy(uint32_t);
int32_t v_macos_kernel_semaphore_wait(uint32_t);
int32_t v_macos_kernel_semaphore_signal(uint32_t);
int32_t v_macos_kernel_vm_allocate(uint64_t *, uint64_t, int32_t);
int32_t v_macos_kernel_vm_deallocate(uint64_t, uint64_t);
int32_t v_macos_kernel_vm_remap(uint64_t, uint64_t, uint64_t *, int32_t, int32_t,
    int32_t *, int32_t *);
/* ULF_NO_ERRNO is rejected: these return a positive Darwin errno or zero.
 * The V wrappers normalize it to the BF errno numbering without updating cells. */
int32_t v_macos_kernel_ulock_wait(uint32_t, void *, uint64_t, uint32_t);
int32_t v_macos_kernel_ulock_wait2(uint32_t, void *, uint64_t, uint64_t);
int32_t v_macos_kernel_ulock_wake(uint32_t, void *, uint64_t);
int32_t v_macos_kernel_page_size(uint64_t *);

#endif
