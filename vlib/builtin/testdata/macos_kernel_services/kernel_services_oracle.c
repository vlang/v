#include "kernel_services.h"

#include <stddef.h>
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <mach/mach_port.h>
#include <mach/mig_errors.h>
#include <mach/task.h>
#include <mach/thread_act.h>

/* Wire structures come from the target SDK's generated MIG headers. Message
 * numbers and trap ABI: XNU osfmk/mach definitions and kern/syscall_sw.c; scalar
 * mach_msg2 packing/retry rules: libsyscall/mach/mach_msg.c and mach/message.h.
 * No generated MIG client is linked, as it would call libSystem. */
static const NDR_record_t v_macos_ndr = {0, 0, 0, 0, 1, 0, 0, 0};

/* Volatile stores keep optimization from introducing an out-of-line __bzero
 * or memset dependency. Only request bytes need initializing, not reply space. */
static void v_macos_clear(void *memory, size_t length) {
    volatile unsigned char *bytes = memory;
    for (size_t i = 0; i < length; ++i) bytes[i] = 0;
}
_Static_assert(sizeof(mach_msg_header_t) == 24, "Mach message header ABI");
_Static_assert(offsetof(__Request__thread_create_running_t, new_state) == 40,
    "thread_create_running MIG ABI");
_Static_assert(sizeof(__Request__mach_vm_remap_t) == 92, "mach_vm_remap request ABI");
_Static_assert(sizeof(__Reply__mach_vm_remap_t) == 52, "mach_vm_remap reply ABI");

uint64_t v_macos_kernel_mach_trap6(uint32_t index, uint64_t a1, uint64_t a2,
    uint64_t a3, uint64_t a4, uint64_t a5, uint64_t a6) {
#if defined(__x86_64__)
    register uint64_t r10 __asm__("r10") = a4;
    register uint64_t r8 __asm__("r8") = a5;
    register uint64_t r9 __asm__("r9") = a6;
    uint64_t result = UINT64_C(0x1000000) | index;
    __asm__ volatile("syscall" : "+a"(result), "+D"(a1), "+S"(a2),
        "+d"(a3), "+r"(r10), "+r"(r8), "+r"(r9) : : "rcx", "r11", "cc", "memory");
    return result;
#elif defined(__aarch64__)
    register uint64_t x0 __asm__("x0") = a1;
    register uint64_t x1 __asm__("x1") = a2;
    register uint64_t x2 __asm__("x2") = a3;
    register uint64_t x3 __asm__("x3") = a4;
    register uint64_t x4 __asm__("x4") = a5;
    register uint64_t x5 __asm__("x5") = a6;
    register int64_t x16 __asm__("x16") = -(int64_t)index;
    __asm__ volatile("svc #0x80" : "+r"(x0), "+r"(x1), "+r"(x2),
        "+r"(x3), "+r"(x4), "+r"(x5), "+r"(x16) : : "cc", "memory");
    return x0;
#else
#error macOS kernel services require x86_64 or arm64
#endif
}

/* Unlike inline asm, a callable assembly stub retains the kernel's expected
 * C stack slots for the seventh/eighth arguments on x86_64. */
extern int32_t v_macos_kernel_msg2(void *, uint64_t, uint64_t, uint64_t,
    uint64_t, uint64_t, uint64_t, uint64_t);
#if defined(__x86_64__)
__asm__(".text\n.p2align 4\n.globl _v_macos_kernel_msg2\n"
    ".private_extern _v_macos_kernel_msg2\n_v_macos_kernel_msg2:\n"
    "movq %rcx, %r10\nmovl $0x100002f, %eax\nsyscall\nret\n");
#elif defined(__aarch64__)
__asm__(".text\n.p2align 2\n.globl _v_macos_kernel_msg2\n"
    ".private_extern _v_macos_kernel_msg2\n_v_macos_kernel_msg2:\n"
    "mov x16, #-47\nsvc #0x80\nret\n");
#endif

uint32_t v_macos_kernel_task_self(void) {
    return (uint32_t)v_macos_kernel_mach_trap6(28, 0, 0, 0, 0, 0, 0);
}
uint32_t v_macos_kernel_thread_self(void) {
    return (uint32_t)v_macos_kernel_mach_trap6(27, 0, 0, 0, 0, 0, 0);
}
uint32_t v_macos_kernel_reply_port(void) {
    return (uint32_t)v_macos_kernel_mach_trap6(26, 0, 0, 0, 0, 0, 0);
}

static int32_t v_macos_port_release(uint32_t task, uint32_t port) {
    return (int32_t)v_macos_kernel_mach_trap6(18, task, port, 0, 0, 0, 0);
}
int32_t v_macos_kernel_port_deallocate(uint32_t port) {
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t result = v_macos_port_release(task, port);
    v_macos_port_release(task, task);
    return result;
}
int32_t v_macos_kernel_port_destroy_receive(uint32_t port) {
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t result = (int32_t)v_macos_kernel_mach_trap6(19, task, port,
        MACH_PORT_RIGHT_RECEIVE, (uint64_t)(int64_t)-1, 0, 0);
    v_macos_port_release(task, task);
    return result;
}

static uint64_t v_macos_pair(uint32_t low, uint32_t high) {
    return low | ((uint64_t)high << 32);
}

/* Request/reply storage includes space for a minimal kernel trailer. The RPC
 * owns a fresh receive right, so concurrent callers never share a reply port.
 * A receive interruption retries receive only: it must not create a second
 * thread or VM mapping by resending an already accepted request. */
static int32_t v_macos_rpc(mach_msg_header_t *message, uint32_t capacity,
    uint32_t task, uint32_t destination, int32_t id, int complex) {
    uint32_t reply = v_macos_kernel_reply_port();
    if (!reply) return KERN_RESOURCE_SHORTAGE;
    uint32_t size = message->msgh_size;
    message->msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND,
        MACH_MSG_TYPE_MAKE_SEND_ONCE) | (complex ? MACH_MSGH_BITS_COMPLEX : 0);
    message->msgh_remote_port = destination;
    message->msgh_local_port = reply;
    message->msgh_voucher_port = MACH_PORT_NULL;
    message->msgh_id = id;
    uint32_t descriptors = complex ? ((mach_msg_base_t *)message)->body.msgh_descriptor_count : 0;
    uint64_t options = UINT64_C(0x200000000) | MACH_SEND_MSG | MACH_RCV_MSG;
    uint64_t bits_size = v_macos_pair(message->msgh_bits, size);
    uint64_t ports = v_macos_pair(destination, reply);
    uint64_t voucher_id = v_macos_pair(0, (uint32_t)id);
    uint64_t desc_reply = v_macos_pair(descriptors, reply);
    int32_t code;
    do {
        code = v_macos_kernel_msg2(message, options, bits_size, ports, voucher_id,
            desc_reply, capacity, 0);
    } while (code == MACH_SEND_INTERRUPTED);
    while (code == MACH_RCV_INTERRUPTED) {
        code = v_macos_kernel_msg2(message, options & ~(uint64_t)MACH_SEND_MSG,
            bits_size & UINT64_C(0xffffffff), ports, voucher_id, desc_reply,
            capacity, 0);
    }
    (void)v_macos_kernel_mach_trap6(19, task, reply, MACH_PORT_RIGHT_RECEIVE,
        (uint64_t)(int64_t)-1, 0, 0);
    if (code != MACH_MSG_SUCCESS) return code;
    if (message->msgh_id != id + 100) return MIG_REPLY_MISMATCH;
    return KERN_SUCCESS;
}

static int32_t v_macos_simple_reply(mach_msg_header_t *message, size_t expected) {
    if (message->msgh_bits & MACH_MSGH_BITS_COMPLEX) return MIG_TYPE_ERROR;
    if (message->msgh_size < sizeof(mig_reply_error_t)) return MIG_TYPE_ERROR;
    int32_t code = ((mig_reply_error_t *)message)->RetCode;
    if (code != KERN_SUCCESS) return code;
    return message->msgh_size == expected ? KERN_SUCCESS : MIG_TYPE_ERROR;
}

int32_t v_macos_kernel_port_type(uint32_t port, uint32_t *type) {
    union {
        __Request__mach_port_type_t request;
        __Reply__mach_port_type_t reply;
        unsigned char trailer[sizeof(__Reply__mach_port_type_t) + MAX_TRAILER_SIZE];
    } message;
    v_macos_clear(&message.request, sizeof(message.request));
    *type = 0;
    message.request.Head.msgh_size = sizeof(message.request);
    message.request.NDR = v_macos_ndr;
    message.request.name = port;
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, task, 3201, 0);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    code = v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
    if (code == KERN_SUCCESS) *type = message.reply.ptype;
    return code;
}

int32_t v_macos_kernel_thread_create_running(int32_t flavor, const uint32_t *state,
    uint32_t count, uint32_t *thread) {
    union {
        __Request__thread_create_running_t request;
        __Reply__thread_create_running_t reply;
        mig_reply_error_t error;
        unsigned char trailer[sizeof(__Request__thread_create_running_t) + MAX_TRAILER_SIZE];
    } message;
    *thread = MACH_PORT_NULL;
    if (!state || count > sizeof(message.request.new_state) / sizeof(natural_t))
        return KERN_INVALID_ARGUMENT;
    v_macos_clear(&message.request, offsetof(__Request__thread_create_running_t, new_state));
    message.request.Head.msgh_size = (mach_msg_size_t)(offsetof(__Request__thread_create_running_t,
        new_state) + count * sizeof(natural_t));
    message.request.NDR = v_macos_ndr;
    message.request.flavor = flavor;
    message.request.new_stateCnt = count;
    for (uint32_t i = 0; i < count; ++i) message.request.new_state[i] = state[i];
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, task, 3412, 0);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    if (!(message.reply.Head.msgh_bits & MACH_MSGH_BITS_COMPLEX))
        return v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
    if (message.reply.Head.msgh_size != sizeof(message.reply)
        || message.reply.msgh_body.msgh_descriptor_count != 1
        || message.reply.child_act.type != MACH_MSG_PORT_DESCRIPTOR
        || message.reply.child_act.disposition != MACH_MSG_TYPE_MOVE_SEND
        || !MACH_PORT_VALID(message.reply.child_act.name)) return MIG_TYPE_ERROR;
    *thread = message.reply.child_act.name;
    return KERN_SUCCESS;
}

int32_t v_macos_kernel_thread_terminate(uint32_t thread) {
    union {
        __Request__thread_terminate_t request;
        __Reply__thread_terminate_t reply;
        unsigned char trailer[sizeof(__Reply__thread_terminate_t) + MAX_TRAILER_SIZE];
    } message;
    v_macos_clear(&message.request, sizeof(message.request));
    message.request.Head.msgh_size = sizeof(message.request);
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    uint32_t self = v_macos_kernel_thread_self();
    if (thread == self && self) {
        v_macos_port_release(task, self);
        v_macos_port_release(task, task);
        return KERN_INVALID_ARGUMENT;
    }
    if (self) v_macos_port_release(task, self);
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, thread, 3600, 0);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    return v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
}

int32_t v_macos_kernel_semaphore_create(int32_t policy, int32_t value, uint32_t *semaphore) {
    union {
        __Request__semaphore_create_t request;
        __Reply__semaphore_create_t reply;
        mig_reply_error_t error;
        unsigned char trailer[sizeof(__Reply__semaphore_create_t) + MAX_TRAILER_SIZE];
    } message;
    v_macos_clear(&message.request, sizeof(message.request));
    *semaphore = MACH_PORT_NULL;
    message.request.Head.msgh_size = sizeof(message.request);
    message.request.NDR = v_macos_ndr;
    message.request.policy = policy;
    message.request.value = value;
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, task, 3418, 0);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    if (!(message.reply.Head.msgh_bits & MACH_MSGH_BITS_COMPLEX))
        return v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
    if (message.reply.Head.msgh_size != sizeof(message.reply)
        || message.reply.msgh_body.msgh_descriptor_count != 1
        || message.reply.semaphore.type != MACH_MSG_PORT_DESCRIPTOR
        || message.reply.semaphore.disposition != MACH_MSG_TYPE_MOVE_SEND
        || !MACH_PORT_VALID(message.reply.semaphore.name)) return MIG_TYPE_ERROR;
    *semaphore = message.reply.semaphore.name;
    return KERN_SUCCESS;
}

int32_t v_macos_kernel_semaphore_destroy(uint32_t semaphore) {
    union {
        __Request__semaphore_destroy_t request;
        __Reply__semaphore_destroy_t reply;
        unsigned char trailer[sizeof(__Request__semaphore_destroy_t) + MAX_TRAILER_SIZE];
    } message;
    v_macos_clear(&message.request, sizeof(message.request));
    message.request.Head.msgh_size = sizeof(message.request);
    message.request.msgh_body.msgh_descriptor_count = 1;
    message.request.semaphore.name = semaphore;
    message.request.semaphore.disposition = MACH_MSG_TYPE_COPY_SEND;
    message.request.semaphore.type = MACH_MSG_PORT_DESCRIPTOR;
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, task, 3419, 1);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    return v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
}

int32_t v_macos_kernel_semaphore_wait(uint32_t semaphore) {
    return (int32_t)v_macos_kernel_mach_trap6(36, semaphore, 0, 0, 0, 0, 0);
}
int32_t v_macos_kernel_semaphore_signal(uint32_t semaphore) {
    return (int32_t)v_macos_kernel_mach_trap6(33, semaphore, 0, 0, 0, 0, 0);
}

int32_t v_macos_kernel_vm_allocate(uint64_t *address, uint64_t size, int32_t flags) {
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = (int32_t)v_macos_kernel_mach_trap6(10, task, (uintptr_t)address,
        size, (uint32_t)flags, 0, 0);
    v_macos_port_release(task, task);
    return code;
}
int32_t v_macos_kernel_vm_deallocate(uint64_t address, uint64_t size) {
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    int32_t code = (int32_t)v_macos_kernel_mach_trap6(12, task, address, size, 0, 0, 0);
    v_macos_port_release(task, task);
    return code;
}
int32_t v_macos_kernel_vm_remap(uint64_t source, uint64_t size, uint64_t *target,
    int32_t flags, int32_t copy, int32_t *cur_protection, int32_t *max_protection) {
    union {
        __Request__mach_vm_remap_t request;
        __Reply__mach_vm_remap_t reply;
        unsigned char trailer[sizeof(__Request__mach_vm_remap_t) + MAX_TRAILER_SIZE];
    } message;
    v_macos_clear(&message.request, sizeof(message.request));
    *cur_protection = 0;
    *max_protection = 0;
    uint32_t task = v_macos_kernel_task_self();
    if (!task) return KERN_RESOURCE_SHORTAGE;
    message.request.Head.msgh_size = sizeof(message.request);
    message.request.msgh_body.msgh_descriptor_count = 1;
    message.request.src_task.name = task;
    message.request.src_task.disposition = MACH_MSG_TYPE_COPY_SEND;
    message.request.src_task.type = MACH_MSG_PORT_DESCRIPTOR;
    message.request.NDR = v_macos_ndr;
    message.request.target_address = *target;
    message.request.size = size;
    message.request.mask = 0;
    message.request.flags = flags;
    message.request.src_address = source;
    message.request.copy = copy;
    message.request.inheritance = VM_INHERIT_SHARE;
    int32_t code = v_macos_rpc(&message.request.Head, sizeof(message), task, task, 4813, 1);
    v_macos_port_release(task, task);
    if (code != KERN_SUCCESS) return code;
    code = v_macos_simple_reply(&message.reply.Head, sizeof(message.reply));
    if (code == KERN_SUCCESS) {
        *target = message.reply.target_address;
        *cur_protection = message.reply.cur_protection;
        *max_protection = message.reply.max_protection;
    }
    return code;
}

int32_t v_macos_kernel_bsd_call6(uint32_t number, uint64_t a1, uint64_t a2,
    uint64_t a3, uint64_t a4, uint64_t a5, uint64_t a6, uint64_t *value) {
#if defined(__x86_64__)
    register uint64_t r10 __asm__("r10") = a4;
    register uint64_t r8 __asm__("r8") = a5;
    register uint64_t r9 __asm__("r9") = a6;
    uint64_t result = UINT64_C(0x2000000) | number;
    unsigned char failed;
    __asm__ volatile("syscall\nsetc %1" : "+a"(result), "=qm"(failed),
        "+D"(a1), "+S"(a2), "+d"(a3), "+r"(r10), "+r"(r8), "+r"(r9)
        : : "rcx", "r11", "cc", "memory");
    if (value) *value = failed ? 0 : result;
    return failed ? (int32_t)result : 0;
#else
    register uint64_t x0 __asm__("x0") = a1;
    register uint64_t x1 __asm__("x1") = a2;
    register uint64_t x2 __asm__("x2") = a3;
    register uint64_t x3 __asm__("x3") = a4;
    register uint64_t x4 __asm__("x4") = a5;
    register uint64_t x5 __asm__("x5") = a6;
    register uint64_t x16 __asm__("x16") = number;
    uint32_t failed;
    __asm__ volatile("svc #0x80\ncset %w[failed], cs" : "+r"(x0), "+r"(x1),
        "+r"(x2), "+r"(x3), "+r"(x4), "+r"(x5), "+r"(x16), [failed] "=r"(failed)
        : : "cc", "memory");
    if (value) *value = failed ? 0 : x0;
    return failed ? (int32_t)x0 : 0;
#endif
}

static int32_t v_macos_bsd6(uint32_t number, uint64_t a1, uint64_t a2,
    uint64_t a3, uint64_t a4, uint64_t a5, uint64_t a6) {
    return v_macos_kernel_bsd_call6(number, a1, a2, a3, a4, a5, a6, NULL);
}
int32_t v_macos_kernel_ulock_wait(uint32_t operation, void *address, uint64_t value,
    uint32_t timeout) {
    if (operation & UINT32_C(0x01000000)) return 22;
    return v_macos_bsd6(515, operation, (uintptr_t)address, value, timeout, 0, 0);
}
int32_t v_macos_kernel_ulock_wait2(uint32_t operation, void *address, uint64_t value,
    uint64_t timeout) {
    if (operation & UINT32_C(0x01000000)) return 22;
    return v_macos_bsd6(544, operation, (uintptr_t)address, value, timeout, 0, 0);
}
int32_t v_macos_kernel_ulock_wake(uint32_t operation, void *address, uint64_t value) {
    if (operation & UINT32_C(0x01000000)) return 22;
    return v_macos_bsd6(516, operation, (uintptr_t)address, value, 0, 0, 0);
}

int32_t v_macos_kernel_thread_exit_mapped(uint64_t address, uint64_t size,
    uint32_t thread_right, uint32_t semaphore) {
    /* XNU's BSD361 handler does not require pthread registration. It unmaps
     * this range, terminates only the caller, signals the optional semaphore,
     * and drops the optional thread send reference entirely inside the kernel. */
    return v_macos_bsd6(361, address, size, thread_right, semaphore, 0, 0);
}
int32_t v_macos_kernel_thread_exit(void) {
    return v_macos_kernel_thread_exit_mapped(0, 0, 0, 0);
}

int32_t v_macos_kernel_page_size(uint64_t *size) {
    /* sysctl({CTL_HW, HW_PAGESIZE}); the kernel writes a native C int. */
    int32_t name[2] = {6, 7}, value = 0;
    uint64_t length = sizeof(value);
    int32_t code = v_macos_bsd6(202, (uintptr_t)name, 2, (uintptr_t)&value,
        (uintptr_t)&length, 0, 0);
    *size = 0;
    if (code != 0) return code;
    if (length != sizeof(value) || value <= 0 || (value & (value - 1))) return 5;
    *size = (uint32_t)value;
    return 0;
}
