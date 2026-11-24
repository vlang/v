/* Linux ABI oracle for the actual C generated from the V startup helpers.
 * The production runtime never includes this file. Darwin syscall transport is
 * replaced here; this does not claim to execute a Mach-O or Darwin's kernel. */
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <pthread.h>
#include <sched.h>

typedef int32_t i32;
typedef int64_t i64;
typedef uint32_t u32;
typedef uint64_t u64;
typedef uint8_t u8;
typedef struct { u8 *str; i64 len; } string;
#define builtin__max_int INT64_MAX
#define VNORETURN __attribute__((noreturn))
#define _S(s) ((string){(u8 *)(s), sizeof(s)-1})
#define _SL(name, text, length) static string name = {(u8 *)(text), length}
static VNORETURN void v_panic(string s) { (void)s; abort(); }

/* Independently declared native layouts/constants, with explicit size checks. */
enum {
    MH_MAGIC_64 = 0xfeedfacf, MH_EXECUTE = 2, MH_DYLDLINK = 4,
    LC_SEGMENT_64 = 0x19, SECTION_TYPE = 0xff,
    S_MOD_INIT_FUNC_POINTERS = 9, S_MOD_TERM_FUNC_POINTERS = 10,
    S_THREAD_LOCAL_REGULAR = 0x11, S_THREAD_LOCAL_ZEROFILL = 0x12,
    S_THREAD_LOCAL_VARIABLES = 0x13, S_INIT_FUNC_OFFSETS = 0x16
};
struct mach_header_64 {
    uint32_t magic; int32_t cputype, cpusubtype;
    uint32_t filetype, ncmds, sizeofcmds, flags, reserved;
};
struct load_command { uint32_t cmd, cmdsize; };
struct segment_command_64 {
    uint32_t cmd, cmdsize; char segname[16];
    uint64_t vmaddr, vmsize, fileoff, filesize;
    int32_t maxprot, initprot; uint32_t nsects, flags;
};
struct section_64 {
    char sectname[16], segname[16]; uint64_t addr, size;
    uint32_t offset, align, reloff, nreloc, flags, reserved1, reserved2, reserved3;
};
_Static_assert(sizeof(struct mach_header_64) == 32, "Mach header ABI");
_Static_assert(sizeof(struct segment_command_64) == 72, "Mach segment ABI");
_Static_assert(sizeof(struct section_64) == 80, "Mach section ABI");

static struct {
    struct mach_header_64 header;
    struct segment_command_64 segment;
    struct section_64 sections[8];
    struct segment_command_64 second_segment;
    struct section_64 second_sections[2];
} oracle_image __attribute__((section(".oracle_header")));
#define _mh_execute_header (*(const struct mach_header_64 *)&oracle_image.header)

typedef struct { u64 arg0, arg1; } multi_return_u64_u64;
typedef struct { struct load_command *arg0; bool arg1; }
    multi_return_struct_load_commandptr_bool;
#include "../../prealloc_atomics.h"
static void *activated_tls;
static int exit_pipe = -1;
static multi_return_u64_u64 sys_call1(u64 n, u64 status) {
    assert(n == 0x2000001); _exit((int)status);
}
static multi_return_u64_u64 sys_call2(u64 n, u64 p, u64 zero) {
    assert(n == 0x3000003 && p && zero == 0);
    activated_tls = (void *)(uintptr_t)p;
    return (multi_return_u64_u64){0, 0};
}
static multi_return_u64_u64 sys_call3(u64 n, u64 fd, u64 bytes, u64 length) {
    assert(n == 0x2000004);
    assert(write((int)fd, (void *)(uintptr_t)bytes, length) == (ssize_t)length);
    return (multi_return_u64_u64){length, 0};
}
static multi_return_u64_u64 sys_call6(u64 n, u64 address, u64 length,
    u64 prot, u64 flags, u64 fd, u64 offset) {
    assert(n == 0x20000c5 && !address && length && prot == 3);
    assert(flags == 0x1002 && fd == UINT64_MAX && !offset);
    void *p = mmap(NULL, length, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(p != MAP_FAILED);
    return (multi_return_u64_u64){(uintptr_t)p, 0};
}

/* LP64 CRT ABI, independently declared from Apple's crt_externs.h:
 * https://github.com/apple-oss-distributions/Libc/blob/main/include/crt_externs.h */
extern i32 *_NSGetArgc(void);
extern char ***_NSGetArgv(void);
extern char ***_NSGetEnviron(void);
extern char **_NSGetProgname(void);
extern struct mach_header_64 *_NSGetMachExecuteHeader(void);
static void *__malloc(size_t size);
static void __free(void *pointer);
static _Thread_local bool owns_registry_lock;
static int observed_cas_i32(i32 *pointer, int old, int value);
static int observed_store_i32(i32 *pointer, int value);
/* Observe ownership per thread while retaining the real atomic primitives.
 * Another worker may legitimately hold the lock while this one allocates. */
#define v_prealloc_atomic_cas_i32 observed_cas_i32
#define v_prealloc_atomic_store_i32 observed_store_i32
#include "generated_startup.h"
#undef v_prealloc_atomic_cas_i32
#undef v_prealloc_atomic_store_i32
static int observed_cas_i32(i32 *pointer, int old, int value) {
    int acquired = v_prealloc_atomic_cas_i32(pointer, old, value);
    if (pointer == &g_bare_macos_exit_registry_lock && acquired) {
        assert(!owns_registry_lock); owns_registry_lock = true;
    }
    return acquired;
}
static int observed_store_i32(i32 *pointer, int value) {
    if (pointer == &g_bare_macos_exit_registry_lock) {
        assert(owns_registry_lock && value == 0); owns_registry_lock = false;
    }
    return v_prealloc_atomic_store_i32(pointer, value);
}

static int active_allocations;
static int allocation_limit = -1;
static int close_on_allocation;
static void *__malloc(size_t size) {
    assert(!owns_registry_lock);
    if (allocation_limit == 0) return NULL;
    if (allocation_limit > 0) --allocation_limit;
    void *pointer = malloc(size);
    assert(pointer);
    __atomic_add_fetch(&active_allocations, 1, __ATOMIC_SEQ_CST);
    if (close_on_allocation) {
        close_on_allocation = 0;
        bare_macos_drain_atexit(true);
    }
    return pointer;
}
static void __free(void *pointer) {
    assert(!owns_registry_lock);
    assert(pointer && __atomic_fetch_sub(&active_allocations, 1, __ATOMIC_SEQ_CST) > 0);
    free(pointer);
}

/* Independent Darwin ProgramVars layout, rather than a second use of V's
 * generated definition. These assertions detect an incorrect field order. */
struct OracleProgramVars {
    const void *mh;
    int32_t *argc;
    const char ***argv;
    const char ***environ;
    const char **progname;
};
_Static_assert(sizeof(BareMacProgramVars) == sizeof(struct OracleProgramVars), "ProgramVars size");
#define CHECK_PROGRAM_VAR(field) \
    _Static_assert(offsetof(BareMacProgramVars, field) == offsetof(struct OracleProgramVars, field), "ProgramVars " #field)
CHECK_PROGRAM_VAR(mh);
CHECK_PROGRAM_VAR(argc);
CHECK_PROGRAM_VAR(argv);
CHECK_PROGRAM_VAR(environ);
CHECK_PROGRAM_VAR(progname);
#undef CHECK_PROGRAM_VAR

static unsigned char template[1024] __attribute__((aligned(256)));
static BareMacTlvDescriptor descriptors[2];
static int sequence[8], sequence_length;
static char **expected_argv, **expected_env, **expected_apple;
static void initializer1(i32 argc, char **argv, char **env, char **apple, BareMacProgramVars *vars) {
    assert(argc == 2 && argv == expected_argv && env == expected_env && apple == expected_apple);
    assert(vars->mh == &oracle_image.header && *(i32 *)vars->argc == 2);
    assert(*(char ***)vars->argv == argv && *(char ***)vars->environ == env);
    assert(!strcmp(*(char **)vars->progname, "probe"));
    assert(activated_tls && *(uintptr_t *)activated_tls);
    sequence[sequence_length++] = 1;
}
static void initializer2(i32 a, char **b, char **c, char **d, BareMacProgramVars *e) {
    initializer1(a, b, c, d, e); sequence[sequence_length - 1] = 2;
}
static void offset_initializer1(i32 a, char **b, char **c, char **d, BareMacProgramVars *e) {
    initializer1(a, b, c, d, e); sequence[sequence_length - 1] = 5;
}
static void offset_initializer2(i32 a, char **b, char **c, char **d, BareMacProgramVars *e) {
    initializer1(a, b, c, d, e); sequence[sequence_length - 1] = 6;
}
static int register_from_finalizer;
static void exit_callback_c(void);
static void finalizer1(void *context) {
    assert(context == NULL); sequence[sequence_length++] = 3;
    if (exit_pipe >= 0) assert(write(exit_pipe, "1", 1) == 1);
    if (register_from_finalizer) assert(atexit(exit_callback_c) == 0);
}
static void finalizer2(void *context) {
    assert(context == NULL); sequence[sequence_length++] = 4;
    if (exit_pipe >= 0) assert(write(exit_pipe, "2", 1) == 1);
}
static void (*initializers[])(i32, char **, char **, char **, BareMacProgramVars *) = {initializer1, initializer2};
static void (*finalizers[])(void *) = {finalizer1, finalizer2};
static uint32_t initializer_offsets[2];

static void reset_image(void) {
    memset(&oracle_image, 0, sizeof oracle_image);
    oracle_image.header.magic = 0xfeedfacf;
    oracle_image.header.filetype = 2;
    oracle_image.header.ncmds = 1;
    oracle_image.header.sizeofcmds = sizeof oracle_image.segment + sizeof oracle_image.sections;
    oracle_image.segment.cmd = 0x19;
    oracle_image.segment.cmdsize = oracle_image.header.sizeofcmds;
    oracle_image.segment.nsects = 8;
    oracle_image.sections[0] = (struct section_64){.addr=(uintptr_t)(template + 17), .size=30, .align=8, .flags=0x11};
    oracle_image.sections[1] = (struct section_64){.addr=(uintptr_t)(template + 47), .size=300, .align=4, .flags=0x12};
    descriptors[0] = (BareMacTlvDescriptor){.offset=(uintptr_t)(template + 17)};
    descriptors[1] = (BareMacTlvDescriptor){.offset=(uintptr_t)(template + 300)};
    oracle_image.sections[2] = (struct section_64){.addr=(uintptr_t)descriptors, .size=sizeof descriptors, .flags=0x13};
    oracle_image.sections[6] = (struct section_64){.addr=(uintptr_t)initializers, .size=sizeof initializers, .flags=9};
    oracle_image.sections[4] = (struct section_64){.addr=(uintptr_t)finalizers, .size=sizeof finalizers, .flags=10};
    /* Put offsets before pointers physically, as TEXT commonly precedes DATA.
     * Darwin still dispatches all pointer constructors before any offsets.
     * The fixture-only linker places this writable mock header before text.
     * Real compiled callbacks then have native unsigned 32-bit image offsets. */
    uintptr_t header = (uintptr_t)&oracle_image.header;
    assert((uintptr_t)offset_initializer1 >= header && (uintptr_t)offset_initializer1 - header <= UINT32_MAX);
    assert((uintptr_t)offset_initializer2 >= header && (uintptr_t)offset_initializer2 - header <= UINT32_MAX);
    initializer_offsets[0] = (uint32_t)((uintptr_t)offset_initializer1 - header);
    initializer_offsets[1] = (uint32_t)((uintptr_t)offset_initializer2 - header);
    oracle_image.sections[3] = (struct section_64){.addr=(uintptr_t)initializer_offsets, .size=sizeof initializer_offsets, .flags=S_INIT_FUNC_OFFSETS};
    for (size_t i = 0; i < 30; ++i) template[17+i] = (unsigned char)(i+1);
}

static void check_invalid_images(void) {
    reset_image(); oracle_image.header.magic = 0; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.header.filetype = 6; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.header.flags = 4; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.header.ncmds = 2; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.header.sizeofcmds--; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.segment.cmdsize = 7; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.segment.cmdsize = 73; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.segment.nsects++; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.sections[0].align = 64; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.sections[0].align = 63; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.sections[0].addr = UINT64_MAX; assert(bare_macos_tls_prepare() == 22);
    reset_image(); oracle_image.sections[2].size--; assert(bare_macos_tls_prepare() == 22);
    reset_image(); descriptors[0].offset = 1; assert(bare_macos_tls_prepare() == 22);
}

static void check_tls(void) {
    reset_image();
    /* An empty section has no storage or alignment requirement. */
    oracle_image.sections[5] = (struct section_64){.addr=0, .size=0, .align=63, .flags=0x12};
    assert(bare_macos_tls_prepare() == 0);
    assert(descriptors[0].offset == 0 && descriptors[1].offset == 283);
    size_t size = bare_macos_tls_storage_size();
    assert(size == 16 + 255 + 330);
    unsigned char *storage = malloc(size * 2 + 16); assert(storage);
    assert(bare_macos_tls_init_storage(NULL, size) == 22);
    assert(bare_macos_tls_init_storage(storage, size - 1) == 22);
    assert(bare_macos_tls_init_storage(storage + 1, size) == 22);
    assert(bare_macos_tls_init_storage((void *)(UINTPTR_MAX - 7), size) == 22);
    for (size_t alignment_offset = 0; alignment_offset < 256; alignment_offset += 8) {
        unsigned char *base = storage + alignment_offset;
        memset(base, 0xcd, size);
        assert(bare_macos_tls_init_storage(base, size) == 0);
        uintptr_t data = *(uintptr_t *)base;
        assert(((uintptr_t *)base)[1] == (uintptr_t)base);
        assert(data % 256 == (uintptr_t)(template + 17) % 256);
        assert(data >= (uintptr_t)base + 16 && data + 330 <= (uintptr_t)base + size);
        assert(!memcmp((void *)data, template + 17, 330));
    }
    free(storage);
    reset_image(); oracle_image.sections[0].flags = 0; oracle_image.sections[1].flags = 0;
    oracle_image.sections[2].flags = 0;
    assert(bare_macos_tls_prepare() == 0 && bare_macos_tls_storage_size() == 31);
}

static void check_unreadable_tls_gaps(void) {
    size_t page = (size_t)sysconf(_SC_PAGESIZE);
    unsigned char *source = mmap(NULL, page * 3, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(source != MAP_FAILED);
    source[0] = 27; source[page * 2] = 81;
    assert(mprotect(source + page, page, PROT_NONE) == 0);
    reset_image();
    oracle_image.sections[0] = (struct section_64){.addr=(uintptr_t)source, .size=1, .align=4, .flags=0x11};
    oracle_image.sections[1] = (struct section_64){.addr=(uintptr_t)(source + page * 2), .size=1, .align=4, .flags=0x11};
    oracle_image.sections[2].flags = 0;
    assert(bare_macos_tls_prepare() == 0);
    size_t size = bare_macos_tls_storage_size();
    void *storage = malloc(size); assert(storage); memset(storage, 0xcd, size);
    assert(bare_macos_tls_init_storage(storage, size) == 0);
    unsigned char *data = *(unsigned char **)storage;
    assert(data[0] == 27 && data[page * 2] == 81);
    for (size_t i = 1; i < page * 2; ++i) assert(data[i] == 0);
    free(storage); assert(munmap(source, page * 3) == 0);
}

static void prepare(void) {
    reset_image(); g_bare_macos_finalizers_ready = g_bare_macos_exit_started = 0;
    sequence_length = 0;
    static uintptr_t stack[] = {2, 0, 0, 0, 0, 0, 0, 0};
    stack[1] = (uintptr_t)"/tmp/bin/probe";
    stack[2] = (uintptr_t)"argument";
    stack[4] = (uintptr_t)"KEY=value";
    stack[6] = (uintptr_t)"executable_path=/tmp/bin/probe";
    expected_argv = (char **)(stack + 1);
    expected_env = (char **)(stack + 4);
    expected_apple = (char **)(stack + 6);
    assert(bare_macos_prepare_start(stack) == expected_apple);
    assert(sequence_length == 4 && sequence[0] == 1 && sequence[1] == 2 && sequence[2] == 5 && sequence[3] == 6);
    assert(*_NSGetArgc() == 2 && *_NSGetArgv() == expected_argv);
    assert(*_NSGetEnviron() == expected_env);
    assert(!strcmp(*_NSGetProgname(), "probe"));
    assert((void *)_NSGetMachExecuteHeader() == &oracle_image.header);
    assert(g_bare_macos_finalizers_ready == 1);
    assert(munmap(activated_tls, bare_macos_tls_storage_size()) == 0);
}

static int callback_log[16], callback_count, bulk_count;
static int parallel_counts[4];
static void log_callback(int number) {
    assert(!owns_registry_lock);
    assert(callback_count < 16); callback_log[callback_count++] = number;
}
static void callback_one(void) { log_callback(1); }
static void callback_two(void) { log_callback(2); }
static void callback_three(void) { log_callback(3); }
static void callback_register(void) { log_callback(4); assert(atexit(callback_two) == 0); }
static void callback_bulk(void) { ++bulk_count; }
static void callback_parallel0(void) { ++parallel_counts[0]; }
static void callback_parallel1(void) { ++parallel_counts[1]; }
static void callback_parallel2(void) { ++parallel_counts[2]; }
static void callback_parallel3(void) { ++parallel_counts[3]; }
static void reset_registry(void) {
    allocation_limit = -1; close_on_allocation = 0;
    bare_macos_drain_atexit(false);
    assert(g_bare_macos_exit_head == NULL && active_allocations == 0);
    g_bare_macos_exit_inline_used = 0;
    g_bare_macos_exit_registry_closed = false;
    g_bare_macos_exit_started = 0;
    callback_count = bulk_count = 0;
    memset(parallel_counts, 0, sizeof parallel_counts);
}
static void *register_parallel(void *argument) {
    uintptr_t id = (uintptr_t)argument;
    void (*functions[])(void) = {callback_parallel0, callback_parallel1,
        callback_parallel2, callback_parallel3};
    for (int i = 0; i < 1000; ++i) assert(atexit(functions[id]) == 0);
    return NULL;
}
static pthread_barrier_t close_gate;
static int publishers_ready, accepted_counts[4], rejected_counts[4];
static void *register_while_draining(void *argument) {
    uintptr_t id = (uintptr_t)argument;
    void (*functions[])(void) = {callback_parallel0, callback_parallel1,
        callback_parallel2, callback_parallel3};
    for (int i = 0; i < 1000; ++i) {
        if (i == 500) {
            __atomic_add_fetch(&publishers_ready, 1, __ATOMIC_SEQ_CST);
            int result = pthread_barrier_wait(&close_gate);
            assert(result == 0 || result == PTHREAD_BARRIER_SERIAL_THREAD);
        }
        if (atexit(functions[id]) == 0) ++accepted_counts[id];
        else ++rejected_counts[id];
    }
    return NULL;
}
static void *drain_while_registering(void *unused) {
    (void)unused;
    do {
        bare_macos_drain_atexit(false);
        sched_yield();
    } while (__atomic_load_n(&publishers_ready, __ATOMIC_SEQ_CST) != 4);
    bare_macos_drain_atexit(true);
    int result = pthread_barrier_wait(&close_gate);
    assert(result == 0 || result == PTHREAD_BARRIER_SERIAL_THREAD);
    return NULL;
}
static void check_publish_drain_overlap(void) {
    reset_registry();
    publishers_ready = 0;
    memset(accepted_counts, 0, sizeof accepted_counts);
    memset(rejected_counts, 0, sizeof rejected_counts);
    assert(pthread_barrier_init(&close_gate, NULL, 5) == 0);
    pthread_t workers[4], drainer;
    assert(pthread_create(&drainer, NULL, drain_while_registering, NULL) == 0);
    for (uintptr_t id = 0; id < 4; ++id) assert(pthread_create(workers + id, NULL, register_while_draining, (void *)id) == 0);
    for (int id = 0; id < 4; ++id) assert(pthread_join(workers[id], NULL) == 0);
    assert(pthread_join(drainer, NULL) == 0 && pthread_barrier_destroy(&close_gate) == 0);
    for (int id = 0; id < 4; ++id) {
        assert(accepted_counts[id] == 500 && rejected_counts[id] == 500);
        assert(parallel_counts[id] == accepted_counts[id]);
    }
    assert(g_bare_macos_exit_head == NULL && active_allocations == 0);
    reset_registry();
}
static void check_atexit_registry(void) {
    reset_registry();
    assert(bare_macos_atexit(NULL) != 0);
    int (*volatile register_callback)(void (*)(void)) = atexit;
    assert(register_callback(callback_one) == 0);
    assert(atexit(callback_two) == 0 && atexit(callback_three) == 0 && atexit(callback_one) == 0);
    bare_macos_drain_atexit(false);
    assert(callback_count == 4 && callback_log[0] == 1 && callback_log[1] == 3
        && callback_log[2] == 2 && callback_log[3] == 1);

    reset_registry();
    assert(atexit(callback_one) == 0 && atexit(callback_register) == 0);
    bare_macos_drain_atexit(true);
    assert(callback_count == 3 && callback_log[0] == 4 && callback_log[1] == 2 && callback_log[2] == 1);
    assert(atexit(callback_one) != 0 && g_bare_macos_exit_head == NULL);

    reset_registry();
    allocation_limit = 0;
    for (int i = 0; i < 32; ++i) assert(atexit(callback_bulk) == 0);
    assert(active_allocations == 0);
    assert(atexit(callback_bulk) != 0);
    bare_macos_drain_atexit(false);
    assert(bulk_count == 32 && active_allocations == 0);

    reset_registry();
    for (int i = 0; i < 200; ++i) assert(atexit(callback_bulk) == 0);
    assert(active_allocations > 0);
    bare_macos_drain_atexit(true);
    assert(bulk_count == 200 && active_allocations == 0);

    reset_registry();
    for (int i = 0; i < 32; ++i) assert(atexit(callback_bulk) == 0);
    close_on_allocation = 1;
    assert(atexit(callback_one) != 0);
    assert(g_bare_macos_exit_registry_closed && active_allocations == 0 && bulk_count == 32);
    assert(callback_count == 0);

    reset_registry();
    pthread_t workers[4];
    for (uintptr_t id = 0; id < 4; ++id) assert(pthread_create(workers + id, NULL, register_parallel, (void *)id) == 0);
    for (int id = 0; id < 4; ++id) assert(pthread_join(workers[id], NULL) == 0);
    bare_macos_drain_atexit(true);
    for (int id = 0; id < 4; ++id) assert(parallel_counts[id] == 1000);
    assert(active_allocations == 0);
    reset_registry();
}
static void exit_callback_a(void) { assert(write(exit_pipe, "A", 1) == 1); }
static void exit_callback_b(void) { assert(write(exit_pipe, "B", 1) == 1); }
static void exit_callback_c(void) { assert(write(exit_pipe, "C", 1) == 1); }
static void exit_callback_reenter(void) {
    assert(write(exit_pipe, "R", 1) == 1); bare_macos_normal_exit(42);
}
static void exit_callback_emergency(void) {
    assert(write(exit_pipe, "E", 1) == 1); bare_macos_exit_immediate(43);
}

static void check_exit(int mode, const char *expected) {
    int pipefd[2]; assert(pipe(pipefd) == 0);
    pid_t child = fork(); assert(child >= 0);
    if (!child) {
        close(pipefd[0]); exit_pipe = pipefd[1];
        assert(atexit(exit_callback_a) == 0 && atexit(exit_callback_b) == 0);
        if (mode == 5) register_from_finalizer = 1;
        if (mode == 6) assert(atexit(exit_callback_reenter) == 0);
        if (mode == 7) assert(atexit(exit_callback_emergency) == 0);
        if (mode == 1) bare_macos_exit_immediate(41);
        if (mode == 2) bare_macos_posix_exit_immediate(41);
        if (mode == 3) g_bare_macos_finalizers_ready = 0;
        if (mode == 4) g_bare_macos_exit_started = 1;
        bare_macos_normal_exit(41);
    }
    close(pipefd[1]);
    char output[8] = {0}; size_t used = 0; ssize_t n;
    while ((n = read(pipefd[0], output + used, sizeof output - 1 - used)) > 0) used += (size_t)n;
    close(pipefd[0]); int status;
    assert(waitpid(child, &status, 0) == child && WIFEXITED(status) && WEXITSTATUS(status) == (mode == 6 ? 42 : mode == 7 ? 43 : 41));
    assert(!strcmp(output, expected));
}

static void check_startup_failure(void) {
    int descriptors[2]; assert(pipe(descriptors) == 0);
    pid_t child = fork(); assert(child >= 0);
    if (!child) {
        close(descriptors[0]); assert(dup2(descriptors[1], STDERR_FILENO) == STDERR_FILENO);
        close(descriptors[1]);
        oracle_image.header.magic = 0;
        uintptr_t stack[] = {0};
        bare_macos_prepare_start(stack);
        _exit(99);
    }
    close(descriptors[1]);
    char message[128] = {0}; size_t used = 0; ssize_t n;
    while ((n = read(descriptors[0], message + used, sizeof message - 1 - used)) > 0) used += (size_t)n;
    close(descriptors[0]);
    int status; assert(waitpid(child, &status, 0) == child);
    assert(WIFEXITED(status) && WEXITSTATUS(status) == 127);
    assert(!strcmp(message, "V: unable to initialize standalone macOS runtime\n"));
}

int main(void) {
    uintptr_t unused[4];
    assert(bare_macos_tls_storage_size() == 0);
    assert(bare_macos_tls_init_storage(unused, sizeof unused) == 22);
    assert(bare_macos_tls_activate(NULL) == 22);
    check_invalid_images(); check_startup_failure(); check_tls(); check_unreadable_tls_gaps(); prepare();
    bare_macos_run_finalizers();
    assert(sequence_length == 6 && sequence[4] == 4 && sequence[5] == 3);
    /* Reverse order must also hold across distinct segments/sections. */
    sequence_length = 0;
    oracle_image.header.ncmds = 2;
    oracle_image.header.sizeofcmds += sizeof oracle_image.second_segment + sizeof oracle_image.second_sections;
    oracle_image.second_segment.cmd = 0x19;
    oracle_image.second_segment.cmdsize = sizeof oracle_image.second_segment + sizeof oracle_image.second_sections;
    oracle_image.second_segment.nsects = 2;
    oracle_image.sections[4].size = sizeof finalizers[0];
    oracle_image.second_sections[1] = (struct section_64){.addr=(uintptr_t)(finalizers + 1), .size=sizeof finalizers[0], .flags=10};
    bare_macos_run_finalizers();
    assert(sequence_length == 2 && sequence[0] == 4 && sequence[1] == 3);
    check_atexit_registry(); check_publish_drain_overlap();
    check_exit(0, "BA21"); check_exit(1, ""); check_exit(2, "");
    check_exit(3, "BA"); check_exit(4, ""); check_exit(5, "BA21C");
    check_exit(6, "R"); check_exit(7, "E");
    return 0;
}
