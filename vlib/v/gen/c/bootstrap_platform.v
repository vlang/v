module c

// headerless_compiler_platform_helpers supplies SDK-independent native facades
// for a portable compiler snapshot. Each facade follows the consumer's platform.
fn (mut g FlatGen) headerless_compiler_platform_helpers() {
	g.emit_c_runtime_declarations(['stat'])
	g.writeln('#if defined(__FreeBSD__) || defined(__OpenBSD__)')
	g.emit_c_runtime_declarations(['sysctl'])
	g.writeln('#endif')
	g.write(headerless_compiler_darwin_memory)
	g.write(headerless_compiler_linux_syscalls)
	g.write(headerless_compiler_file_metadata)
	g.collect_preserved_c_fns(['mach_host_self', 'mach_task_self', 'mach_port_deallocate',
		'host_page_size', 'host_statistics64', 'task_info', 'syscall', 'v_runtime_free_memory_darwin',
		'v3_linux_parent_death_signal', 'v3_linux_memory_file', 'v3_linux_name_worker',
		'v3_modulecache_file_metadata'])
}

// Mach statistics append fields in new revisions; the count limits the kernel's
// writes to the complete revision 2 structure declared here.
const headerless_compiler_darwin_memory = r'
#ifdef __APPLE__
typedef unsigned int host_t;
typedef unsigned long vm_size_t;
typedef struct {
	u32 free_count, active_count, inactive_count, wire_count;
	u64 zero_fill_count, reactivations, pageins, pageouts, faults, cow_faults;
	u64 lookups, hits, purges;
	u32 purgeable_count, speculative_count;
	u64 decompressions, compressions, swapins, swapouts;
	u32 compressor_page_count, throttled_count, external_page_count, internal_page_count;
	u64 total_uncompressed_pages_in_compressor, swapped_count;
} vm_statistics64_data_t;

host_t mach_host_self(void);
task_t mach_task_self(void);
int mach_port_deallocate(task_t, host_t);
int host_page_size(host_t, vm_size_t*);
int host_statistics64(host_t, int, i32*, u32*);
int task_info(task_t, int, void*, u32*);

static int v_runtime_free_memory_darwin(u64* bytes, i32* code) {
	vm_statistics64_data_t statistics = {0};
	u32 count = (u32)(sizeof(statistics) / sizeof(i32));
	vm_size_t page_size = 0;
	host_t host = mach_host_self();
	int stage = 0;
	*code = host_statistics64(host, 4, (i32*)&statistics, &count);
	if (*code != 0) {
		stage = 1;
	} else {
		*code = host_page_size(host, &page_size);
		if (*code != 0) stage = 2;
	}
	mach_port_deallocate(mach_task_self(), host);
	if (stage == 0) *bytes = (u64)statistics.free_count * page_size;
	return stage;
}
#endif
'

const headerless_compiler_linux_syscalls = r'
#if defined(__linux__) && !defined(__ANDROID__)
int prctl(int, ...);
long syscall(long, ...);

static int v3_linux_parent_death_signal(int signal_number) {
	return prctl(1, (unsigned long)signal_number, 0UL, 0UL, 0UL);
}

static int v3_linux_memory_file(const char* name) {
#if defined(__x86_64__)
	return (int)syscall(319, name, 0U);
#elif defined(__i386__)
	return (int)syscall(356, name, 0U);
#elif defined(__arm__)
	return (int)syscall(385, name, 0U);
#elif defined(__aarch64__) || (defined(__riscv) && __riscv_xlen == 64) || defined(__loongarch_lp64)
	return (int)syscall(279, name, 0U);
#else
	return (int)syscall(-1, name, 0U);
#endif
}

static int v3_linux_name_worker(void) {
	return prctl(15, "v3-pool", 0UL, 0UL, 0UL);
}
#endif
'

// BSD and Windows retain the existing content-signature fallback. On Darwin
// and Linux the standalone stat layout exposes the same timestamps as the SDK.
const headerless_compiler_file_metadata = r'
static int v3_modulecache_file_metadata(const char* path, u64* device, u64* inode,
	u64* size, u64* mtime_seconds, u64* mtime_nanoseconds,
	u64* ctime_seconds, u64* ctime_nanoseconds) {
#if defined(__APPLE__) || defined(__linux__)
	struct stat info;
	if (stat(path, &info) != 0) return 0;
	*device = (u64)info.st_dev;
	*inode = (u64)info.st_ino;
	*size = (u64)info.st_size;
	*mtime_seconds = (u64)info.st_mtime;
	*mtime_nanoseconds = (u64)info.st_mtimensec;
	*ctime_seconds = (u64)info.st_ctime;
	*ctime_nanoseconds = (u64)info.st_ctimensec;
	return 1;
#else
	(void)path;
	(void)device;
	(void)inode;
	(void)size;
	(void)mtime_seconds;
	(void)mtime_nanoseconds;
	(void)ctime_seconds;
	(void)ctime_nanoseconds;
	return 0;
#endif
}
'
