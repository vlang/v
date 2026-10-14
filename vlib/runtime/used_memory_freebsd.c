#include <sys/types.h>
#include <sys/user.h>
#include <libprocstat.h>
#include <stdint.h>
#include <unistd.h>

/* Keep SDK-owned kinfo_proc storage inside the cached native object. */
uint64_t v_runtime_freebsd_resident_pages(void) {
	struct procstat *status = procstat_open_sysctl();
	if (!status) return 0;
	unsigned int count = 0;
	struct kinfo_proc *processes = procstat_getprocs(status,
		KERN_PROC_PID | KERN_PROC_INC_THREAD, getpid(), &count);
	uint64_t pages = processes ? (uint64_t)processes[0].ki_rssize : 0;
	if (processes) procstat_freeprocs(status, processes);
	procstat_close(status);
	return pages;
}
