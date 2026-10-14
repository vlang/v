#define _GNU_SOURCE
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

int v3_linux_parent_death_signal(int signal_number) {
	return prctl(PR_SET_PDEATHSIG, (unsigned long)signal_number, 0UL, 0UL, 0UL);
}

int v3_linux_memory_file(const char *name) {
	return (int)syscall(SYS_memfd_create, name, 0U);
}

int v3_linux_name_worker(void) {
	return prctl(PR_SET_NAME, "v3-pool", 0UL, 0UL, 0UL);
}
