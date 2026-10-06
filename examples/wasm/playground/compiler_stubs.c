#include <errno.h>
#include <semaphore.h>
#include <spawn.h>
#include <time.h>

/* These host-only operations are linked by the compiler driver but are not used
 * when compiling wasm serially with the memory monitor disabled. */
int posix_spawnp(pid_t *pid, const char *file,
                const posix_spawn_file_actions_t *actions,
                const posix_spawnattr_t *attributes,
                char *const argv[], char *const envp[]) {
    return ENOSYS;
}

int sem_timedwait(sem_t *semaphore, const struct timespec *deadline) {
    errno = ENOSYS;
    return -1;
}
