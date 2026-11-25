#include "libc_compat.h"
#include "libc_ipc_abi.h"
#include <stdint.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/stat.h>

extern int v_ipc_semget(int, int, int);
extern int v_ipc_semctl(int, int, int, uintptr_t);
extern int v_ipc_semop(int, uintptr_t, size_t);
extern int v_ipc_semtimedop(int, uintptr_t, size_t, uintptr_t);
extern int v_ipc_shmget(int, size_t, int);
extern uintptr_t v_ipc_shmat(int, uintptr_t, int);
extern int v_ipc_shmdt(uintptr_t);
extern int v_ipc_shmctl(int, int, uintptr_t);
extern int v_ipc_msgget(int, int);
extern int v_ipc_msgsnd(int, uintptr_t, size_t, int);
extern ssize_t v_ipc_msgrcv(int, uintptr_t, size_t, int64_t, int);
extern int v_ipc_msgctl(int, int, uintptr_t);
extern int v_ipc_ftok(uintptr_t, int);

_Static_assert(sizeof(key_t) == 4 && sizeof(long) == 8, "native Linux IPC scalar ABI");
int semget(key_t key, int count, int flags) { return v_ipc_semget(key, count, flags); }
/* semctl's optional union semun is one native register. A jump preserves its
 * actual representation (pointer or int) without reading a mismatched C vararg. */
#if defined(__x86_64__)
__asm__(".pushsection .text\n.global semctl\n.type semctl,@function\nsemctl:\njmp v_ipc_semctl\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text\n.global semctl\n.type semctl,%function\nsemctl:\nb v_ipc_semctl\n.popsection\n");
#endif
int semop(int id, struct sembuf *operations, size_t count) {
    return v_ipc_semop(id, (uintptr_t)operations, count);
}
int semtimedop(int id, struct sembuf *operations, size_t count, const struct timespec *timeout) {
    return v_ipc_semtimedop(id, (uintptr_t)operations, count, (uintptr_t)timeout);
}
int shmget(key_t key, size_t size, int flags) { return v_ipc_shmget(key, size, flags); }
void *shmat(int id, const void *address, int flags) {
    return (void *)v_ipc_shmat(id, (uintptr_t)address, flags);
}
int shmdt(const void *address) { return v_ipc_shmdt((uintptr_t)address); }
int shmctl(int id, int command, struct shmid_ds *data) { return v_ipc_shmctl(id, command, (uintptr_t)data); }
int msgget(key_t key, int flags) { return v_ipc_msgget(key, flags); }
int msgsnd(int id, const void *message, size_t size, int flags) {
    return v_ipc_msgsnd(id, (uintptr_t)message, size, flags);
}
ssize_t msgrcv(int id, void *message, size_t size, long kind, int flags) {
    return v_ipc_msgrcv(id, (uintptr_t)message, size, kind, flags);
}
int msgctl(int id, int command, struct msqid_ds *data) { return v_ipc_msgctl(id, command, (uintptr_t)data); }
key_t ftok(const char *path, int project) { return v_ipc_ftok((uintptr_t)path, project); }
int v_ipc_file_ids(uintptr_t path, uint64_t *device, uint64_t *inode) {
    struct stat record;
    int result = stat((const char *)path, &record);
    if (!result) { *device = record.st_dev; *inode = record.st_ino; }
    return result;
}
