#ifndef V_RUNTIME_IPC_PROBE_H
#define V_RUNTIME_IPC_PROBE_H
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

union v_probe_semun { int val; struct semid_ds *buf; unsigned short *array; struct seminfo *__buf; };
#define IPC_CHECK(test) do { if (!(test)) { result = __LINE__; goto cleanup; } } while (0)
static int ipc_native_probe(void) {
    int result = 0, semid = -1, shmid = -1, msgid = -1, fd = -1;
    pid_t child = -1;
    unsigned char *memory = (void *)-1;
    char path[] = "/tmp/vlibc-ipc-XXXXXX";
    fd = mkstemp(path);
    IPC_CHECK(fd >= 0);
    struct stat file;
    IPC_CHECK(!fstat(fd, &file));
    key_t expected = (key_t)((0x5au << 24) | ((file.st_dev & 255) << 16) | (file.st_ino & 65535));
    IPC_CHECK(ftok(path, 0x15a) == expected && ftok(path, 0x5a) == expected);
    errno = 0;
    IPC_CHECK(ftok("/vlibc-ipc-no-such-file", 1) == (key_t)-1 && errno == ENOENT);

    semid = semget(IPC_PRIVATE, 2, IPC_CREAT | 0600);
    IPC_CHECK(semid >= 0);
    struct semid_ds seminfo;
    union v_probe_semun argument = {.buf = &seminfo};
    IPC_CHECK(!semctl(semid, 0, IPC_STAT, argument) && seminfo.sem_nsems == 2);
    argument.val = 2;
    IPC_CHECK(!semctl(semid, 0, SETVAL, argument));
    IPC_CHECK(semctl(semid, 0, GETVAL) == 2);
    unsigned short values[] = {2, 3}, copied[] = {0, 0};
    argument.array = values;
    IPC_CHECK(!semctl(semid, 0, SETALL, argument));
    argument.array = copied;
    IPC_CHECK(!semctl(semid, 0, GETALL, argument) && copied[0] == 2 && copied[1] == 3);
    struct sembuf operations[] = {{0, -2, IPC_NOWAIT}, {1, -3, IPC_NOWAIT}};
    IPC_CHECK(!semop(semid, operations, 2));
    errno = 0;
    IPC_CHECK(semop(semid, operations, 2) == -1 && errno == EAGAIN);
    operations[0].sem_flg = 0; operations[0].sem_op = -1;
    struct timespec timeout = {0, 0};
    errno = 0;
    IPC_CHECK(semtimedop(semid, operations, 1, &timeout) == -1 && errno == EAGAIN);
    timeout.tv_nsec = 1000000000;
    errno = 0;
    IPC_CHECK(semtimedop(semid, operations, 1, &timeout) == -1 && errno == EINVAL);
    argument.val = 1;
    IPC_CHECK(!semctl(semid, 0, SETVAL, argument));
    child = fork();
    IPC_CHECK(child >= 0);
    if (!child) {
        struct sembuf take = {0, -1, SEM_UNDO | IPC_NOWAIT};
        _exit(semop(semid, &take, 1) || semctl(semid, 0, GETVAL) != 0 ? 91 : 0);
    }
    int status = 0;
    IPC_CHECK(waitpid(child, &status, 0) == child);
    child = -1;
    IPC_CHECK(WIFEXITED(status) && !WEXITSTATUS(status) && semctl(semid, 0, GETVAL) == 1);
    IPC_CHECK(!semctl(semid, 0, IPC_RMID)); semid = -1;

    shmid = shmget(IPC_PRIVATE, 4096, IPC_CREAT | 0600);
    IPC_CHECK(shmid >= 0);
    struct shmid_ds shminfo;
    IPC_CHECK(!shmctl(shmid, IPC_STAT, &shminfo) && shminfo.shm_segsz == 4096);
    memory = shmat(shmid, NULL, 0);
    IPC_CHECK(memory != (void *)-1);
    memory[0] = 37;
    child = fork();
    IPC_CHECK(child >= 0);
    if (!child) {
        if (shmdt(memory)) _exit(92);
        unsigned char *attached = shmat(shmid, NULL, 0);
        if (attached == (void *)-1 || attached[0] != 37) _exit(93);
        attached[0] = 81;
        _exit(shmdt(attached) ? 94 : 0);
    }
    status = 0;
    IPC_CHECK(waitpid(child, &status, 0) == child);
    child = -1;
    IPC_CHECK(WIFEXITED(status) && !WEXITSTATUS(status) && memory[0] == 81);
    IPC_CHECK(!shmctl(shmid, IPC_RMID, NULL)); shmid = -1;
    IPC_CHECK(memory[0] == 81 && !shmdt(memory)); memory = (void *)-1;
    errno = 0;
    IPC_CHECK(shmat(-1, NULL, 0) == (void *)-1 && errno == EINVAL);

    msgid = msgget(IPC_PRIVATE, IPC_CREAT | 0600);
    IPC_CHECK(msgid >= 0);
    struct { long kind; char text[16]; } message = {7, "native queue"}, received = {0};
    IPC_CHECK(!msgsnd(msgid, &message, 12, IPC_NOWAIT));
    struct msqid_ds msginfo;
    IPC_CHECK(!msgctl(msgid, IPC_STAT, &msginfo) && msginfo.msg_qnum == 1);
    errno = 0;
    IPC_CHECK(msgrcv(msgid, &received, sizeof(received.text), 8, IPC_NOWAIT) == -1 && errno == ENOMSG);
    errno = 0;
    IPC_CHECK(msgrcv(msgid, &received, 4, 7, IPC_NOWAIT) == -1 && errno == E2BIG);
    IPC_CHECK(msgrcv(msgid, &received, 4, 7, IPC_NOWAIT | MSG_NOERROR) == 4);
    IPC_CHECK(received.kind == 7 && !memcmp(received.text, "nati", 4));
    IPC_CHECK(!msgctl(msgid, IPC_STAT, &msginfo) && msginfo.msg_qnum == 0);
    IPC_CHECK(!msgctl(msgid, IPC_RMID, NULL)); msgid = -1;
cleanup:
    if (child > 0) { kill(child, SIGKILL); while (waitpid(child, NULL, 0) < 0 && errno == EINTR) {} }
    if (memory != (void *)-1) shmdt(memory);
    if (semid >= 0) semctl(semid, 0, IPC_RMID);
    if (shmid >= 0) shmctl(shmid, IPC_RMID, NULL);
    if (msgid >= 0) msgctl(msgid, IPC_RMID, NULL);
    if (fd >= 0) close(fd);
    unlink(path);
    if (!result) puts("SysV semaphores, shared memory, queues, process undo and IPC metadata passed");
    return result;
}
#undef IPC_CHECK
#endif
