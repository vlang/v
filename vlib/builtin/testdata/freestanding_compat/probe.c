/* Exercise the symbol names selected by large-file system headers in a native object. */
#define _FILE_OFFSET_BITS 64
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "probe.h"
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#ifndef PATH_MAX
#include <linux/limits.h>
#endif
#include <netdb.h>
#include <net/if.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/auxv.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/sendfile.h>
#include <sys/resource.h>
#include <sys/statvfs.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)

static pthread_mutex_t counter_lock = PTHREAD_MUTEX_INITIALIZER;
static int counter;
void v_freestanding_compat_increment(void) {
    for (int i = 0; i < 10000; ++i) {
        pthread_mutex_lock(&counter_lock);
        ++counter;
        pthread_mutex_unlock(&counter_lock);
    }
}
int v_freestanding_compat_total(void) { return counter; }

static volatile sig_atomic_t delivered;
static void receive_signal(int number) { delivered = number; }

static int check_realpath_limit(const char *directory) {
    char link[PATH_MAX], path[PATH_MAX], expected[PATH_MAX], resolved[PATH_MAX];
    CHECK(realpath(directory, expected));
    int n = snprintf(link, sizeof link, "%s/hop", directory);
    CHECK(n > 0 && (size_t)n < sizeof link && symlink(".", link) == 0);
    n = snprintf(path, sizeof path, "%s", expected);
    CHECK(n > 0 && (size_t)n + 41 * (sizeof "/hop" - 1) < sizeof path);
    for (int depth = 1; depth <= 41; ++depth) {
        memcpy(path + n, "/hop", sizeof "/hop");
        n += sizeof "/hop" - 1;
        if (depth == 39 || depth == 40)
            CHECK(realpath(path, resolved) && !strcmp(resolved, expected));
    }
    errno = 0;
    CHECK(!realpath(path, resolved) && errno == ELOOP);
    CHECK(unlink(link) == 0);
    return 0;
}

int v_freestanding_compat_check(const char *directory) {
    int link_error = check_realpath_limit(directory);
    if (link_error) return link_error;
    struct sigaction previous_action;
    CHECK(sigaction(SIGSTOP, NULL, &previous_action) == 0);
    CHECK(sigaction(SIGKILL, NULL, &previous_action) == 0);
    for (int reserved = 32; reserved <= 33; ++reserved) {
        errno = 0;
        CHECK(sigaction(reserved, NULL, &previous_action) == -1 && errno == EINVAL);
        errno = 0;
        CHECK(signal(reserved, SIG_IGN) == SIG_ERR && errno == EINVAL);
    }
    char unknown_interface[IF_NAMESIZE];
    errno = 0;
    CHECK(!if_indextoname(0, unknown_interface) && errno == ENXIO);
    struct addrinfo canonical_hints = {.ai_flags = AI_CANONNAME, .ai_socktype = SOCK_STREAM};
    struct addrinfo *canonical_result = NULL;
    CHECK(getaddrinfo(NULL, "80", &canonical_hints, &canonical_result) == EAI_BADFLAGS);
    struct sockaddr_in numeric = {.sin_family = AF_INET};
    char numeric_name[NI_MAXHOST];
    CHECK(getnameinfo((const struct sockaddr *)&numeric, sizeof numeric, numeric_name,
                      sizeof numeric_name, NULL, 0, NI_NAMEREQD | NI_NUMERICHOST) == EAI_NONAME);
    char path[1024], renamed[1024], text[128];
    CHECK(snprintf(path, sizeof path, "%s/data", directory) > 0);
    CHECK(snprintf(renamed, sizeof renamed, "%s/renamed", directory) > 0);
    const unsigned char payload[] = {0, 255, 42, 65, 66};
    FILE *file = fopen(path, "w+b");
    CHECK(file != NULL);
    setbuf(file, NULL);
    CHECK(fwrite(payload, 1, sizeof payload, file) == sizeof payload);
    CHECK(ftell(file) == (long)sizeof payload);
    CHECK(fseek(file, 0, SEEK_SET) == 0);
    unsigned char data[8] = {0};
    CHECK(fread(data, 2, 3, file) == 2);
    CHECK(memcmp(data, payload, sizeof payload) == 0);
    CHECK(feof(file) && !ferror(file));
    CHECK(fseek(file, 0, SEEK_SET) == 0 && !feof(file));
    struct stat metadata;
    errno = E2BIG;
    CHECK(fstat(fileno(file), &metadata) == 0 && errno == E2BIG);
    CHECK(metadata.st_size == (off_t)sizeof payload && S_ISREG(metadata.st_mode));
    CHECK(metadata.st_mtim.tv_sec > 0 && metadata.st_mtim.tv_nsec < 1000000000);
    CHECK(stat(path, &metadata) == 0 && metadata.st_size == (off_t)sizeof payload);
    CHECK(fclose(file) == 0);
    file = fopen("/dev/null", "wb");
    CHECK(file != NULL);
    CHECK(fread(data, 1, 1, file) == 0 && ferror(file) && errno == EBADF);
    clearerr(file);
    CHECK(!ferror(file) && !feof(file));
    CHECK(fclose(file) == 0);
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    CHECK(fd >= 0 && (fcntl(fd, F_GETFD) & FD_CLOEXEC));
    errno = E2BIG;
    CHECK(pread(fd, data, 3, 1) == 3 && errno == E2BIG);
    CHECK(memcmp(data, payload + 1, 3) == 0);
    CHECK(pread(-1, data, 1, 0) == -1 && errno == EBADF);
    CHECK(fstat(-1, &metadata) == -1 && errno == EBADF);
    errno = 0;
    CHECK(fstat(AT_FDCWD, &metadata) == -1 && errno == EBADF);
    struct stat cwd;
    CHECK(stat(".", &cwd) == 0 && fstatat(AT_FDCWD, "", &metadata, AT_EMPTY_PATH) == 0);
    CHECK(metadata.st_dev == cwd.st_dev && metadata.st_ino == cwd.st_ino);
    struct statvfs by_path, by_fd;
    CHECK(statvfs(path, &by_path) == 0 && fstatvfs(fd, &by_fd) == 0);
    CHECK(by_fd.f_frsize > 0 && by_fd.f_frsize == by_path.f_frsize && by_fd.f_fsid == by_path.f_fsid);
    struct rlimit limit;
    CHECK(getrlimit(RLIMIT_NOFILE, &limit) == 0 && setrlimit(RLIMIT_NOFILE, &limit) == 0);
    CHECK(close(-1) == -1 && errno == EBADF);

    int pair[2];
    CHECK(socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, pair) == 0);
    CHECK(send(pair[0], payload, sizeof payload, MSG_NOSIGNAL) == (ssize_t)sizeof payload);
    CHECK(recv(pair[1], data, sizeof payload, MSG_WAITALL) == (ssize_t)sizeof payload);
    CHECK(memcmp(data, payload, sizeof payload) == 0);
    off_t offset = 1;
    CHECK(sendfile(pair[0], fd, &offset, 3) == 3 && offset == 4);
    CHECK(recv(pair[1], data, 3, MSG_WAITALL) == 3 && memcmp(data, payload + 1, 3) == 0);
    CHECK(shutdown(pair[0], SHUT_WR) == 0 && recv(pair[1], data, 1, 0) == 0);
    CHECK(close(pair[0]) == 0 && close(pair[1]) == 0 && close(fd) == 0);
    off_t large_size = ((off_t)1 << 32) + 17;
    CHECK(truncate(path, large_size) == 0 && fstatat(AT_FDCWD, path, &metadata, 0) == 0);
    CHECK(metadata.st_size == large_size);
    CHECK(rename(path, renamed) == 0 && remove(renamed) == 0);
    CHECK(stat(renamed, &metadata) == -1 && errno == ENOENT);

    int notification = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
    int epoll = epoll_create1(EPOLL_CLOEXEC);
    CHECK(notification >= 0 && epoll >= 0);
    struct epoll_event event = {.events = EPOLLIN, .data.u64 = 0x12345678};
    CHECK(epoll_ctl(epoll, EPOLL_CTL_ADD, notification, &event) == 0);
    uint64_t one = 1, received = 0;
    CHECK(write(notification, &one, sizeof one) == sizeof one);
    CHECK(epoll_wait(epoll, &event, 1, 1000) == 1 && event.data.u64 == 0x12345678);
    CHECK(read(notification, &received, sizeof received) == sizeof received && received == 1);
    CHECK(epoll_wait(epoll, &event, 1, 0) == 0);
    CHECK(close(notification) == 0 && close(epoll) == 0);

    int listener = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
    CHECK(listener >= 0);
    int enabled = 1;
    CHECK(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &enabled, sizeof enabled) == 0);
    enabled = 0;
    socklen_t option_size = sizeof enabled;
    CHECK(getsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &enabled, &option_size) == 0 && enabled);
    struct sockaddr_in address = {.sin_family = AF_INET};
    CHECK(inet_pton(AF_INET, "127.0.0.1", &address.sin_addr) == 1);
    CHECK(bind(listener, (struct sockaddr *)&address, sizeof address) == 0);
    socklen_t address_size = sizeof address;
    CHECK(getsockname(listener, (struct sockaddr *)&address, &address_size) == 0);
    CHECK(listen(listener, 2) == 0);
    int client = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
    CHECK(client >= 0 && connect(client, (struct sockaddr *)&address, address_size) == 0);
    int accepted = accept4(listener, NULL, NULL, SOCK_NONBLOCK | SOCK_CLOEXEC);
    CHECK(accepted >= 0 && (fcntl(accepted, F_GETFL) & O_NONBLOCK));
    CHECK(fcntl(accepted, F_SETFD, FD_CLOEXEC) == 0 && (fcntl(accepted, F_GETFD) & FD_CLOEXEC));
    CHECK(close(accepted) == 0 && close(client) == 0 && close(listener) == 0);
    CHECK(send(-1, data, 1, 0) == -1 && errno == EBADF);

    int udp = socket(AF_INET, SOCK_DGRAM | SOCK_CLOEXEC, 0);
    CHECK(udp >= 0);
    address.sin_port = 0;
    CHECK(bind(udp, (struct sockaddr *)&address, sizeof address) == 0);
    address_size = sizeof address;
    CHECK(getsockname(udp, (struct sockaddr *)&address, &address_size) == 0);
    CHECK(sendto(udp, "packet", 6, 0, (struct sockaddr *)&address, address_size) == 6);
    char packet[8];
    struct sockaddr_in sender;
    socklen_t sender_size = sizeof sender;
    CHECK(recvfrom(udp, packet, sizeof packet, 0, (struct sockaddr *)&sender, &sender_size) == 6 &&
          !memcmp(packet, "packet", 6) && sender.sin_port == address.sin_port);
    CHECK(close(udp) == 0);
    CHECK(sendto(-1, packet, 1, 0, (struct sockaddr *)&address, address_size) == -1 && errno == EBADF);
    CHECK(recvfrom(-1, packet, 1, 0, NULL, NULL) == -1 && errno == EBADF);

    unsigned char ip[16], unchanged[16];
    memset(ip, 0xa5, sizeof ip);
    memcpy(unchanged, ip, sizeof ip);
    CHECK(inet_pton(AF_INET, "127.01.0.1", ip) == 0 && memcmp(ip, unchanged, sizeof ip) == 0);
    CHECK(inet_pton(AF_INET6, "2001:db8::1", ip) == 1 && ip[0] == 0x20 && ip[15] == 1);
    CHECK(inet_pton(AF_INET6, "::ffff:192.0.2.1", ip) == 1 && ip[10] == 255 && ip[15] == 1);
    CHECK(inet_pton(AF_INET6, "1:2:3:4:5:6:7::8", ip) == 0);
    CHECK(inet_pton(AF_INET6, "1:::2", ip) == 0);
    struct addrinfo hints = {.ai_family = AF_UNSPEC, .ai_socktype = SOCK_STREAM}, *results;
    CHECK(getaddrinfo("127.0.0.1", "6969", &hints, &results) == 0);
    CHECK(results->ai_family == AF_INET && results->ai_socktype == SOCK_STREAM && results->ai_next == NULL);
    unsigned char *port = (unsigned char *)&((struct sockaddr_in *)results->ai_addr)->sin_port;
    CHECK(port[0] == 27 && port[1] == 57);
    freeaddrinfo(results);
    CHECK(getaddrinfo("::1", "443", &hints, &results) == 0 && results->ai_family == AF_INET6);
    freeaddrinfo(results);
    CHECK(getaddrinfo("localhost", "80", &hints, &results) == 0 && results->ai_next != NULL);
    freeaddrinfo(results);
    hints.ai_flags = AI_NUMERICHOST;
    CHECK(getaddrinfo("not-a-numeric-host.invalid", "80", &hints, &results) == EAI_NONAME && results == NULL);
    CHECK(getaddrinfo("127.0.0.1", "65536", &hints, &results) == EAI_SERVICE);

    void *allocated = (void *)(uintptr_t)1;
    errno = E2BIG;
    CHECK(posix_memalign(&allocated, 3, 64) == EINVAL && allocated == (void *)(uintptr_t)1 && errno == E2BIG);
    CHECK(posix_memalign(&allocated, 64, 129) == 0 && allocated && (uintptr_t)allocated % 64 == 0 && errno == E2BIG);
    memset(allocated, 0xa5, 129);
    explicit_bzero(allocated, 129);
    for (int i = 0; i < 129; ++i) CHECK(((unsigned char *)allocated)[i] == 0);
    free(allocated);
    CHECK(strcmp("a", "b") < 0 && strcmp("\xff", "\x7f") > 0);
    const char sample[] = "abcabc";
    CHECK(strchr(sample, 0) == sample + 6 && strchr(sample, 'b') == sample + 1);
    CHECK(strstr(sample, "bca") == sample + 1 && strstr(sample, "abcd") == NULL);
    CHECK(snprintf(text, sizeof text, "%+08d %#x %.0u %zu %lld", -42, 255u, 0u, (size_t)123, LLONG_MIN) == 39);
    CHECK(strcmp(text, "-0000042 0xff  123 -9223372036854775808") == 0);
    CHECK(snprintf(text, 5, "%s", "abcdef") == 6 && strcmp(text, "abcd") == 0);
    CHECK(snprintf(NULL, 0, "%*.*s", -8, 3, "abcdef") == 8);
    CHECK(snprintf(text, sizeof text, "%#o %#.0o %02X %%", 8u, 0u, 255u) == 10 && strcmp(text, "010 0 FF %") == 0);
    CHECK(snprintf(text, 1, "%2147483647s", "") == INT_MAX && text[0] == 0);

    time_t timestamp = -1;
    struct tm calendar;
    CHECK(gmtime_r(&timestamp, &calendar) == &calendar && calendar.tm_year == 69 && calendar.tm_mon == 11 &&
          calendar.tm_mday == 31 && calendar.tm_hour == 23 && calendar.tm_min == 59 && calendar.tm_sec == 59);
    timestamp = 951782400; /* 2000-02-29 */
    CHECK(gmtime_r(&timestamp, &calendar) == &calendar && calendar.tm_year == 100 && calendar.tm_mon == 1 &&
          calendar.tm_mday == 29 && calendar.tm_yday == 59 && calendar.tm_wday == 2);
    CHECK(time(&timestamp) > 0 && timestamp > 0);
    errno = E2BIG;
    CHECK(sysconf(_SC_PAGESIZE) >= 4096 && errno == E2BIG);
#ifdef _SC_MINSIGSTKSZ
    long minimum_stack = sysconf(_SC_MINSIGSTKSZ);
    long recommended_stack = SIGSTKSZ;
    CHECK(minimum_stack > 0 && recommended_stack >= minimum_stack && errno == E2BIG);
    unsigned long kernel_minimum = getauxval(AT_MINSIGSTKSZ);
    if (kernel_minimum) CHECK((unsigned long)minimum_stack == kernel_minimum);
    stack_t previous_stack;
    stack_t alternate = {.ss_sp = malloc(recommended_stack), .ss_size = recommended_stack};
    CHECK(alternate.ss_sp && !sigaltstack(&alternate, &previous_stack));
    CHECK(!sigaltstack(&previous_stack, NULL));
    free(alternate.ss_sp);
#endif
    CHECK(sysconf(_SC_NPROCESSORS_ONLN) > 0 && sysconf(_SC_PHYS_PAGES) > 0);
    CHECK(sysconf(-1) == -1 && errno == EINVAL);
    cpu_set_t cpus;
    memset(&cpus, 0xa5, sizeof cpus);
    CHECK(sched_getaffinity(0, sizeof cpus, &cpus) == 0 && CPU_COUNT(&cpus) > 0);
    CHECK(syscall(SYS_getpid) == getpid());
    CHECK(syscall(SYS_close, -1) == -1 && errno == EBADF);
    CHECK(syscall(0x7fffffff) == -1 && errno == ENOSYS);
    CHECK(signal(SIGUSR1, receive_signal) != SIG_ERR);
    sigset_t blocked, previous;
    CHECK(sigemptyset(&blocked) == 0 && sigaddset(&blocked, SIGUSR1) == 0);
    CHECK(sigismember(&blocked, SIGUSR1) == 1 && sigismember(&blocked, SIGUSR2) == 0);
    errno = E2BIG;
    CHECK(pthread_sigmask(SIG_BLOCK, &blocked, &previous) == 0 && errno == E2BIG);
    CHECK(syscall(SYS_kill, getpid(), SIGUSR1) == 0 && delivered == 0);
    CHECK(sigprocmask(SIG_SETMASK, &previous, NULL) == 0 && delivered == SIGUSR1);
    CHECK(sigdelset(&blocked, SIGUSR1) == 0 && sigismember(&blocked, SIGUSR1) == 0);
    CHECK(sigaddset(&blocked, 65) == -1 && errno == EINVAL);
    delivered = 0;
    CHECK(syscall(SYS_kill, getpid(), SIGUSR1) == 0 && delivered == SIGUSR1);
    CHECK(signal(SIGUSR1, SIG_DFL) == receive_signal);
    CHECK(signal(SIGKILL, receive_signal) == SIG_ERR && errno == EINVAL);
    pthread_mutex_t mutex;
    CHECK(pthread_mutex_init(&mutex, NULL) == 0);
    CHECK(pthread_mutex_lock(&mutex) == 0 && pthread_mutex_trylock(&mutex) == EBUSY);
    CHECK(pthread_mutex_destroy(&mutex) == EBUSY);
    CHECK(pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_trylock(&mutex) == 0);
    CHECK(pthread_mutex_unlock(&mutex) == 0 && pthread_mutex_destroy(&mutex) == 0);
    return 0;
}
