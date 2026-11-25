#include "libc_compat.h"
#include <fcntl.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/xattr.h>
#include <unistd.h>
#include <sched.h>
#if defined(__GLIBC__)
typedef __rlimit_resource_t v_native_limit_resource;
#else
typedef int v_native_limit_resource;
#endif

/* Public native prototypes only; every operation and errno decision is V. */
extern int32_t v_native_dup(int32_t fd);
int dup(int fd) { return (int)v_native_dup((int32_t)fd); }
extern int32_t v_native_dup3(int32_t oldfd, int32_t newfd, int32_t flags);
int dup3(int oldfd, int newfd, int flags) { return (int)v_native_dup3((int32_t)oldfd, (int32_t)newfd, (int32_t)flags); }
extern int32_t v_native_accept(int32_t fd, uintptr_t address, uintptr_t size);
int accept(int fd, struct sockaddr * address, socklen_t * size) { return (int)v_native_accept((int32_t)fd, (uintptr_t)address, (uintptr_t)size); }
extern int32_t v_native_getpeername(int32_t fd, uintptr_t address, uintptr_t size);
int getpeername(int fd, struct sockaddr * address, socklen_t * size) { return (int)v_native_getpeername((int32_t)fd, (uintptr_t)address, (uintptr_t)size); }
extern int32_t v_native_chdir(uintptr_t path);
int chdir(const char * path) { return (int)v_native_chdir((uintptr_t)path); }
extern int32_t v_native_fchdir(int32_t fd);
int fchdir(int fd) { return (int)v_native_fchdir((int32_t)fd); }
extern int32_t v_native_fchmod(int32_t fd, uint32_t mode);
int fchmod(int fd, mode_t mode) { return (int)v_native_fchmod((int32_t)fd, (uint32_t)mode); }
extern int32_t v_native_fchown(int32_t fd, uint32_t user, uint32_t group);
int fchown(int fd, uid_t user, gid_t group) { return (int)v_native_fchown((int32_t)fd, (uint32_t)user, (uint32_t)group); }
extern int32_t v_native_chown(uintptr_t path, uint32_t user, uint32_t group);
int chown(const char * path, uid_t user, gid_t group) { return (int)v_native_chown((uintptr_t)path, (uint32_t)user, (uint32_t)group); }
extern int32_t v_native_lchown(uintptr_t path, uint32_t user, uint32_t group);
int lchown(const char * path, uid_t user, gid_t group) { return (int)v_native_lchown((uintptr_t)path, (uint32_t)user, (uint32_t)group); }
extern int32_t v_native_fsync(int32_t fd);
int fsync(int fd) { return (int)v_native_fsync((int32_t)fd); }
extern int32_t v_native_fdatasync(int32_t fd);
int fdatasync(int fd) { return (int)v_native_fdatasync((int32_t)fd); }
extern int32_t v_native_ftruncate(int32_t fd, int64_t length);
int ftruncate(int fd, off_t length) { return (int)v_native_ftruncate((int32_t)fd, (int64_t)length); }
#if defined(__GLIBC__)
_Static_assert(sizeof(off_t) == sizeof(off64_t), "native 64-bit truncation");
extern __typeof__(ftruncate64) ftruncate64 __attribute__((alias("ftruncate")));
#endif
extern uint32_t v_native_umask(uint32_t mode);
mode_t umask(mode_t mode) { return (mode_t)v_native_umask((uint32_t)mode); }
extern int32_t v_native_unlinkat(int32_t fd, uintptr_t path, int32_t flags);
int unlinkat(int fd, const char * path, int flags) { return (int)v_native_unlinkat((int32_t)fd, (uintptr_t)path, (int32_t)flags); }
extern int32_t v_native_linkat(int32_t oldfd, uintptr_t oldpath, int32_t newfd, uintptr_t newpath, int32_t flags);
int linkat(int oldfd, const char * oldpath, int newfd, const char * newpath, int flags) { return (int)v_native_linkat((int32_t)oldfd, (uintptr_t)oldpath, (int32_t)newfd, (uintptr_t)newpath, (int32_t)flags); }
extern int64_t v_native_readlinkat(int32_t fd, uintptr_t path, uintptr_t buffer, uintptr_t size);
ssize_t readlinkat(int fd, const char * path, char * buffer, size_t size) { return (ssize_t)v_native_readlinkat((int32_t)fd, (uintptr_t)path, (uintptr_t)buffer, (uintptr_t)size); }
extern int32_t v_native_mknodat(int32_t fd, uintptr_t path, uint32_t mode, uint64_t device);
int mknodat(int fd, const char * path, mode_t mode, dev_t device) { return (int)v_native_mknodat((int32_t)fd, (uintptr_t)path, (uint32_t)mode, (uint64_t)device); }
extern int32_t v_native_mknod(uintptr_t path, uint32_t mode, uint64_t device);
int mknod(const char * path, mode_t mode, dev_t device) { return (int)v_native_mknod((uintptr_t)path, (uint32_t)mode, (uint64_t)device); }
extern int32_t v_native_mkfifo(uintptr_t path, uint32_t mode);
int mkfifo(const char * path, mode_t mode) { return (int)v_native_mkfifo((uintptr_t)path, (uint32_t)mode); }
extern int32_t v_native_flock(int32_t fd, int32_t operation);
int flock(int fd, int operation) { return (int)v_native_flock((int32_t)fd, (int32_t)operation); }
extern int32_t v_native_utimensat(int32_t fd, uintptr_t path, uintptr_t times, int32_t flags);
int utimensat(int fd, const char * path, const struct timespec * times, int flags) { return (int)v_native_utimensat((int32_t)fd, (uintptr_t)path, (uintptr_t)times, (int32_t)flags); }
extern int32_t v_native_futimens(int32_t fd, uintptr_t times);
int futimens(int fd, const struct timespec * times) { return (int)v_native_futimens((int32_t)fd, (uintptr_t)times); }
extern int32_t v_native_madvise(uintptr_t address, uintptr_t size, int32_t advice);
int madvise(void * address, size_t size, int advice) { return (int)v_native_madvise((uintptr_t)address, (uintptr_t)size, (int32_t)advice); }
extern int32_t v_native_mlock(uintptr_t address, uintptr_t size);
int mlock(const void * address, size_t size) { return (int)v_native_mlock((uintptr_t)address, (uintptr_t)size); }
extern int32_t v_native_munlock(uintptr_t address, uintptr_t size);
int munlock(const void * address, size_t size) { return (int)v_native_munlock((uintptr_t)address, (uintptr_t)size); }
extern int32_t v_native_getrlimit(int32_t resource, uintptr_t limit);
int getrlimit(v_native_limit_resource resource, struct rlimit * limit) { return (int)v_native_getrlimit((int32_t)resource, (uintptr_t)limit); }
extern int32_t v_native_setrlimit(int32_t resource, uintptr_t limit);
int setrlimit(v_native_limit_resource resource, const struct rlimit * limit) { return (int)v_native_setrlimit((int32_t)resource, (uintptr_t)limit); }
extern int32_t v_native_gettid(void);
pid_t gettid(void) { return (pid_t)v_native_gettid(); }
extern int32_t v_native_sched_yield(void);
int sched_yield(void) { return (int)v_native_sched_yield(); }
extern int32_t v_native_gettimeofday(uintptr_t stamp, uintptr_t zone);
int gettimeofday(struct timeval * stamp, void * zone) { return (int)v_native_gettimeofday((uintptr_t)stamp, (uintptr_t)zone); }
extern int32_t v_native_waitpid(int32_t pid, uintptr_t status, int32_t options);
pid_t waitpid(pid_t pid, int * status, int options) { return (pid_t)v_native_waitpid((int32_t)pid, (uintptr_t)status, (int32_t)options); }
extern int32_t v_native_wait4(int32_t,uintptr_t,int32_t,uintptr_t);
pid_t wait(int *status) { return (pid_t)v_native_wait4(-1,(uintptr_t)status,0,0); }
pid_t wait3(int *status,int options,struct rusage *usage) { return (pid_t)v_native_wait4(-1,(uintptr_t)status,options,(uintptr_t)usage); }
pid_t wait4(pid_t pid,int *status,int options,struct rusage *usage) { return (pid_t)v_native_wait4((int32_t)pid,(uintptr_t)status,options,(uintptr_t)usage); }
extern int32_t v_native_waitid(int32_t,uint32_t,uintptr_t,int32_t);
int waitid(idtype_t kind,id_t id,siginfo_t *info,int options) { return v_native_waitid((int32_t)kind,(uint32_t)id,(uintptr_t)info,options); }
extern int64_t v_native_getxattr(uintptr_t path, uintptr_t name, uintptr_t value, uintptr_t size);
ssize_t getxattr(const char * path, const char * name, void * value, size_t size) { return (ssize_t)v_native_getxattr((uintptr_t)path, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size); }
extern int64_t v_native_lgetxattr(uintptr_t path, uintptr_t name, uintptr_t value, uintptr_t size);
ssize_t lgetxattr(const char * path, const char * name, void * value, size_t size) { return (ssize_t)v_native_lgetxattr((uintptr_t)path, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size); }
extern int64_t v_native_fgetxattr(int32_t fd, uintptr_t name, uintptr_t value, uintptr_t size);
ssize_t fgetxattr(int fd, const char * name, void * value, size_t size) { return (ssize_t)v_native_fgetxattr((int32_t)fd, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size); }
extern int32_t v_native_setxattr(uintptr_t path, uintptr_t name, uintptr_t value, uintptr_t size, int32_t flags);
int setxattr(const char * path, const char * name, const void * value, size_t size, int flags) { return (int)v_native_setxattr((uintptr_t)path, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size, (int32_t)flags); }
extern int32_t v_native_lsetxattr(uintptr_t path, uintptr_t name, uintptr_t value, uintptr_t size, int32_t flags);
int lsetxattr(const char * path, const char * name, const void * value, size_t size, int flags) { return (int)v_native_lsetxattr((uintptr_t)path, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size, (int32_t)flags); }
extern int32_t v_native_fsetxattr(int32_t fd, uintptr_t name, uintptr_t value, uintptr_t size, int32_t flags);
int fsetxattr(int fd, const char * name, const void * value, size_t size, int flags) { return (int)v_native_fsetxattr((int32_t)fd, (uintptr_t)name, (uintptr_t)value, (uintptr_t)size, (int32_t)flags); }
extern int64_t v_native_listxattr(uintptr_t path, uintptr_t list, uintptr_t size);
ssize_t listxattr(const char * path, char * list, size_t size) { return (ssize_t)v_native_listxattr((uintptr_t)path, (uintptr_t)list, (uintptr_t)size); }
extern int64_t v_native_llistxattr(uintptr_t path, uintptr_t list, uintptr_t size);
ssize_t llistxattr(const char * path, char * list, size_t size) { return (ssize_t)v_native_llistxattr((uintptr_t)path, (uintptr_t)list, (uintptr_t)size); }
extern int64_t v_native_flistxattr(int32_t fd, uintptr_t list, uintptr_t size);
ssize_t flistxattr(int fd, char * list, size_t size) { return (ssize_t)v_native_flistxattr((int32_t)fd, (uintptr_t)list, (uintptr_t)size); }
extern int32_t v_native_removexattr(uintptr_t path, uintptr_t name);
int removexattr(const char * path, const char * name) { return (int)v_native_removexattr((uintptr_t)path, (uintptr_t)name); }
extern int32_t v_native_lremovexattr(uintptr_t path, uintptr_t name);
int lremovexattr(const char * path, const char * name) { return (int)v_native_lremovexattr((uintptr_t)path, (uintptr_t)name); }
extern int32_t v_native_fremovexattr(int32_t fd, uintptr_t name);
int fremovexattr(int fd, const char * name) { return (int)v_native_fremovexattr((int32_t)fd, (uintptr_t)name); }
extern int64_t v_native_recvmsg(int32_t fd, uintptr_t message, int32_t flags);
ssize_t recvmsg(int fd, struct msghdr * message, int flags) { return (ssize_t)v_native_recvmsg((int32_t)fd, (uintptr_t)message, (int32_t)flags); }
extern int64_t v_native_sendmsg(int32_t fd, uintptr_t message, int32_t flags);
ssize_t sendmsg(int fd, const struct msghdr * message, int flags) { return (ssize_t)v_native_sendmsg((int32_t)fd, (uintptr_t)message, (int32_t)flags); }
/* The glibc and musl prototypes use different signedness for flags. */
#ifdef __GLIBC__
typedef int v_mmsg_flags;
#else
typedef unsigned int v_mmsg_flags;
#endif
extern int32_t v_native_recvmmsg(int32_t fd, uintptr_t messages, uint32_t count, int32_t flags, uintptr_t timeout);
int recvmmsg(int fd, struct mmsghdr * messages, unsigned int count, v_mmsg_flags flags, struct timespec * timeout) { return (int)v_native_recvmmsg((int32_t)fd, (uintptr_t)messages, (uint32_t)count, (int32_t)flags, (uintptr_t)timeout); }
extern int32_t v_native_sendmmsg(int32_t fd, uintptr_t messages, uint32_t count, int32_t flags);
int sendmmsg(int fd, struct mmsghdr * messages, unsigned int count, v_mmsg_flags flags) { return (int)v_native_sendmmsg((int32_t)fd, (uintptr_t)messages, (uint32_t)count, (int32_t)flags); }

#include <elf.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <termios.h>
#include <string.h>
extern char **environ;
extern const uintptr_t *v_freestanding_auxiliary_vector;
uintptr_t v_native_environment(void) { return (uintptr_t)environ; }
/* Linux amd64 and arm64 use 19 control characters in the kernel ABI.
 * The public structure has additional slots before the speed fields. */
struct v_kernel_termios2 {
    uint32_t iflag, oflag, cflag, lflag;
    unsigned char line, cc[19];
    uint32_t ispeed, ospeed;
};
_Static_assert(offsetof(struct v_kernel_termios2, ispeed) == 36, "kernel termios2 speed offset");
_Static_assert(NCCS >= 19, "kernel control characters fit in public termios");
#if B38400 == 38400
/* glibc's numeric speed ABI also exposes the encoded baud bits in c_cflag.
 * BOTHER carries a numeric rate directly. Zero input baud follows output. */
static uint32_t v_termios_speed(uint32_t *bits, uint32_t numeric) {
    static const uint32_t rates[] = {
        0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800,
        9600, 19200, 38400, UINT32_MAX, 57600, 115200, 230400, 460800,
        500000, 576000, 921600, 1000000, 1152000, 1500000, 2000000,
        2500000, 3000000, 3500000, 4000000
    };
    if (*bits != CBAUDEX)
        return rates[(*bits & 15) + ((*bits & CBAUDEX) ? 16 : 0)];
    for (unsigned i = 0; i < sizeof rates / sizeof *rates; i++) {
        if (i != 16 && numeric == rates[i]) {
            *bits = (i & 15) | (i >= 16 ? CBAUDEX : 0);
            break;
        }
    }
    return numeric;
}
static void v_termios_normalize(struct v_kernel_termios2 *kernel) {
    uint32_t output = kernel->cflag & CBAUD;
    uint32_t input = (kernel->cflag & CIBAUD) >> 16;
    kernel->ospeed = v_termios_speed(&output, kernel->ospeed);
    kernel->ispeed = v_termios_speed(&input, kernel->ispeed);
    if (kernel->ispeed == 0) {
        input = output;
        kernel->ispeed = kernel->ospeed;
    }
    kernel->cflag = (kernel->cflag & ~(CBAUD | CIBAUD)) | output | (input << 16);
}
#endif
int32_t v_native_termios_get(int32_t fd, uintptr_t address) {
    struct termios *out = (struct termios *)address;
    struct v_kernel_termios2 kernel = {0};
#if B38400 == 38400
    const unsigned long request = _IOR('T', 0x2a, struct v_kernel_termios2);
#else
    const unsigned long request = TCGETS;
#endif
    if (v_linux_result(V_RAW3(SYS_ioctl, fd, request, &kernel)) < 0) return -1;
#if B38400 == 38400
    v_termios_normalize(&kernel);
#endif
    *out = (struct termios){0};
    out->c_iflag = kernel.iflag;
    out->c_oflag = kernel.oflag;
    out->c_cflag = kernel.cflag;
    out->c_lflag = kernel.lflag;
    out->c_line = kernel.line;
    memcpy(out->c_cc, kernel.cc, sizeof kernel.cc);
#if B38400 == 38400
    out->c_ispeed = kernel.ispeed;
    out->c_ospeed = kernel.ospeed;
#elif defined(_HAVE_STRUCT_TERMIOS_C_ISPEED) && defined(_HAVE_STRUCT_TERMIOS_C_OSPEED)
    out->c_ispeed = out->c_ospeed = kernel.cflag & CBAUD;
#endif
    return 0;
}
int32_t v_native_termios_set(int32_t fd, int32_t action, uintptr_t address) {
    const struct termios *in = (const struct termios *)address;
    struct v_kernel_termios2 kernel = { .iflag=in->c_iflag, .oflag=in->c_oflag,
        .cflag=in->c_cflag, .lflag=in->c_lflag, .line=in->c_line };
    memcpy(kernel.cc, in->c_cc, sizeof kernel.cc);
#if B38400 == 38400
    kernel.ispeed = in->c_ispeed;
    kernel.ospeed = in->c_ospeed;
    v_termios_normalize(&kernel);
    if (kernel.ispeed == kernel.ospeed) kernel.cflag &= ~CIBAUD;
    const unsigned long requests[] = { _IOW('T', 0x2b, struct v_kernel_termios2),
        _IOW('T', 0x2c, struct v_kernel_termios2), _IOW('T', 0x2d, struct v_kernel_termios2) };
#else
    const unsigned long requests[] = {TCSETS, TCSETSW, TCSETSF};
#endif
    return (int32_t)v_linux_result(V_RAW3(SYS_ioctl, fd, requests[action], &kernel));
}
_Static_assert(sizeof(struct timeval) == 16 && offsetof(struct timeval, tv_usec) == 8, "native timeval ABI");
_Static_assert(sizeof(struct rlimit) == 16 && offsetof(struct rlimit, rlim_max) == 8, "native rlimit ABI");
extern int32_t v_native_execv(uintptr_t, uintptr_t);
extern int32_t v_native_execvp(uintptr_t, uintptr_t);
extern int32_t v_native_fork(void);
extern int32_t v_native_fchmodat(int32_t, uintptr_t, uint32_t, int32_t);
extern int32_t v_native_futimesat(int32_t, uintptr_t, uintptr_t, int32_t);
extern int32_t v_native_truncate(uintptr_t, int64_t);
extern int32_t v_native_tcgetattr(int32_t, uintptr_t);
extern int32_t v_native_tcsetattr(int32_t, int32_t, uintptr_t);
extern uintptr_t v_native_getauxval(uintptr_t);
extern uintptr_t v_native_secure_getenv(uintptr_t);
extern int32_t v_native_prctl(int32_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);
extern void v_native_sync(void);
extern int32_t v_native_processors(int32_t);
extern int32_t v_native_pagesize(void);
int execv(const char *path, char *const arguments[]) { return v_native_execv((uintptr_t)path, (uintptr_t)arguments); }
int execvp(const char *file, char *const arguments[]) { return v_native_execvp((uintptr_t)file, (uintptr_t)arguments); }
pid_t fork(void) { return (pid_t)v_native_fork(); }
int fchmodat(int fd, const char *path, mode_t mode, int flags) { return v_native_fchmodat(fd, (uintptr_t)path, mode, flags); }
int lchmod(const char *path, mode_t mode) { return v_native_fchmodat(AT_FDCWD, (uintptr_t)path, mode, AT_SYMLINK_NOFOLLOW); }
int futimesat(int fd, const char *path, const struct timeval times[2]) { return v_native_futimesat(fd, (uintptr_t)path, (uintptr_t)times, 0); }
int lutimes(const char *path, const struct timeval times[2]) { return v_native_futimesat(AT_FDCWD, (uintptr_t)path, (uintptr_t)times, AT_SYMLINK_NOFOLLOW); }
int truncate(const char *path, off_t size) { return v_native_truncate((uintptr_t)path, size); }
int tcgetattr(int fd, struct termios *settings) { return v_native_tcgetattr(fd, (uintptr_t)settings); }
int tcsetattr(int fd, int action, const struct termios *settings) { return v_native_tcsetattr(fd, action, (uintptr_t)settings); }
unsigned long getauxval(unsigned long type) { return v_native_getauxval(type); }
char *secure_getenv(const char *name) { return (char *)v_native_secure_getenv((uintptr_t)name); }
void sync(void) { v_native_sync(); }
int get_nprocs(void) { return v_native_processors(0); }
int get_nprocs_conf(void) { return v_native_processors(1); }
int getpagesize(void) { return v_native_pagesize(); }

/* Preserve request-dependent argument registers without reading C va_args at
 * types different from the caller's pointer/integer actual arguments. */
int v_native_prctl_abi(int option, uintptr_t a, uintptr_t b, uintptr_t c, uintptr_t d) {
    return v_native_prctl(option, a, b, c, d);
}
#if defined(__x86_64__)
__asm__(".pushsection .text.prctl,\"ax\",@progbits\n.global prctl\n.type prctl,@function\nprctl:\njmp v_native_prctl_abi\n.size prctl,.-prctl\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text.prctl,\"ax\",%progbits\n.global prctl\n.type prctl,%function\nprctl:\nb v_native_prctl_abi\n.size prctl,.-prctl\n.popsection\n");
#endif
#include <sys/statvfs.h>
extern int32_t v_native_statvfs(uintptr_t,int32_t,uintptr_t);
extern int32_t v_native_ttyname(int32_t,uintptr_t,uintptr_t);
extern void v_native_closefrom(int32_t);
int statvfs(const char *p,struct statvfs *v) { return v_native_statvfs((uintptr_t)p,-1,(uintptr_t)v); }
int fstatvfs(int fd,struct statvfs *v) { return v_native_statvfs(0,fd,(uintptr_t)v); }
int ttyname_r(int fd,char *out,size_t n) { return v_native_ttyname(fd,(uintptr_t)out,n); }
void closefrom(int low) { v_native_closefrom(low); }
void v_native_statvfs_assign(uintptr_t output,uintptr_t values) {
    struct statvfs *v=(struct statvfs *)output;
    const uint64_t *f=(const uint64_t *)values;
    *v=(struct statvfs){.f_bsize=f[0],.f_frsize=f[1],.f_blocks=f[2],.f_bfree=f[3],.f_bavail=f[4],.f_files=f[5],.f_ffree=f[6],.f_favail=f[7],.f_fsid=f[8],.f_flag=f[9],.f_namemax=f[10]};
}
#include <sys/sysmacros.h>
/* Conservatively disable glibc-header single-thread shortcuts: V workers and
 * native workers may both exist. This is ABI storage, not pthread state. */
char __libc_single_threaded = 0;
extern uint32_t v_native_device_major(uint64_t);
extern uint32_t v_native_device_minor(uint64_t);
extern uint64_t v_native_device_make(uint32_t,uint32_t);
extern void v_native_divide(int64_t,int64_t,uintptr_t);
extern uintptr_t v_native_memset_explicit(uintptr_t,int32_t,uintptr_t);
extern uintptr_t v_native_basename(uintptr_t);
unsigned int gnu_dev_major(dev_t device) { return v_native_device_major(device); }
unsigned int gnu_dev_minor(dev_t device) { return v_native_device_minor(device); }
dev_t gnu_dev_makedev(unsigned int major,unsigned int minor) { return (dev_t)v_native_device_make(major,minor); }
lldiv_t lldiv(long long numerator,long long denominator) {
    int64_t values[2];
    v_native_divide(numerator,denominator,(uintptr_t)values);
    return (lldiv_t){.quot=values[0],.rem=values[1]};
}
void *memset_explicit(void *p,int c,size_t n) { return (void *)v_native_memset_explicit((uintptr_t)p,c,n); }
char *__xpg_basename(char *path) { return (char *)v_native_basename((uintptr_t)path); }

#include <arpa/inet.h>
#undef htons
#undef htonl
#undef ntohs
#undef ntohl
extern uint16_t v_native_byteorder_16(uint16_t);
extern uint32_t v_native_byteorder_32(uint32_t);
uint16_t htons(uint16_t v) { return v_native_byteorder_16(v); }
uint16_t ntohs(uint16_t v) { return v_native_byteorder_16(v); }
uint32_t htonl(uint32_t v) { return v_native_byteorder_32(v); }
uint32_t ntohl(uint32_t v) { return v_native_byteorder_32(v); }

#include <grp.h>
#include <utime.h>
_Static_assert(sizeof(struct utimbuf)==16,"Linux time64 utimbuf ABI");
extern int32_t v_native_mkdirat(int32_t,uintptr_t,uint32_t);
int mkdirat(int fd, const char *path, mode_t mode) { return v_native_mkdirat((int32_t)fd,(uintptr_t)path,(uint32_t)mode); }
extern int32_t v_native_renameat(int32_t,uintptr_t,int32_t,uintptr_t);
int renameat(int oldfd,const char *old,int newfd,const char *new) { return v_native_renameat(oldfd,(uintptr_t)old,newfd,(uintptr_t)new); }
extern int32_t v_native_renameat2(int32_t,uintptr_t,int32_t,uintptr_t,uint32_t);
int renameat2(int oldfd,const char *old,int newfd,const char *new,unsigned int flags) { return v_native_renameat2(oldfd,(uintptr_t)old,newfd,(uintptr_t)new,flags); }
extern int32_t v_native_symlinkat(uintptr_t,int32_t,uintptr_t);
int symlinkat(const char *target,int fd,const char *path) { return v_native_symlinkat((uintptr_t)target,fd,(uintptr_t)path); }
extern int32_t v_native_fchownat(int32_t,uintptr_t,uint32_t,uint32_t,int32_t);
int fchownat(int fd,const char *path,uid_t uid,gid_t gid,int flags) { return v_native_fchownat(fd,(uintptr_t)path,uid,gid,flags); }
extern int32_t v_native_faccessat(int32_t,uintptr_t,int32_t,int32_t);
int faccessat(int fd,const char *path,int mode,int flags) { return v_native_faccessat(fd,(uintptr_t)path,mode,flags); }
extern int32_t v_native_eaccess(uintptr_t,int32_t);
int eaccess(const char *path,int mode) { return v_native_eaccess((uintptr_t)path,mode); }
int euidaccess(const char *path,int mode) { return v_native_eaccess((uintptr_t)path,mode); }
extern int32_t v_native_setpgid(int32_t,int32_t);
int setpgid(pid_t pid,pid_t group) { return v_native_setpgid(pid,group); }
int setpgrp(void) { return v_native_setpgid(0,0); }
extern int32_t v_native_getpgid(int32_t);
pid_t getpgid(pid_t pid) { return v_native_getpgid(pid); }
pid_t getpgrp(void) { return v_native_getpgid(0); }
extern int32_t v_native_getsid(int32_t);
pid_t getsid(pid_t pid) { return v_native_getsid(pid); }
extern int32_t v_native_usleep(uint32_t);
int usleep(useconds_t microseconds) { return v_native_usleep(microseconds); }
extern int64_t v_native_pwrite(int32_t,uintptr_t,uintptr_t,int64_t);
ssize_t pwrite(int fd,const void *input,size_t count,off_t offset) { return v_native_pwrite(fd,(uintptr_t)input,count,offset); }
extern int32_t v_native_utime(uintptr_t,uintptr_t);
int utime(const char *path,const struct utimbuf *times) { return v_native_utime((uintptr_t)path,(uintptr_t)times); }
extern int32_t v_native_chroot(uintptr_t);
int chroot(const char *path) { return v_native_chroot((uintptr_t)path); }
#if defined(__GLIBC__)
extern __typeof__(pwrite64) pwrite64 __attribute__((alias("pwrite")));
#endif
extern int32_t v_native_credentials(int32_t,uint32_t,uint32_t,uint32_t,uintptr_t,uintptr_t);
int setuid(uid_t id) { return v_native_credentials(0,id,0,0,0,0); }
int setgid(gid_t id) { return v_native_credentials(1,id,0,0,0,0); }
int setreuid(uid_t real,uid_t effective) { return v_native_credentials(2,real,effective,0,0,0); }
int setregid(gid_t real,gid_t effective) { return v_native_credentials(3,real,effective,0,0,0); }
int setresuid(uid_t real,uid_t effective,uid_t saved) { return v_native_credentials(4,real,effective,saved,0,0); }
int setresgid(gid_t real,gid_t effective,gid_t saved) { return v_native_credentials(5,real,effective,saved,0,0); }
int setgroups(size_t count,const gid_t *groups) { return v_native_credentials(6,0,0,0,(uintptr_t)groups,count); }
int seteuid(uid_t id) { return v_native_credentials(7,id,0,0,0,0); }
int setegid(gid_t id) { return v_native_credentials(8,id,0,0,0,0); }

extern int32_t v_native_absolute_32(int32_t);
extern int64_t v_native_absolute_64(int64_t);
int abs(int value) { return v_native_absolute_32(value); }
long labs(long value) { return (long)v_native_absolute_64(value); }
long long llabs(long long value) { return (long long)v_native_absolute_64(value); }
div_t div(int first,int second) {
    int64_t result[2]; v_native_divide(first,second,(uintptr_t)result);
    return (div_t){.quot=(int)result[0],.rem=(int)result[1]};
}
ldiv_t ldiv(long first,long second) {
    int64_t result[2]; v_native_divide(first,second,(uintptr_t)result);
    return (ldiv_t){.quot=(long)result[0],.rem=(long)result[1]};
}
