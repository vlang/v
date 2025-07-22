module builtin

@[typedef]
pub struct C.FILE {}

// <string.h>
fn C.memcpy(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memcmp(const_s1 voidptr, const_s2 voidptr, n usize) int

fn C.memmove(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memset(str voidptr, c int, n usize) voidptr

@[trusted]
fn C.calloc(int, int) &u8

fn C.atoi(&char) int

fn C.malloc(int) &u8

fn C.realloc(a &u8, b int) &u8

fn C.free(ptr voidptr)

fn C.aligned_alloc(align isize, size isize) voidptr

@[noreturn; trusted]
fn C.exit(code int)

fn C.qsort(base voidptr, items usize, item_size usize, cb C.qsort_callback_func)

fn C.strlen(s &char) int

@[trusted]
fn C.isdigit(c int) bool

// stdio.h
fn C.popen(c &char, t &char) voidptr

// <libproc.h>
pub fn proc_pidpath(int, voidptr, int) int

fn C.realpath(const_path &char, resolved_path &char) &char

// fn C.chmod(byteptr, mode_t) int
fn C.chmod(path &char, mode u32) int

fn C.printf(const_format &char, opt ...voidptr) int
fn C.dprintf(fd int, const_format &char, opt ...voidptr) int
fn C.fprintf(fstream &C.FILE, const_format &char, opt ...voidptr) int
fn C.sprintf(str &char, const_format &char, opt ...voidptr) int
fn C.snprintf(str &char, size usize, const_format &char, opt ...voidptr) int
fn C.wprintf(const_format &u16, opt ...voidptr) int

// used by Android for (e)println to output to the Android log system / logcat
pub fn C.android_print(fstream voidptr, format &char, opt ...voidptr)

fn C.sscanf(str &char, const_format &char, opt ...voidptr) int
fn C.scanf(const_format &char, opt ...voidptr) int

fn C.puts(msg &char) int
@[trusted]
fn C.abs(f64) f64

fn C.fputs(msg &char, fstream &C.FILE) int

fn C.fflush(fstream &C.FILE) int

// TODO: define args in these functions
fn C.fseek(stream &C.FILE, offset int, whence int) int

fn C.fopen(filename &char, mode &char) &C.FILE

fn C.fileno(&C.FILE) int

fn C.fread(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fwrite(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fclose(stream &C.FILE) int

fn C.pclose(stream &C.FILE) int

fn C.open(path &char, flags int, mode ...int) int
fn C.close(fd int) int

fn C.strrchr(s &char, c int) &char
fn C.strchr(s &char, c int) &char

// process execution, os.process:
@[trusted]
fn C.getpid() int

@[trusted]
fn C.getuid() int

@[trusted]
fn C.geteuid() int

fn C.system(cmd &char) int

fn C.posix_spawn(child_pid &int, path &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) int

fn C.posix_spawnp(child_pid &int, exefile &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) int

fn C.execve(cmd_path &char, args voidptr, envs voidptr) int

fn C.execvp(cmd_path &char, args &&char) int

fn C._execve(cmd_path &char, args voidptr, envs voidptr) int

fn C._execvp(cmd_path &char, args &&char) int

fn C.strcmp(s1 &char, s2 &char) int

@[trusted]
fn C.fork() int

fn C.wait(status &int) int

fn C.waitpid(pid int, status &int, options int) int

@[trusted]
fn C.kill(pid int, sig int) int

fn C.setenv(&char, &char, int) int

fn C.unsetenv(&char) int

fn C.access(path &char, amode int) int

fn C.remove(filename &char) int

fn C.rmdir(path &char) int

fn C.chdir(path &char) int

fn C._wchdir(dirname &u16) int

fn C.rewind(stream &C.FILE) int

fn C.ftell(&C.FILE) isize

fn C.stat(&char, voidptr) int

fn C.lstat(path &char, buf &C.stat) int

fn C.statvfs(const_path &char, buf &C.statvfs) int

fn C.rename(old_filename &char, new_filename &char) int

fn C.fgets(str &char, n int, stream &C.FILE) int

fn C.fgetpos(&C.FILE, voidptr) int

@[trusted]
fn C.sigemptyset() int

fn C.getcwd(buf &char, size usize) &char

@[trusted]
fn C.mktime() int

fn C.gettimeofday(tv &C.timeval, tz &C.timezone) int

@[trusted]
fn C.sleep(seconds u32) u32

// fn C.usleep(usec useconds_t) int
@[trusted]
fn C.usleep(usec u32) int

@[typedef]
pub struct C.DIR {
}

fn C.opendir(&char) &C.DIR

fn C.closedir(dirp &C.DIR) int

// fn C.mkdir(path &char, mode mode_t) int
fn C.mkdir(path &char, mode u32) int

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
@[trusted]
fn C.rand() int

// C.srand seeds the internal PRNG with the given value.
@[trusted]
fn C.srand(seed u32)

fn C.atof(str &char) f64

@[trusted]
fn C.tolower(c int) int

@[trusted]
fn C.toupper(c int) int

@[trusted]
fn C.isspace(c int) int

fn C.strchr(s &char, c int) &char

@[trusted]
fn C.getchar() int

@[trusted]
fn C.putchar(int) int

fn C.strdup(s &char) &char

fn C.strncasecmp(s &char, s2 &char, n int) int

fn C.strcasecmp(s &char, s2 &char) int

fn C.strncmp(s &char, s2 &char, n int) int

@[trusted]
fn C.strerror(int) &char

@[trusted]
fn C.WIFEXITED(status int) bool

@[trusted]
fn C.WEXITSTATUS(status int) int

@[trusted]
fn C.WIFSIGNALED(status int) bool

@[trusted]
fn C.WTERMSIG(status int) int

@[trusted]
fn C.isatty(fd int) int

fn C.syscall(number int, va ...voidptr) int

fn C.sysctl(name &int, namelen u32, oldp voidptr, oldlenp voidptr, newp voidptr, newlen usize) int

// fn C.setbuf()
fn C.setbuf(voidptr, &char)

fn C.wcslen(str voidptr) usize

fn C._pclose(stream &C.FILE) int

fn C._putenv(envstring &char) int

// pthread.h
fn C.pthread_mutex_init(voidptr, voidptr) int

fn C.pthread_mutex_lock(voidptr) int

fn C.pthread_mutex_unlock(voidptr) int

fn C.pthread_mutex_destroy(voidptr) int

fn C.pthread_rwlockattr_init(voidptr) int

fn C.pthread_rwlockattr_setkind_np(voidptr, int) int

fn C.pthread_rwlockattr_setpshared(voidptr, int) int

fn C.pthread_rwlock_init(voidptr, voidptr) int

fn C.pthread_rwlock_rdlock(voidptr) int

fn C.pthread_rwlock_wrlock(voidptr) int

fn C.pthread_rwlock_unlock(voidptr) int

fn C.pthread_condattr_init(voidptr) int

fn C.pthread_condattr_setpshared(voidptr, int) int

fn C.pthread_condattr_destroy(voidptr) int

fn C.pthread_cond_init(voidptr, voidptr) int

fn C.pthread_cond_signal(voidptr) int

fn C.pthread_cond_wait(voidptr, voidptr) int

fn C.pthread_cond_timedwait(voidptr, voidptr, voidptr) int

fn C.pthread_cond_destroy(voidptr) int

fn C.sem_init(voidptr, int, u32) int

fn C.sem_post(voidptr) int

fn C.sem_wait(voidptr) int

fn C.sem_trywait(voidptr) int

fn C.sem_timedwait(voidptr, voidptr) int

fn C.sem_destroy(voidptr) int

// MacOS semaphore functions
@[trusted]
fn C.dispatch_semaphore_create(i64) voidptr

fn C.dispatch_semaphore_signal(voidptr) i64

fn C.dispatch_semaphore_wait(voidptr, u64) i64

@[trusted]
fn C.dispatch_time(u64, i64) u64

fn C.dispatch_release(voidptr)

// file descriptor based reading/writing
fn C.read(fd int, buf voidptr, count usize) int

fn C.write(fd int, buf voidptr, count usize) int

fn C.close(fd int) int

// pipes
fn C.pipe(pipefds &int) int

fn C.dup2(oldfd int, newfd int) int

// used by ios for println
fn C.WrappedNSLog(str &u8)

// absolute value
@[trusted]
fn C.abs(number int) int
