module gen

const (
	macos_libc_defs = '

// ==============================================
// macos libc definitions

#define NULL 0

#define PRId32    "ld"
#define PRId64    "lld"
#define PRIu32    "lu"
#define PRIu64    "llu"

#define FLT_EPSILON 1E-5

#define DBL_EPSILON 1E-9
#define UINT64_MAX 18446744073709551615ULL


extern void *__stdoutp;
#define stdout __stdoutp

extern void *__stderrp;
#define stderr __stderrp

extern void *__stdinp;
#define stdin __stdinp

extern char **environ;

typedef __builtin_va_list va_list; //__darwin_va_list;
//typedef __builtin_va_end va_end;
//typedef __builtin_va_start va_start;
//typedef __darwin_va_list va_list;
#define va_start  __builtin_va_start
#define va_end __builtin_va_end
#define va_list __builtin_va_list
#define va_arg __builtin_va_arg


typedef void* FILE;

typedef signed char                int8_t;
typedef short int                int16_t;
typedef int                        int32_t;
typedef long int                int64_t;
typedef unsigned char                uint8_t;
typedef unsigned short int        uint16_t;
typedef unsigned int                uint32_t;
typedef unsigned long int        uint64_t;

typedef int32_t dev_t;
typedef uint16_t mode_t;
typedef uint16_t nlink_t;
typedef uint32_t uid_t;
typedef uint32_t gid_t;
typedef int64_t off_t;
typedef int64_t blkcnt_t;
typedef int32_t blksize_t;

typedef long __darwin_time_t;
typedef long time_t;
typedef int32_t __darwin_suseconds_t;

struct timespec
{
 __darwin_time_t tv_sec;
 long tv_nsec;
};

extern int * __error(void);
#define errno (*__error())


struct stat {
	dev_t st_dev;
	mode_t st_mode;
	nlink_t st_nlink;
	uint64_t st_ino;
	uid_t st_uid;
	gid_t st_gid;
	dev_t st_rdev;

	time_t		st_atime;	/* [XSI] Time of last access */
	long		st_atimensec;	/* nsec of last access */
	time_t		st_mtime;	/* [XSI] Last data modification time */
	long		st_mtimensec;	/* last data modification nsec */
	time_t		st_ctime;	/* [XSI] Time of last status change */
	long		st_ctimensec;

/*
	struct timespec st_atimespec;
	struct timespec st_mtimespec;
	struct timespec st_ctimespec;
	struct timespec st_birthtimespec;
	*/
	off_t st_size;
	blkcnt_t st_blocks;
	blksize_t st_blksize;
	uint32_t st_flags;
	uint32_t st_gen;
	int32_t st_lspare;
	int64_t st_qspare[2];
};

#define S_IFMT 0170000
#define S_IFDIR 0040000
#define S_IFCHR 0020000

#define S_IFLNK 0xa000
#define S_IXUSR 0100
#define S_IXGRP 0010
#define S_IXOTH 0001

#define S_IFBLK 0x6000
#define S_IFIFO 0x1000
#define S_IFSOCK 0xc000
#define S_IRUSR 0x100
#define S_IWUSR 0x80
#define S_IRGRP 0x20
#define S_IWGRP 0x10
#define S_IROTH 0x4
#define S_IWOTH 0x2

#define SEEK_SET 0
#define SEEK_END 2


struct winsize {
 unsigned short ws_row;
 unsigned short ws_col;
 unsigned short ws_xpixel;
 unsigned short ws_ypixel;
};

struct tm {
 int tm_sec;
 int tm_min;
 int tm_hour;
 int tm_mday;
 int tm_mon;
 int tm_year;
 int tm_wday;
 int tm_yday;
 int tm_isdst;
 long tm_gmtoff;
 char *tm_zone;
};


struct mach_timebase_info {
 uint32_t numer;
 uint32_t denom;
};

typedef struct mach_timebase_info *mach_timebase_info_t;
typedef struct mach_timebase_info mach_timebase_info_data_t;




struct timeval
{
 __darwin_time_t tv_sec;
 __darwin_suseconds_t tv_usec;
};

typedef long unsigned int size_t;



struct dirent {
	uint64_t d_ino;
	uint64_t d_seekoff;
	uint16_t d_reclen;
	uint16_t d_namlen;
	uint8_t d_type;
	char d_name[1024];
};


#define CLOCK_MONOTONIC 6

#define TIOCGWINSZ ioctl(1, ((uint32_t)0x40000000 | ((sizeof(struct winsize) & 0x1fff) << 16) | (((\'t\')) << 8) | ((104))), &w)



#define WEXITSTATUS(waitret) (((*(int *)&(waitret)) >> 8) & 0x000000ff)
#define WIFEXITED(waitret) (((*(int *)&(waitret)) & 0177) == 0)
#define WIFSIGNALED(waitret) (((*(int *)&(waitret)) & 0177) != 0177 && ((*(int *)&(waitret)) & 0177) != 0)
#define WTERMSIG(waitret) (((*(int *)&(waitret)) & 0177))



typedef int kern_return_t;
typedef long ssize_t;
typedef int32_t useconds_t;


typedef int32_t pid_t;



// ==============================================
//  macos libc fns

char *fgets(char * restrict, int, void *);
char *getenv(const char *);
void *memcpy(void *__dst, const void *__src, size_t __n);
int vsnprintf(char * restrict __str, size_t __size, const char * restrict __format, va_list) __attribute__((__format__ (__printf__, 3, 0)));
int vsscanf(const char * restrict __str, const char * restrict __format, va_list) __attribute__((__format__ (__scanf__, 2, 0)));
int vsprintf(char * restrict, const char * restrict, va_list) __attribute__((__format__ (__printf__, 2, 0))) __attribute__((__availability__(swift, unavailable, message="Use vsnprintf instead.")));

void *malloc(size_t __size) __attribute__((__warn_unused_result__)) __attribute__((alloc_size(1)));
void *calloc(size_t __count, size_t __size) __attribute__((__warn_unused_result__)) __attribute__((alloc_size(1,2)));
void free(void *);
void *realloc(void *__ptr, size_t __size) __attribute__((__warn_unused_result__)) __attribute__((alloc_size(2)));

void *valloc(size_t) __attribute__((alloc_size(1)));
size_t strlen(const char *__s);
int access(const char *, int);
void *fopen(const char * restrict __filename, const char * restrict __mode) __asm("_" "fopen" );
int fputs(const char * restrict, void * restrict) __asm("_" "fputs" );

void *memset(void *__b, int __c, size_t __len);
void qsort(void *__base, size_t __nel, size_t __width,
     int (* _Nonnull __compar)(const void *, const void *));
uint64_t mach_absolute_time(void);
kern_return_t mach_timebase_info( mach_timebase_info_t info);

unsigned int
  sleep(unsigned int) __asm("_" "sleep" );
int usleep(useconds_t) __asm("_" "usleep" );
int stat(const char *, struct stat *);
int fclose(void *);
struct dirent *readdir(void *);
int closedir(void *) __asm("_" "closedir" );
int fseek(void *, long, int);
ssize_t read(int, void *, size_t) __asm("_" "read" );
void *memmove(void *__dst, const void *__src, size_t __len);
struct tm *localtime(const time_t *);
long strtol(const char *__str, char **__endptr, int __base);
time_t timegm(struct tm * const);
int ioctl(int, unsigned long, ...);
int gettimeofday(struct timeval * restrict, void * restrict);
int fflush(void *);
int snprintf(char * restrict __str, size_t __size, const char * restrict __format, ...) __attribute__((__format__ (__printf__, 3, 4)));
time_t time(time_t *);
//int clock_gettime(clockid_t __clock_id, struct timespec *__tp);
int clock_gettime(int __clock_id, struct timespec *__tp);
int mkdir(const char *, mode_t);
int symlink(const char *, const char *);
void *opendir(const char *);
size_t fwrite(const void * restrict __ptr, size_t __size, size_t __nitems, void * restrict __stream) __asm("_" "fwrite" );
int chmod(const char *, mode_t) __asm("_" "chmod" );
int backtrace(void**,int) __attribute__((availability(macosx,introduced=10.5)));
int memcmp(const void *__s1, const void *__s2, size_t __n);
void *popen(const char *, const char *) __asm("_" "popen" ) __attribute__((__availability__(swift, unavailable, message="Use posix_spawn APIs or NSTask instead.")));
    void(*signal(int, void (*)(int)))(int);
pid_t fork(void) __attribute__((availability(watchos,unavailable))) __attribute__((availability(tvos,unavailable)));
char *realpath(const char * restrict, char * restrict);
int chdir(const char *);
pid_t wait(int *) __asm("_" "wait" );
char *getcwd(char *, size_t);
int remove(const char *);
int system(const char *) __asm("_" "system" );
int lstat(const char *, struct stat *);
pid_t getpid(void);
ssize_t getline(char ** restrict __linep, size_t * restrict __linecapp, void * restrict __stream) __attribute__((availability(macosx,introduced=10.7)));
int rmdir(const char *);
int pclose(void *) __attribute__((__availability__(swift, unavailable, message="Use posix_spawn APIs or NSTask instead.")));
char *strerror(int __errnum) __asm("_" "strerror" );
void *fdopen(int, const char *) __asm("_" "fdopen" );
void rewind(void *);
long ftell(void *);
size_t fread(void * restrict __ptr, size_t __size, size_t __nitems, void * restrict __stream);
int unsetenv(const char *) __asm("_" "unsetenv" );
char** backtrace_symbols(void* const*,int) __attribute__((availability(macosx,introduced=10.5)));
void backtrace_symbols_fd(void* const*,int,int) __attribute__((availability(macosx,introduced=10.5)));
int toupper(int);
int tolower(int);
int isatty(int);
int setenv(const char * __name, const char * __value, int __overwrite) __asm("_" "setenv" );
void exit(int) __attribute__((__noreturn__));
int fprintf(void * restrict, const char * restrict, ...) __attribute__((__format__ (__printf__, 2, 3)));
int printf(const char * restrict, ...) __attribute__((__format__ (__printf__, 1, 2)));
int getchar(void);
int open(const char *, int, ...) __asm("_" "open" );
 int rename(const char *old, const char *new);
int  fileno(FILE *stream);

// ==============================================








'
)
