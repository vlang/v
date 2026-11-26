#define _GNU_SOURCE
#include "vpac_runtime_probe.h"
#include <errno.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <grp.h>
#include <iconv.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <limits.h>
#include <locale.h>
#include <poll.h>
#include <pthread.h>
#include <semaphore.h>
#include <spawn.h>
#include <sys/mman.h>
#include <pwd.h>
#include <regex.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/statvfs.h>
#include <sys/un.h>
#include <syslog.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <limits.h>
#include <wchar.h>
#include <wctype.h>
#define CHECK(test) do { if (!(test)) { fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #test); return __LINE__; } } while (0)
#include "fortify_probe.h"
#include "numeric_locale_probe.h"
#include "ipc_probe.h"
#include "directory_scan_probe.h"
#include "glob_probe.h"
#include "cancellation_probe.h"
#include "pthread_metadata_probe.h"
#include "shell_probe.h"

/* These calls precede V's module initializer, just like native DSO constructors. */
static int constructor_status;
#ifdef __GLIBC__
static const wchar_t *constructor_weekday;
#endif
static int early_runtime_probe(void) {
    int printed=-1;
    CHECK(call___fprintf_chk(stderr,1,"%n",&printed)==0 && printed==0);
    time_t epoch=0; struct tm tm; CHECK(gmtime_r(&epoch,&tm) && tm.tm_year==70);
    CHECK(setlocale(LC_ALL,"C.UTF-8")); CHECK(iswalpha(L'Ж') && towlower(L'Ж')==L'ж');
#ifdef __GLIBC__
    constructor_weekday=(const wchar_t *)nl_langinfo(_NL_WABDAY_1);
    CHECK(constructor_weekday && !wcscmp(constructor_weekday,L"Sun"));
#endif
    iconv_t converter=iconv_open("UTF-8","CP437"); CHECK(converter!=(iconv_t)-1);
    char source[]={(char)0x82},target[8]={0},*input=source,*output=target;
    size_t left=1,space=sizeof target;
    CHECK(iconv(converter,&input,&left,&output,&space)==0 && !memcmp(target,"\xc3\xa9",2));
    CHECK(!iconv_close(converter));
    return 0;
}
__attribute__((constructor)) static void native_constructor_probe(void) { constructor_status=early_runtime_probe(); }

static pthread_key_t key;
static pthread_once_t once=PTHREAD_ONCE_INIT;
static int once_count,destructor_count;
static pthread_mutex_t counter_mutex=PTHREAD_MUTEX_INITIALIZER;
#ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
static pthread_mutex_t static_recursive=PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
static pthread_mutex_t static_errorcheck=PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP;
static int static_recursive_count;
static void *static_recursive_worker(void *unused) {
    (void)unused;
    for(int i=0;i<200;i++) {
        if(pthread_mutex_lock(&static_recursive) || pthread_mutex_trylock(&static_recursive))return (void *)1;
        ++static_recursive_count;
        if(pthread_mutex_unlock(&static_recursive) || pthread_mutex_unlock(&static_recursive))return (void *)2;
    }
    return NULL;
}
#endif
static pthread_rwlock_t rw=PTHREAD_RWLOCK_INITIALIZER;
static void initialize_once(void) { ++once_count; }
static void destroy_value(void *p) {
    pthread_mutex_lock(&counter_mutex); ++destructor_count; pthread_mutex_unlock(&counter_mutex);
    if ((uintptr_t)p==1) pthread_setspecific(key,(void *)2);
}
static int integer_probe(void) {
#ifdef __GLIBC__
    struct random_data generator = {0}, control = {0};
    int32_t storage[32], other[32], value, expected;
    CHECK(!initstate_r(7, (char *)storage, sizeof storage, &generator));
    CHECK(!initstate_r(7, (char *)other, sizeof other, &control));
    for (int i = 0; i < 5; ++i) {
        CHECK(!random_r(&generator, &value) && !random_r(&control, &expected));
    }
    CHECK(!setstate_r((char *)storage, &generator));
    CHECK(!random_r(&generator, &value) && !random_r(&control, &expected) && value == expected);
#endif
    /* Volatile function pointers exercise the public ABI rather than C builtins. */
    int (*volatile absolute)(int)=abs;
    long (*volatile absolute_long)(long)=labs;
    long long (*volatile absolute_ll)(long long)=llabs;
    div_t (*volatile divide)(int,int)=div;
    ldiv_t (*volatile divide_long)(long,long)=ldiv;
    lldiv_t (*volatile divide_ll)(long long,long long)=lldiv;
    CHECK(absolute(-INT_MAX)==INT_MAX && absolute(INT_MAX)==INT_MAX && absolute(0)==0);
    CHECK(absolute_long(-LONG_MAX)==LONG_MAX && absolute_long(LONG_MAX)==LONG_MAX && absolute_long(0)==0);
    CHECK(absolute_ll(-LLONG_MAX)==LLONG_MAX && absolute_ll(LLONG_MAX)==LLONG_MAX && absolute_ll(0)==0);
    for(int first=-19;first<=19;first+=19)for(int second=-3;second<=3;second+=6) {
        div_t a=divide(first,second); CHECK(a.quot==first/second && a.rem==first%second);
        long wide=(long)first*1000000000L;
        ldiv_t b=divide_long(wide,second); CHECK(b.quot==wide/second && b.rem==wide%second);
        lldiv_t c=divide_ll(wide,second); CHECK(c.quot==wide/second && c.rem==wide%second);
    }
    return 0;
}
static int wait_probe(void) {
    int status=0; pid_t child=fork(); CHECK(child>=0);
    if(!child)_exit(37);
    struct { uint32_t before; int value; uint32_t after; } guarded={0x12345678,0,0x76543210};
    CHECK(wait(&guarded.value)==child && WIFEXITED(guarded.value) && WEXITSTATUS(guarded.value)==37);
    CHECK(guarded.before==0x12345678 && guarded.after==0x76543210);
    int pipefd[2]; CHECK(!pipe(pipefd)); child=fork(); CHECK(child>=0);
    if(!child) { close(pipefd[1]); char value; if(read(pipefd[0],&value,1)!=1)_exit(99); _exit(13); }
    close(pipefd[0]); status=12345;
    CHECK(!waitpid(child,&status,WNOHANG) && status==12345);
    siginfo_t info; memset(&info,0xa5,sizeof info);
    CHECK(!waitid(P_PID,child,&info,WEXITED|WNOHANG) && info.si_pid==0);
    CHECK(!kill(child,SIGSTOP));
    CHECK(!waitid(P_PID,child,&info,WSTOPPED) && info.si_pid==child && info.si_code==CLD_STOPPED && info.si_status==SIGSTOP);
    CHECK(!kill(child,SIGCONT));
    CHECK(waitpid(child,&status,WCONTINUED)==child && WIFCONTINUED(status));
    CHECK(write(pipefd[1],"x",1)==1); close(pipefd[1]);
    CHECK(!waitid(P_PID,child,&info,WEXITED|WNOWAIT) && info.si_pid==child && info.si_code==CLD_EXITED && info.si_status==13);
    struct rusage usage; memset(&usage,0xa5,sizeof usage);
    CHECK(wait4(child,&status,0,&usage)==child && WIFEXITED(status) && WEXITSTATUS(status)==13);
    CHECK(usage.ru_utime.tv_sec>=0 && usage.ru_utime.tv_usec>=0 && usage.ru_utime.tv_usec<1000000);
    errno=0; CHECK(waitpid(child,NULL,0)==-1 && errno==ECHILD);
    errno=0; CHECK(waitid(P_PID,child,&info,WEXITED)==-1 && errno==ECHILD);
    child=fork(); CHECK(child>=0); if(!child)_exit(55);
    CHECK(wait3(&status,0,&usage)==child && WIFEXITED(status) && WEXITSTATUS(status)==55);
    errno=0; CHECK(waitid((idtype_t)99,0,&info,WEXITED)==-1 && errno==EINVAL);
    return 0;
}
static void *thread_worker(void *p) {
    if (pthread_once(&once,initialize_once) || pthread_setspecific(key,(void *)1) ||
        pthread_getspecific(key)!=(void *)1 || pthread_rwlock_rdlock(&rw) || pthread_rwlock_unlock(&rw)) return (void *)99;
    if ((uintptr_t)p & 1) pthread_exit(p);
    return p;
}
static pthread_key_t shutdown_key;
static int shutdown_fd, shutdown_mode, shutdown_destructions;
static pthread_t shutdown_initial, shutdown_final;
static void shutdown_destructor(void *value) {
    if (value == &shutdown_key) ++shutdown_destructions;
}
static void shutdown_callback(void) {
    void *expected = shutdown_mode == 0 ? &shutdown_key : NULL;
    char marker = pthread_equal(pthread_self(), shutdown_final) &&
        pthread_getspecific(shutdown_key) == expected &&
        shutdown_destructions == (shutdown_mode != 0) ? 'A' : 'X';
    (void)write(shutdown_fd, &marker, 1);
}
static void *shutdown_last_worker(void *unused) {
    (void)unused;
    shutdown_final = pthread_self();
    if (pthread_join(shutdown_initial, NULL)) _exit(94);
    return NULL;
}
static int thread_shutdown_probe(void) {
    CHECK(!fflush(NULL));
    for (int mode = 0; mode < 3; ++mode) {
        int channel[2], status;
        CHECK(!pipe(channel));
        pid_t child = fork();
        CHECK(child >= 0);
        if (!child) {
            close(channel[0]);
            shutdown_fd = channel[1];
            shutdown_mode = mode;
            shutdown_final = shutdown_initial = pthread_self();
            FILE *stream = fdopen(dup(shutdown_fd), "w");
            if (!stream || setvbuf(stream, NULL, _IOFBF, 4096) || fputc('F', stream) == EOF)
                _exit(91);
            if (pthread_key_create(&shutdown_key, shutdown_destructor) ||
                pthread_setspecific(shutdown_key, &shutdown_key) || atexit(shutdown_callback))
                _exit(92);
            if (!mode) exit(0);
            if (mode == 2) {
                pthread_t worker;
                if (pthread_create(&worker, NULL, shutdown_last_worker, NULL)) _exit(93);
            }
            pthread_exit(NULL);
        }
        CHECK(!close(channel[1]));
        CHECK(waitpid(child, &status, 0) == child && WIFEXITED(status) && !WEXITSTATUS(status));
        char markers[3];
        CHECK(read(channel[0], markers, sizeof markers) == 2 && !memcmp(markers, "AF", 2));
        CHECK(!close(channel[0]));
    }
    return 0;
}
static int threads_probe(void) {
    CHECK(!thread_shutdown_probe());
#ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
    pthread_t initializers[2];
    for(int i=0;i<2;i++)CHECK(!pthread_create(&initializers[i],NULL,static_recursive_worker,NULL));
    for(int i=0;i<2;i++) { void *result=(void *)1; CHECK(!pthread_join(initializers[i],&result) && !result); }
    CHECK(static_recursive_count==400);
    CHECK(pthread_mutex_unlock(&static_errorcheck)==EPERM);
    CHECK(!pthread_mutex_lock(&static_errorcheck));
    CHECK(pthread_mutex_lock(&static_errorcheck)==EDEADLK && pthread_mutex_trylock(&static_errorcheck)==EBUSY);
    CHECK(!pthread_mutex_unlock(&static_errorcheck) && !pthread_mutex_destroy(&static_errorcheck));
    CHECK(!pthread_mutex_destroy(&static_recursive));
#endif
    CHECK(!pthread_key_create(&key,destroy_value));
    pthread_attr_t a;
    CHECK(!pthread_attr_init(&a));
    CHECK(pthread_attr_setstacksize(&a,1)==EINVAL);
    CHECK(!pthread_attr_setstacksize(&a,256*1024));
    for (uintptr_t i=2;i<66;i++) {
        pthread_t thread; void *result=NULL;
        CHECK(!pthread_create(&thread,&a,thread_worker,(void *)i));
        CHECK(!pthread_join(thread,&result)); CHECK(result==(void *)i);
    }
    CHECK(once_count==1 && destructor_count==128);
    CHECK(!pthread_attr_destroy(&a));
    CHECK(!pthread_rwlock_wrlock(&rw));
    CHECK(pthread_rwlock_tryrdlock(&rw)==EBUSY);
    CHECK(pthread_rwlock_trywrlock(&rw)==EBUSY);
    CHECK(!pthread_rwlock_unlock(&rw));
    CHECK(!pthread_rwlock_destroy(&rw));
    CHECK(!pthread_key_delete(key));
    puts("pthread create/join/exit, TLS destructors, once and rwlocks passed");
    return 0;
}
static int cookie_closes;
struct cookie { char data[32]; size_t size,at; };
static ssize_t cookie_read(void *p,char *out,size_t count) {
    struct cookie *c=p; if(count>c->size-c->at) count=c->size-c->at;
    memcpy(out,c->data+c->at,count); c->at+=count; return count;
}
static ssize_t cookie_write(void *p,const char *in,size_t count) {
    struct cookie *c=p; if(count>sizeof c->data-c->at) count=sizeof c->data-c->at;
    memcpy(c->data+c->at,in,count); c->at+=count; if(c->at>c->size)c->size=c->at; return count;
}
static int cookie_seek(void *p,off64_t *offset,int whence) {
    struct cookie *c=p; off64_t at=*offset+(whence==SEEK_CUR?c->at:whence==SEEK_END?c->size:0);
    if(at<0 || at>(off64_t)sizeof c->data) { errno=EINVAL; return -1; }
    c->at=at; *offset=at; return 0;
}
static int cookie_close(void *p) { (void)p; ++cookie_closes; return 0; }
static ssize_t partial_read_error(void *cookie, char *out, size_t size) {
    int *calls = cookie;
    if ((*calls)++ == 0 && size) {
        out[0] = 'x';
        return 1;
    }
    errno = EIO;
    return -1;
}
static int stdio_probe(void) {
    struct cookie c={0};
    FILE *f=fopencookie(&c,"w+",(cookie_io_functions_t){cookie_read,cookie_write,cookie_seek,cookie_close});
    CHECK(f); CHECK(fwrite("cookie\n",1,7,f)==7); CHECK(!fseeko(f,0,SEEK_SET));
    char output[32]; CHECK(fgets(output,sizeof output,f)); CHECK(!strcmp(output,"cookie\n"));
    CHECK(ftello(f)==7); CHECK(!fclose(f)); CHECK(cookie_closes==1);
    f=tmpfile(); CHECK(f); CHECK(fputs("temporary",f)>=0); rewind(f);
    CHECK(fread(output,1,9,f)==9 && !memcmp(output,"temporary",9)); CHECK(!fclose(f));
    int calls = 0;
    f = fopencookie(&calls, "r", (cookie_io_functions_t){.read = partial_read_error});
    CHECK(f && !fgets(output, sizeof output, f) && ferror(f) && errno == EIO);
    CHECK(!fclose(f));
    f = tmpfile();
    int (*volatile print)(FILE *, const char *, ...) = fprintf;
    CHECK(f && print(f, "%s", "") == 0 && fwide(f, 0) < 0);
    CHECK(fputs("abcdef", f) >= 0 && !fseek(f, 0, SEEK_SET));
    int shared = dup(fileno(f));
    CHECK(shared >= 0 && fgetc(f) == 'a' && !fclose(f));
    CHECK(lseek(shared, 0, SEEK_CUR) == 1 && !close(shared));
    puts("cookie streams and tmpfile passed"); return 0;
}
static volatile sig_atomic_t signal_seen;
static void signal_handler(int number,siginfo_t *info,void *context) { (void)context; if(info->si_signo==number)signal_seen=number; }
static int system_probe(void) {
    struct sigaction a={0},old={0}; a.sa_sigaction=signal_handler; a.sa_flags=SA_SIGINFO;
    CHECK(!sigemptyset(&a.sa_mask)); CHECK(!sigaction(SIGUSR1,&a,&old)); CHECK(!kill(getpid(),SIGUSR1));
    CHECK(signal_seen==SIGUSR1);
    sigset_t blocked,previous,pending; siginfo_t received={0};
    CHECK(!sigemptyset(&blocked) && !sigaddset(&blocked,SIGUSR2) && !sigaddset(&blocked,SIGUSR1));
    CHECK(!pthread_sigmask(SIG_BLOCK,&blocked,&previous));
    CHECK(!sigqueue(getpid(),SIGUSR2,(union sigval){.sival_int=12345}));
    memset(&pending,0xff,sizeof pending); CHECK(!sigpending(&pending));
    CHECK(sigismember(&pending,SIGUSR2)==1);
    struct timespec immediate={0};
    CHECK(sigtimedwait(&blocked,&received,&immediate)==SIGUSR2);
    CHECK(received.si_code==SI_QUEUE && received.si_pid==getpid() && received.si_uid==getuid() && received.si_value.sival_int==12345);
    errno=0; CHECK(sigtimedwait(&blocked,NULL,&immediate)==-1 && errno==EAGAIN);
    struct timespec invalid_timeout={.tv_nsec=1000000000};
    CHECK(sigtimedwait(&blocked,NULL,&invalid_timeout)==-1 && errno==EINVAL);
    CHECK(!kill(getpid(),SIGUSR2)); int number=0; errno=E2BIG;
    CHECK(!sigwait(&blocked,&number) && number==SIGUSR2 && errno==E2BIG);
    CHECK(!kill(getpid(),SIGUSR2)); CHECK(sigwaitinfo(&blocked,&received)==SIGUSR2);
    CHECK(!sigpending(&pending) && !sigismember(&pending,SIGUSR2));
    signal_seen=0; CHECK(!kill(getpid(),SIGUSR1));
    errno=0; CHECK(sigsuspend(&previous)==-1 && errno==EINTR && signal_seen==SIGUSR1);
    sigset_t restored; CHECK(!pthread_sigmask(SIG_SETMASK,NULL,&restored));
    CHECK(sigismember(&restored,SIGUSR1)==1);
    CHECK(!pthread_sigmask(SIG_SETMASK,&previous,NULL));
    CHECK(!sigaction(SIGUSR1,&old,NULL));
    struct statvfs v; CHECK(!statvfs("/tmp",&v)); CHECK(v.f_bsize>0 && v.f_frsize>0 && v.f_namemax>0);
    struct passwd pw,*p=NULL; char buffer[16384];
    CHECK(!getpwuid_r(getuid(),&pw,buffer,sizeof buffer,&p)); CHECK(p && p->pw_uid==getuid());
    CHECK(getpwnam(p->pw_name));
    struct group gr,*g=NULL; CHECK(!getgrgid_r(getgid(),&gr,buffer,sizeof buffer,&g)); CHECK(g && g->gr_gid==getgid());
    CHECK(getgrnam(g->gr_name));
    puts("sigaction, statvfs and account lookup passed"); return 0;
}
static volatile int credentials_worker_ready,credentials_worker_stop;
static void *credentials_worker(void *unused) {
    (void)unused; int good=!strcmp(strsignal(-777),"Unknown signal -777");
    __atomic_store_n(&credentials_worker_ready,good?1:2,__ATOMIC_RELEASE);
    while(!__atomic_load_n(&credentials_worker_stop,__ATOMIC_ACQUIRE))sched_yield();
    return NULL;
}
static int filesystem_calls_probe(void) {
    mode_t previous_umask=umask(0077);
    char directory[]="/tmp/vlibc-native-fs-XXXXXX";
    CHECK(mkdtemp(directory)); struct stat st; CHECK(!stat(directory,&st) && (st.st_mode&0777)==0700);
    int fd=open(directory,O_RDONLY|O_DIRECTORY|O_CLOEXEC); CHECK(fd>=0);
    CHECK(!mkdirat(fd,"nested",0700));
    int file=openat(fd,"original",O_CREAT|O_EXCL|O_RDWR|O_CLOEXEC,0600); CHECK(file>=0);
    CHECK(lseek(file,3,SEEK_SET)==3 && pwrite(file,"positioned",10,0)==10 && lseek(file,0,SEEK_CUR)==3);
    CHECK(!fchownat(fd,"original",getuid(),getgid(),0));
    CHECK(!faccessat(fd,"original",R_OK|W_OK,AT_EACCESS));
    CHECK(!symlinkat("original",fd,"symbolic"));
    CHECK(!faccessat(fd,"symbolic",F_OK,AT_SYMLINK_NOFOLLOW));
    CHECK(!linkat(fd,"original",fd,"hard",0));
    errno=0; CHECK(renameat2(fd,"original",fd,"hard",1)==-1 && errno==EEXIST);
    CHECK(!renameat2(fd,"original",fd,"renamed",0));
    CHECK(!renameat(fd,"renamed",fd,"original"));
    CHECK(!renameat(fd,"original",fd,"renamed"));
    CHECK(!faccessat(fd,"symbolic",F_OK,AT_SYMLINK_NOFOLLOW));
    errno=0; CHECK(faccessat(fd,"symbolic",F_OK,0)==-1 && errno==ENOENT);
    char path[PATH_MAX]; snprintf(path,sizeof path,"%s/renamed",directory);
    struct utimbuf stamp={123456,234567}; CHECK(!utime(path,&stamp));
    CHECK(!fstat(file,&st) && st.st_atime==stamp.actime && st.st_mtime==stamp.modtime);
    CHECK(!close(file)); CHECK(!unlinkat(fd,"symbolic",0)); CHECK(!unlinkat(fd,"hard",0));
    CHECK(!unlinkat(fd,"renamed",0)); CHECK(!unlinkat(fd,"nested",AT_REMOVEDIR)); CHECK(!close(fd));
    CHECK(!rmdir(directory));
    for(int i=0;i<16;i++) {
        char pattern[]="/tmp/vlibc-native-tmp-XXXXXX.suffix";
        int tmp=mkostemps(pattern,7,O_CLOEXEC|O_RDONLY); CHECK(tmp>=0);
        CHECK((fcntl(tmp,F_GETFD)&FD_CLOEXEC) && (fcntl(tmp,F_GETFL)&O_ACCMODE)==O_RDWR);
        CHECK(!fstat(tmp,&st) && (st.st_mode&0777)==0600);
        CHECK(!close(tmp)&&!unlink(pattern));
    }
    char invalid[]="/tmp/XX"; errno=0; CHECK(!mkdtemp(invalid) && errno==EINVAL);
    CHECK(!strcmp(strsignal(SIGPIPE),"Broken pipe"));
    CHECK(!strcmp(strsignal(-2147483647-1),"Unknown signal -2147483648"));
    CHECK(!strcmp(strsignal(SIGRTMIN),"Real-time signal 0"));
    umask(previous_umask);
    char *description=strsignal(-999);
    pthread_t worker; CHECK(!pthread_create(&worker,NULL,credentials_worker,NULL));
    while(!__atomic_load_n(&credentials_worker_ready,__ATOMIC_ACQUIRE))sched_yield();
    CHECK(__atomic_load_n(&credentials_worker_ready,__ATOMIC_ACQUIRE)==1 && !strcmp(description,"Unknown signal -999"));
#ifndef V_RUNTIME_HOSTED_ORACLE
    errno=0; CHECK(setuid(getuid())==-1 && errno==ENOTSUP);
#endif
    pid_t child=fork(); CHECK(child>=0);
    if(child==0) {
        int success=!setuid(getuid())&&!setgid(getgid())&&!seteuid(geteuid())&&!setegid(getegid())
            &&!setreuid((uid_t)-1,geteuid())&&!setregid((gid_t)-1,getegid())
            &&!setresuid((uid_t)-1,geteuid(),(uid_t)-1)&&!setresgid((gid_t)-1,getegid(),(gid_t)-1);
        errno=0; success=success&&seteuid((uid_t)-1)==-1&&errno==EINVAL;
        errno=0; int groups_result=setgroups(65537,NULL);
        success=success&&groups_result==-1&&(errno==EINVAL||errno==EPERM);
#ifndef V_RUNTIME_HOSTED_ORACLE
        errno=0; success=success&&setgroups((size_t)1<<32,NULL)==-1&&errno==EINVAL;
#endif
        _exit(success?0:1);
    }
    int status=0; CHECK(waitpid(child,&status,0)==child && WIFEXITED(status) && WEXITSTATUS(status)==0);
    __atomic_store_n(&credentials_worker_stop,1,__ATOMIC_RELEASE); CHECK(!pthread_join(worker,NULL));
    puts("directory-relative files, positioned writes, temporary paths and credential guards passed");
    return 0;
}
static int interfaces_probe(void) {
    for(int pass=0;pass<16;pass++) {
        struct ifaddrs *list=NULL; CHECK(!getifaddrs(&list)); int loopback=0;
        for(struct ifaddrs *a=list;a;a=a->ifa_next) {
            CHECK(a->ifa_name && *a->ifa_name);
            if(a->ifa_flags&IFF_LOOPBACK) { loopback=1; CHECK(if_nametoindex(a->ifa_name)>0); }
            if(a->ifa_addr && a->ifa_addr->sa_family==AF_INET6) {
                struct sockaddr_in6 *s=(struct sockaddr_in6 *)a->ifa_addr;
                if(IN6_IS_ADDR_LOOPBACK(&s->sin6_addr)) CHECK(s->sin6_scope_id==0);
            }
        }
        CHECK(loopback); freeifaddrs(list);
    }
    int first=dup(2),second=dup(2); CHECK(first>=3 && second>first);
    pid_t child=fork(); CHECK(child>=0);
    if(child==0) { closefrom(first); _exit(fcntl(first,F_GETFD)==-1 && errno==EBADF && fcntl(second,F_GETFD)==-1 && errno==EBADF ? 0 : 1); }
    int status=0; CHECK(waitpid(child,&status,0)==child && WIFEXITED(status) && WEXITSTATUS(status)==0);
    CHECK(!close(first) && !close(second));
    puts("interface snapshots, names and closefrom passed"); return 0;
}
static int calendar_probe(void) {
    CHECK(!setenv("TZ","UTC0",1)); tzset();
    time_t large = (time_t)1 << 53;
    CHECK(difftime(large + 1, large) == 1 && difftime(large, large + 1) == -1);
    time_t epoch=0; struct tm t;
    CHECK(gmtime_r(&epoch,&t)); CHECK(t.tm_year==70 && t.tm_mon==0 && t.tm_mday==1 && t.tm_wday==4 && t.tm_yday==0);
    char buffer[64]; CHECK(asctime_r(&t,buffer)); CHECK(!strcmp(buffer,"Thu Jan  1 00:00:00 1970\n"));
    CHECK(timegm(&t)==0);
    t=(struct tm){.tm_year=70,.tm_mon=0,.tm_mday=1,.tm_isdst=1};
    CHECK(mktime(&t)==-3600 && t.tm_year==69 && t.tm_mon==11 && t.tm_mday==31 && t.tm_hour==23);
    t=(struct tm){.tm_year=120,.tm_mon=14,.tm_mday=0,.tm_hour=25,.tm_min=70,.tm_sec=80};
    time_t normalized=timegm(&t); struct tm canonical; CHECK(gmtime_r(&normalized,&canonical));
    CHECK(canonical.tm_year==t.tm_year && canonical.tm_mon==t.tm_mon && canonical.tm_mday==t.tm_mday && t.tm_hour==2 && t.tm_min==11 && t.tm_sec==20);
    CHECK(!setenv("TZ","EST5EDT,M3.2.0,M11.1.0",1));
    t=(struct tm){.tm_year=70,.tm_mday=1,.tm_isdst=-1};
    CHECK(mktime(&t)==18000 && timezone==18000 && daylight &&
          !strcmp(tzname[0],"EST") && !strcmp(tzname[1],"EDT"));
    epoch=1719792000; CHECK(localtime_r(&epoch,&t)); CHECK(t.tm_gmtoff==-14400 && t.tm_isdst>0);
    CHECK(mktime(&t)==epoch); CHECK(!strcmp(t.tm_zone,"EDT"));
    t.tm_isdst=0; CHECK(mktime(&t)==epoch+3600 && t.tm_isdst>0);
    CHECK(!setenv("TZ","JST-9",1)); CHECK(localtime(&epoch)); CHECK(!strcmp(tzname[0],"JST")); CHECK(localtime_r(&epoch,&t)); CHECK(t.tm_gmtoff==32400 && t.tm_isdst==0);
    CHECK(!setenv("TZ","UTC0",1)); tzset();
    CHECK(!setenv("TZ", "EST5", 1));
    tzset();
    struct tm midnight = {.tm_year = 70, .tm_mday = 1, .tm_isdst = -1};
    CHECK(strftime(buffer, sizeof buffer, "%s", &midnight) == 5 && !strcmp(buffer, "18000"));
    CHECK(!setenv("TZ", "UTC0", 1));
    tzset();
    CHECK(clock()!=(clock_t)-1); struct timespec resolution; CHECK(!clock_getres(CLOCK_MONOTONIC,&resolution));
    puts("calendar, timezone reload and clock APIs passed"); return 0;
}
static int strings_probe(void) {
    char b[32]; CHECK(stpncpy(b,"ab",5)==b+2 && !memcmp(b,"ab\0\0\0",5));
    CHECK(strlcpy(b,"abcdef",4)==6 && !strcmp(b,"abc")); CHECK(strlcat(b,"def",sizeof b)==6 && !strcmp(b,"abcdef"));
    const char memory[]= "abca"; CHECK(memrchr(memory,'a',4)==memory+3);
    CHECK(strverscmp("v2","v10")<0 && strverscmp("v01","v1")<0);
    char input[]="::alpha:beta::",*save=NULL; CHECK(!strcmp(strtok_r(input,":",&save),"alpha"));
    CHECK(!strcmp(strtok_r(NULL,":",&save),"beta")); CHECK(!strtok_r(NULL,":",&save));
    CHECK(setlocale(LC_ALL,"C.UTF-8")); CHECK(iswalpha(L'Ж') && towlower(L'Ж')==L'ж');
    CHECK(wcwidth(L'界')==2 && wcwidth(0x301)==0 && wcwidth(L'\n')==-1);
    const char *multibyte="A\xd0\x96"; wchar_t w[4]={0}; mbstate_t state={0};
    CHECK(mbsnrtowcs(w,&multibyte,3,4,&state)==2 && w[0]==L'A' && w[1]==L'Ж');
    const wchar_t *wide=w; char encoded[8]; memset(&state,0,sizeof state);
    CHECK(wcsnrtombs(encoded,&wide,3,sizeof encoded,&state)==3 && !strcmp(encoded,"A\xd0\x96"));
    puts("string and wide-character APIs passed"); return 0;
}
static int encoding_probe(void) {
    iconv_t converter=iconv_open("UTF-8","CP437"); CHECK(converter!=(iconv_t)-1);
    char source[]={ (char)0x82,(char)0xe1 }; char output[16]={0}; char *in=source,*out=output; size_t left=2,space=sizeof output;
    CHECK(iconv(converter,&in,&left,&out,&space)==0 && left==0 && out-output==4 && !memcmp(output,"\xc3\xa9\xc3\x9f",4));
    CHECK(!iconv_close(converter));
    converter=iconv_open("UTF-16LE","UTF-8"); CHECK(converter!=(iconv_t)-1);
    in="\xf0\x9f\x98\x80"; left=4; out=output; space=2; errno=0;
    CHECK(iconv(converter,&in,&left,&out,&space)==(size_t)-1 && errno==E2BIG && left==4 && out==output);
    space=sizeof output; CHECK(!iconv(converter,&in,&left,&out,&space)); CHECK(out-output==4 && !memcmp(output,"\x3d\xd8\0\xde",4));
    in="\xf0\x9f"; left=2; out=output; space=sizeof output; errno=0;
    CHECK(iconv(converter,&in,&left,&out,&space)==(size_t)-1 && errno==EINVAL && left==2);
    in="\xc0\xaf"; left=2; errno=0;
    CHECK(iconv(converter,&in,&left,&out,&space)==(size_t)-1 && errno==EILSEQ && left==2);
    CHECK(!iconv_close(converter));
    puts("iconv round trips, output limits and invalid sequences passed"); return 0;
}
static int regex_probe(void) {
    const struct { const char *pattern,*text; int flags,expected; } cases[]={
        {"(ab|a)+([0-9]{2,4})","xxaba123yy",REG_EXTENDED,0},
        {"^a.*b$","a\nb",REG_EXTENDED|REG_NEWLINE,REG_NOMATCH},
        {"^b$","a\nb\nc",REG_EXTENDED|REG_NEWLINE,0},
        {"[[:alpha:]_][[:alnum:]_]*","23_Ж",REG_EXTENDED,0},
        {"\\(ab\\)\\1","zzabab",0,0},
        {"(a|aa)(a?)","aa",REG_EXTENDED,0},
        {"(a*)*","aaa",REG_EXTENDED,0},
        {"^ж+$","Жж",REG_EXTENDED|REG_ICASE,0},
        {"^Σ$","ς",REG_EXTENDED|REG_ICASE,0},
        {"^[Σ]$","ς",REG_EXTENDED|REG_ICASE,0},
        {"^[ς]$","σ",REG_EXTENDED|REG_ICASE,0},
        {"^[^ς]$","Σ",REG_EXTENDED|REG_ICASE,REG_NOMATCH},
        {"^(Σ)\\1$","Σς",REG_EXTENDED|REG_ICASE,0},
        {"^s$","ſ",REG_EXTENDED|REG_ICASE,0},
        {"^K$","k",REG_EXTENDED|REG_ICASE,REG_NOMATCH},
        {"[[:lower:]]+","ABCж",REG_EXTENDED|REG_ICASE,0},
    };
    for(size_t i=0;i<sizeof cases/sizeof *cases;i++) {
        regex_t re; CHECK(!regcomp(&re,cases[i].pattern,cases[i].flags));
        regmatch_t m[4]; int code=regexec(&re,cases[i].text,4,m,0); CHECK(code==cases[i].expected);
        printf("regex %zu %d",i,code); if(!code)for(size_t j=0;j<4;j++)printf(" %d:%d",(int)m[j].rm_so,(int)m[j].rm_eo); puts("");
        regfree(&re);
    }
    regex_t bounded;
    regmatch_t range = {1, 2};
    CHECK(!regcomp(&bounded, "^a", REG_EXTENDED | REG_NEWLINE));
    CHECK(regexec(&bounded, "xa", 1, &range, REG_STARTEND) == REG_NOMATCH);
    range = (regmatch_t){1, 2};
    CHECK(!regexec(&bounded, "\na", 1, &range, REG_STARTEND) && range.rm_so == 1 && range.rm_eo == 2);
    regfree(&bounded);
    regex_t basic;
    CHECK(!regcomp(&basic,"^*abc",0));
    CHECK(!regexec(&basic,"*abc",0,NULL,0) && regexec(&basic,"abc",0,NULL,0)==REG_NOMATCH);
    regfree(&basic);
    CHECK(fnmatch("a[\\-z]c","a-c",0)==0 && fnmatch("a[\\-z]c","abc",0)==FNM_NOMATCH);
    CHECK(fnmatch("[\\]]","]",0)==0 && fnmatch("[\\]]","]",FNM_NOESCAPE)==FNM_NOMATCH);
    CHECK(fnmatch("a/*/[[:digit:]]?.v","a/b/12.v",FNM_PATHNAME)==0);
    CHECK(fnmatch("*",".hidden",FNM_PERIOD)==FNM_NOMATCH);
    CHECK(fnmatch("*.*",".hidden",FNM_PERIOD)==FNM_NOMATCH);
    CHECK(fnmatch("*","a/b",FNM_PATHNAME)==FNM_NOMATCH);
    CHECK(fnmatch("[a-z]","Ж",FNM_CASEFOLD)==FNM_NOMATCH);
    CHECK(fnmatch("Ж?","жЯ",FNM_CASEFOLD)==0);
    puts("regex and fnmatch passed"); return 0;
}
static struct sockaddr_un logger_address;
static int logger_enabled;
extern int __real_connect(int,const struct sockaddr *,socklen_t);
int __wrap_connect(int fd,const struct sockaddr *address,socklen_t length) {
    if(logger_enabled && address->sa_family==AF_UNIX && !strcmp(((const struct sockaddr_un *)address)->sun_path,"/dev/log"))
        return __real_connect(fd,(const struct sockaddr *)&logger_address,sizeof logger_address);
    return __real_connect(fd,address,length);
}
static int logger_probe(const char *directory) {
    memset(&logger_address,0,sizeof logger_address); logger_address.sun_family=AF_UNIX;
    snprintf(logger_address.sun_path,sizeof logger_address.sun_path,"%s/vlibc-log-%ld",directory,(long)getpid());
    int receiver=socket(AF_UNIX,SOCK_DGRAM|SOCK_CLOEXEC,0); CHECK(receiver>=0);
    CHECK(!bind(receiver,(struct sockaddr *)&logger_address,sizeof logger_address)); logger_enabled=1;
    openlog("vpac-probe",LOG_PID,LOG_USER); errno=ENOENT; syslog(LOG_WARNING,"native failure: %m"); CHECK(errno==ENOENT);
    struct pollfd ready={receiver,POLLIN,0}; CHECK(poll(&ready,1,1000)==1);
    char output[1024]={0}; ssize_t n=recv(receiver,output,sizeof output-1,0); CHECK(n>0);
    CHECK(strstr(output,"vpac-probe[") && strstr(output,"native failure: No such file or directory"));
    int prior=setlogmask(LOG_MASK(LOG_ERR)); syslog(LOG_WARNING,"filtered"); CHECK(poll(&ready,1,0)==0); setlogmask(prior);
    closelog(); logger_enabled=0; CHECK(!close(receiver)); CHECK(!unlink(logger_address.sun_path));
    puts("syslog private socket, errno formatting and filtering passed"); return 0;
}
static int spawn_abi_probe(void) {
    int pipefd[2]; CHECK(!pipe2(pipefd, O_CLOEXEC));
    posix_spawn_file_actions_t actions;
    posix_spawnattr_t attr;
    CHECK(!posix_spawn_file_actions_init(&actions) && !posix_spawnattr_init(&attr));
    int policy;
    CHECK(!posix_spawnattr_setschedpolicy(&attr, SCHED_FIFO));
    CHECK(posix_spawnattr_setschedpolicy(&attr, -1) == EINVAL);
    CHECK(!posix_spawnattr_getschedpolicy(&attr, &policy) && policy == SCHED_FIFO);
    CHECK(!posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDOUT_FILENO));
    CHECK(!posix_spawn_file_actions_addclose(&actions, pipefd[0]));
    CHECK(!posix_spawn_file_actions_addclose(&actions, pipefd[1]));
    char path[] = "/dev/null";
    CHECK(!posix_spawn_file_actions_addopen(&actions, STDIN_FILENO, path, O_RDONLY, 0));
    path[0] = 'x'; /* addopen owns a snapshot. */
    sigset_t mask, readback;
    sigemptyset(&mask); sigaddset(&mask, SIGUSR1);
    CHECK(!posix_spawnattr_setsigmask(&attr, &mask) && !posix_spawnattr_getsigmask(&attr, &readback));
    CHECK(sigismember(&readback, SIGUSR1) == 1);
    CHECK(!posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETPGROUP));
    CHECK(!posix_spawnattr_setpgroup(&attr, 0));
    char *args[] = {"sh", "-c", "printf %s \"$SPAWN_TEST\"", NULL};
    char *env[] = {"SPAWN_TEST=child env", NULL};
    pid_t pid;
    CHECK(!posix_spawnp(&pid, "sh", &actions, &attr, args, env));
    CHECK(!close(pipefd[1]));
    char buffer[32] = {0};
    CHECK(read(pipefd[0], buffer, sizeof buffer) == 9 && !strcmp(buffer,"child env"));
    int status; CHECK(waitpid(pid, &status, 0) == pid && WIFEXITED(status) && !WEXITSTATUS(status));
    CHECK(!close(pipefd[0]) && !posix_spawn_file_actions_destroy(&actions));
    CHECK(!posix_spawnattr_destroy(&attr));
    pid = -1;
    CHECK(posix_spawn(&pid, "/does-not-exist-vlibc", NULL, NULL, args, env) == ENOENT && pid == -1);
    puts("posix_spawn file actions, attributes, PATH and child errors passed");
    return 0;
}
static int fork_sequence;
static void first_prepare(void) { fork_sequence = fork_sequence * 10 + 1; }
static void last_prepare(void) { fork_sequence = fork_sequence * 10 + 2; }
static void first_parent(void) { fork_sequence = fork_sequence * 10 + 3; }
static void last_parent(void) { fork_sequence = fork_sequence * 10 + 4; }
static void first_child(void) { fork_sequence = fork_sequence * 10 + 5; }
static void last_child(void) { fork_sequence = fork_sequence * 10 + 6; }

static int stack_contains_local(void) {
    pthread_attr_t attr;
    void *base;
    size_t size, guard;
    char sample;
    CHECK(!pthread_getattr_np(pthread_self(), &attr));
    CHECK(!pthread_attr_getstack(&attr, &base, &size));
    CHECK((uintptr_t)&sample >= (uintptr_t)base && (uintptr_t)&sample - (uintptr_t)base < size);
    CHECK(!pthread_attr_getguardsize(&attr, &guard) && guard < size);
    CHECK(!pthread_attr_destroy(&attr));
    return 0;
}
static void *check_worker_stack(void *unused) {
    (void)unused;
    return (void *)(uintptr_t)stack_contains_local();
}
static int collector_abi_probe(void) {
    CHECK(!stack_contains_local());
    pthread_t thread;
    void *result;
    CHECK(!pthread_create(&thread, NULL, check_worker_stack, NULL));
    CHECK(!pthread_join(thread, &result) && !result);
    sem_t *sem = mmap(NULL, sizeof(*sem), PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_SHARED, -1, 0);
    CHECK(sem != MAP_FAILED);
    CHECK(!sem_init(sem, 1, SEM_VALUE_MAX));
    CHECK(sem_post(sem) == -1 && errno == EOVERFLOW);
    CHECK(!sem_destroy(sem) && !sem_init(sem, 1, 0));
    int value;
    CHECK(!sem_getvalue(sem, &value) && !value);
    CHECK(sem_trywait(sem) == -1 && errno == EAGAIN);
    struct timespec expired = {0};
    CHECK(sem_timedwait(sem, &expired) == -1 && errno == ETIMEDOUT);
    CHECK(sem_clockwait(sem, CLOCK_MONOTONIC, &expired) == -1 && errno == ETIMEDOUT);
    CHECK(!pthread_atfork(first_prepare, first_parent, first_child));
    CHECK(!pthread_atfork(last_prepare, last_parent, last_child));
    pid_t pid = fork();
    CHECK(pid >= 0);
    if (!pid) _exit(fork_sequence != 2156 || sem_post(sem));
    CHECK(fork_sequence == 2134);
    int status;
    CHECK(waitpid(pid, &status, 0) == pid && WIFEXITED(status) && !WEXITSTATUS(status));
    CHECK(!sem_wait(sem) && !sem_destroy(sem) && !munmap(sem, sizeof(*sem)));
    puts("pthread stack bounds, fork callbacks and POSIX semaphores passed");
    return 0;
}
int32_t v_vpac_runtime_probe(const char *directory) {
    setvbuf(stdout,NULL,_IOLBF,0);
    CHECK(constructor_status==0);
#ifdef __GLIBC__
    CHECK(constructor_weekday==(const wchar_t *)nl_langinfo(_NL_WABDAY_1) && !wcscmp(constructor_weekday,L"Sun"));
#endif
    int code;
    if((code=pthread_metadata_native_probe()) || (code=cancellation_native_probe()) || (code=glob_native_probe()) || (code=strerror_native_probe()) || (code=integer_probe()) || (code=wait_probe()) || (code=fortify_probe()) || (code=numeric_native_probe()) || (code=locale_native_probe()) || (code=ipc_native_probe()) || (code=directory_scan_native_probe()) || (code=media_string_process_probe()) || (code=shell_native_probe()) || (code=threads_probe()) || (code=stdio_probe()) || (code=system_probe()) || (code=filesystem_calls_probe()) || (code=interfaces_probe()) || (code=calendar_probe()) ||
       (code=strings_probe()) || (code=encoding_probe()) || (code=regex_probe()))return code;
    if ((code = spawn_abi_probe()) || (code = collector_abi_probe())) return code;
#ifndef V_RUNTIME_HOSTED_ORACLE
    /* libc uses a hidden connect call, which --wrap cannot redirect. Only
     * the replacement runtime runs this private-socket logger test. */
    if((code=logger_probe(directory)))return code;
#else
    (void)directory;
#endif
    return 0;
}
#ifdef V_RUNTIME_HOSTED_ORACLE
int main(void) { int code=v_vpac_runtime_probe("/tmp"); if(code)fprintf(stderr,"oracle failed at C line %d\n",code); return !!code; }
#endif
