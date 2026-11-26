#define _GNU_SOURCE
#include <errno.h>
#include <fenv.h>
#include <glob.h>
#include <fcntl.h>
#include <iconv.h>
#include <locale.h>
#include <math.h>
#include <pthread.h>
#include <regex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#include <wctype.h>
#define CHECK(c) do { if (!(c)) { fprintf(stderr, "review regression line %d\n", __LINE__); return 1; } } while (0)
static uint64_t bits(double x) { uint64_t b; memcpy(&b, &x, 8); return b; }
static int numeric(void) {
    double (*volatile call)(double, double *) = modf;
    const uint64_t cases[] = {UINT64_C(0x7ff0000000000001), UINT64_C(0x7ff8000000000001),
        UINT64_C(0x7ff0000000000000), UINT64_C(0xfff0000000000000), 0, UINT64_C(0x8000000000000000)};
    for (unsigned i=0; i<sizeof cases/sizeof *cases; ++i) {
        double x, whole; memcpy(&x, &cases[i], 8); feclearexcept(FE_ALL_EXCEPT);
        double fraction = call(x, &whole);
        uint64_t f=bits(fraction), w=bits(whole); int raised=fetestexcept(FE_ALL_EXCEPT);
        CHECK(w == (i==0 ? cases[i] | UINT64_C(0x8000000000000) : cases[i]));
        CHECK(f == (i<2 ? w : cases[i] & UINT64_C(0x8000000000000000)));
        CHECK(raised == (i==0 ? FE_INVALID : 0));
    }
    return 0;
}
static int constants(void) {
    long page=sysconf(_SC_PAGESIZE), tick=sysconf(_SC_CLK_TCK);
    struct rlimit saved, limit; CHECK(!getrlimit(RLIMIT_NOFILE, &saved));
    limit=saved; limit.rlim_cur=0; CHECK(!setrlimit(RLIMIT_NOFILE, &limit));
    errno=0; long p=sysconf(_SC_PAGESIZE); int pe=errno;
    errno=0; long t=sysconf(_SC_CLK_TCK); int te=errno;
    int restored=setrlimit(RLIMIT_NOFILE, &saved);
    CHECK(!restored && page>0 && tick>0 && p==page && t==tick && !pe && !te);
    return 0;
}
struct input_state { FILE *stream; int ready, kind; };
static void unlock_stream(void *s) { if (s) funlockfile(s); }
static void *input_worker(void *arg) {
    struct input_state *s=arg;
    if (s->kind==2) flockfile(s->stream);
    pthread_cleanup_push(unlock_stream, s->kind==2 ? s->stream : NULL);
    __atomic_store_n(&s->ready, 1, __ATOMIC_RELEASE);
    if (s->kind!=1) pthread_cancel(pthread_self());
    if (s->kind==3) { char b; (void)fread(&b,1,1,s->stream); }
    else if (s->kind==4) { char b[8]; (void)fgets(b,sizeof b,s->stream); }
    else if (s->kind==5) { int n; (void)fscanf(s->stream,"%d",&n); }
    else (void)fgetc(s->stream);
    pthread_cleanup_pop(0);
    return (void *)17;
}
static int input_cancellation(void) {
    for (int kind=0; kind<6; ++kind) {
        int p[2]; CHECK(!pipe(p)); struct input_state s={.stream=fdopen(p[0],"r"),.kind=kind};
        CHECK(s.stream && !setvbuf(s.stream,NULL,_IONBF,0));
        pthread_t t; CHECK(!pthread_create(&t,NULL,input_worker,&s));
        while (!__atomic_load_n(&s.ready,__ATOMIC_ACQUIRE)) usleep(1000);
        if (kind==1) { usleep(50000); CHECK(!pthread_cancel(t)); }
        void *result; CHECK(!pthread_join(t,&result) && result==PTHREAD_CANCELED);
        CHECK(!ftrylockfile(s.stream)); funlockfile(s.stream);
        CHECK(!fclose(s.stream) && !close(p[1]));
    }
    return 0;
}
struct output_state { FILE *stream; int kind; };
static void *output_worker(void *arg) {
    struct output_state *s=arg;
    pthread_cancel(pthread_self());
    if (s->kind==0) (void)fputc('A',s->stream);
    else if (s->kind==1) (void)fprintf(s->stream,"%s","A");
    else (void)fflush(s->kind==2 ? s->stream : NULL);
    return (void *)17;
}
static int output_cancellation(void) {
    for(int kind=0;kind<4;++kind) {
        int p[2]; CHECK(!pipe(p)); int flags=fcntl(p[1],F_GETFL);
        CHECK(flags>=0 && !fcntl(p[1],F_SETFL,flags|O_NONBLOCK));
        char buffer[4096]; memset(buffer,'x',sizeof buffer);
        while(write(p[1],buffer,sizeof buffer)>0) {}
        CHECK(errno==EAGAIN && !fcntl(p[1],F_SETFL,flags));
        struct output_state s={.stream=fdopen(p[1],"w"),.kind=kind};
        CHECK(s.stream && !setvbuf(s.stream,NULL,kind<2 ? _IONBF : _IOFBF,4096));
        if(kind>=2) CHECK(fputc('A',s.stream)=='A');
        pthread_t t; void *result;
        CHECK(!pthread_create(&t,NULL,output_worker,&s) && !pthread_join(t,&result));
        CHECK(result==PTHREAD_CANCELED && !ftrylockfile(s.stream)); funlockfile(s.stream);
        CHECK(!fcntl(p[0],F_SETFL,O_NONBLOCK));
        while(read(p[0],buffer,sizeof buffer)>0) {}
        CHECK(!fclose(s.stream) && !close(p[0]));
    }
    return 0;
}
static int captures(void) {
    const char *patterns[]={"((b)|.)","((a)|a)"}, *inputs[]={"b","a"};
    for (unsigned i=0;i<2;++i) {
        regex_t re; regmatch_t m[3]; CHECK(!regcomp(&re,patterns[i],REG_EXTENDED));
        CHECK(!regexec(&re,inputs[i],3,m,0));
        for (unsigned j=0;j<3;++j) CHECK(m[j].rm_so==0 && m[j].rm_eo==1);
        regfree(&re);
    }
    const char *greedy[]={"(a|aa)(a?)","(a*)(a*)"};
    const regmatch_t expected[][3]={{{0,2},{0,1},{1,2}},{{0,2},{0,2},{2,2}}};
    for(unsigned i=0;i<2;++i) {
        regex_t re; regmatch_t m[3]; CHECK(!regcomp(&re,greedy[i],REG_EXTENDED));
        CHECK(!regexec(&re,"aa",3,m,0));
        for(unsigned j=0;j<3;++j) CHECK(m[j].rm_so==expected[i][j].rm_so && m[j].rm_eo==expected[i][j].rm_eo);
        regfree(&re);
    }
    CHECK(setlocale(LC_CTYPE,"C.UTF-8"));
    CHECK(iswlower(0xdf) && iswlower(0xaa) && iswupper(0x3d2));
    CHECK(iswupper(0x2160) && iswalpha(0x2160) && iswalnum(0x2160) && !iswpunct(0x2160));
    const char *classes[]={"^[[:lower:]]$","^[[:upper:]]$","^[[:alpha:]]$","^[[:punct:]]$"};
    const char *strings[]={"\xc3\x9f","\xcf\x92","\xe2\x85\xa0","\xe2\x85\xa0"};
    for(unsigned i=0;i<4;++i) {
        regex_t re; CHECK(!regcomp(&re,classes[i],REG_EXTENDED));
        CHECK(regexec(&re,strings[i],0,NULL,0)==(i==3 ? REG_NOMATCH : 0)); regfree(&re);
    }
    CHECK(setlocale(LC_CTYPE,"C")); return 0;
}
static int conversion(void) {
    const char *from[]={"UTF-16","UTF-32","UCS-2","UTF-8","UTF-16","UTF-32"};
    const char *to[]={"UTF-8","UTF-8","UTF-8","UCS-2","UTF-8","UTF-8"};
    const unsigned char input[][12]={{0x41,0,0xe9,0},{0x41,0,0,0,0xe9,0,0,0},
        {0x41,0,0xe9,0},{0x41,0xc3,0xa9},{0xfe,0xff,0,0x41,0,0xe9},
        {0,0,0xfe,0xff,0,0,0,0x41,0,0,0,0xe9}};
    const size_t lengths[]={4,8,4,3,6,12};
    for(unsigned i=0;i<6;++i) {
        iconv_t cd=iconv_open(to[i],from[i]); CHECK(cd!=(iconv_t)-1);
        char out[32]={0}, *dst=out, *src=(char *)input[i]; size_t left=lengths[i], room=sizeof out;
        CHECK(iconv(cd,&src,&left,&dst,&room)==0 && left==0);
        CHECK(dst-out==(i==3 ? 4 : 3));
        CHECK(!memcmp(out,i==3 ? "A\0\xe9\0" : "A\xc3\xa9",i==3 ? 4 : 3));
        CHECK(!iconv_close(cd));
    }
    return 0;
}
static pthread_rwlock_t static_rw=PTHREAD_RWLOCK_WRITER_NONRECURSIVE_INITIALIZER_NP;
struct rw_state { pthread_rwlock_t *rw; int ready, result; };
static void *writer(void *arg) {
    struct rw_state *s=arg; __atomic_store_n(&s->ready,1,__ATOMIC_RELEASE);
    if (!pthread_rwlock_wrlock(s->rw)) pthread_rwlock_unlock(s->rw);
    return NULL;
}
static void *reader(void *arg) {
    struct rw_state *s=arg;
    for(int attempt=0;attempt<3000;++attempt) {
        s->result=pthread_rwlock_tryrdlock(s->rw);
        if(s->result) break;
        pthread_rwlock_unlock(s->rw); usleep(1000);
    }
    return NULL;
}
static int rwlocks(void) {
    pthread_rwlockattr_t attr; pthread_rwlock_t dynamic;
    CHECK(!pthread_rwlockattr_init(&attr));
    CHECK(!pthread_rwlockattr_setkind_np(&attr,PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP));
    CHECK(!pthread_rwlock_init(&dynamic,&attr)); pthread_rwlockattr_destroy(&attr);
    pthread_rwlock_t *locks[]={&static_rw,&dynamic};
    for(unsigned i=0;i<2;++i) {
        struct rw_state s={.rw=locks[i]}; pthread_t w,r;
        CHECK(!pthread_rwlock_rdlock(s.rw) && !pthread_create(&w,NULL,writer,&s));
        while (!__atomic_load_n(&s.ready,__ATOMIC_ACQUIRE)) usleep(1000);
        /* The reader retries until the announced writer has entered its wait. */
        CHECK(!pthread_create(&r,NULL,reader,&s) && !pthread_join(r,NULL));
        CHECK(s.result==EBUSY); CHECK(!pthread_rwlock_unlock(s.rw) && !pthread_join(w,NULL));
        CHECK(!pthread_rwlock_destroy(s.rw));
    }
    return 0;
}
static int quoting(void) {
    char dir[]="/tmp/vlibc-review-XXXXXX", path[256], pattern[256]; CHECK(mkdtemp(dir));
    snprintf(path,sizeof path,"%s/file",dir); FILE *f=fopen(path,"w"); CHECK(f && !fclose(f));
    const char *suffix[]={"/*","/file"};
    for(unsigned i=0;i<2;++i) {
        snprintf(pattern,sizeof pattern,"%s\\%s",dir,suffix[i]); glob_t g={0};
        CHECK(!glob(pattern,0,NULL,&g) && g.gl_pathc==1 && !strcmp(g.gl_pathv[0],path)); globfree(&g);
        CHECK(glob(pattern,GLOB_NOESCAPE,NULL,&g)==GLOB_NOMATCH); globfree(&g);
    }
    CHECK(!unlink(path) && !rmdir(dir)); return 0;
}
int v_review_findings_probe(void) {
    CHECK(!numeric() && !constants() && !input_cancellation() && !output_cancellation() && !captures());
    CHECK(!conversion() && !rwlocks() && !quoting());
    return 0;
}
#ifdef HOSTED_REVIEW
int main(void) { return v_review_findings_probe(); }
#endif
