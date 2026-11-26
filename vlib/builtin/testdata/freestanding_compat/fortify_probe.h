#ifndef V_FORTIFY_PROBE_H
#define V_FORTIFY_PROBE_H
#include <stdarg.h>
#include <sys/mman.h>
#include <sys/resource.h>
extern void *__memcpy_chk(void *,const void *,size_t,size_t);
extern void *__memmove_chk(void *,const void *,size_t,size_t);
extern void *__memset_chk(void *,int,size_t,size_t);
extern void *__mempcpy_chk(void *,const void *,size_t,size_t);
extern char *__strcpy_chk(char *,const char *,size_t);
extern char *__stpcpy_chk(char *,const char *,size_t);
extern char *__strncpy_chk(char *,const char *,size_t,size_t);
extern char *__stpncpy_chk(char *,const char *,size_t,size_t);
extern char *__strcat_chk(char *,const char *,size_t);
extern char *__strncat_chk(char *,const char *,size_t,size_t);
extern wchar_t *__wmemcpy_chk(wchar_t *,const wchar_t *,size_t,size_t);
extern wchar_t *__wmemmove_chk(wchar_t *,const wchar_t *,size_t,size_t);
extern wchar_t *__wmemset_chk(wchar_t *,wchar_t,size_t,size_t);
extern wchar_t *__wcpcpy_chk(wchar_t *,const wchar_t *,size_t);
extern int __snprintf_chk(char *,size_t,int,size_t,const char *,...);
extern int __sprintf_chk(char *,int,size_t,const char *,...);
extern int __asprintf_chk(char **,int,const char *,...);
extern int __fprintf_chk(FILE *,int,const char *,...);
extern int __dprintf_chk(int,int,const char *,...);
extern int __vsnprintf_chk(char *,size_t,int,size_t,const char *,va_list);
extern int __vsprintf_chk(char *,int,size_t,const char *,va_list);
extern ssize_t __read_chk(int,void *,size_t,size_t);
extern size_t __fread_chk(void *,size_t,size_t,size_t,FILE *);
extern char *__fgets_chk(char *,size_t,int,FILE *);
extern wchar_t *__fgetws_chk(wchar_t *,size_t,int,FILE *);
extern size_t __mbsrtowcs_chk(wchar_t *,const char **,size_t,mbstate_t *,size_t);
extern int __open_2(const char *,int);
extern long __fdelt_chk(long);
extern void __stack_chk_fail(void) __attribute__((noreturn));
#ifndef V_RUNTIME_HOSTED_ORACLE
extern void __stack_chk_fail_local(void) __attribute__((noreturn));
#endif
#define FORTIFY_CALL(name) static __typeof__(name) *volatile call_##name=name
FORTIFY_CALL(__memcpy_chk); FORTIFY_CALL(__memmove_chk); FORTIFY_CALL(__memset_chk);
FORTIFY_CALL(__mempcpy_chk); FORTIFY_CALL(__strcpy_chk); FORTIFY_CALL(__stpcpy_chk);
FORTIFY_CALL(__strncpy_chk); FORTIFY_CALL(__stpncpy_chk); FORTIFY_CALL(__strcat_chk);
FORTIFY_CALL(__strncat_chk); FORTIFY_CALL(__wmemcpy_chk); FORTIFY_CALL(__wmemmove_chk);
FORTIFY_CALL(__wmemset_chk); FORTIFY_CALL(__wcpcpy_chk); FORTIFY_CALL(__snprintf_chk);
FORTIFY_CALL(__sprintf_chk); FORTIFY_CALL(__asprintf_chk); FORTIFY_CALL(__fprintf_chk);
FORTIFY_CALL(__dprintf_chk); FORTIFY_CALL(__read_chk); FORTIFY_CALL(__fread_chk);
FORTIFY_CALL(__fgets_chk); FORTIFY_CALL(__fgetws_chk);
FORTIFY_CALL(__open_2); FORTIFY_CALL(__fdelt_chk); FORTIFY_CALL(__mbsrtowcs_chk);
FORTIFY_CALL(__stack_chk_fail);
#ifndef V_RUNTIME_HOSTED_ORACLE
FORTIFY_CALL(__stack_chk_fail_local);
#endif
#undef FORTIFY_CALL
static int fortified_va(char *out,size_t capacity,const char *format,...) {
    va_list args; va_start(args,format);
    int n=__vsnprintf_chk(out,capacity,1,capacity,format,args);
    va_end(args); return n;
}
static int fortified_sprintf_va(char *out,size_t capacity,const char *format,...) {
    va_list args; va_start(args,format);
    int n=__vsprintf_chk(out,1,capacity,format,args);
    va_end(args); return n;
}
static void fortify_bad_case(int which) {
    char out[16]={0}; wchar_t wide[4]; int n=0;
    switch(which) {
    case 0: call___memcpy_chk(out,"1234",4,3); break;
    case 1: call___strcpy_chk(out,"abc",3); break;
    case 2: call___strncpy_chk(out,"a",4,3); break;
    case 3: memcpy(out,"abcd",4); call___strcat_chk(out,"",4); break;
    case 4: call___wmemcpy_chk(wide,L"abcd",4,3); break;
    case 5: call___fread_chk(out,sizeof out,SIZE_MAX,2,stdin); break;
    case 6: call___read_chk(-1,out,5,4); break;
    case 7: call___snprintf_chk(out,5,1,4,"x"); break;
    case 8: call___sprintf_chk(out,1,4,"%s","abcd"); break;
    case 9: memcpy(out,"%n",3); call___snprintf_chk(out+8,8,1,8,out,&n); break;
    case 10: call___open_2("/tmp/fortify-must-not-create",O_CREAT|O_WRONLY); break;
    case 11: call___fdelt_chk(-1); break;
    case 12: {
        size_t page=(size_t)sysconf(_SC_PAGESIZE);
        char *area=mmap(NULL,page*2,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
        if(area==MAP_FAILED)_exit(91);
        char *format=area+page-2; memcpy(format,"%n",3);
        if(mprotect(area,page,PROT_READ))_exit(92);
        call___snprintf_chk(out,sizeof out,1,sizeof out,format,&n); break;
    }
    case 13: {
        FILE *f=tmpfile(); if(!f)_exit(93); fputs("too long\n",f); rewind(f);
        call___fgets_chk(out,4,100,f); break;
    }
    case 14: call___stack_chk_fail(); break;
#ifndef V_RUNTIME_HOSTED_ORACLE
    case 15: call___stack_chk_fail_local(); break;
#endif
    }
    _exit(90);
}
static int fortify_probe(void) {
    struct { char before[8],data[16],after[8]; } guarded;
    memset(&guarded,'g',sizeof guarded);
    char *out=guarded.data;
    CHECK(call___memcpy_chk(out,"abcd",4,4)==out);
    CHECK(!memcmp(out,"abcd",4) && !memcmp(guarded.before,"gggggggg",8) && !memcmp(guarded.after,"gggggggg",8));
    CHECK(call___memmove_chk(out+1,out,4,4)==out+1 && !memcmp(out,"aabcd",5));
    CHECK(call___memset_chk(out,'z',4,4)==out && !memcmp(out,"zzzz",4));
    CHECK(call___mempcpy_chk(out,"xy",2,2)==out+2 && !memcmp(out,"xy",2));
    CHECK(call___strcpy_chk(out,"abc",4)==out && !strcmp(out,"abc"));
    CHECK(call___stpcpy_chk(out,"xy",3)==out+2 && !strcmp(out,"xy"));
    CHECK(call___strncpy_chk(out,"x",4,4)==out && !memcmp(out,"x\0\0\0",4));
    CHECK(call___stpncpy_chk(out,"abcd",3,3)==out+3 && !memcmp(out,"abc",3));
    CHECK(call___stpncpy_chk(out,"a",3,3)==out+1 && !memcmp(out,"a\0\0",3));
    CHECK(call___strcpy_chk(out,"",1)==out);
    CHECK(call___strncat_chk(out,"abcdef",2,3)==out && !strcmp(out,"ab"));
    CHECK(call___strcat_chk(out,"c",4)==out && !strcmp(out,"abc"));
    wchar_t wide[8]; CHECK(call___wmemset_chk(wide,L'Ж',3,3)==wide && wide[2]==L'Ж');
    CHECK(call___wmemcpy_chk(wide,L"abc",4,4)==wide && !wcscmp(wide,L"abc"));
    CHECK(call___wmemmove_chk(wide+1,wide,3,3)==wide+1 && !wmemcmp(wide,L"aabc",4));
    CHECK(call___wcpcpy_chk(wide,L"ab",3)==wide+2 && !wcscmp(wide,L"ab"));
    int n=-1; CHECK(call___snprintf_chk(out,16,1,16,"%s%n","abc",&n)==3 && n==3 && !strcmp(out,"abc"));
    errno=ENOENT; n=-1; char error_text[128];
    CHECK(call___snprintf_chk(error_text,sizeof error_text,1,sizeof error_text,"%m%n",&n)>0);
    CHECK(!strcmp(error_text,strerror(ENOENT)) && n==(int)strlen(error_text) && errno==ENOENT);
    char writable_format[16]; memcpy(writable_format,"%%n:%s",7);
    CHECK(call___snprintf_chk(out,16,1,16,writable_format,"ok")==5 && !strcmp(out,"%n:ok"));
    memcpy(writable_format,"%n",3); n=-1;
    CHECK(call___snprintf_chk(out,16,0,16,writable_format,&n)==0 && n==0);
    CHECK(call___snprintf_chk(out,4,1,4,"%s","abcdef")==6 && !strcmp(out,"abc"));
    CHECK(fortified_va(out,16,"%d:%s",7,"va")==4 && !strcmp(out,"7:va"));
    CHECK(fortified_sprintf_va(out,SIZE_MAX,"%s","unknown")==7 && !strcmp(out,"unknown"));
    char *allocated=NULL; CHECK(call___asprintf_chk(&allocated,1,"%d:%s",8,"grow")==6 && !strcmp(allocated,"8:grow")); free(allocated);
    FILE *file=tmpfile(); CHECK(file);
    CHECK(call___fprintf_chk(file,1,"%s %d\n","stream",9)==9); CHECK(!fseek(file,0,SEEK_SET));
    CHECK(call___fread_chk(out,16,1,9,file)==9 && !memcmp(out,"stream 9\n",9)); CHECK(!fclose(file));
    file=tmpfile(); CHECK(file); CHECK(fputs("a\n",file)>=0); rewind(file);
    CHECK(call___fgets_chk(out,4,100,file)==out && !strcmp(out,"a\n"));
    CHECK(!call___fgets_chk(out,4,100,file)); rewind(file);
    CHECK(!call___fgets_chk(out,4,0,file));
    CHECK(!call___fgets_chk(out,4,1,file)); CHECK(!fclose(file));
    file=tmpfile(); CHECK(file); CHECK(fputws(L"a\n",file)>=0); rewind(file);
    CHECK(call___fgetws_chk(wide,4,100,file)==wide && !wcscmp(wide,L"a\n")); CHECK(!fclose(file));
    int fds[2]; CHECK(!pipe(fds)); CHECK(call___dprintf_chk(fds[1],1,"%s %d","pipe",4)==6); close(fds[1]);
    CHECK(call___read_chk(fds[0],out,16,16)==6 && !memcmp(out,"pipe 4",6)); close(fds[0]);
    const char *text="abc"; mbstate_t state={0};
    CHECK(call___mbsrtowcs_chk(wide,&text,4,&state,4)==3 && !wcscmp(wide,L"abc") && !text);
    CHECK(call___fdelt_chk(0)==0 && call___fdelt_chk(1023)==15);
    int fd=call___open_2("/dev/null",O_RDONLY); CHECK(fd>=0); close(fd);
    size_t page=(size_t)sysconf(_SC_PAGESIZE);
    char *area=mmap(NULL,page*2,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0); CHECK(area!=MAP_FAILED);
    char *format=area+page-2; memcpy(format,"%n",3); CHECK(!mprotect(area,page*2,PROT_READ));
    n=-1; CHECK(call___snprintf_chk(out,16,1,16,format,&n)==0 && n==0); CHECK(!munmap(area,page*2));
    for(int which=0;which<
#ifdef V_RUNTIME_HOSTED_ORACLE
        15
#else
        16
#endif
        ;which++) {
        pid_t child=fork(); CHECK(child>=0);
        if(!child) {
            struct rlimit no_core={0,0}; setrlimit(RLIMIT_CORE,&no_core);
            int sink=open("/dev/null",O_WRONLY); if(sink>=0) { dup2(sink,2); close(sink); }
            fortify_bad_case(which);
        }
        int status=0; CHECK(waitpid(child,&status,0)==child);
        CHECK(WIFSIGNALED(status) && WTERMSIG(status)==SIGABRT);
    }
    puts("fortified memory, strings, streams, formatting and rejection checks passed");
    return 0;
}
#endif
