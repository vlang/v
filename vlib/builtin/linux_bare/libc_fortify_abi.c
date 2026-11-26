/* Native ABI and va_list forwarding; checks and buffer operations are V. */
#include "libc_compat.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <wchar.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <syslog.h>
extern void v_fortify_fail(void) __attribute__((noreturn));
extern void v_stack_fail(void) __attribute__((noreturn));
extern void v_fortify_bound(uintptr_t,uintptr_t);
extern void v_fortify_product(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_fortify_memory(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,int32_t);
extern uintptr_t v_fortify_string(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,int32_t);
extern void v_fortify_format(uintptr_t,int32_t);
extern uintptr_t v_fortify_sprintf_capacity(uintptr_t);
extern void v_fortify_sprintf_result(int32_t,uintptr_t);
extern int32_t v_fortify_fd_output(int32_t,uintptr_t,int32_t);
extern uintptr_t v_fortify_fgets(uintptr_t,uintptr_t,int32_t,uintptr_t,uintptr_t);
extern void v_fortify_open(int32_t,int32_t,int32_t);
extern int64_t v_fortify_fdelt(int64_t);
_Noreturn void __chk_fail(void) { v_fortify_fail(); }
_Noreturn void __fortify_fail(const char *message) { (void)message; v_fortify_fail(); }
_Noreturn void __stack_chk_fail(void) { v_stack_fail(); }
_Noreturn void __stack_chk_fail_local(void) { v_stack_fail(); }
void *__memcpy_chk(void *out,const void *in,size_t n,size_t capacity) { return (void *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,1,0); }
void *__memmove_chk(void *out,const void *in,size_t n,size_t capacity) { return (void *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,1,1); }
void *__mempcpy_chk(void *out,const void *in,size_t n,size_t capacity) { return (void *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,1,3); }
void *__memset_chk(void *out,int value,size_t n,size_t capacity) { return (void *)v_fortify_memory((uintptr_t)out,(uintptr_t)value,n,capacity,1,2); }
wchar_t *__wmemcpy_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { return (wchar_t *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,4,0); }
wchar_t *__wmemmove_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { return (wchar_t *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,4,1); }
wchar_t *__wmempcpy_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { return (wchar_t *)v_fortify_memory((uintptr_t)out,(uintptr_t)in,n,capacity,4,3); }
wchar_t *__wmemset_chk(wchar_t *out,wchar_t value,size_t n,size_t capacity) { return (wchar_t *)v_fortify_memory((uintptr_t)out,(uintptr_t)value,n,capacity,4,2); }
char *__strcpy_chk(char *out,const char *in,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,1,0); return out; }
char *__stpcpy_chk(char *out,const char *in,size_t capacity) { return (char *)v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,1,0); }
char *__strncpy_chk(char *out,const char *in,size_t n,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,1,1); return out; }
char *__stpncpy_chk(char *out,const char *in,size_t n,size_t capacity) { return (char *)v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,1,1); }
char *__strcat_chk(char *out,const char *in,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,1,2); return out; }
char *__strncat_chk(char *out,const char *in,size_t n,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,1,3); return out; }
wchar_t *__wcscpy_chk(wchar_t *out,const wchar_t *in,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,4,0); return out; }
wchar_t *__wcpcpy_chk(wchar_t *out,const wchar_t *in,size_t capacity) { return (wchar_t *)v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,4,0); }
wchar_t *__wcsncpy_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,4,1); return out; }
wchar_t *__wcpncpy_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { return (wchar_t *)v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,4,1); }
wchar_t *__wcscat_chk(wchar_t *out,const wchar_t *in,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,0,capacity,4,2); return out; }
wchar_t *__wcsncat_chk(wchar_t *out,const wchar_t *in,size_t n,size_t capacity) { v_fortify_string((uintptr_t)out,(uintptr_t)in,n,capacity,4,3); return out; }

int __vfprintf_chk(FILE *file,int flag,const char *format,va_list args) { v_fortify_format((uintptr_t)format,flag); return vfprintf(file,format,args); }
int __fprintf_chk(FILE *file,int flag,const char *format,...) { va_list args; va_start(args,format); int n=__vfprintf_chk(file,flag,format,args); va_end(args); return n; }
int __vprintf_chk(int flag,const char *format,va_list args) { return __vfprintf_chk(stdout,flag,format,args); }
int __printf_chk(int flag,const char *format,...) { va_list args; va_start(args,format); int n=__vprintf_chk(flag,format,args); va_end(args); return n; }
int __vsnprintf_chk(char *out,size_t size,int flag,size_t capacity,const char *format,va_list args) { v_fortify_bound(size,capacity); v_fortify_format((uintptr_t)format,flag); return vsnprintf(out,size,format,args); }
int __snprintf_chk(char *out,size_t size,int flag,size_t capacity,const char *format,...) { va_list args; va_start(args,format); int n=__vsnprintf_chk(out,size,flag,capacity,format,args); va_end(args); return n; }
int __vsprintf_chk(char *out,int flag,size_t capacity,const char *format,va_list args) { size_t size=v_fortify_sprintf_capacity(capacity); v_fortify_format((uintptr_t)format,flag); int n=vsnprintf(out,size,format,args); v_fortify_sprintf_result(n,capacity); return n; }
int __sprintf_chk(char *out,int flag,size_t capacity,const char *format,...) { va_list args; va_start(args,format); int n=__vsprintf_chk(out,flag,capacity,format,args); va_end(args); return n; }
int __vasprintf_chk(char **out,int flag,const char *format,va_list args) { v_fortify_format((uintptr_t)format,flag); return vasprintf(out,format,args); }
int __asprintf_chk(char **out,int flag,const char *format,...) { va_list args; va_start(args,format); int n=__vasprintf_chk(out,flag,format,args); va_end(args); return n; }
int __vdprintf_chk(int fd,int flag,const char *format,va_list args) { v_fortify_format((uintptr_t)format,flag); char *out=NULL; int n=vasprintf(&out,format,args); return v_fortify_fd_output(fd,(uintptr_t)out,n); }
int __dprintf_chk(int fd,int flag,const char *format,...) { va_list args; va_start(args,format); int n=__vdprintf_chk(fd,flag,format,args); va_end(args); return n; }
int vdprintf(int fd,const char *format,va_list args) { return __vdprintf_chk(fd,0,format,args); }
int dprintf(int fd,const char *format,...) { va_list args; va_start(args,format); int n=vdprintf(fd,format,args); va_end(args); return n; }
void __vsyslog_chk(int priority,int flag,const char *format,va_list args) { v_fortify_format((uintptr_t)format,flag); vsyslog(priority,format,args); }
void __syslog_chk(int priority,int flag,const char *format,...) { va_list args; va_start(args,format); __vsyslog_chk(priority,flag,format,args); va_end(args); }
ssize_t __read_chk(int fd,void *out,size_t n,size_t capacity) { v_fortify_bound(n,capacity); return read(fd,out,n); }
ssize_t __pread_chk(int fd,void *out,size_t n,off_t offset,size_t capacity) { v_fortify_bound(n,capacity); return pread(fd,out,n,offset); }
ssize_t __pread64_chk(int fd,void *out,size_t n,off64_t offset,size_t capacity) { v_fortify_bound(n,capacity); return pread(fd,out,n,(off_t)offset); }
ssize_t __recv_chk(int fd,void *out,size_t n,size_t capacity,int flags) { v_fortify_bound(n,capacity); return recv(fd,out,n,flags); }
ssize_t __recvfrom_chk(int fd,void *out,size_t n,size_t capacity,int flags,struct sockaddr *address,socklen_t *size) { v_fortify_bound(n,capacity); return recvfrom(fd,out,n,flags,address,size); }
ssize_t __readlink_chk(const char *path,char *out,size_t n,size_t capacity) { v_fortify_bound(n,capacity); return readlink(path,out,n); }
ssize_t __readlinkat_chk(int fd,const char *path,char *out,size_t n,size_t capacity) { v_fortify_bound(n,capacity); return readlinkat(fd,path,out,n); }
char *__getcwd_chk(char *out,size_t n,size_t capacity) { v_fortify_bound(n,capacity); return getcwd(out,n); }
int __gethostname_chk(char *out,size_t n,size_t capacity) { v_fortify_bound(n,capacity); return gethostname(out,n); }
char *__fgets_chk(char *out,size_t capacity,int n,FILE *file) { return (char *)v_fortify_fgets((uintptr_t)out,capacity,n,(uintptr_t)file,1); }
char *__fgets_unlocked_chk(char *out,size_t capacity,int n,FILE *file) { return (char *)v_fortify_fgets((uintptr_t)out,capacity,n,(uintptr_t)file,1); }
size_t __fread_chk(void *out,size_t capacity,size_t size,size_t count,FILE *file) { v_fortify_product(size,count,capacity); return fread(out,size,count,file); }
size_t __fread_unlocked_chk(void *out,size_t capacity,size_t size,size_t count,FILE *file) { v_fortify_product(size,count,capacity); return fread_unlocked(out,size,count,file); }
size_t __mbsrtowcs_chk(wchar_t *out,const char **in,size_t n,mbstate_t *state,size_t capacity) { v_fortify_bound(n,capacity); return mbsrtowcs(out,in,n,state); }
size_t __mbsnrtowcs_chk(wchar_t *out,const char **in,size_t bytes,size_t n,mbstate_t *state,size_t capacity) { v_fortify_bound(n,capacity); return mbsnrtowcs(out,in,bytes,n,state); }
size_t __wcsrtombs_chk(char *out,const wchar_t **in,size_t n,mbstate_t *state,size_t capacity) { v_fortify_bound(n,capacity); return wcsrtombs(out,in,n,state); }
size_t __wcsnrtombs_chk(char *out,const wchar_t **in,size_t units,size_t n,mbstate_t *state,size_t capacity) { v_fortify_bound(n,capacity); return wcsnrtombs(out,in,units,n,state); }
int __open_2(const char *path,int flags) { v_fortify_open(flags,O_CREAT,O_TMPFILE); return open(path,flags); }
int __open64_2(const char *path,int flags) { v_fortify_open(flags,O_CREAT,O_TMPFILE); return open64(path,flags); }
int __openat_2(int fd,const char *path,int flags) { v_fortify_open(flags,O_CREAT,O_TMPFILE); return openat(fd,path,flags); }
int __openat64_2(int fd,const char *path,int flags) { v_fortify_open(flags,O_CREAT,O_TMPFILE); return openat64(fd,path,flags); }
long __fdelt_chk(long fd) { return (long)v_fortify_fdelt(fd); }

wchar_t *__fgetws_chk(wchar_t *out,size_t capacity,int n,FILE *file) { return (wchar_t *)v_fortify_fgets((uintptr_t)out,capacity,n,(uintptr_t)file,4); }
wchar_t *__fgetws_unlocked_chk(wchar_t *out,size_t capacity,int n,FILE *file) { return (wchar_t *)v_fortify_fgets((uintptr_t)out,capacity,n,(uintptr_t)file,4); }
