#include "libc_compat.h"
#include <string.h>
#include <locale.h>
/* Native prototypes only. Buffer traversal and C/POSIX collation are V. */
extern uintptr_t v_string_chr_nul(uintptr_t,int32_t);
extern uintptr_t v_string_memrchr(uintptr_t,int32_t,uintptr_t);
extern uintptr_t v_string_mempcpy(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_stpncpy(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_strncat(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_strlcpy(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_strlcat(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_strtok(uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_string_token(uintptr_t,uintptr_t);
extern uintptr_t v_string_case_search(uintptr_t,uintptr_t);
extern int32_t v_string_collate(uintptr_t,uintptr_t);
extern uintptr_t v_string_transform(uintptr_t,uintptr_t,uintptr_t);
extern int32_t v_string_version_compare(uintptr_t,uintptr_t);
char *strchrnul(const char *s,int c) { return (char *)v_string_chr_nul((uintptr_t)s,c); }
void *memrchr(const void *s,int c,size_t n) { return (void *)v_string_memrchr((uintptr_t)s,c,n); }
void *mempcpy(void *d,const void *s,size_t n) { return (void *)v_string_mempcpy((uintptr_t)d,(uintptr_t)s,n); }
char *stpncpy(char *d,const char *s,size_t n) { return (char *)v_string_stpncpy((uintptr_t)d,(uintptr_t)s,n); }
char *strncat(char *d,const char *s,size_t n) { return (char *)v_string_strncat((uintptr_t)d,(uintptr_t)s,n); }
size_t strlcpy(char *d,const char *s,size_t n) { return v_string_strlcpy((uintptr_t)d,(uintptr_t)s,n); }
size_t strlcat(char *d,const char *s,size_t n) { return v_string_strlcat((uintptr_t)d,(uintptr_t)s,n); }
char *strtok_r(char *s,const char *d,char **saved) { return (char *)v_string_strtok((uintptr_t)s,(uintptr_t)d,(uintptr_t)saved); }
char *strtok(char *s,const char *d) { return (char *)v_string_token((uintptr_t)s,(uintptr_t)d); }
char *strcasestr(const char *s,const char *needle) { return (char *)v_string_case_search((uintptr_t)s,(uintptr_t)needle); }
int strcoll(const char *a,const char *b) { return v_string_collate((uintptr_t)a,(uintptr_t)b); }
int strcoll_l(const char *a,const char *b,locale_t l) { (void)l; return v_string_collate((uintptr_t)a,(uintptr_t)b); }
size_t strxfrm(char *d,const char *s,size_t n) { return v_string_transform((uintptr_t)d,(uintptr_t)s,n); }
size_t strxfrm_l(char *d,const char *s,size_t n,locale_t l) { (void)l; return v_string_transform((uintptr_t)d,(uintptr_t)s,n); }
int strverscmp(const char *a,const char *b) { return v_string_version_compare((uintptr_t)a,(uintptr_t)b); }
