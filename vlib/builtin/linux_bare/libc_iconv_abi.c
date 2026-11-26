#include "libc_compat.h"
#include <iconv.h>
extern uintptr_t v_iconv_open(uintptr_t,uintptr_t);
extern int32_t v_iconv_close(uintptr_t);
extern uintptr_t v_iconv_convert(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
iconv_t iconv_open(const char *to,const char *from) { return (iconv_t)v_iconv_open((uintptr_t)to,(uintptr_t)from); }
int iconv_close(iconv_t c) { return v_iconv_close((uintptr_t)c); }
size_t iconv(iconv_t c,char **in,size_t *left,char **out,size_t *space) { return v_iconv_convert((uintptr_t)c,(uintptr_t)in,(uintptr_t)left,(uintptr_t)out,(uintptr_t)space); }
