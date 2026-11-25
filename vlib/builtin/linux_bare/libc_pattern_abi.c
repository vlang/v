#include "libc_compat.h"
#include "libc_pattern_abi.h"
#include <limits.h>
#include <regex.h>
#include <fnmatch.h>
#include <string.h>
/* regex_t and regmatch_t layouts belong to the selected C headers. The
 * compiled implementation is an opaque V-owned object stored in regex_t. */
_Static_assert(sizeof(regex_t)>=sizeof(uintptr_t),"native regex storage");
extern int32_t v_pattern_compile(uintptr_t,int32_t,uintptr_t,uintptr_t);
extern int32_t v_pattern_execute(uintptr_t,uintptr_t,uintptr_t,uintptr_t,int32_t);
extern void v_pattern_free(uintptr_t);
extern int32_t v_pattern_fnmatch(uintptr_t,uintptr_t,int32_t);
extern uintptr_t v_pattern_error(int32_t,uintptr_t,uintptr_t);
int regcomp(regex_t *out,const char *pattern,int flags) {
    uintptr_t object=0,groups=0;
    int code=v_pattern_compile((uintptr_t)pattern,flags,(uintptr_t)&object,(uintptr_t)&groups);
    memset(out,0,sizeof *out);
    memcpy(out,&object,sizeof object);
    out->re_nsub=groups;
    return code;
}
int regexec(const regex_t *re,const char *input,size_t count,regmatch_t matches[],int flags) {
    uintptr_t object;
    memcpy(&object,re,sizeof object);
    return v_pattern_execute(object,(uintptr_t)input,count,(uintptr_t)matches,flags);
}
void regfree(regex_t *re) {
    uintptr_t object;
    memcpy(&object,re,sizeof object);
    v_pattern_free(object);
    memset(re,0,sizeof *re);
}
size_t regerror(int code,const regex_t *re,char *buffer,size_t size) { (void)re; return v_pattern_error(code,(uintptr_t)buffer,size); }
int fnmatch(const char *pattern,const char *input,int flags) { return v_pattern_fnmatch((uintptr_t)pattern,(uintptr_t)input,flags); }
void v_pattern_match_get(uintptr_t address,uintptr_t index,uintptr_t output) {
    const regmatch_t *source=(const regmatch_t *)address;
    int64_t *target=(int64_t *)output;
    target[0]=source[index].rm_so; target[1]=source[index].rm_eo;
}
void v_pattern_match_set(uintptr_t address,uintptr_t index,int64_t start,int64_t end) {
    regmatch_t *target=(regmatch_t *)address;
    target[index].rm_so=(regoff_t)start; target[index].rm_eo=(regoff_t)end;
}
int64_t v_pattern_offset_max(void) { return sizeof(regoff_t)==4 ? INT32_MAX : INT64_MAX; }
