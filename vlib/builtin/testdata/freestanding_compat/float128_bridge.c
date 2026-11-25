#include <stddef.h>
#include <stdint.h>
#include <quadmath.h>
/* Native binary128 argument/return and varargs ABIs only. Numeric conversion
 * is performed by the real quadmath library; format validation is V logic. */
extern int32_t v_float128_format(uintptr_t,uintptr_t);
__float128 strtof128(const char *text,char **end) { return strtoflt128(text,end); }
int strfromf128(char *output,size_t size,const char *format,__float128 value) {
    char native_format[16];
    if(v_float128_format((uintptr_t)format,(uintptr_t)native_format))return -1;
    return quadmath_snprintf(output,size,native_format,value);
}
