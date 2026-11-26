#include "libc_compat.h"
#include "libc_locale_abi.h"
#include <langinfo.h>
#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>

/* Native locale/wchar/long-double ABIs only. Supported C/POSIX/C.UTF-8
 * locales share the numeric syntax, while character conversion is per-thread. */
float strtof_l(const char *text, char **end, locale_t locale) { (void)locale; return strtof(text,end); }
double strtod_l(const char *text, char **end, locale_t locale) { (void)locale; return strtod(text,end); }
long double strtold_l(const char *text, char **end, locale_t locale) { (void)locale; return strtold(text,end); }
uintptr_t v_locale_select(uintptr_t locale) { return (uintptr_t)uselocale((locale_t)locale); }
uintptr_t v_locale_time_format(uintptr_t out,uintptr_t count,uintptr_t format,uintptr_t calendar,uintptr_t locale) {
    return strftime_l((char *)out,count,(const char *)format,(const struct tm *)calendar,
                      locale ? (locale_t)locale : uselocale((locale_t)0));
}
extern uintptr_t v_wide_strftime(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
size_t wcsftime_l(wchar_t *out,size_t n,const wchar_t *format,const struct tm *calendar,locale_t locale) {
    return v_wide_strftime((uintptr_t)out,n,(uintptr_t)format,(uintptr_t)calendar,(uintptr_t)locale);
}
size_t wcsftime(wchar_t *out,size_t n,const wchar_t *format,const struct tm *calendar) {
    return v_wide_strftime((uintptr_t)out,n,(uintptr_t)format,(uintptr_t)calendar,0);
}

locale_t __newlocale(int mask,const char *name,locale_t base) { return newlocale(mask,name,base); }
locale_t __duplocale(locale_t locale) { return duplocale(locale); }
void __freelocale(locale_t locale) { freelocale(locale); }
locale_t __uselocale(locale_t locale) { return uselocale(locale); }
char *__nl_langinfo_l(nl_item item,locale_t locale) { return nl_langinfo_l(item,locale); }
int __strcoll_l(const char *a,const char *b,locale_t locale) { return strcoll_l(a,b,locale); }
size_t __strxfrm_l(char *out,const char *in,size_t n,locale_t locale) { return strxfrm_l(out,in,n,locale); }
float __strtof_l(const char *text,char **end,locale_t locale) { return strtof_l(text,end,locale); }
double __strtod_l(const char *text,char **end,locale_t locale) { return strtod_l(text,end,locale); }
long double __strtold_l(const char *text,char **end,locale_t locale) { return strtold_l(text,end,locale); }
size_t __strftime_l(char *out,size_t n,const char *format,const struct tm *calendar,locale_t locale) {
    return strftime_l(out,n,format,calendar,locale);
}
size_t __wcsftime_l(wchar_t *out,size_t n,const wchar_t *format,const struct tm *calendar,locale_t locale) {
    return wcsftime_l(out,n,format,calendar,locale);
}
wctype_t __wctype_l(const char *name,locale_t locale) { return wctype_l(name,locale); }
int __iswctype_l(wint_t value,wctype_t type,locale_t locale) { return iswctype_l(value,type,locale); }
wint_t __towlower_l(wint_t value,locale_t locale) { return towlower_l(value,locale); }
wint_t __towupper_l(wint_t value,locale_t locale) { return towupper_l(value,locale); }
int __wcscoll_l(const wchar_t *a,const wchar_t *b,locale_t locale) { return wcscoll_l(a,b,locale); }
size_t __wcsxfrm_l(wchar_t *out,const wchar_t *in,size_t n,locale_t locale) { return wcsxfrm_l(out,in,n,locale); }
