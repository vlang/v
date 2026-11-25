#include "libc_compat.h"
#include "libc_wide_abi.h"
#include <langinfo.h>
#include <locale.h>
#include <string.h>
#include <uchar.h>
#include <wchar.h>
#include <wctype.h>
#undef wcschr
#undef wcsrchr
#undef wmemchr
_Static_assert(sizeof(wchar_t)==4 && sizeof(wint_t)==4 && sizeof(mbstate_t)==8,"supported Linux wide-character ABI");
static __thread mbstate_t v_wide_states[2];
uintptr_t v_wide_state(int32_t n) { return (uintptr_t)&v_wide_states[n]; }
uintptr_t v_wide_codeset(uintptr_t l) { return (uintptr_t)nl_langinfo_l(CODESET,(locale_t)l); }
uintptr_t v_wide_decode(uintptr_t out,uintptr_t in,uintptr_t n,uintptr_t state) { return mbrtowc((wchar_t *)out,(const char *)in,n,(mbstate_t *)state); }
uintptr_t v_wide_encode(uintptr_t out,uint32_t value,uintptr_t state) { return wcrtomb((char *)out,(wchar_t)value,(mbstate_t *)state); }
extern int32_t v_wide_class(uint32_t,uintptr_t,uintptr_t);
extern uintptr_t v_wide_class_name(uintptr_t);
extern uint32_t v_wide_case(uint32_t,int32_t,uintptr_t);
extern int32_t v_wide_width(uint32_t);
extern uintptr_t v_wide_length(uintptr_t,uintptr_t);
extern int32_t v_wide_compare(uintptr_t,uintptr_t,uintptr_t,int32_t);
extern uintptr_t v_wide_find(uintptr_t,uint32_t,uintptr_t,int32_t,int32_t);
extern uintptr_t v_wide_fill(uintptr_t,uint32_t,uintptr_t);
extern uintptr_t v_wide_transform(uintptr_t,uintptr_t,uintptr_t);
extern int32_t v_wide_single_byte(uint32_t);
extern uintptr_t v_wide_mbsnrtowcs(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
extern uintptr_t v_wide_wcsnrtombs(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);

#undef iswalnum
#undef iswalnum_l
int iswalnum(wint_t v) { return v_wide_class(v,1,0); }
int iswalnum_l(wint_t v,locale_t l) { return v_wide_class(v,1,(uintptr_t)l); }

#undef iswalpha
#undef iswalpha_l
int iswalpha(wint_t v) { return v_wide_class(v,2,0); }
int iswalpha_l(wint_t v,locale_t l) { return v_wide_class(v,2,(uintptr_t)l); }

#undef iswblank
#undef iswblank_l
int iswblank(wint_t v) { return v_wide_class(v,3,0); }
int iswblank_l(wint_t v,locale_t l) { return v_wide_class(v,3,(uintptr_t)l); }

#undef iswcntrl
#undef iswcntrl_l
int iswcntrl(wint_t v) { return v_wide_class(v,4,0); }
int iswcntrl_l(wint_t v,locale_t l) { return v_wide_class(v,4,(uintptr_t)l); }

#undef iswdigit
#undef iswdigit_l
int iswdigit(wint_t v) { return v_wide_class(v,5,0); }
int iswdigit_l(wint_t v,locale_t l) { return v_wide_class(v,5,(uintptr_t)l); }

#undef iswgraph
#undef iswgraph_l
int iswgraph(wint_t v) { return v_wide_class(v,6,0); }
int iswgraph_l(wint_t v,locale_t l) { return v_wide_class(v,6,(uintptr_t)l); }

#undef iswlower
#undef iswlower_l
int iswlower(wint_t v) { return v_wide_class(v,7,0); }
int iswlower_l(wint_t v,locale_t l) { return v_wide_class(v,7,(uintptr_t)l); }

#undef iswprint
#undef iswprint_l
int iswprint(wint_t v) { return v_wide_class(v,8,0); }
int iswprint_l(wint_t v,locale_t l) { return v_wide_class(v,8,(uintptr_t)l); }

#undef iswpunct
#undef iswpunct_l
int iswpunct(wint_t v) { return v_wide_class(v,9,0); }
int iswpunct_l(wint_t v,locale_t l) { return v_wide_class(v,9,(uintptr_t)l); }

#undef iswspace
#undef iswspace_l
int iswspace(wint_t v) { return v_wide_class(v,10,0); }
int iswspace_l(wint_t v,locale_t l) { return v_wide_class(v,10,(uintptr_t)l); }

#undef iswupper
#undef iswupper_l
int iswupper(wint_t v) { return v_wide_class(v,11,0); }
int iswupper_l(wint_t v,locale_t l) { return v_wide_class(v,11,(uintptr_t)l); }

#undef iswxdigit
#undef iswxdigit_l
int iswxdigit(wint_t v) { return v_wide_class(v,12,0); }
int iswxdigit_l(wint_t v,locale_t l) { return v_wide_class(v,12,(uintptr_t)l); }

wctype_t wctype(const char *name) { return (wctype_t)v_wide_class_name((uintptr_t)name); }
wctype_t wctype_l(const char *name,locale_t l) { (void)l; return (wctype_t)v_wide_class_name((uintptr_t)name); }
int iswctype(wint_t v,wctype_t t) { return v_wide_class(v,(uintptr_t)t,0); }
int iswctype_l(wint_t v,wctype_t t,locale_t l) { return v_wide_class(v,(uintptr_t)t,(uintptr_t)l); }
wint_t towlower(wint_t v) { return v_wide_case(v,0,0); }
wint_t towupper(wint_t v) { return v_wide_case(v,1,0); }
wint_t towlower_l(wint_t v,locale_t l) { return v_wide_case(v,0,(uintptr_t)l); }
wint_t towupper_l(wint_t v,locale_t l) { return v_wide_case(v,1,(uintptr_t)l); }
int wcwidth(wchar_t v) { return v_wide_width((uint32_t)v); }
size_t wcslen(const wchar_t *s) { return v_wide_length((uintptr_t)s,SIZE_MAX); }
size_t wcsnlen(const wchar_t *s,size_t n) { return v_wide_length((uintptr_t)s,n); }
int wcscmp(const wchar_t *a,const wchar_t *b) { return v_wide_compare((uintptr_t)a,(uintptr_t)b,SIZE_MAX,1); }
int wcsncmp(const wchar_t *a,const wchar_t *b,size_t n) { return v_wide_compare((uintptr_t)a,(uintptr_t)b,n,1); }
int wmemcmp(const wchar_t *a,const wchar_t *b,size_t n) { return v_wide_compare((uintptr_t)a,(uintptr_t)b,n,0); }
wchar_t *wcschr(const wchar_t *s,wchar_t c) { return (wchar_t *)v_wide_find((uintptr_t)s,(uint32_t)c,SIZE_MAX,0,1); }
wchar_t *wcsrchr(const wchar_t *s,wchar_t c) { return (wchar_t *)v_wide_find((uintptr_t)s,(uint32_t)c,SIZE_MAX,1,1); }
wchar_t *wmemchr(const wchar_t *s,wchar_t c,size_t n) { return (wchar_t *)v_wide_find((uintptr_t)s,(uint32_t)c,n,0,0); }
wchar_t *wmemset(wchar_t *s,wchar_t c,size_t n) { return (wchar_t *)v_wide_fill((uintptr_t)s,(uint32_t)c,n); }
wchar_t *wmemcpy(wchar_t *d,const wchar_t *s,size_t n) { return memcpy(d,s,n*sizeof(wchar_t)); }
wchar_t *wmemmove(wchar_t *d,const wchar_t *s,size_t n) { return memmove(d,s,n*sizeof(wchar_t)); }
int wcscoll(const wchar_t *a,const wchar_t *b) { return v_wide_compare((uintptr_t)a,(uintptr_t)b,SIZE_MAX,1); }
int wcscoll_l(const wchar_t *a,const wchar_t *b,locale_t l) { (void)l; return v_wide_compare((uintptr_t)a,(uintptr_t)b,SIZE_MAX,1); }
size_t wcsxfrm(wchar_t *d,const wchar_t *s,size_t n) { return v_wide_transform((uintptr_t)d,(uintptr_t)s,n); }
size_t wcsxfrm_l(wchar_t *d,const wchar_t *s,size_t n,locale_t l) { (void)l; return v_wide_transform((uintptr_t)d,(uintptr_t)s,n); }
wint_t btowc(int c) { return (wint_t)v_wide_single_byte((uint32_t)c); }
int wctob(wint_t c) { return v_wide_single_byte(c); }
size_t mbrtoc32(char32_t *v,const char *s,size_t n,mbstate_t *state) { return v_wide_decode((uintptr_t)v,(uintptr_t)s,n,(uintptr_t)state); }
size_t c32rtomb(char *s,char32_t v,mbstate_t *state) { return v_wide_encode((uintptr_t)s,v,(uintptr_t)state); }
size_t mbsnrtowcs(wchar_t *out,const char **in,size_t n,size_t len,mbstate_t *state) { return v_wide_mbsnrtowcs((uintptr_t)out,(uintptr_t)in,n,len,(uintptr_t)state); }
size_t wcsnrtombs(char *out,const wchar_t **in,size_t n,size_t len,mbstate_t *state) { return v_wide_wcsnrtombs((uintptr_t)out,(uintptr_t)in,n,len,(uintptr_t)state); }
