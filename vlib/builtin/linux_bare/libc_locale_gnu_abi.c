#include "libc_compat.h"
#include "libc_locale_abi.h"
#include <langinfo.h>
#include <locale.h>
/* Native nl_item constants only; lookup, ownership and widening are V logic.
 * Rows: first item, last item, source item/value/category, representation. */
static const int32_t v_locale_gnu_items[][4]={
#if defined(__GLIBC__)
    {_NL_WABDAY_1,_NL_WABDAY_7,ABDAY_1,1},
    {_NL_WDAY_1,_NL_WDAY_7,DAY_1,1},
    {_NL_WABMON_1,_NL_WABMON_12,ABMON_1,1},
    {_NL_WMON_1,_NL_WMON_12,MON_1,1},
    {_NL_WAM_STR,_NL_WPM_STR,AM_STR,1},
    {_NL_WD_T_FMT,_NL_WD_T_FMT,D_T_FMT,1},
    {_NL_WD_FMT,_NL_WD_FMT,D_FMT,1},
    {_NL_WT_FMT,_NL_WT_FMT,T_FMT,1},
    {_NL_WT_FMT_AMPM,_NL_WT_FMT_AMPM,T_FMT_AMPM,1},
    {_NL_WERA_YEAR,_NL_WERA_YEAR,ERA_YEAR,1},
    {_NL_WERA_D_FMT,_NL_WERA_D_FMT,ERA_D_FMT,1},
    {_NL_WERA_D_T_FMT,_NL_WERA_D_T_FMT,ERA_D_T_FMT,1},
    {_NL_WERA_T_FMT,_NL_WERA_T_FMT,ERA_T_FMT,1},
    {_NL_WALT_DIGITS,_NL_WALT_DIGITS,ALT_DIGITS,1},
    {_NL_NUMERIC_DECIMAL_POINT_WC,_NL_NUMERIC_DECIMAL_POINT_WC,'.',2},
    {_NL_NUMERIC_THOUSANDS_SEP_WC,_NL_NUMERIC_THOUSANDS_SEP_WC,0,2},
    {_NL_MONETARY_DECIMAL_POINT_WC,_NL_MONETARY_DECIMAL_POINT_WC,0,2},
    {_NL_MONETARY_THOUSANDS_SEP_WC,_NL_MONETARY_THOUSANDS_SEP_WC,0,2},
    /* GNU stores unavailable monetary bytes as 255; localeconv normalizes
     * those bytes to CHAR_MAX separately. */
    {INT_FRAC_DIGITS,N_SIGN_POSN,255,3},
    {INT_P_CS_PRECEDES,INT_N_SIGN_POSN,255,3},
    {_NL_NUMERIC_CODESET,_NL_NUMERIC_CODESET,LC_NUMERIC,4},
    {_NL_MONETARY_CODESET,_NL_MONETARY_CODESET,LC_MONETARY,4},
    {_NL_TIME_CODESET,_NL_TIME_CODESET,LC_TIME,4},
    {_NL_MESSAGES_CODESET,_NL_MESSAGES_CODESET,LC_MESSAGES,4},
    {_NL_LOCALE_NAME(LC_CTYPE),_NL_LOCALE_NAME(LC_CTYPE),LC_CTYPE,5},
    {_NL_LOCALE_NAME(LC_NUMERIC),_NL_LOCALE_NAME(LC_NUMERIC),LC_NUMERIC,5},
    {_NL_LOCALE_NAME(LC_TIME),_NL_LOCALE_NAME(LC_TIME),LC_TIME,5},
    {_NL_LOCALE_NAME(LC_COLLATE),_NL_LOCALE_NAME(LC_COLLATE),LC_COLLATE,5},
    {_NL_LOCALE_NAME(LC_MONETARY),_NL_LOCALE_NAME(LC_MONETARY),LC_MONETARY,5},
    {_NL_LOCALE_NAME(LC_MESSAGES),_NL_LOCALE_NAME(LC_MESSAGES),LC_MESSAGES,5},
#else
    {-1,-1,0,0},
#endif
};
uintptr_t v_locale_gnu_data(void) { return (uintptr_t)v_locale_gnu_items; }
uint32_t v_locale_gnu_count(void) {
#if defined(__GLIBC__)
    return sizeof v_locale_gnu_items / sizeof v_locale_gnu_items[0];
#else
    return 0;
#endif
}
_Static_assert(sizeof v_locale_gnu_items[0]==16,"native nl_item metadata ABI");
_Static_assert(sizeof v_locale_gnu_items / sizeof v_locale_gnu_items[0]<=32,"wide locale cache capacity");
extern uintptr_t v_locale_langinfo(int32_t,uintptr_t);
char *nl_langinfo_l(nl_item item,locale_t locale) { return (char *)v_locale_langinfo(item,(uintptr_t)locale); }
