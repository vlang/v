#include "libc_compat.h"
#include "libc_stdio_abi.h"
#include <ctype.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

/* Supported locale data is deliberately explicit: C/POSIX and C.UTF-8.
 * Unknown locale names fail, rather than claiming to install missing data. */
static unsigned short v_ctype[384];
static int v_lower_table[384], v_upper_table[384], v_locale_lock, v_tables_ready;
struct v_locale {
#if defined(__GLIBC__)
    struct __locale_struct abi;
#endif
    unsigned int utf8, owned;
};
static struct v_locale v_global_locale;
static __thread struct v_locale *v_thread_locale;
static __thread const unsigned short *v_ctype_pointer;
static __thread const int *v_lower_pointer, *v_upper_pointer;
static __thread char v_locale_name[256];
static const char *v_category_names[6] = {"LC_CTYPE",   "LC_NUMERIC",  "LC_TIME",
                                          "LC_COLLATE", "LC_MONETARY", "LC_MESSAGES"};
static unsigned int v_global_categories;
static int v_ascii_space(unsigned int c) { return c == ' ' || (c >= '\t' && c <= '\r'); }
static void v_locale_tables(void) {
    if (__atomic_load_n(&v_tables_ready, __ATOMIC_ACQUIRE))
        return;
    v_linux_lock(&v_locale_lock);
    if (!v_tables_ready) {
        for (int c = -128; c < 256; ++c) {
            unsigned int u = (unsigned char)c;
            v_lower_table[c + 128] = c == -1 ? -1 : u >= 'A' && u <= 'Z' ? u + 32 : u;
            v_upper_table[c + 128] = c == -1 ? -1 : u >= 'a' && u <= 'z' ? u - 32 : u;
#if defined(__GLIBC__)
            unsigned short bits = 0;
            if (c != -1 && u < 128) {
                if (u >= 'A' && u <= 'Z')
                    bits |= _ISupper | _ISalpha | _ISalnum;
                if (u >= 'a' && u <= 'z')
                    bits |= _ISlower | _ISalpha | _ISalnum;
                if (u >= '0' && u <= '9')
                    bits |= _ISdigit | _ISalnum;
                if ((u >= '0' && u <= '9') || (u >= 'a' && u <= 'f') || (u >= 'A' && u <= 'F'))
                    bits |= _ISxdigit;
                if (v_ascii_space(u))
                    bits |= _ISspace;
                if (u == ' ' || u == '\t')
                    bits |= _ISblank;
                if (u < 32 || u == 127)
                    bits |= _IScntrl;
                if (u >= 32 && u < 127)
                    bits |= _ISprint;
                if (u > 32 && u < 127) {
                    bits |= _ISgraph;
                    if (!(bits & _ISalnum))
                        bits |= _ISpunct;
                }
            }
            v_ctype[c + 128] = bits;
#endif
        }
#if defined(__GLIBC__)
        v_global_locale.abi.__ctype_b = v_ctype + 128;
        v_global_locale.abi.__ctype_tolower = v_lower_table + 128;
        v_global_locale.abi.__ctype_toupper = v_upper_table + 128;
        for (int i = 0; i < 13; ++i)
            v_global_locale.abi.__names[i] = "C";
#endif
        __atomic_store_n(&v_tables_ready, 1, __ATOMIC_RELEASE);
    }
    v_linux_unlock(&v_locale_lock);
}
static struct v_locale *v_current_locale(void) {
    v_locale_tables();
    return v_thread_locale ? v_thread_locale : &v_global_locale;
}
int v_freestanding_locale_utf8(void) { return !!(v_current_locale()->utf8 & (1u << LC_CTYPE)); }
const unsigned short **__ctype_b_loc(void) {
    v_locale_tables();
    v_ctype_pointer = v_ctype + 128;
    return &v_ctype_pointer;
}
const int **__ctype_tolower_loc(void) {
    v_locale_tables();
    v_lower_pointer = v_lower_table + 128;
    return &v_lower_pointer;
}
const int **__ctype_toupper_loc(void) {
    v_locale_tables();
    v_upper_pointer = v_upper_table + 128;
    return &v_upper_pointer;
}
size_t __ctype_get_mb_cur_max(void) { return v_freestanding_locale_utf8() ? 4 : 1; }
static int v_locale_kind(const char *name, int category) {
    if (!*name) {
        name = getenv("LC_ALL");
        if (!name || !*name)
            name = getenv(v_category_names[category]);
        if (!name || !*name)
            name = getenv("LANG");
        if (!name || !*name)
            name = "C";
    }
    if (!strcmp(name, "C") || !strcmp(name, "POSIX"))
        return 0;
    if (!strcmp(name, "C.UTF-8") || !strcmp(name, "C.utf8"))
        return 1;
    errno = ENOENT;
    return -1;
}
static int v_locale_composite(const char *name, unsigned int *bits) {
    unsigned int seen = 0, result = 0;
    while (*name) {
        const char *equals = strchr(name, '=');
        if (!equals) {
            errno = EINVAL;
            return -1;
        }
        int category = -1;
        for (int i = 0; i < 6; ++i)
            if ((size_t)(equals - name) == strlen(v_category_names[i]) &&
                !strncmp(name, v_category_names[i], equals - name))
                category = i;
        if (category < 0 || (seen & (1u << category))) {
            errno = EINVAL;
            return -1;
        }
        size_t length = strcspn(equals + 1, ";");
        char value[32];
        if (!length || length >= sizeof value) {
            errno = ENOENT;
            return -1;
        }
        memcpy(value, equals + 1, length);
        value[length] = 0;
        int kind = v_locale_kind(value, category);
        if (kind < 0)
            return -1;
        result |= (unsigned int)kind << category;
        seen |= 1u << category;
        name = equals + 1 + length;
        if (*name == ';')
            ++name;
    }
    if (seen != 63) {
        errno = EINVAL;
        return -1;
    }
    *bits = result;
    return 0;
}

char *setlocale(int category, const char *name) {
    if (category < LC_CTYPE || category > LC_ALL) {
        errno = EINVAL;
        return NULL;
    }
    v_locale_tables();
    v_linux_lock(&v_locale_lock);
    unsigned int bits = v_global_categories;
    if (name) {
        int composite = category == LC_ALL && strchr(name, '=');
        if (composite && v_locale_composite(name, &bits) < 0) {
            v_linux_unlock(&v_locale_lock);
            return NULL;
        }
        for (int i = 0; !composite && i < 6; ++i)
            if (category == LC_ALL || category == i) {
                int kind = v_locale_kind(name, i);
                if (kind < 0) {
                    v_linux_unlock(&v_locale_lock);
                    return NULL;
                }
                bits = (bits & ~(1u << i)) | ((unsigned int)kind << i);
            }
        v_global_categories = bits;
        __atomic_store_n(&v_global_locale.utf8, bits, __ATOMIC_RELEASE);
#if defined(__GLIBC__)
        for (int i = 0; i < 6; ++i)
            v_global_locale.abi.__names[i] = bits & (1u << i) ? "C.UTF-8" : "C";
#endif
    }
    char *result;
    if (category != LC_ALL)
        result = bits & (1u << category) ? "C.UTF-8" : "C";
    else if (!bits || bits == 63)
        result = bits ? "C.UTF-8" : "C";
    else {
        /* Stable thread-local query storage, as allowed by setlocale. */
        char *out = v_locale_name;
        for (int i = 0; i < 6; ++i) {
            if (i)
                *out++ = ';';
            out = stpcpy(out, v_category_names[i]);
            *out++ = '=';
            out = stpcpy(out, bits & (1u << i) ? "C.UTF-8" : "C");
        }
        result = v_locale_name;
    }
    v_linux_unlock(&v_locale_lock);
    return result;
}
extern int32_t v_locale_mask(int32_t,int32_t,int32_t);
locale_t newlocale(int mask, const char *name, locale_t base) {
    mask=v_locale_mask(mask,1<<LC_ALL,LC_ALL_MASK);
    if (!name || mask & ~LC_ALL_MASK || base == LC_GLOBAL_LOCALE) {
        errno = EINVAL;
        return (locale_t)0;
    }
    v_locale_tables();
    struct v_locale *old = (void *)base;
    unsigned int bits = old ? old->utf8 : 0;
    for (int i = 0; i < 6; ++i)
        if (mask & (1 << i)) {
            int kind = v_locale_kind(name, i);
            if (kind < 0)
                return (locale_t)0;
            bits = (bits & ~(1u << i)) | ((unsigned int)kind << i);
        }
    struct v_locale *created = malloc(sizeof *created);
    if (!created) {
        errno = ENOMEM;
        return (locale_t)0;
    }
    *created = v_global_locale;
    created->utf8 = bits;
    created->owned = 1;
#if defined(__GLIBC__)
    for (int i = 0; i < 6; ++i)
        created->abi.__names[i] = bits & (1u << i) ? "C.UTF-8" : "C";
#endif
    if (old && old->owned)
        free(old);
    return (locale_t)(void *)created;
}
locale_t duplocale(locale_t source) {
    struct v_locale *old = source == LC_GLOBAL_LOCALE ? &v_global_locale : (void *)source;
    if (!old) {
        errno = EINVAL;
        return (locale_t)0;
    }
    v_locale_tables();
    struct v_locale *created = malloc(sizeof *created);
    if (!created) {
        errno = ENOMEM;
        return (locale_t)0;
    }
    *created = *old;
    created->owned = 1;
    return (locale_t)(void *)created;
}
void freelocale(locale_t locale) {
    struct v_locale *l = (void *)locale;
    if (l && locale != LC_GLOBAL_LOCALE && l->owned)
        free(l);
}
locale_t uselocale(locale_t locale) {
    locale_t old = v_thread_locale ? (locale_t)(void *)v_thread_locale : LC_GLOBAL_LOCALE;
    if (locale)
        v_thread_locale = locale == LC_GLOBAL_LOCALE ? NULL : (void *)locale;
    return old;
}
struct lconv *localeconv(void) {
    static struct lconv result = {.decimal_point = ".",
                                  .thousands_sep = "",
                                  .grouping = "",
                                  .int_curr_symbol = "",
                                  .currency_symbol = "",
                                  .mon_decimal_point = "",
                                  .mon_thousands_sep = "",
                                  .mon_grouping = "",
                                  .positive_sign = "",
                                  .negative_sign = "",
                                  .int_frac_digits = CHAR_MAX,
                                  .frac_digits = CHAR_MAX,
                                  .p_cs_precedes = CHAR_MAX,
                                  .p_sep_by_space = CHAR_MAX,
                                  .n_cs_precedes = CHAR_MAX,
                                  .n_sep_by_space = CHAR_MAX,
                                  .p_sign_posn = CHAR_MAX,
                                  .n_sign_posn = CHAR_MAX,
                                  .int_p_cs_precedes = CHAR_MAX,
                                  .int_p_sep_by_space = CHAR_MAX,
                                  .int_n_cs_precedes = CHAR_MAX,
                                  .int_n_sep_by_space = CHAR_MAX,
                                  .int_p_sign_posn = CHAR_MAX,
                                  .int_n_sign_posn = CHAR_MAX};
    return &result;
}
static const char *v_days[] = {"Sunday",   "Monday", "Tuesday", "Wednesday",
                               "Thursday", "Friday", "Saturday"};
static const char *v_abdays[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
static const char *v_months[] = {"January",   "February", "March",    "April",
                                 "May",       "June",     "July",     "August",
                                 "September", "October",  "November", "December"};
static const char *v_abmonths[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
static char *v_locale_base_item(nl_item item, locale_t locale) {
    struct v_locale *l = locale == LC_GLOBAL_LOCALE ? &v_global_locale : (void *)locale;
    if (!l)
        l = v_current_locale();
    if (item == CODESET)
        return l->utf8 & (1u << LC_CTYPE) ? "UTF-8" : "ANSI_X3.4-1968";
    if (item >= DAY_1 && item <= DAY_7)
        return (char *)v_days[item - DAY_1];
    if (item >= ABDAY_1 && item <= ABDAY_7)
        return (char *)v_abdays[item - ABDAY_1];
    if (item >= MON_1 && item <= MON_12)
        return (char *)v_months[item - MON_1];
    if (item >= ABMON_1 && item <= ABMON_12)
        return (char *)v_abmonths[item - ABMON_1];
    switch (item) {
    case D_T_FMT:
        return "%a %b %e %H:%M:%S %Y";
    case D_FMT:
        return "%m/%d/%y";
    case T_FMT:
        return "%H:%M:%S";
    case T_FMT_AMPM:
        return "%I:%M:%S %p";
    case AM_STR:
        return "AM";
    case PM_STR:
        return "PM";
    case RADIXCHAR:
        return ".";
    case YESEXPR:
        return "^[yY]";
    case NOEXPR:
        return "^[nN]";
    default:
        return "";
    }
}
uintptr_t v_locale_langinfo_base(int32_t item,uintptr_t locale) {
    return (uintptr_t)v_locale_base_item((nl_item)item,(locale_t)locale);
}
uint32_t v_locale_bits(uintptr_t locale) {
    const struct v_locale *value=locale==(uintptr_t)LC_GLOBAL_LOCALE ? &v_global_locale :
        locale ? (const struct v_locale *)locale : v_current_locale();
    return value->utf8;
}
char *nl_langinfo(nl_item item) {
    return nl_langinfo_l(item, (locale_t)(void *)v_current_locale());
}

/* mbstate_t is opaque; these eight bytes describe only our own UTF-8 decoder. */
struct v_mbstate {
    uint32_t value;
    unsigned char remaining, total, reserved[2];
};
_Static_assert(sizeof(mbstate_t) >= sizeof(struct v_mbstate), "bare multibyte state");
static __thread mbstate_t v_mb_read_state, v_mb_length_state, v_mb_write_state;
int mbsinit(const mbstate_t *state) {
    struct v_mbstate s = {0};
    if (state)
        memcpy(&s, state, sizeof s);
    return !s.remaining;
}
static size_t v_decode_multibyte(wchar_t *out, const char *text, size_t size, mbstate_t *state, int utf8) {
    if (!state)
        state = &v_mb_read_state;
    if (!text) {
        text = "";
        size = 1;
        out = NULL;
    }
    if (!size)
        return (size_t)-2;
    struct v_mbstate s;
    memcpy(&s, state, sizeof s);
    size_t used = 0;
    if (!s.remaining) {
        unsigned char c = text[used++];
        if (c < 128) {
            if (out)
                *out = c;
            return c ? 1 : 0;
        }
        if (utf8 < 0) utf8 = v_freestanding_locale_utf8();
        if (!utf8)
            goto invalid;
        if (c >= 0xc2 && c <= 0xdf) {
            s.value = c & 31;
            s.remaining = 1;
            s.total = 2;
        } else if (c >= 0xe0 && c <= 0xef) {
            s.value = c & 15;
            s.remaining = 2;
            s.total = 3;
        } else if (c >= 0xf0 && c <= 0xf4) {
            s.value = c & 7;
            s.remaining = 3;
            s.total = 4;
        } else
            goto invalid;
    }
    while (s.remaining && used < size) {
        unsigned char c = text[used++];
        if ((c & 0xc0) != 0x80)
            goto invalid;
        if (s.remaining == s.total - 1 &&
            ((s.total == 3 && ((s.value == 0 && c < 0xa0) || (s.value == 13 && c > 0x9f))) ||
             (s.total == 4 && ((s.value == 0 && c < 0x90) || (s.value == 4 && c > 0x8f)))))
            goto invalid;
        s.value = (s.value << 6) | (c & 63);
        --s.remaining;
        if (!s.remaining &&
            (s.value > 0x10ffff || (s.value >= 0xd800 && s.value <= 0xdfff) ||
             (s.total == 2 && s.value < 0x80) || (s.total == 3 && s.value < 0x800) ||
             (s.total == 4 && s.value < 0x10000)))
            goto invalid;
    }
    memcpy(state, &s, sizeof s);
    if (s.remaining)
        return (size_t)-2;
    if (out)
        *out = s.value;
    memset(state, 0, sizeof *state);
    return used;
invalid:
    memset(state, 0, sizeof *state);
    errno = EILSEQ;
    return (size_t)-1;
}
size_t mbrtowc(wchar_t *out, const char *text, size_t size, mbstate_t *state) {
    return v_decode_multibyte(out, text, size, state, -1);
}
uintptr_t v_stdio_decode(uintptr_t output, uintptr_t input, uintptr_t state, int32_t utf8) {
    mbstate_t native;
    memcpy(&native, (void *)state, sizeof native);
    size_t count = v_decode_multibyte((wchar_t *)output, (const char *)input, 1, &native, utf8);
    memcpy((void *)state, &native, sizeof native);
    return count;
}
size_t mbrlen(const char *s, size_t n, mbstate_t *state) {
    return mbrtowc(NULL, s, n, state ? state : &v_mb_length_state);
}
static size_t v_encode_multibyte(char *out, wchar_t wc, mbstate_t *state, int utf8) {
    if (!state)
        state = &v_mb_write_state;
    memset(state, 0, sizeof *state);
    if (!out)
        return 1;
    uint32_t c = wc;
    if (c < 128) {
        out[0] = c;
        return 1;
    }
    if (utf8 < 0) utf8 = v_freestanding_locale_utf8();
    if (!utf8 || c > 0x10ffff || (c >= 0xd800 && c <= 0xdfff)) {
        errno = EILSEQ;
        return (size_t)-1;
    }
    if (c < 0x800) {
        out[0] = 0xc0 | (c >> 6);
        out[1] = 0x80 | (c & 63);
        return 2;
    }
    if (c < 0x10000) {
        out[0] = 0xe0 | (c >> 12);
        out[1] = 0x80 | ((c >> 6) & 63);
        out[2] = 0x80 | (c & 63);
        return 3;
    }
    out[0] = 0xf0 | (c >> 18);
    out[1] = 0x80 | ((c >> 12) & 63);
    out[2] = 0x80 | ((c >> 6) & 63);
    out[3] = 0x80 | (c & 63);
    return 4;
}
size_t wcrtomb(char *out, wchar_t wc, mbstate_t *state) {
    return v_encode_multibyte(out, wc, state, -1);
}
uintptr_t v_stdio_encode(uintptr_t output, uint32_t value, uintptr_t state, int32_t utf8) {
    mbstate_t native;
    memcpy(&native, (void *)state, sizeof native);
    size_t count = v_encode_multibyte((char *)output, (wchar_t)value, &native, utf8);
    memcpy((void *)state, &native, sizeof native);
    return count;
}
int mbtowc(wchar_t *out, const char *s, size_t n) {
    static __thread mbstate_t state;
    if (!s) {
        memset(&state, 0, sizeof state);
        return 0;
    }
    size_t r = mbrtowc(out, s, n, &state);
    if (r == (size_t)-2) {
        memset(&state, 0, sizeof state);
        errno = EILSEQ;
        return -1;
    }
    return (int)r;
}
int mblen(const char *s, size_t n) { return mbtowc(NULL, s, n); }
int wctomb(char *out, wchar_t wc) {
    if (!out)
        return 0;
    return (int)wcrtomb(out, wc, NULL);
}
int(isupper)(int c) { return (unsigned int)c - 'A' < 26; }
int(islower)(int c) { return (unsigned int)c - 'a' < 26; }
int(isalpha)(int c) { return ((unsigned int)c | 32) - 'a' < 26; }
int(isdigit)(int c) { return (unsigned int)c - '0' < 10; }
int(isalnum)(int c) { return (isalpha)(c) || (isdigit)(c); }
int(isxdigit)(int c) { return (isdigit)(c) || ((unsigned int)c | 32) - 'a' < 6; }
int(isspace)(int c) { return v_ascii_space((unsigned int)c); }
int(isblank)(int c) { return c == ' ' || c == '\t'; }
int(iscntrl)(int c) { return (unsigned int)c < 32 || c == 127; }
int(isprint)(int c) { return (unsigned int)c - 32 < 95; }
int(isgraph)(int c) { return (unsigned int)c - 33 < 94; }
int(ispunct)(int c) { return (isgraph)(c) && !(isalnum)(c); }
int(tolower)(int c) { return (isupper)(c) ? c + 32 : c; }
int(toupper)(int c) { return (islower)(c) ? c - 32 : c; }

/* LC_MESSAGES accepts only C/POSIX/C.UTF-8, whose translations are the
 * source messages. Other locale names are rejected by setlocale/newlocale. */


static __thread mbstate_t v_mbs_string_state, v_wcs_string_state;
size_t mbsrtowcs(wchar_t *out, const char **source, size_t limit, mbstate_t *state) {
    if (!state) state = &v_mbs_string_state;
    mbstate_t query = *state;
    mbstate_t *active = out ? state : &query;
    const char *p = *source;
    size_t count = 0;
    while (!out || count < limit) {
        wchar_t wc;
        size_t n = mbrtowc(&wc, p, MB_CUR_MAX, active);
        if (n == (size_t)-1 || n == (size_t)-2) {
            if (out) *source = p;
            errno = EILSEQ;
            return (size_t)-1;
        }
        if (!n) {
            if (out) { out[count] = 0; *source = NULL; }
            return count;
        }
        if (out) out[count] = wc;
        p += n;
        count++;
    }
    *source = p;
    return count;
}
size_t mbstowcs(wchar_t *out, const char *text, size_t limit) {
    mbstate_t state = {0};
    return mbsrtowcs(out, &text, limit, &state);
}
size_t wcsrtombs(char *out, const wchar_t **source, size_t limit, mbstate_t *state) {
    if (!state) state = &v_wcs_string_state;
    const wchar_t *p = *source;
    size_t count = 0;
    for (;;) {
        if (out && count == limit) { *source = p; return count; }
        char encoded[4];
        mbstate_t next = *state;
        size_t n = wcrtomb(encoded, *p, &next);
        if (n == (size_t)-1) {
            if (out) *source = p;
            return n;
        }
        if (out && n > limit - count) { *source = p; return count; }
        if (out) { memcpy(out + count, encoded, n); *state = next; }
        if (!*p) {
            if (out) *source = NULL;
            return count;
        }
        count += n;
        p++;
    }
}
size_t wcstombs(char *out, const wchar_t *text, size_t limit) {
    mbstate_t state = {0};
    return wcsrtombs(out, &text, limit, &state);
}
