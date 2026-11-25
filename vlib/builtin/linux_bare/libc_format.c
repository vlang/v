#include "libc_cancel_abi.h"
/* Formatting engine derived from musl src/stdio/vfprintf.c.
 * Copyright (c) 2005-2020 Rich Felker, et al.; see COPYRIGHT.musl.
 * Adaptations: output sinks, standalone frexpl, C-locale wide characters,
 * bounded-buffer padding, exact directed rounding, and field-width overflow checks. */
#include "libc_compat.h"
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <wchar.h>

/* Use compiler classification operations rather than a hosted libm ABI. */
#undef signbit
#undef isfinite
#define signbit(value) __builtin_signbit(value)
#define isfinite(value) __builtin_isfinite(value)

/* Floating-point evaluation must observe the calling thread's mode, including
 * when this translation unit is built with production optimization. */
#if defined(__clang__)
#pragma STDC FENV_ACCESS ON
#elif defined(__GNUC__)
#pragma GCC optimize ("rounding-math")
#endif

/* POSIX requires at least nine numbered arguments. */
#undef NL_ARGMAX
#define NL_ARGMAX 9
struct v_format_output { FILE *stream; char *buffer; size_t capacity, used; int error, growing; };
static int v_format_isdigit(int value) { return value >= '0' && value <= '9'; }
static size_t v_format_strnlen(const char *text, size_t limit) {
    size_t length = 0;
    while (length < limit && text[length]) ++length;
    return length;
}
/* C and C.UTF-8 share the locale module's stateless encoder. */
static int v_format_wctomb(char *buffer, wchar_t value) {
    return (int)wcrtomb(buffer, value, NULL);
}
/* Software binary128 arithmetic need not observe FPCR. Make the formatting
 * decision from the caller's mode and exact discarded bits/digits instead. */
static unsigned int v_format_rounding_mode(void) {
#if defined(__x86_64__)
    unsigned short control;
    __asm__ volatile("fnstcw %0" : "=m"(control));
    return (control >> 10) & 3;
#elif defined(__aarch64__)
    uint64_t control;
    __asm__ volatile("mrs %0, fpcr" : "=r"(control));
    unsigned int mode = (unsigned int)(control >> 22) & 3;
    return mode == 1 ? 2 : mode == 2 ? 1 : mode;
#endif
}
static int v_format_increment(__uint128_t remainder, __uint128_t half, int tail, int odd, int negative) {
    if (!remainder && !tail) return 0;
    switch (v_format_rounding_mode()) {
    case 1: return negative;
    case 2: return !negative;
    case 3: return 0;
    default: return remainder > half || (remainder == half && (tail || odd));
    }
}
static long double v_format_hex_round(long double value, int precision, int negative) {
    if (!value || precision < 0 || precision > (LDBL_MANT_DIG - 1) / 4) return value;
    int drop = LDBL_MANT_DIG - 1 - 4 * precision;
    if (drop <= 0) return value;
    __uint128_t mantissa;
#if LDBL_MANT_DIG == 64
    union { long double value; struct { uint64_t mantissa; uint16_t exponent; } bits; } shape = {value};
    mantissa = shape.bits.mantissa;
#elif LDBL_MANT_DIG == 113
    union { long double value; uint64_t words[2]; } shape = {value};
    mantissa = ((__uint128_t)(shape.words[1] & UINT64_C(0xffffffffffff)) << 64) | shape.words[0] | ((__uint128_t)1 << 112);
#elif LDBL_MANT_DIG == 53
    union { double value; uint64_t bits; } shape = {(double)value};
    mantissa = (shape.bits & UINT64_C(0xfffffffffffff)) | (UINT64_C(1) << 52);
#endif
    __uint128_t mask = ((__uint128_t)1 << drop) - 1;
    __uint128_t kept = mantissa >> drop;
    if (v_format_increment(mantissa & mask, (__uint128_t)1 << (drop - 1), 0, (int)(kept & 1), negative)) ++kept;
    mantissa = kept << drop;
    if (mantissa >= ((__uint128_t)1 << LDBL_MANT_DIG)) return 2.0L;
#if LDBL_MANT_DIG == 64
    shape.bits.mantissa = (uint64_t)mantissa;
#elif LDBL_MANT_DIG == 113
    shape.words[0] = (uint64_t)mantissa;
    shape.words[1] = (shape.words[1] & UINT64_C(0xffff000000000000)) | (uint64_t)(mantissa >> 64 & UINT64_C(0xffffffffffff));
#elif LDBL_MANT_DIG == 53
    shape.bits = (shape.bits & UINT64_C(0xfff0000000000000)) | (uint64_t)(mantissa & UINT64_C(0xfffffffffffff));
#endif
    return shape.value;
}
static long double v_format_frexpl(long double value, int *exponent) {
    *exponent = 0;
    if (!value || !isfinite(value)) return value;
#if LDBL_MANT_DIG == 64 && LDBL_MAX_EXP == 16384 && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    union { long double value; struct { uint64_t mantissa; uint16_t exponent; } bits; } shape = {value};
    unsigned int encoded = shape.bits.exponent & 0x7fff;
    if (!encoded) {
        value = v_format_frexpl(value * 0x1p120L, exponent);
        *exponent -= 120;
        return value;
    }
    *exponent = (int)encoded - 0x3ffe;
    shape.bits.exponent = (shape.bits.exponent & 0x8000) | 0x3ffe;
    return shape.value;
#elif LDBL_MANT_DIG == 113 && LDBL_MAX_EXP == 16384 && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    union { long double value; uint64_t words[2]; } shape = {value};
    unsigned int encoded = (unsigned int)(shape.words[1] >> 48) & 0x7fff;
    if (!encoded) {
        value = v_format_frexpl(value * 0x1p120L, exponent);
        *exponent -= 120;
        return value;
    }
    *exponent = (int)encoded - 0x3ffe;
    shape.words[1] = (shape.words[1] & UINT64_C(0x8000ffffffffffff)) | UINT64_C(0x3ffe000000000000);
    return shape.value;
#elif LDBL_MANT_DIG == 53 && LDBL_MAX_EXP == 1024
    union { double value; uint64_t bits; } shape = {(double)value};
    unsigned int encoded = (unsigned int)(shape.bits >> 52) & 0x7ff;
    if (!encoded) {
        value = v_format_frexpl(value * 0x1p64L, exponent);
        *exponent -= 64;
        return value;
    }
    *exponent = (int)encoded - 0x3fe;
    shape.bits = (shape.bits & UINT64_C(0x800fffffffffffff)) | UINT64_C(0x3fe0000000000000);
    return shape.value;
#else
#error unsupported standalone long double representation
#endif
}

/* Some useful macros */

#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define MIN(a,b) ((a)<(b) ? (a) : (b))

/* Convenient bit representation for modifier flags, which all fall
 * within 31 codepoints of the space character. */

#define ALT_FORM   (1U << ('#' - ' '))
#define ZERO_PAD   (1U << ('0' - ' '))
#define LEFT_ADJ   (1U << ('-' - ' '))
#define PAD_POS    (1U << (' ' - ' '))
#define MARK_POS   (1U << ('+' - ' '))
#define GROUPED    (1U << ('\'' - ' '))

#define FLAGMASK (ALT_FORM|ZERO_PAD|LEFT_ADJ|PAD_POS|MARK_POS|GROUPED)

/* State machine to accept length modifiers + conversion specifiers.
 * Result is 0 on failure, or an argument type to pop on success. */

enum {
	BARE, LPRE, LLPRE, HPRE, HHPRE, BIGLPRE,
	ZTPRE, JPRE,
	STOP,
	PTR, INT, UINT, ULLONG,
	LONG, ULONG,
	SHORT, USHORT, CHAR, UCHAR,
	LLONG, SIZET, IMAX, UMAX, PDIFF, UIPTR,
	DBL, LDBL,
	NOARG,
	MAXSTATE
};

#define S(x) [(x)-'A']

static const unsigned char states[]['z'-'A'+1] = {
	{ /* 0: bare types */
		S('d') = INT, S('i') = INT,
		S('o') = UINT, S('u') = UINT, S('x') = UINT, S('X') = UINT,
		S('e') = DBL, S('f') = DBL, S('g') = DBL, S('a') = DBL,
		S('E') = DBL, S('F') = DBL, S('G') = DBL, S('A') = DBL,
		S('c') = INT, S('C') = UINT,
		S('s') = PTR, S('S') = PTR, S('p') = UIPTR, S('n') = PTR,
		S('m') = NOARG,
		S('l') = LPRE, S('h') = HPRE, S('L') = BIGLPRE,
		S('z') = ZTPRE, S('j') = JPRE, S('t') = ZTPRE,
	}, { /* 1: l-prefixed */
		S('d') = LONG, S('i') = LONG,
		S('o') = ULONG, S('u') = ULONG, S('x') = ULONG, S('X') = ULONG,
		S('e') = DBL, S('f') = DBL, S('g') = DBL, S('a') = DBL,
		S('E') = DBL, S('F') = DBL, S('G') = DBL, S('A') = DBL,
		S('c') = UINT, S('s') = PTR, S('n') = PTR,
		S('l') = LLPRE,
	}, { /* 2: ll-prefixed */
		S('d') = LLONG, S('i') = LLONG,
		S('o') = ULLONG, S('u') = ULLONG,
		S('x') = ULLONG, S('X') = ULLONG,
		S('n') = PTR,
	}, { /* 3: h-prefixed */
		S('d') = SHORT, S('i') = SHORT,
		S('o') = USHORT, S('u') = USHORT,
		S('x') = USHORT, S('X') = USHORT,
		S('n') = PTR,
		S('h') = HHPRE,
	}, { /* 4: hh-prefixed */
		S('d') = CHAR, S('i') = CHAR,
		S('o') = UCHAR, S('u') = UCHAR,
		S('x') = UCHAR, S('X') = UCHAR,
		S('n') = PTR,
	}, { /* 5: L-prefixed */
		S('e') = LDBL, S('f') = LDBL, S('g') = LDBL, S('a') = LDBL,
		S('E') = LDBL, S('F') = LDBL, S('G') = LDBL, S('A') = LDBL,
		S('n') = PTR,
	}, { /* 6: z- or t-prefixed (assumed to be same size) */
		S('d') = PDIFF, S('i') = PDIFF,
		S('o') = SIZET, S('u') = SIZET,
		S('x') = SIZET, S('X') = SIZET,
		S('n') = PTR,
	}, { /* 7: j-prefixed */
		S('d') = IMAX, S('i') = IMAX,
		S('o') = UMAX, S('u') = UMAX,
		S('x') = UMAX, S('X') = UMAX,
		S('n') = PTR,
	}
};

#define OOB(x) ((unsigned)(x)-'A' > 'z'-'A')

union arg
{
	uintmax_t i;
	long double f;
	void *p;
};

static void pop_arg(union arg *arg, int type, va_list *ap)
{
	switch (type) {
	       case PTR:	arg->p = va_arg(*ap, void *);
	break; case INT:	arg->i = va_arg(*ap, int);
	break; case UINT:	arg->i = va_arg(*ap, unsigned int);
	break; case LONG:	arg->i = va_arg(*ap, long);
	break; case ULONG:	arg->i = va_arg(*ap, unsigned long);
	break; case ULLONG:	arg->i = va_arg(*ap, unsigned long long);
	break; case SHORT:	arg->i = (short)va_arg(*ap, int);
	break; case USHORT:	arg->i = (unsigned short)va_arg(*ap, int);
	break; case CHAR:	arg->i = (signed char)va_arg(*ap, int);
	break; case UCHAR:	arg->i = (unsigned char)va_arg(*ap, int);
	break; case LLONG:	arg->i = va_arg(*ap, long long);
	break; case SIZET:	arg->i = va_arg(*ap, size_t);
	break; case IMAX:	arg->i = va_arg(*ap, intmax_t);
	break; case UMAX:	arg->i = va_arg(*ap, uintmax_t);
	break; case PDIFF:	arg->i = va_arg(*ap, ptrdiff_t);
	break; case UIPTR:	arg->i = (uintptr_t)va_arg(*ap, void *);
	break; case DBL:	arg->f = va_arg(*ap, double);
	break; case LDBL:	arg->f = va_arg(*ap, long double);
	}
}

static int v_format_reserve(struct v_format_output *f, size_t size) {
    if (!f->growing) return 0;
    if (size > SIZE_MAX - f->used - 1) { errno = EOVERFLOW; f->error = 1; return -1; }
    size_t needed = f->used + size + 1;
    if (needed <= f->capacity) return 0;
    size_t capacity = f->capacity ? f->capacity : 128;
    while (capacity < needed) { if (capacity > SIZE_MAX / 2) { capacity = needed; break; } capacity *= 2; }
    char *buffer = realloc(f->buffer, capacity);
    if (!buffer) { errno = ENOMEM; f->error = 1; return -1; }
    f->buffer = buffer; f->capacity = capacity; return 0;
}

static void out(struct v_format_output *f, const char *text, size_t size)
{
    if (f->error || v_format_reserve(f, size) < 0) return;
    if (f->stream) {
        if (fwrite_unlocked(text, 1, size, f->stream) != size) f->error = 1;
    } else {
        size_t room = f->capacity && f->used < f->capacity - 1 ? f->capacity - 1 - f->used : 0;
        if (room > size) room = size;
        if (room) memcpy(f->buffer + f->used, text, room);
    }
    f->used += size;
}

static void pad(struct v_format_output *f, char ch, int width, int length, int flags)
{
    if (flags & (LEFT_ADJ | ZERO_PAD) || length >= width) return;
    size_t size = (unsigned int)(width - length);
    if (!f->stream) {
        if (f->error || v_format_reserve(f, size) < 0) return;
        size_t room = f->capacity && f->used < f->capacity - 1 ? f->capacity - 1 - f->used : 0;
        if (room > size) room = size;
        if (room) memset(f->buffer + f->used, ch, room);
        f->used += size;
        return;
    }
    char bytes[256];
    memset(bytes, ch, sizeof bytes);
    while (size && !f->error) {
        size_t count = size < sizeof bytes ? size : sizeof bytes;
        out(f, bytes, count);
        size -= count;
    }
}

static const char xdigits[16] = {
	"0123456789ABCDEF"
};

static char *fmt_x(uintmax_t x, char *s, int lower)
{
	for (; x; x>>=4) *--s = xdigits[(x&15)]|lower;
	return s;
}

static char *fmt_o(uintmax_t x, char *s)
{
	for (; x; x>>=3) *--s = '0' + (x&7);
	return s;
}

static char *fmt_u(uintmax_t x, char *s)
{
	unsigned long y;
	for (   ; x>ULONG_MAX; x/=10) *--s = '0' + x%10;
	for (y=x;           y; y/=10) *--s = '0' + y%10;
	return s;
}

/* Do not override this check. The floating point printing code below
 * depends on the float.h constants being right. If they are wrong, it
 * may overflow the stack. */
#if LDBL_MANT_DIG == 53
typedef char compiler_defines_long_double_incorrectly[9-(int)sizeof(long double)];
#endif

static int fmt_fp(struct v_format_output *f, long double y, int w, int p, int fl, int t)
{
	uint32_t big[(LDBL_MANT_DIG+28)/29 + 1          // mantissa expansion
		+ (LDBL_MAX_EXP+LDBL_MANT_DIG+28+8)/9]; // exponent expansion
	uint32_t *a, *d, *r, *z;
	int e2=0, e, i, j, l;
	char buf[9+LDBL_MANT_DIG/4], *s;
	const char *prefix="-0X+0X 0X-0x+0x 0x";
	int pl;
	char ebuf0[3*sizeof(int)], *ebuf=&ebuf0[3*sizeof(int)], *estr;

	pl=1;
	if (signbit(y)) {
		y=-y;
	} else if (fl & MARK_POS) {
		prefix+=3;
	} else if (fl & PAD_POS) {
		prefix+=6;
	} else prefix++, pl=0;

	if (!isfinite(y)) {
		char *s = (t&32)?"inf":"INF";
		if (y!=y) s=(t&32)?"nan":"NAN";
		pad(f, ' ', w, 3+pl, fl&~ZERO_PAD);
		out(f, prefix, pl);
		out(f, s, 3);
		pad(f, ' ', w, 3+pl, fl^LEFT_ADJ);
		return MAX(w, 3+pl);
	}

	y = v_format_frexpl(y, &e2) * 2;
	if (y) e2--;

	if ((t|32)=='a') {
		if (t&32) prefix += 9;
		pl += 2;
		y = v_format_hex_round(y, p, *prefix == '-');

		estr=fmt_u(e2<0 ? -e2 : e2, ebuf);
		if (estr==ebuf) *--estr='0';
		*--estr = (e2<0 ? '-' : '+');
		*--estr = t+('p'-'a');

		s=buf;
		do {
			int x=y;
			*s++=xdigits[x]|(t&32);
			y=16*(y-x);
			if (s-buf==1 && (y||p>0||(fl&ALT_FORM))) *s++='.';
		} while (y);

		if (p > INT_MAX-2-(ebuf-estr)-pl)
			return -1;
		if (p && s-buf-2 < p)
			l = (p+2) + (ebuf-estr);
		else
			l = (s-buf) + (ebuf-estr);

		pad(f, ' ', w, pl+l, fl);
		out(f, prefix, pl);
		pad(f, '0', w, pl+l, fl^ZERO_PAD);
		out(f, buf, s-buf);
		pad(f, '0', l-(ebuf-estr)-(s-buf), 0, 0);
		out(f, estr, ebuf-estr);
		pad(f, ' ', w, pl+l, fl^LEFT_ADJ);
		return MAX(w, pl+l);
	}
	if (p<0) p=6;

	if (y) y *= 0x1p28, e2-=28;

	if (e2<0) a=r=z=big;
	else a=r=z=big+sizeof(big)/sizeof(*big) - LDBL_MANT_DIG - 1;

	do {
		*z = y;
		y = 1000000000*(y-*z++);
	} while (y);

	while (e2>0) {
		uint32_t carry=0;
		int sh=MIN(29,e2);
		for (d=z-1; d>=a; d--) {
			uint64_t x = ((uint64_t)*d<<sh)+carry;
			*d = x % 1000000000;
			carry = x / 1000000000;
		}
		if (carry) *--a = carry;
		while (z>a && !z[-1]) z--;
		e2-=sh;
	}
	while (e2<0) {
		uint32_t carry=0, *b;
		int sh=MIN(9,-e2), need=1+(p+LDBL_MANT_DIG/3U+8)/9;
		for (d=a; d<z; d++) {
			uint32_t rm = *d & (1<<sh)-1;
			*d = (*d>>sh) + carry;
			carry = (1000000000>>sh) * rm;
		}
		if (!*a) a++;
		if (carry) *z++ = carry;
		/* Avoid (slow!) computation past requested precision */
		b = (t|32)=='f' ? r : a;
		if (z-b > need) z = b+need;
		e2+=sh;
	}

	if (a<z) for (i=10, e=9*(r-a); *a>=i; i*=10, e++);
	else e=0;

	/* Perform rounding: j is precision after the radix (possibly neg) */
	int64_t requested = (int64_t)p - ((t|32)!='f')*e - ((t|32)=='g' && p);
	if (requested < 9*(z-r-1)) {
		j = (int)requested;
		uint32_t x;
		/* We avoid C's broken division of negative numbers */
		d = r + 1 + ((j+9*LDBL_MAX_EXP)/9 - LDBL_MAX_EXP);
		j += 9*LDBL_MAX_EXP;
		j %= 9;
		for (i=10, j++; j<9; i*=10, j++);
		x = *d % i;
		/* Are there any significant digits past j? */
		if (x || d+1!=z) {
			int odd = (*d/i & 1) || (i==1000000000 && d>a && (d[-1]&1));
			*d -= x;
			if (v_format_increment(x, i/2, d+1!=z, odd, pl && *prefix=='-')) {
				*d = *d + i;
				while (*d > 999999999) {
					*d--=0;
					if (d<a) *--a=0;
					(*d)++;
				}
				for (i=10, e=9*(r-a); *a>=i; i*=10, e++);
			}
		}
		if (z>d+1) z=d+1;
	}
	for (; z>a && !z[-1]; z--);

	if ((t|32)=='g') {
		int64_t precision = p ? p : 1;
		if (precision>e && e>=-4) {
			t--;
			precision-=e+1;
		} else {
			t-=2;
			precision--;
		}
		if (!(fl&ALT_FORM)) {
			/* Count trailing zeros in last place */
			if (z>a && z[-1]) for (i=10, j=0; z[-1]%i==0; i*=10, j++);
			else j=9;
			if ((t|32)=='f')
				precision = MIN(precision,MAX(0,9*(z-r-1)-j));
			else
				precision = MIN(precision,MAX(0,9*(z-r-1)+e-j));
		}
		if (precision > INT_MAX) return -1;
		p = (int)precision;
	}
	if (p > INT_MAX-1-(p || (fl&ALT_FORM)))
		return -1;
	l = 1 + p + (p || (fl&ALT_FORM));
	if ((t|32)=='f') {
		if (e > INT_MAX-l) return -1;
		if (e>0) l+=e;
	} else {
		estr=fmt_u(e<0 ? -e : e, ebuf);
		while(ebuf-estr<2) *--estr='0';
		*--estr = (e<0 ? '-' : '+');
		*--estr = t;
		if (ebuf-estr > INT_MAX-l) return -1;
		l += ebuf-estr;
	}

	if (l > INT_MAX-pl) return -1;
	pad(f, ' ', w, pl+l, fl);
	out(f, prefix, pl);
	pad(f, '0', w, pl+l, fl^ZERO_PAD);

	if ((t|32)=='f') {
		if (a>r) a=r;
		for (d=a; d<=r; d++) {
			char *s = fmt_u(*d, buf+9);
			if (d!=a) while (s>buf) *--s='0';
			else if (s==buf+9) *--s='0';
			out(f, s, buf+9-s);
		}
		if (p || (fl&ALT_FORM)) out(f, ".", 1);
		for (; d<z && p>0; d++, p-=9) {
			char *s = fmt_u(*d, buf+9);
			while (s>buf) *--s='0';
			out(f, s, MIN(9,p));
		}
		pad(f, '0', p, 0, 0);
	} else {
		if (z<=a) z=a+1;
		for (d=a; d<z && p>=0; d++) {
			char *s = fmt_u(*d, buf+9);
			if (s==buf+9) *--s='0';
			if (d!=a) while (s>buf) *--s='0';
			else {
				out(f, s++, 1);
				if (p>0||(fl&ALT_FORM)) out(f, ".", 1);
			}
			out(f, s, MIN(buf+9-s, p));
			p -= buf+9-s;
		}
		pad(f, '0', p, 0, 0);
		out(f, estr, ebuf-estr);
	}

	pad(f, ' ', w, pl+l, fl^LEFT_ADJ);

	return MAX(w, pl+l);
}

static int getint(char **s) {
	int i;
	for (i=0; v_format_isdigit(**s); (*s)++) {
		if (i > INT_MAX/10U || **s-'0' > INT_MAX-10*i) i = -1;
		else i = 10*i + (**s-'0');
	}
	return i;
}

static int printf_core(struct v_format_output *f, const char *fmt, va_list *ap, union arg *nl_arg, int *nl_type)
{
	char *a, *z, *s=(char *)fmt;
	unsigned l10n=0, fl;
	int w, p, xp;
	union arg arg;
	int argpos;
	unsigned st, ps;
	int cnt=0, l=0;
	size_t i;
	char buf[sizeof(uintmax_t)*3];
	const char *prefix;
	int t, pl;
	wchar_t wc[2], *ws;
	char mb[4];

	for (;;) {
		/* This error is only specified for snprintf, but since it's
		 * unspecified for other forms, do the same. Stop immediately
		 * on overflow; otherwise %n could produce wrong results. */
		if (l > INT_MAX - cnt) goto overflow;

		/* Update output count, end loop when fmt is exhausted */
		cnt += l;
		if (!*s) break;

		/* Handle literal text and %% format specifiers */
		for (a=s; *s && *s!='%'; s++);
		for (z=s; s[0]=='%' && s[1]=='%'; z++, s+=2);
		if (z-a > INT_MAX-cnt) goto overflow;
		l = z-a;
		if (f) out(f, a, l);
		if (l) continue;

		if (v_format_isdigit(s[1]) && s[2]=='$') {
			l10n=1;
			argpos = s[1]-'0';
			s+=3;
		} else {
			argpos = -1;
			s++;
		}

		/* Read modifier flags */
		for (fl=0; (unsigned)*s-' '<32 && (FLAGMASK & (1U << (*s - ' '))); s++)
			fl |= 1U << (*s - ' ');

		/* Read field width */
		if (*s=='*') {
			if (v_format_isdigit(s[1]) && s[2]=='$') {
				l10n=1;
				if (!f) nl_type[s[1]-'0'] = INT, w = 0;
				else w = nl_arg[s[1]-'0'].i;
				s+=3;
			} else if (!l10n) {
				w = f ? va_arg(*ap, int) : 0;
				s++;
			} else goto inval;
			if (w == INT_MIN) goto overflow;
			if (w<0) fl|=LEFT_ADJ, w=-w;
		} else if ((w=getint(&s))<0) goto overflow;

		/* Read precision */
		if (*s=='.' && s[1]=='*') {
			if (v_format_isdigit(s[2]) && s[3]=='$') {
				if (!f) nl_type[s[2]-'0'] = INT, p = 0;
				else p = nl_arg[s[2]-'0'].i;
				s+=4;
			} else if (!l10n) {
				p = f ? va_arg(*ap, int) : 0;
				s+=2;
			} else goto inval;
			xp = (p>=0);
		} else if (*s=='.') {
			s++;
			p = getint(&s);
			xp = 1;
		} else {
			p = -1;
			xp = 0;
		}

		/* Format specifier state machine */
		st=0;
		do {
			if (OOB(*s)) goto inval;
			ps=st;
			st=states[st]S(*s++);
		} while (st-1<STOP);
		if (!st) goto inval;

		/* Check validity of argument type (nl/normal) */
		if (st==NOARG) {
			if (argpos>=0) goto inval;
		} else {
			if (argpos>=0) {
				if (!f) nl_type[argpos]=st;
				else arg=nl_arg[argpos];
			} else if (f) pop_arg(&arg, st, ap);
			else return 0;
		}

		if (!f) continue;

		/* Do not process any new directives once in error state. */
		if (f->error) return -1;

		z = buf + sizeof(buf);
		prefix = "-+   0X0x";
		pl = 0;
		t = s[-1];

		/* Transform ls,lc -> S,C */
		if (ps && (t&15)==3) t&=~32;

		/* - and 0 flags are mutually exclusive */
		if (fl & LEFT_ADJ) fl &= ~ZERO_PAD;

		switch(t) {
		case 'n':
			switch(ps) {
			case BARE: *(int *)arg.p = cnt; break;
			case LPRE: *(long *)arg.p = cnt; break;
			case LLPRE: *(long long *)arg.p = cnt; break;
			case HPRE: *(unsigned short *)arg.p = cnt; break;
			case HHPRE: *(unsigned char *)arg.p = cnt; break;
			case ZTPRE: *(size_t *)arg.p = cnt; break;
			case JPRE: *(uintmax_t *)arg.p = cnt; break;
			}
			continue;
		case 'p':
			p = MAX(p, 2*sizeof(void*));
			t = 'x';
			fl |= ALT_FORM;
		case 'x': case 'X':
			a = fmt_x(arg.i, z, t&32);
			if (arg.i && (fl & ALT_FORM)) prefix+=(t>>4), pl=2;
			if (0) {
		case 'o':
			a = fmt_o(arg.i, z);
			if ((fl&ALT_FORM) && p<z-a+1) p=z-a+1;
			} if (0) {
		case 'd': case 'i':
			pl=1;
			if (arg.i>INTMAX_MAX) {
				arg.i=-arg.i;
			} else if (fl & MARK_POS) {
				prefix++;
			} else if (fl & PAD_POS) {
				prefix+=2;
			} else pl=0;
		case 'u':
			a = fmt_u(arg.i, z);
			}
			if (xp && p<0) goto overflow;
			if (xp) fl &= ~ZERO_PAD;
			if (!arg.i && !p) {
				a=z;
				break;
			}
			p = MAX(p, z-a + !arg.i);
			break;
		narrow_c:
		case 'c':
			*(a=z-(p=1))=arg.i;
			fl &= ~ZERO_PAD;
			break;
		case 'm':
			if (1) a = strerror(errno); else
		case 's':
			a = arg.p ? arg.p : "(null)";
			z = a + v_format_strnlen(a, p<0 ? INT_MAX : p);
			if (p<0 && *z) goto overflow;
			p = z-a;
			fl &= ~ZERO_PAD;
			break;
		case 'C':
			if (!arg.i) goto narrow_c;
			wc[0] = arg.i;
			wc[1] = 0;
			arg.p = wc;
			p = -1;
		case 'S':
			ws = arg.p;
			for (i=l=0; i<p && *ws && (l=v_format_wctomb(mb, *ws++))>=0 && l<=p-i; i+=l);
			if (l<0) return -1;
			if (i > INT_MAX) goto overflow;
			p = i;
			pad(f, ' ', w, p, fl);
			ws = arg.p;
			for (i=0; i<0U+p && *ws && i+(l=v_format_wctomb(mb, *ws++))<=p; i+=l)
				out(f, mb, l);
			pad(f, ' ', w, p, fl^LEFT_ADJ);
			l = w>p ? w : p;
			continue;
		case 'e': case 'f': case 'g': case 'a':
		case 'E': case 'F': case 'G': case 'A':
			if (xp && p<0) goto overflow;
			l = fmt_fp(f, arg.f, w, p, fl, t);
			if (l<0) goto overflow;
			continue;
		}

		if (p < z-a) p = z-a;
		if (p > INT_MAX-pl) goto overflow;
		if (w < pl+p) w = pl+p;
		if (w > INT_MAX-cnt) goto overflow;

		pad(f, ' ', w, pl+p, fl);
		out(f, prefix, pl);
		pad(f, '0', w, pl+p, fl^ZERO_PAD);
		pad(f, '0', p, z-a, 0);
		out(f, a, z-a);
		pad(f, ' ', w, pl+p, fl^LEFT_ADJ);

		l = w;
	}

	if (f) return cnt;
	if (!l10n) return 0;

	for (i=1; i<=NL_ARGMAX && nl_type[i]; i++)
		pop_arg(nl_arg+i, nl_type[i], ap);
	for (; i<=NL_ARGMAX && !nl_type[i]; i++);
	if (i<=NL_ARGMAX) goto inval;
	return 1;

inval:
	errno = EINVAL;
	return -1;
overflow:
	errno = EOVERFLOW;
	return -1;
}


static int v_format(struct v_format_output *output, const char *format, va_list arguments) {
    int types[NL_ARGMAX + 1] = {0};
    union arg values[NL_ARGMAX + 1];
    va_list copy;
    va_copy(copy, arguments);
    int result = printf_core(NULL, format, &copy, values, types);
    if (result >= 0) result = printf_core(output, format, &copy, values, types);
    va_end(copy);
    return output->error ? -1 : result;
}
int vsnprintf(char *buffer, size_t capacity, const char *format, va_list arguments) {
    if (capacity > INT_MAX) { errno = EOVERFLOW; return -1; }
    struct v_format_output output = {NULL, buffer, capacity, 0, 0};
    int result = v_format(&output, format, arguments);
    if (capacity) buffer[output.used < capacity ? output.used : capacity - 1] = 0;
    return result;
}
int snprintf(char *buffer, size_t capacity, const char *format, ...) {
    va_list arguments;
    va_start(arguments, format);
    int result = vsnprintf(buffer, capacity, format, arguments);
    va_end(arguments);
    return result;
}
int vfprintf(FILE *stream, const char *format, va_list arguments) {
    uint32_t shield = v_cancel_shield_enter();
    struct v_format_output output = {stream, NULL, 0, 0, 0};
    flockfile(stream);
    int result = v_format(&output, format, arguments);
    funlockfile(stream);
    v_cancel_shield_leave(shield);
    return result;
}
int fprintf(FILE *stream, const char *format, ...) {
    va_list arguments;
    va_start(arguments, format);
    int result = vfprintf(stream, format, arguments);
    va_end(arguments);
    return result;
}
int vprintf(const char *format, va_list arguments) { return vfprintf(stdout, format, arguments); }
int printf(const char *format, ...) {
    va_list arguments;
    va_start(arguments, format);
    int result = vfprintf(stdout, format, arguments);
    va_end(arguments);
    return result;
}

int vsprintf(char *buffer, const char *format, va_list args) {
    struct v_format_output output = {.buffer = buffer, .capacity = INT_MAX};
    int result = v_format(&output, format, args);
    buffer[output.used] = 0;
    return result;
}
int sprintf(char *buffer, const char *format, ...) {
    va_list args; va_start(args, format);
    int result = vsprintf(buffer, format, args); va_end(args); return result;
}
int vasprintf(char **result, const char *format, va_list args) {
    *result = NULL;
    struct v_format_output output = {.growing = 1};
    int count = v_format(&output, format, args);
    if (count < 0 || v_format_reserve(&output, 0) < 0) { free(output.buffer); return -1; }
    output.buffer[output.used] = 0; *result = output.buffer; return count;
}
int asprintf(char **result, const char *format, ...) {
    va_list args; va_start(args, format);
    int count = vasprintf(result, format, args); va_end(args); return count;
}
