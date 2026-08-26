module fastc

import os
import strings
import v3.gen.c.naming
import v3.pref
import v3.scanner
import v3.token

// FastC parses scanner tokens and emits C immediately. It deliberately has no
// AST, semantic-checker, transformer, mark-used, or conventional cgen path.

const c_preamble = r'#ifndef __cplusplus
typedef _Bool bool;
#define true ((bool)1)
#define false ((bool)0)
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef unsigned char byte;
typedef int32_t rune;
typedef float f32;
typedef double f64;
typedef const char *string;
typedef void *voidptr;
typedef unsigned char *byteptr;
typedef char *charptr;
typedef void *chan;
typedef struct { void *data; int offset; int len; int cap; int flags; } array;
typedef struct { void *data; int len; } map;
typedef struct { void *data; void *err; unsigned char state; } Option;
typedef union { uintptr_t word; long double alignment; unsigned char data[64]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) value = (expression); MultiReturnValue result = {0}; memcpy(result.data, &value, sizeof(value)); result; })

static void v_fastc_print_string(const char *value) { fputs(value ? value : "", stdout); }
static void v_fastc_print_bool(bool value) { fputs(value ? "true" : "false", stdout); }
static void v_fastc_print_char(char value) { fputc(value, stdout); }
static void v_fastc_print_signed(long long value) { printf("%lld", value); }
static void v_fastc_print_unsigned(unsigned long long value) { printf("%llu", value); }
static void v_fastc_println_string(const char *value) { puts(value ? value : ""); }
static void v_fastc_println_bool(bool value) { puts(value ? "true" : "false"); }
static void v_fastc_println_char(char value) { fputc(value, stdout); fputc(10, stdout); }
static void v_fastc_println_signed(long long value) { printf("%lld\n", value); }
static void v_fastc_println_unsigned(unsigned long long value) { printf("%llu\n", value); }
static bool builtin__string_eq(const char *left, const char *right) { return strcmp(left ? left : "", right ? right : "") == 0; }
static bool builtin__string_lt(const char *left, const char *right) { return strcmp(left ? left : "", right ? right : "") < 0; }
#define _S(value) ((string)(value))
#define _SLIT0 ((string)"")
static string builtin__string_plus_many(int count, string *parts) {
	size_t total = 0;
	for (int i = 0; i < count; i++) total += strlen(parts[i] ? parts[i] : "");
	char *result = malloc(total + 1);
	if (result == NULL) return "";
	char *cursor = result;
	for (int i = 0; i < count; i++) {
		const char *part = parts[i] ? parts[i] : "";
		size_t length = strlen(part);
		memcpy(cursor, part, length);
		cursor += length;
	}
	*cursor = 0;
	return result;
}
static string v_fastc_signed_str(long long value) {
	char *result = malloc(32);
	if (result == NULL) return "";
	snprintf(result, 32, "%lld", value);
	return result;
}
static string v_fastc_unsigned_str(unsigned long long value) {
	char *result = malloc(32);
	if (result == NULL) return "";
	snprintf(result, 32, "%llu", value);
	return result;
}
static int v_fastc_utf8_next_cp(const unsigned char *value, size_t length, size_t *index) {
	unsigned char first = value[*index];
	if (first < 0x80) {
		(*index)++;
		return first;
	}
	size_t bytes = (first & 0xE0) == 0xC0 ? 2 : ((first & 0xF0) == 0xE0 ? 3 : ((first & 0xF8) == 0xF0 ? 4 : 1));
	if (*index + bytes > length) {
		(*index)++;
		return first;
	}
	int codepoint = first & (bytes == 2 ? 0x1F : (bytes == 3 ? 0x0F : (bytes == 4 ? 0x07 : 0x7F)));
	for (size_t offset = 1; offset < bytes; offset++) codepoint = (codepoint << 6) | (value[*index + offset] & 0x3F);
	*index += bytes;
	return codepoint;
}
static int v_fastc_codepoint_is_combining(int codepoint) {
	return (codepoint >= 0x0300 && codepoint <= 0x036F) || (codepoint >= 0x1AB0 && codepoint <= 0x1AFF) || (codepoint >= 0x1DC0 && codepoint <= 0x1DFF) || (codepoint >= 0x20D0 && codepoint <= 0x20FF) || (codepoint >= 0xFE00 && codepoint <= 0xFE0F) || (codepoint >= 0xFE20 && codepoint <= 0xFE2F) || (codepoint >= 0x1F3FB && codepoint <= 0x1F3FF) || codepoint == 0x0E31 || (codepoint >= 0x0E34 && codepoint <= 0x0E3A) || (codepoint >= 0x0E47 && codepoint <= 0x0E4E);
}
static int v_fastc_codepoint_is_wide(int codepoint) {
	return (codepoint >= 0x1100 && codepoint <= 0x115F) || (codepoint >= 0x2329 && codepoint <= 0x232A) || (codepoint >= 0x2E80 && codepoint <= 0xA4CF) || (codepoint >= 0xAC00 && codepoint <= 0xD7A3) || (codepoint >= 0xF900 && codepoint <= 0xFAFF) || (codepoint >= 0xFE10 && codepoint <= 0xFE19) || (codepoint >= 0xFE30 && codepoint <= 0xFE6F) || (codepoint >= 0xFF00 && codepoint <= 0xFF60) || (codepoint >= 0xFFE0 && codepoint <= 0xFFE6) || (codepoint >= 0x1F000 && codepoint <= 0x1FAFF);
}
static int v_fastc_utf8_display_width(const char *value) {
	const unsigned char *text = (const unsigned char *)(value ? value : "");
	size_t length = strlen((const char *)text);
	size_t index = 0;
	int width = 0;
	int joined = 0;
	while (index < length) {
		int codepoint = v_fastc_utf8_next_cp(text, length, &index);
		if (codepoint == 0x200D) {
			joined = 1;
			continue;
		}
		if (v_fastc_codepoint_is_combining(codepoint)) continue;
		if (joined) {
			joined = 0;
			continue;
		}
		width += v_fastc_codepoint_is_wide(codepoint) ? 2 : 1;
	}
	return width;
}
static string v_fastc_string_pad(const char *value, int width, bool left_align) {
	const char *text = value ? value : "";
	if (width < 0) {
		left_align = true;
		width = -width;
	}
	int visible = v_fastc_utf8_display_width(text);
	if (visible >= width) return text;
	size_t length = strlen(text);
	int padding = width - visible;
	char *result = malloc(length + (size_t)padding + 1);
	if (result == NULL) return "";
	if (left_align) {
		memcpy(result, text, length);
		memset(result + length, 32, (size_t)padding);
	} else {
		memset(result, 32, (size_t)padding);
		memcpy(result + padding, text, length);
	}
	result[length + (size_t)padding] = 0;
	return result;
}
static string v_fastc_integer_format(unsigned long long magnitude, bool negative, const char *format) {
	size_t format_len = strlen(format);
	char specifier = format_len > 0 ? format[format_len - 1] : 100;
	bool left_align = format_len > 1 && format[0] == 45;
	size_t width_start = left_align ? 1 : 0;
	bool zero_pad = !left_align && width_start < format_len - 1 && format[width_start] == 48;
	int width = 0;
	for (size_t i = width_start; i + 1 < format_len; i++) width = width * 10 + format[i] - 48;
	unsigned base = specifier == 120 || specifier == 88 ? 16 : specifier == 111 ? 8 : specifier == 98 ? 2 : 10;
	char reversed[65];
	int digit_count = 0;
	unsigned char encoded_rune[4];
	int rune_byte_count = 0;
	int rune_display_width = 0;
	if (specifier == 99 && negative) {
		negative = false;
		zero_pad = false;
	} else if (specifier == 99) {
		bool valid_codepoint = magnitude <= 1114111;
		unsigned codepoint = (unsigned)magnitude;
		if (valid_codepoint && codepoint >= 55296 && codepoint <= 57343) codepoint = 65533;
		rune_display_width = !valid_codepoint || codepoint == 0x200D || v_fastc_codepoint_is_combining(codepoint) ? 0 : (v_fastc_codepoint_is_wide(codepoint) ? 2 : 1);
		if (valid_codepoint && codepoint <= 127) {
			encoded_rune[rune_byte_count++] = (unsigned char)codepoint;
		} else if (valid_codepoint && codepoint <= 2047) {
			encoded_rune[rune_byte_count++] = (unsigned char)(192 | (codepoint >> 6));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		} else if (valid_codepoint && codepoint <= 65535) {
			encoded_rune[rune_byte_count++] = (unsigned char)(224 | (codepoint >> 12));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 6) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		} else if (valid_codepoint) {
			encoded_rune[rune_byte_count++] = (unsigned char)(240 | (codepoint >> 18));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 12) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 6) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		}
		negative = false;
		zero_pad = false;
	} else {
		const char *digits = specifier == 88 ? "0123456789ABCDEF" : "0123456789abcdef";
		do {
			reversed[digit_count++] = digits[magnitude % base];
			magnitude /= base;
		} while (magnitude != 0);
	}
	int display_len = specifier == 99 ? rune_display_width : digit_count + (negative ? 1 : 0);
	int padding = width > display_len ? width - display_len : 0;
	int content_bytes = specifier == 99 ? rune_byte_count : digit_count + (negative ? 1 : 0);
	int result_len = content_bytes + padding;
	char *result = malloc((size_t)result_len + 1);
	if (result == NULL) return "";
	int cursor = 0;
	if (!left_align && !zero_pad) while (cursor < padding) result[cursor++] = 32;
	if (negative) result[cursor++] = 45;
	if (!left_align && zero_pad) while (cursor < padding + (negative ? 1 : 0)) result[cursor++] = 48;
	if (specifier == 99) {
		for (int i = 0; i < rune_byte_count; i++) result[cursor++] = (char)encoded_rune[i];
	} else {
		while (digit_count > 0) result[cursor++] = reversed[--digit_count];
	}
	if (left_align) while (cursor < result_len) result[cursor++] = 32;
	result[cursor] = 0;
	return result;
}
static string v_fastc_signed_format(long long value, const char *format) {
	bool negative = value < 0;
	unsigned long long magnitude = negative ? (unsigned long long)(-(value + 1)) + 1 : (unsigned long long)value;
	return v_fastc_integer_format(magnitude, negative, format);
}
static string v_fastc_unsigned_format(unsigned long long value, const char *format) {
	return v_fastc_integer_format(value, false, format);
}
static string v_fastc_bool_str(bool value) { return value ? "true" : "false"; }

/* Float formatting belongs to the V strconv routines. Leaving float and double
 * unmatched makes TinyCC reject unsupported printing instead of silently
 * applying printf %g semantics. */
#define V_FASTC_PRINT_SELECT(value, string_fn, bool_fn, char_fn, signed_fn, unsigned_fn) _Generic((value), char *: string_fn, const char *: string_fn, bool: bool_fn, char: char_fn, signed char: signed_fn, short: signed_fn, int: signed_fn, long: signed_fn, long long: signed_fn, unsigned char: unsigned_fn, unsigned short: unsigned_fn, unsigned int: unsigned_fn, unsigned long: unsigned_fn, unsigned long long: unsigned_fn)(value)
#define print(value) V_FASTC_PRINT_SELECT(value, v_fastc_print_string, v_fastc_print_bool, v_fastc_print_char, v_fastc_print_signed, v_fastc_print_unsigned)
#define println(value) V_FASTC_PRINT_SELECT(value, v_fastc_println_string, v_fastc_println_bool, v_fastc_println_char, v_fastc_println_signed, v_fastc_println_unsigned)

'

const c_integer_comparison_helpers = r'// signed/unsigned comparisons preserve mathematical ordering
static inline bool v_fastc_us_eq(u64 a, i64 b) { return b >= 0 && a == (u64)b; }
static inline bool v_fastc_us_ne(u64 a, i64 b) { return b < 0 || a != (u64)b; }
static inline bool v_fastc_us_gt(u64 a, i64 b) { return b < 0 || a > (u64)b; }
static inline bool v_fastc_us_lt(u64 a, i64 b) { return b > 0 && a < (u64)b; }
static inline bool v_fastc_us_ge(u64 a, i64 b) { return b <= 0 || a >= (u64)b; }
static inline bool v_fastc_us_le(u64 a, i64 b) { return b >= 0 && a <= (u64)b; }

'

// Keep source identifiers distinct from C keywords while preserving the same
// spelling used by the conventional C generator.
const fastc_c_reserved_identifiers = {
	'asm':      true
	'auto':     true
	'break':    true
	'case':     true
	'char':     true
	'const':    true
	'continue': true
	'default':  true
	'do':       true
	'double':   true
	'else':     true
	'enum':     true
	'extern':   true
	'false':    true
	'float':    true
	'for':      true
	'goto':     true
	'if':       true
	'inline':   true
	'int':      true
	'long':     true
	'register': true
	'restrict': true
	'return':   true
	'short':    true
	'signed':   true
	'sizeof':   true
	'static':   true
	'stdin':    true
	'stderr':   true
	'stdout':   true
	'struct':   true
	'switch':   true
	'true':     true
	'typedef':  true
	'typeof':   true
	'union':    true
	'unix':     true
	'unsigned': true
	'void':     true
	'volatile': true
	'while':    true
}

fn fastc_c_identifier(name string) string {
	return if name in fastc_c_reserved_identifiers {
		'__v_fastc_keyword_${name}'
	} else {
		name
	}
}

const c_selfhost_preamble = r'#ifndef __cplusplus
typedef _Bool bool;
#define true ((bool)1)
#define false ((bool)0)
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef unsigned char byte;
typedef int32_t rune;
typedef float f32;
typedef double f64;
typedef void *voidptr;
typedef unsigned char *byteptr;
typedef char *charptr;
typedef void *chan;
typedef union { uintptr_t word; long double alignment; unsigned char data[64]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) value = (expression); MultiReturnValue result = {0}; memcpy(result.data, &value, sizeof(value)); result; })

#define _S(s) ((string){.str=(byteptr)("" s), .len=(sizeof(s)-1), .is_lit=1})
#define _SLIT0 _S("")

static void *v_fastc_interface_box(const void *value, usize size) {
	void *copy = malloc(size);
	if (copy != NULL) memcpy(copy, value, size);
	return copy;
}

static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};
static inline u64 wyhash64(u64 a, u64 b) { a ^= _wyp[0]; b ^= _wyp[1]; a *= 0xa0761d6478bd642full; b *= 0xe7037ed1a0b428dbull; return (a ^ (a >> 32)) ^ (b ^ (b >> 32)); }
static inline u64 wyhash(const void *key, size_t len, u64 seed, const u64 *secret) { const unsigned char *p = (const unsigned char *)key; u64 h = seed ^ secret[0] ^ (u64)len; for (size_t i = 0; i < len; i++) h = wyhash64(h ^ (u64)p[i], secret[(i + 1) & 3]); return h; }

'

const c_selfhost_runtime = r'static void vheap_alloc(void *p, u64 n) { (void)p; (void)n; }
static void vheap_free(void *p) { (void)p; }

static int v_fastc_string_compare(const string *left, const string *right) {
	int common = left->len < right->len ? left->len : right->len;
	int order = common > 0 ? memcmp(left->str, right->str, (size_t)common) : 0;
	if (order != 0) return order;
	return (left->len > right->len) - (left->len < right->len);
}

static bool v_fastc_string_contains(string value, string substring) {
	if (substring.len == 0) return true;
	if (substring.len > value.len) return false;
	for (int offset = 0; offset <= value.len - substring.len; offset++) {
		if (memcmp(value.str + offset, substring.str, (size_t)substring.len) == 0) return true;
	}
	return false;
}

static int v_fastc_utf8_next_cp(const unsigned char *value, size_t length, size_t *index) {
	unsigned char first = value[*index];
	if (first < 0x80) {
		(*index)++;
		return first;
	}
	size_t bytes = (first & 0xE0) == 0xC0 ? 2 : ((first & 0xF0) == 0xE0 ? 3 : ((first & 0xF8) == 0xF0 ? 4 : 1));
	if (*index + bytes > length) {
		(*index)++;
		return first;
	}
	int codepoint = first & (bytes == 2 ? 0x1F : (bytes == 3 ? 0x0F : (bytes == 4 ? 0x07 : 0x7F)));
	for (size_t offset = 1; offset < bytes; offset++) codepoint = (codepoint << 6) | (value[*index + offset] & 0x3F);
	*index += bytes;
	return codepoint;
}
static int v_fastc_codepoint_is_combining(int codepoint) {
	return (codepoint >= 0x0300 && codepoint <= 0x036F) || (codepoint >= 0x1AB0 && codepoint <= 0x1AFF) || (codepoint >= 0x1DC0 && codepoint <= 0x1DFF) || (codepoint >= 0x20D0 && codepoint <= 0x20FF) || (codepoint >= 0xFE00 && codepoint <= 0xFE0F) || (codepoint >= 0xFE20 && codepoint <= 0xFE2F) || (codepoint >= 0x1F3FB && codepoint <= 0x1F3FF) || codepoint == 0x0E31 || (codepoint >= 0x0E34 && codepoint <= 0x0E3A) || (codepoint >= 0x0E47 && codepoint <= 0x0E4E);
}
static int v_fastc_codepoint_is_wide(int codepoint) {
	return (codepoint >= 0x1100 && codepoint <= 0x115F) || (codepoint >= 0x2329 && codepoint <= 0x232A) || (codepoint >= 0x2E80 && codepoint <= 0xA4CF) || (codepoint >= 0xAC00 && codepoint <= 0xD7A3) || (codepoint >= 0xF900 && codepoint <= 0xFAFF) || (codepoint >= 0xFE10 && codepoint <= 0xFE19) || (codepoint >= 0xFE30 && codepoint <= 0xFE6F) || (codepoint >= 0xFF00 && codepoint <= 0xFF60) || (codepoint >= 0xFFE0 && codepoint <= 0xFFE6) || (codepoint >= 0x1F000 && codepoint <= 0x1FAFF);
}
static int v_fastc_string_display_width(string value) {
	size_t length = (size_t)value.len;
	size_t index = 0;
	int width = 0;
	int joined = 0;
	while (index < length) {
		int codepoint = v_fastc_utf8_next_cp(value.str, length, &index);
		if (codepoint == 0x200D) {
			joined = 1;
			continue;
		}
		if (v_fastc_codepoint_is_combining(codepoint)) continue;
		if (joined) {
			joined = 0;
			continue;
		}
		width += v_fastc_codepoint_is_wide(codepoint) ? 2 : 1;
	}
	return width;
}

static string v_fastc_string_pad(string value, int width, bool left_align) {
	if (width < 0) {
		left_align = true;
		width = -width;
	}
	int visible = v_fastc_string_display_width(value);
	if (visible >= width) return value;
	int padding = width - visible;
	byteptr result = malloc((size_t)value.len + (size_t)padding + 1);
	if (result == NULL) return _SLIT0;
	if (left_align) {
		memcpy(result, value.str, (size_t)value.len);
		memset(result + value.len, 32, (size_t)padding);
	} else {
		memset(result, 32, (size_t)padding);
		memcpy(result + padding, value.str, (size_t)value.len);
	}
	result[value.len + padding] = 0;
	return (string){.str = result, .len = value.len + padding, .is_lit = 0};
}

static string v_fastc_integer_format(unsigned long long magnitude, bool negative, const char *format) {
	size_t format_len = strlen(format);
	char specifier = format_len > 0 ? format[format_len - 1] : 100;
	bool left_align = format_len > 1 && format[0] == 45;
	size_t width_start = left_align ? 1 : 0;
	bool zero_pad = !left_align && width_start < format_len - 1 && format[width_start] == 48;
	int width = 0;
	for (size_t i = width_start; i + 1 < format_len; i++) width = width * 10 + format[i] - 48;
	unsigned base = specifier == 120 || specifier == 88 ? 16 : specifier == 111 ? 8 : specifier == 98 ? 2 : 10;
	char reversed[65];
	int digit_count = 0;
	unsigned char encoded_rune[4];
	int rune_byte_count = 0;
	int rune_display_width = 0;
	if (specifier == 99 && negative) {
		negative = false;
		zero_pad = false;
	} else if (specifier == 99) {
		bool valid_codepoint = magnitude <= 1114111;
		unsigned codepoint = (unsigned)magnitude;
		if (valid_codepoint && codepoint >= 55296 && codepoint <= 57343) codepoint = 65533;
		rune_display_width = !valid_codepoint || codepoint == 0x200D || v_fastc_codepoint_is_combining(codepoint) ? 0 : (v_fastc_codepoint_is_wide(codepoint) ? 2 : 1);
		if (valid_codepoint && codepoint <= 127) {
			encoded_rune[rune_byte_count++] = (unsigned char)codepoint;
		} else if (valid_codepoint && codepoint <= 2047) {
			encoded_rune[rune_byte_count++] = (unsigned char)(192 | (codepoint >> 6));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		} else if (valid_codepoint && codepoint <= 65535) {
			encoded_rune[rune_byte_count++] = (unsigned char)(224 | (codepoint >> 12));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 6) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		} else if (valid_codepoint) {
			encoded_rune[rune_byte_count++] = (unsigned char)(240 | (codepoint >> 18));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 12) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | ((codepoint >> 6) & 63));
			encoded_rune[rune_byte_count++] = (unsigned char)(128 | (codepoint & 63));
		}
		negative = false;
		zero_pad = false;
	} else {
		const char *digits = specifier == 88 ? "0123456789ABCDEF" : "0123456789abcdef";
		do {
			reversed[digit_count++] = digits[magnitude % base];
			magnitude /= base;
		} while (magnitude != 0);
	}
	int display_len = specifier == 99 ? rune_display_width : digit_count + (negative ? 1 : 0);
	int padding = width > display_len ? width - display_len : 0;
	int content_bytes = specifier == 99 ? rune_byte_count : digit_count + (negative ? 1 : 0);
	int result_len = content_bytes + padding;
	byteptr result = malloc((size_t)result_len + 1);
	if (result == NULL) return _SLIT0;
	int cursor = 0;
	if (!left_align && !zero_pad) while (cursor < padding) result[cursor++] = 32;
	if (negative) result[cursor++] = 45;
	if (!left_align && zero_pad) while (cursor < padding + (negative ? 1 : 0)) result[cursor++] = 48;
	if (specifier == 99) {
		for (int i = 0; i < rune_byte_count; i++) result[cursor++] = encoded_rune[i];
	} else {
		while (digit_count > 0) result[cursor++] = reversed[--digit_count];
	}
	if (left_align) while (cursor < result_len) result[cursor++] = 32;
	result[cursor] = 0;
	return (string){.str = result, .len = result_len, .is_lit = 0};
}

static string v_fastc_signed_format(long long value, const char *format) {
	bool negative = value < 0;
	unsigned long long magnitude = negative ? (unsigned long long)(-(value + 1)) + 1 : (unsigned long long)value;
	return v_fastc_integer_format(magnitude, negative, format);
}

static string v_fastc_unsigned_format(unsigned long long value, const char *format) {
	return v_fastc_integer_format(value, false, format);
}

static int builtin__array_index(array values, voidptr value) {
	for (int i = 0; i < values.len; i++) {
		void *item = (u8 *)values.data + (size_t)i * (size_t)values.element_size;
		if (memcmp(item, value, (size_t)values.element_size) == 0) return i;
	}
	return -1;
}

static void builtin__array_sort(array *values) {
	if (values == NULL || values->len < 2) return;
	if (values->element_size != (int)sizeof(string)) return;
	string *items = (string *)values->data;
	for (int i = 1; i < values->len; i++) {
		string current = items[i];
		int j = i;
		while (j > 0 && v_fastc_string_compare(&items[j - 1], &current) > 0) {
			items[j] = items[j - 1];
			j--;
		}
		items[j] = current;
	}
}

'

enum FastcDeclaredTypeKind {
	struct_
	union_
	enum_
	interface_
	alias_
}

struct FastcFunctionSignature {
	parameter_types          []string
	parameter_mutability     []bool
	return_type              string
	return_types             []string
	option_type              string
	is_variadic              bool
	last_parameter_is_params bool
	is_public                bool
	is_disabled              bool
	module_name              string
	path                     string
}

fn fastc_string_types_equal(left []string, right []string) bool {
	if left.len != right.len {
		return false
	}
	for i, item in left {
		if item != right[i] {
			return false
		}
	}
	return true
}

fn fastc_bool_types_equal(left []bool, right []bool) bool {
	if left.len != right.len {
		return false
	}
	for i, item in left {
		if item != right[i] {
			return false
		}
	}
	return true
}

struct FastcLocal {
	is_mut       bool
	is_reference bool
	typ          string
}

struct FastcExpressionToken {
	tok             token.Token
	lit             string
	source          string
	unsafe_depth    int
	is_mut_argument bool
	is_statement    bool
mut:
	typ string
}

struct FastcRenderedExpression {
	source string
	typ    string
}

struct FastcInterpolationWidth {
	width      int
	left_align bool
}

struct FastcStructField {
	name           string
	typ            string
	is_public      bool
	is_mutable     bool
	is_required    bool
	module_name    string
	path           string
	imports        map[string]string
	default_source string
mut:
	default_value string
	storage_path  []string
}

struct FastcInterfaceField {
	name       string
	typ        string
	is_mutable bool
}

struct FastcSourceHeader {
	module_name   string
	imports       map[string]string
	import_order  []string
	blank_imports []string
	has_globals   bool
}

struct FastcSourceFile {
	path   string
	source string
	header FastcSourceHeader
}

struct FastcQueuedSource {
	path        string
	module_name string
}

struct FastcHoistedCSource {
	directives       string
	conditional_code string
	body             string
}

// GenerationResult contains generated C and the complete resolved V source graph.
pub struct GenerationResult {
pub:
	c_source     string
	source_paths []string
}

struct FastcGlobalDeclarations {
	declarations        string
	module_initializers map[string]string
}

struct FastcConstantDeclarations {
	macros              string
	declarations        string
	module_initializers map[string]string
	compile_time_values map[string]string
}

struct FastcConstantValue {
	key          string
	c_name       string
	module_name  string
	value        string
	typ          string
	dependencies []string
	is_runtime   bool
}

struct FastcComptimeCondition {
	value bool
	tok   token.Token
}

struct FastcComptimeBlock {
	source string
	tok    token.Token
}

struct FastcDeclarationAttribute {
	tok        token.Token
	is_enabled bool
	is_flag    bool
	is_params  bool
	is_typedef bool
}

struct FastcEnumInfo {
	c_name  string
	name    string
	fields  []string
	is_flag bool
}

struct FastcTypeDeclarations {
	declarations        string
	enum_string_helpers string
	alias_base_types    map[string]string
}

struct FastcLoopBlockResult {
	terminates          bool
	has_reachable_break bool
}

struct Parser {
	prefs          &pref.Preferences
	path           string
	module_name    string
	imports        map[string]string
	declared_types map[string]bool
	// Generated C spelling -> declared type key, precomputed once per program
	// (see fastc_declared_type_c_names); semantic_type_key resolves through it.
	declared_type_c_names map[string]string
	// `__v_fastc_`-prefixed generated C names, the only possible collisions for
	// temporary_namespace candidates (see fastc_reserved_temporary_c_names).
	fastc_prefixed_c_names []string
	declared_kinds         map[string]FastcDeclaredTypeKind
	enum_flags             map[string]bool
	alias_base_types       map[string]string
	struct_fields          map[string]map[string]string
	struct_field_info      map[string][]FastcStructField
	interface_fields       map[string]FastcInterfaceField
	constants              map[string]string
	constant_values        map[string]string
	public_constants       map[string]bool
	globals                map[string]string
	public_globals         map[string]bool
	used_function_names    map[string]bool
	selfhost               bool
	has_startup_inits      bool
	has_cleanup_hooks      bool
mut:
	s                        scanner.Scanner
	tok                      token.Token
	lit                      string
	out                      strings.Builder
	protos                   strings.Builder
	indent                   int
	in_main                  bool
	has_main                 bool
	unsafe_depth             int
	temp_id                  int
	locals                   map[string]FastcLocal
	functions                map[string]FastcFunctionSignature
	constant_types           map[string]string
	global_types             map[string]string
	return_type              string
	return_types             []string
	option_return_type       string
	current_function         string
	current_receiver         string
	current_method_is_static bool
	expected_expression_type string
	capturing_defer          bool
	defer_depth              int
	captured_defer_lines     []string
	deferred_lines           []string
	deferred_block_starts    []int
	loop_defer_block_starts  []int
	loop_has_breaks          []bool
	statement_reachable      bool
	last_expression_type     string
	last_expression          []FastcExpressionToken
	last_multi_return_types  []string
	fixed_array_types        map[string]string
	composite_types          map[string]bool
	expression_depth         int
	// Comparison-handler results memoized per token subrange for the duration
	// of one top-level expression (see fastc_comparison_memo_key). The boolean
	// operator scans in those handlers re-recurse over shared subranges;
	// without the memo that search re-renders the same ranges combinatorially.
	comparison_memo map[i64]FastcRenderedExpression
	has_c_functions bool
	// Spawn lowering registrations (see spawn.v): thread struct typedefs,
	// creator/run/waiter helper definitions, and thread type -> value type.
	spawn_typedefs     map[string]string
	spawn_helpers      map[string]string
	thread_value_types map[string]string
}

// fastc_comparison_memo_key identifies a token subrange within the current
// expression. Every recursive slice shares the top array's backing storage, so
// the data pointer plus length is exact; `tag` separates the two comparison
// handlers. Token arrays are 8-byte aligned and prealloc never reuses a
// buffer while the memo is live (it is cleared per top-level expression).
fn fastc_comparison_memo_key(tokens []FastcExpressionToken, tag i64) i64 {
	if tokens.len >= 32768 {
		return 0
	}
	return i64(((u64(tokens.data) >> 3) << 16) | (u64(tokens.len) & 0x7fff) | (u64(tag) << 62))
}

// generate scans V source and emits C as each declaration and statement is consumed. It does
// not construct an AST or invoke semantic type checking. Unsupported syntax is returned as an
// error; FastC never retries through an AST-based backend.
pub fn generate(source string, path string, prefs &pref.Preferences) !string {
	header := fastc_scan_source_header(source, path, prefs)!
	if header.imports.len > 0 || header.blank_imports.len > 0 {
		return error('fastc parser does not support imports through the single-source API in ${path}')
	}
	return generate_source_files([
		FastcSourceFile{
			path:   path
			source: source
			header: header
		},
	], prefs)
}

// generate_files discovers imports from the input files and emits one C translation unit for
// the complete source graph. Discovery and generation use scanner tokens only.
pub fn generate_files(paths []string, prefs &pref.Preferences) !string {
	generation := generate_files_with_source_paths(paths, prefs)!
	return generation.c_source
}

// generate_files_with_source_paths emits C and reports every source read during import discovery.
pub fn generate_files_with_source_paths(paths []string, prefs &pref.Preferences) !GenerationResult {
	sources := fastc_resolve_source_files(paths, prefs)!
	mut source_paths := []string{cap: sources.len}
	for source_file in sources {
		source_paths << source_file.path
	}
	return GenerationResult{
		c_source:     generate_source_files(sources, prefs)!
		source_paths: source_paths
	}
}

// FastcFileGenContext bundles the read-only program tables every per-file
// code-generation Parser starts from. Workers share it across threads; the
// two seed maps are cloned per file, never mutated through the context.
struct FastcFileGenContext {
	prefs                  &pref.Preferences = unsafe { nil }
	declared_types         map[string]bool
	declared_type_c_names  map[string]string
	fastc_prefixed_c_names []string
	has_c_functions        bool
	declared_kinds         map[string]FastcDeclaredTypeKind
	enum_flags             map[string]bool
	alias_base_types       map[string]string
	struct_fields          map[string]map[string]string
	struct_field_info      map[string][]FastcStructField
	interface_fields       map[string]FastcInterfaceField
	constants              map[string]string
	constant_values        map[string]string
	public_constants       map[string]bool
	globals                map[string]string
	public_globals         map[string]bool
	used_function_names    map[string]bool
	has_startup_inits      bool
	has_cleanup_hooks      bool
	functions              map[string]FastcFunctionSignature
	constant_types         map[string]string
	global_types           map[string]string
	fixed_array_types      map[string]string
	composite_types        map[string]bool
}

// FastcFileGenOutput is one source file's generation result. The sequential
// stitch loop consumes them in file order, so parallel workers never touch
// the shared output builders or the merged registration maps.
struct FastcFileGenOutput {
mut:
	prototypes        string
	body              string
	has_main_entry    bool
	fixed_array_types map[string]string
	composite_types   map[string]bool
	spawn_typedefs    map[string]string
	spawn_helpers     map[string]string
	failed            bool
	error_message     string
}

fn fastc_generate_single_file(ctx &FastcFileGenContext, source_file FastcSourceFile) FastcFileGenOutput {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(source_file.path, source_file.source.len)
	file.index_lines_without_digest(source_file.source)
	prefs := ctx.prefs
	mut gen := Parser{
		prefs:                   unsafe { prefs }
		path:                    source_file.path
		module_name:             source_file.header.module_name
		imports:                 source_file.header.imports
		declared_types:          ctx.declared_types
		declared_type_c_names:   ctx.declared_type_c_names
		fastc_prefixed_c_names:  ctx.fastc_prefixed_c_names
		has_c_functions:         ctx.has_c_functions
		comparison_memo:         map[i64]FastcRenderedExpression{}
		spawn_typedefs:          map[string]string{}
		spawn_helpers:           map[string]string{}
		thread_value_types:      map[string]string{}
		declared_kinds:          ctx.declared_kinds
		enum_flags:              ctx.enum_flags
		alias_base_types:        ctx.alias_base_types
		struct_fields:           ctx.struct_fields
		struct_field_info:       ctx.struct_field_info
		interface_fields:        ctx.interface_fields
		constants:               ctx.constants
		constant_values:         ctx.constant_values
		public_constants:        ctx.public_constants
		globals:                 ctx.globals
		public_globals:          ctx.public_globals
		used_function_names:     ctx.used_function_names
		selfhost:                prefs.building_v
		has_startup_inits:       ctx.has_startup_inits
		has_cleanup_hooks:       ctx.has_cleanup_hooks
		s:                       scanner.new_scanner(prefs, .normal)
		out:                     strings.new_builder(source_file.source.len)
		protos:                  strings.new_builder(256)
		functions:               ctx.functions
		constant_types:          ctx.constant_types
		global_types:            ctx.global_types
		fixed_array_types:       ctx.fixed_array_types.clone()
		composite_types:         ctx.composite_types.clone()
		deferred_lines:          []string{}
		deferred_block_starts:   []int{}
		loop_defer_block_starts: []int{}
		loop_has_breaks:         []bool{}
		statement_reachable:     true
	}
	gen.s.init(file, source_file.source)
	generated := gen.run() or {
		return FastcFileGenOutput{
			failed:        true
			error_message: err.msg()
		}
	}
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return FastcFileGenOutput{
			failed:        true
			error_message: 'fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}'
		}
	}
	return FastcFileGenOutput{
		prototypes:        gen.protos.str()
		body:              generated
		has_main_entry:    source_file.header.module_name in ['', 'main'] && gen.has_main
		fixed_array_types: gen.fixed_array_types
		composite_types:   gen.composite_types
		spawn_typedefs:    gen.spawn_typedefs
		spawn_helpers:     gen.spawn_helpers
	}
}

fn generate_source_files(sources []FastcSourceFile, prefs &pref.Preferences) !string {
	mut declared_types := map[string]bool{}
	mut declared_kinds := map[string]FastcDeclaredTypeKind{}
	mut enum_flags := map[string]bool{}
	mut params_structs := map[string]bool{}
	mut struct_fields := map[string]map[string]string{}
	mut struct_field_info := map[string][]FastcStructField{}
	mut constants := map[string]string{}
	mut public_constants := map[string]bool{}
	mut globals := map[string]string{}
	mut public_globals := map[string]bool{}
	for source_file in sources {
		collect_declared_types(source_file.source, source_file.path,
			source_file.header.module_name, prefs, mut declared_types, mut declared_kinds, mut
			enum_flags, mut params_structs)!
	}
	declared_type_c_names := fastc_declared_type_c_names(declared_types)
	for source_file in sources {
		collect_constant_names(source_file.source, source_file.path,
			source_file.header.module_name, prefs, mut constants, mut public_constants)!
		collect_global_names(source_file.source, source_file.path, source_file.header, prefs, mut
			globals, mut public_globals)!
	}
	mut functions := map[string]FastcFunctionSignature{}
	mut interface_methods := map[string]bool{}
	mut interface_fields := map[string]FastcInterfaceField{}
	for source_file in sources {
		collect_function_signatures(source_file.source, source_file.path, source_file.header,
			prefs, declared_types, params_structs, mut functions)!
		collect_interface_method_signatures(source_file.source, source_file.path,
			source_file.header, prefs, declared_types, mut functions, mut interface_methods, mut
			interface_fields)!
	}
	used_function_names := fastc_collect_referenced_function_names(sources, prefs, functions)
	has_c_functions := fastc_functions_declare_c(functions)
	fastc_prefixed_c_names := fastc_reserved_temporary_c_names(functions, globals)
	module_init_calls := fastc_module_init_calls(sources, functions)!
	module_cleanup_calls := fastc_module_cleanup_calls(sources, functions)!
	mut composite_types := map[string]bool{}
	if prefs.building_v {
		// The OS exec helpers build their native argv arrays locally, so this
		// representation does not otherwise appear in a declaration signature.
		composite_types['Array_char_ptr'] = true
	}
	for signature in functions.values() {
		fastc_register_composite_type(signature.return_type, mut composite_types)
		for parameter_type in signature.parameter_types {
			fastc_register_composite_type(parameter_type, mut composite_types)
		}
	}
	type_output := fastc_generate_type_declarations(sources, prefs, declared_types, declared_kinds,
		enum_flags, constants, public_constants, mut struct_fields, mut struct_field_info, mut
		composite_types)!
	type_declarations := type_output.declarations
	mut constant_types := map[string]string{}
	mut global_types := map[string]string{}
	fastc_render_struct_field_defaults(prefs, declared_types, declared_type_c_names,
		fastc_prefixed_c_names, declared_kinds, enum_flags, type_output.alias_base_types,
		struct_fields, mut struct_field_info, functions, constants, public_constants,
		constant_types, globals, public_globals, global_types)!
	constant_output := fastc_generate_constant_declarations(sources, prefs, declared_types,
		declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags,
		type_output.alias_base_types, struct_fields, struct_field_info, functions, constants,
		public_constants, globals, public_globals, mut constant_types)!
	global_output := fastc_generate_global_declarations(sources, prefs, declared_types,
		declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags,
		type_output.alias_base_types, struct_fields, struct_field_info, functions, constants,
		constant_output.compile_time_values, public_constants, constant_types, globals,
		public_globals, mut global_types)!
	for constant_type in constant_types.values() {
		fastc_register_composite_type(constant_type, mut composite_types)
	}
	for global_type in global_types.values() {
		fastc_register_composite_type(global_type, mut composite_types)
	}
	startup_initializers := fastc_generate_startup_initializers(sources,
		constant_output.module_initializers, global_output.module_initializers, module_init_calls)!
	mut prototypes := strings.new_builder(1024)
	mut body := strings.new_builder(4096)
	mut fixed_array_types := map[string]string{}
	mut has_entry_module := false
	for source_file in sources {
		if source_file.header.module_name in ['', 'main'] {
			has_entry_module = true
			break
		}
	}
	mut entry_has_main := false
	ctx := FastcFileGenContext{
		prefs:                  unsafe { prefs }
		declared_types:         declared_types
		declared_type_c_names:  declared_type_c_names
		fastc_prefixed_c_names: fastc_prefixed_c_names
		has_c_functions:        has_c_functions
		declared_kinds:         declared_kinds
		enum_flags:             enum_flags
		alias_base_types:       type_output.alias_base_types
		struct_fields:          struct_fields
		struct_field_info:      struct_field_info
		interface_fields:       interface_fields
		constants:              constants
		constant_values:        constant_output.compile_time_values
		public_constants:       public_constants
		globals:                globals
		public_globals:         public_globals
		used_function_names:    used_function_names
		has_startup_inits:      startup_initializers.len > 0
		has_cleanup_hooks:      module_cleanup_calls.len > 0
		functions:              functions
		constant_types:         constant_types
		global_types:           global_types
		fixed_array_types:      fixed_array_types
		composite_types:        composite_types
	}
	mut spawn_typedefs := map[string]string{}
	mut spawn_helpers := map[string]string{}
	outputs := fastc_generate_file_outputs(&ctx, sources)
	for output in outputs {
		if output.failed {
			return error(output.error_message)
		}
		prototypes.write_string(output.prototypes)
		body.write_string(output.body)
		if output.has_main_entry {
			entry_has_main = true
		}
		for name, array_type in output.fixed_array_types {
			fixed_array_types[name] = array_type
		}
		for name, _ in output.composite_types {
			composite_types[name] = true
		}
		for name, text in output.spawn_typedefs {
			spawn_typedefs[name] = text
		}
		for name, text in output.spawn_helpers {
			spawn_helpers[name] = text
		}
	}
	synthesized_main := if has_entry_module && !entry_has_main {
		fastc_synthesized_main(prefs.building_v, startup_initializers.len > 0,
			module_cleanup_calls.len > 0)
	} else {
		''
	}
	mut late_composite_declarations := strings.new_builder(256)
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for composite_name in composite_names {
		declaration := 'typedef ${if composite_name.starts_with('Array_') {
			'array'
		} else {
			'map'
		}} ${composite_name};'
		if !type_declarations.contains(declaration) {
			late_composite_declarations.writeln(declaration)
		}
	}
	if late_composite_declarations.len > 0 {
		late_composite_declarations.writeln('')
	}
	interface_dispatches := fastc_generate_interface_dispatches(declared_kinds, functions,
		interface_methods, used_function_names, prefs.building_v)
	fixed_array_declarations := fastc_generate_fixed_array_declarations(fixed_array_types)
	preamble := if prefs.building_v { c_selfhost_preamble } else { c_preamble }
	hoisted_body := fastc_hoist_c_directives(body.str())
	mut result := strings.new_builder(preamble.len + c_integer_comparison_helpers.len +
		type_declarations.len + type_output.enum_string_helpers.len + constant_output.macros.len +
		constant_output.declarations.len + global_output.declarations.len + prototypes.len +
		body.len + startup_initializers.len + 96)
	result.write_string(preamble)
	result.write_string(c_integer_comparison_helpers)
	result.write_string(hoisted_body.directives)
	if spawn_typedefs.len > 0 {
		result.write_string(c_spawn_runtime)
		result.writeln('')
	}
	result.write_string(constant_output.macros)
	result.write_string(type_declarations)
	result.write_string(late_composite_declarations.str())
	result.write_string(fixed_array_declarations)
	if spawn_typedefs.len > 0 {
		mut thread_type_names := spawn_typedefs.keys()
		thread_type_names.sort()
		for thread_type_name in thread_type_names {
			result.writeln(spawn_typedefs[thread_type_name])
		}
		result.writeln('')
	}
	result.write_string(constant_output.declarations)
	result.write_string(global_output.declarations)
	result.write_string(prototypes.str())
	if startup_initializers.len > 0 {
		result.writeln('static void v_fastc_init_globals(void);')
	}
	if module_cleanup_calls.len > 0 {
		result.writeln('static void v_fastc_cleanup_modules(void);')
	}
	result.writeln('')
	if prefs.building_v {
		result.write_string(c_selfhost_runtime)
	}
	if spawn_helpers.len > 0 {
		mut spawn_helper_names := spawn_helpers.keys()
		spawn_helper_names.sort()
		for spawn_helper_name in spawn_helper_names {
			result.writeln(spawn_helpers[spawn_helper_name])
			result.writeln('')
		}
	}
	result.write_string(type_output.enum_string_helpers)
	result.write_string(interface_dispatches)
	if startup_initializers.len > 0 {
		result.writeln('static void v_fastc_init_globals(void) {')
		result.write_string(startup_initializers)
		result.writeln('}')
		result.writeln('')
	}
	if module_cleanup_calls.len > 0 {
		result.writeln('static void v_fastc_cleanup_modules(void) {')
		for cleanup_call in module_cleanup_calls {
			result.writeln('\t${cleanup_call}();')
		}
		result.writeln('}')
		result.writeln('')
	}
	result.write_string(synthesized_main)
	result.write_string(hoisted_body.conditional_code)
	result.write_string(hoisted_body.body)
	return result.str()
}

fn fastc_synthesized_main(selfhost bool, has_startup_inits bool, has_cleanup_hooks bool) string {
	mut result := strings.new_builder(256)
	parameters := if selfhost { 'int argc, char **argv' } else { 'void' }
	result.writeln('int main(${parameters}) {')
	result.writeln('\tsetvbuf(stdout, NULL, _IONBF, 0);')
	if selfhost {
		result.writeln('\tg_main_argc = argc;')
		result.writeln('\tg_main_argv = argv;')
	}
	if has_startup_inits {
		result.writeln('\tv_fastc_init_globals();')
	}
	if has_cleanup_hooks {
		result.writeln('\tatexit(v_fastc_cleanup_modules);')
	}
	result.writeln('\treturn 0;')
	result.writeln('}')
	result.writeln('')
	return result.str()
}

fn fastc_interface_method_signatures_match(interface_signature FastcFunctionSignature, candidate_signature FastcFunctionSignature) bool {
	if candidate_signature.return_type != interface_signature.return_type
		|| !fastc_string_types_equal(candidate_signature.return_types, interface_signature.return_types)
		|| candidate_signature.option_type != interface_signature.option_type
		|| candidate_signature.parameter_types.len != interface_signature.parameter_types.len {
		return false
	}
	for i in 0 .. interface_signature.parameter_types.len {
		if i > 0 && candidate_signature.parameter_types[i] != interface_signature.parameter_types[i] {
			return false
		}
		interface_parameter_is_mut := i < interface_signature.parameter_mutability.len
			&& interface_signature.parameter_mutability[i]
		candidate_parameter_is_mut := i < candidate_signature.parameter_mutability.len
			&& candidate_signature.parameter_mutability[i]
		if candidate_parameter_is_mut != interface_parameter_is_mut {
			return false
		}
	}
	return true
}

fn fastc_generate_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool, used_function_names map[string]bool, selfhost bool) string {
	mut out := strings.new_builder(1024)
	mut function_keys := functions.keys()
	function_keys.sort()
	mut interface_method_keys := interface_methods.keys()
	interface_method_keys.sort()
	for interface_key, kind in declared_kinds {
		if kind != .interface_ {
			continue
		}
		prefix := interface_key + '.'
		for interface_method_key in interface_method_keys {
			if !interface_method_key.starts_with(prefix) {
				continue
			}
			interface_signature := functions[interface_method_key]
			if interface_signature.parameter_types.len == 0 {
				continue
			}
			interface_type := fastc_c_declared_type_name(interface_key)
			if interface_signature.parameter_types[0] != interface_type {
				continue
			}
			method_name := interface_method_key.all_after_last('.')
			mut parameters := ['${interface_type} value']
			mut arguments := []string{}
			for i in 1 .. interface_signature.parameter_types.len {
				parameters << '${interface_signature.parameter_types[i]} arg${i}'
				arguments << 'arg${i}'
			}
			c_name := fastc_method_c_name(interface_signature.module_name, interface_type,
				method_name)
			out.writeln('${interface_signature.return_type} ${c_name}(${parameters.join(', ')}) {')
			out.writeln('\tswitch (value._typ) {')
			for candidate_key in function_keys {
				if selfhost && method_name !in used_function_names {
					continue
				}
				if candidate_key == interface_method_key
					|| candidate_key.all_after_last('.') != method_name {
					continue
				}
				receiver_key := candidate_key.all_before_last('.')
				if declared_kinds[receiver_key] in [.interface_, .enum_, .alias_]
					|| receiver_key !in declared_kinds {
					continue
				}
				candidate_signature := functions[candidate_key]
				if !fastc_interface_method_signatures_match(interface_signature,
					candidate_signature) {
					continue
				}
				receiver_type := fastc_c_declared_type_name(receiver_key)
				expected_receiver := candidate_signature.parameter_types[0]
				receiver_argument := if expected_receiver.ends_with('*') {
					'(${receiver_type} *)value._object'
				} else {
					'*(${receiver_type} *)value._object'
				}
				call_arguments := if arguments.len > 0 {
					',' + arguments.join(',')
				} else {
					''
				}
				call := '${fastc_method_c_name(candidate_signature.module_name, receiver_type,
					method_name)}(${receiver_argument}${call_arguments})'
				out.writeln('\tcase __v_typeid_${receiver_type}: ${if interface_signature.return_type == 'void' {
					call + '; return;'
				} else {
					'return ' + call + ';'
				}}')
			}
			out.writeln('\tdefault: ${if interface_signature.return_type == 'void' {
				'return;'
			} else {
				'return (' + interface_signature.return_type + '){0};'
			}}')
			out.writeln('\t}')
			out.writeln('}')
			out.writeln('')
		}
	}
	return out.str()
}

fn fastc_hoist_c_directives(source string) FastcHoistedCSource {
	mut directives := strings.new_builder(256)
	mut conditional_code := strings.new_builder(256)
	mut body := strings.new_builder(source.len)
	mut conditional_depth := 0
	for line in source.split('\n') {
		directive_name := fastc_c_directive_name(line)
		if conditional_depth > 0 {
			conditional_code.writeln(line)
			if directive_name in ['if', 'ifdef', 'ifndef'] {
				conditional_depth++
			} else if directive_name == 'endif' {
				conditional_depth--
			}
		} else if directive_name in ['if', 'ifdef', 'ifndef'] {
			conditional_code.writeln(line)
			conditional_depth = 1
		} else if directive_name != '' {
			directives.writeln(line)
		} else {
			body.writeln(line)
		}
	}
	if directives.len > 0 {
		directives.writeln('')
	}
	if conditional_code.len > 0 {
		conditional_code.writeln('')
	}
	return FastcHoistedCSource{
		directives:       directives.str()
		conditional_code: conditional_code.str()
		body:             body.str()
	}
}

fn fastc_c_directive_name(line string) string {
	if !line.starts_with('#') {
		return ''
	}
	return line[1..].trim_space().fields()[0] or { '' }
}
