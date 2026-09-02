module fastc

import os
import strings
import time
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
/* One multi-return component. Values up to 32 bytes are stored inline; larger
   ones are boxed and referenced through `ptr`, so no component size can
   overflow the slot. */
typedef union { uintptr_t word; long double alignment; void *ptr; unsigned char data[32]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) __v_fastc_multi_value = (expression); MultiReturnValue __v_fastc_multi_result; if (sizeof(__v_fastc_multi_value) <= sizeof(__v_fastc_multi_result.data)) memcpy(__v_fastc_multi_result.data, &__v_fastc_multi_value, sizeof(__v_fastc_multi_value)); else __v_fastc_multi_result.ptr = v_fastc_interface_box(&__v_fastc_multi_value, sizeof(__v_fastc_multi_value)); __v_fastc_multi_result; })
#define V_FASTC_MULTI_SOURCE(slot, size) (((size) <= sizeof((slot).data)) ? (const void *)(slot).data : (const void *)(slot).ptr)

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

static void *v_fastc_interface_box(const void *value, usize size) {
	void *copy = malloc(size);
	if (copy != NULL) memcpy(copy, value, size);
	return copy;
}

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
	if name.len < 2 || name.len > 8 {
		return name
	}
	first := name[0]
	// Bitset of the initial letters used by the reserved-identifier table (`a` is bit 0).
	if first < `a` || first > `w` || (u32(8_259_967) & (u32(1) << (first - `a`))) == 0 {
		return name
	}
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
#ifndef _WIN32
#include <pthread.h>
#endif

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
/* One multi-return component. Values up to 32 bytes are stored inline; larger
   ones are boxed and referenced through `ptr`, so no component size can
   overflow the slot. */
typedef union { uintptr_t word; long double alignment; void *ptr; unsigned char data[32]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) __v_fastc_multi_value = (expression); MultiReturnValue __v_fastc_multi_result; if (sizeof(__v_fastc_multi_value) <= sizeof(__v_fastc_multi_result.data)) memcpy(__v_fastc_multi_result.data, &__v_fastc_multi_value, sizeof(__v_fastc_multi_value)); else __v_fastc_multi_result.ptr = v_fastc_interface_box(&__v_fastc_multi_value, sizeof(__v_fastc_multi_value)); __v_fastc_multi_result; })
#define V_FASTC_MULTI_SOURCE(slot, size) (((size) <= sizeof((slot).data)) ? (const void *)(slot).data : (const void *)(slot).ptr)

#define _S(s) ((string){.str=(byteptr)("" s), .len=(sizeof(s)-1), .is_lit=1})
#define _SLIT0 _S("")

/* Option, result, and interface payloads are boxed on the heap and never
   freed. Boxing through malloc made every option-returning call pay for a
   locked, zero-filling allocation, so each thread bumps its boxes out of a
   private chunk instead; only oversized payloads still go through malloc. */
#ifndef _WIN32
typedef struct { unsigned char *cursor; unsigned char *end; } v_fastc_box_arena;
static pthread_key_t v_fastc_box_arena_key;
static pthread_once_t v_fastc_box_arena_once = PTHREAD_ONCE_INIT;
static void v_fastc_box_arena_create_key(void) { pthread_key_create(&v_fastc_box_arena_key, NULL); }
static void *v_fastc_interface_box(const void *value, usize size) {
	usize aligned = (size + 15) & ~(usize)15;
	usize chunk = 262144;
	v_fastc_box_arena *arena;
	void *copy;
	if (aligned > 65536) {
		copy = malloc(size);
		if (copy != NULL) memcpy(copy, value, size);
		return copy;
	}
	pthread_once(&v_fastc_box_arena_once, v_fastc_box_arena_create_key);
	arena = (v_fastc_box_arena *)pthread_getspecific(v_fastc_box_arena_key);
	if (arena == NULL || (usize)(arena->end - arena->cursor) < aligned) {
		unsigned char *block = (unsigned char *)malloc(sizeof(v_fastc_box_arena) + chunk);
		if (block == NULL) return NULL;
		arena = (v_fastc_box_arena *)block;
		arena->cursor = block + sizeof(v_fastc_box_arena);
		arena->end = arena->cursor + chunk;
		pthread_setspecific(v_fastc_box_arena_key, arena);
	}
	copy = arena->cursor;
	arena->cursor += aligned;
	memcpy(copy, value, size);
	return copy;
}
#else
static void *v_fastc_interface_box(const void *value, usize size) {
	void *copy = malloc(size);
	if (copy != NULL) memcpy(copy, value, size);
	return copy;
}
#endif

static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};
static inline u64 _wymix(u64 a, u64 b) { u64 ha = a >> 32, hb = b >> 32, la = (u32)a, lb = (u32)b, hi, lo; u64 rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb, t = rl + (rm0 << 32), c = t < rl; lo = t + (rm1 << 32); c += lo < t; hi = rh + (rm0 >> 32) + (rm1 >> 32) + c; return lo ^ hi; }
static inline u64 wyhash64(u64 a, u64 b) { a ^= _wyp[0]; b ^= _wyp[1]; a *= 0xa0761d6478bd642full; b *= 0xe7037ed1a0b428dbull; return (a ^ (a >> 32)) ^ (b ^ (b >> 32)); }
/* Assembling the word from its bytes is defined for any alignment and any
   effective type of the key storage; optimizing compilers fold it into one
   load, and the hash stays identical across byte orders. */
#define V_FASTC_LOAD_U64(p) ((u64)(p)[0] | ((u64)(p)[1] << 8) | ((u64)(p)[2] << 16) | ((u64)(p)[3] << 24) | ((u64)(p)[4] << 32) | ((u64)(p)[5] << 40) | ((u64)(p)[6] << 48) | ((u64)(p)[7] << 56))
/* Map keys are hashed by this function on every lookup. It mixes one 64-bit
   word per step and has no helper calls, since TinyCC does not inline. */
static inline u64 wyhash(const void *key, size_t len, u64 seed, const u64 *secret) {
	const unsigned char *p = (const unsigned char *)key;
	size_t n = len;
	u64 h = seed ^ secret[0] ^ ((u64)len * 0x9e3779b97f4a7c15ull);
	while (n >= 8) {
		h = (h ^ V_FASTC_LOAD_U64(p)) * 0xa0761d6478bd642full;
		h ^= h >> 29;
		p += 8;
		n -= 8;
	}
	if (n > 0) {
		u64 v = 0;
		switch (n) {
			case 7: v |= (u64)p[6] << 48;
			case 6: v |= (u64)p[5] << 40;
			case 5: v |= (u64)p[4] << 32;
			case 4: v |= (u64)p[3] << 24;
			case 3: v |= (u64)p[2] << 16;
			case 2: v |= (u64)p[1] << 8;
			default: v |= (u64)p[0];
		}
		h = (h ^ v) * 0xe7037ed1a0b428dbull;
		h ^= h >> 29;
	}
	h *= 0x8ebc6af09c88c6e3ull;
	h ^= h >> 32;
	h *= 0x589965cc75374cc3ull;
	h ^= h >> 29;
	return h;
}

'

// This must follow source `#include` directives: defining the function-like
// fallback before `<pthread.h>` would rewrite declarations in that header.
const c_selfhost_post_directives = r'#ifndef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP
#define pthread_rwlockattr_setkind_np(a, b) (0)
#endif

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

// fastc_multi_return_literal packs already-boxed components into a MultiReturn.
// Filling an uninitialized local slot by slot avoids the zero-fill a partially
// designated compound literal would perform on the whole aggregate.
fn fastc_multi_return_literal(packed_values []string) string {
	mut out := strings.new_builder(64)
	out.write_string('({ MultiReturn __v_fastc_multi; ')
	for i, value in packed_values {
		out.write_string('__v_fastc_multi.values[${i}] = ${value}; ')
	}
	out.write_string('__v_fastc_multi; })')
	return out.str()
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
	// For a variable declared with an option type (`b ?bool`), the C `typ` is the
	// type-erased `Option`; this keeps the wrapped value type so an `if x := b`
	// guard on the bare variable can unwrap it.
	option_value_type string
	// For a function-pointer parameter (`f fn (int) !string`), the C `typ` is a
	// function pointer; these record the callee's return C type and, for an option
	// return, its wrapped value type, so `f(x)` / `f(x) or {…}` infer correctly.
	fn_return_type       string
	fn_option_value_type string
	// For a channel local (`ch chan T` / `ch := chan T{…}`), the C `typ` is the erased
	// `chan`; this keeps the element C type so `<-ch` recovers the received value type.
	chan_element_type string
}

struct FastcLocalScopeChange {
	name         string
	previous     FastcLocal
	had_previous bool
}

struct FastcExpressionToken {
	tok             token.Token
	source          string
	unsafe_depth    int
	is_mut_argument bool
	is_statement    bool
mut:
	lit string
	typ string
}

struct FastcRenderedExpression {
	source string
	typ    string
}

// FastcMemoEntry caches an optional lookup result, including a negative one.
struct FastcMemoEntry {
	found bool
	value string
}

struct FastcInterpolationWidth {
	width      int
	left_align bool
}

struct FastcStructField {
	name                 string
	typ                  string
	is_public            bool
	is_mutable           bool
	is_required          bool
	is_skip              bool
	is_function          bool
	is_optional_function bool
	module_name          string
	path                 string
	default_source       string
mut:
	default_value string
	storage_path  []string
	// For a `chan T` field, the erased C `typ` is `chan`; this keeps the element C type
	// so `<-x.field` recovers the received value type.
	chan_element_type string
	// For an option field (`f ?T`), the erased C `typ` is `Option`; this keeps the
	// wrapped value C type so `if mut v := x.f {` binds `v` to `T` (not the default int).
	option_value_type string
	// Function fields are stored as `voidptr`; retain their signature so member calls
	// can cast the erased storage back to a callable C function pointer.
	fn_parameter_types   []string
	fn_return_type       string
	fn_option_value_type string
	// Imported generic structs retain erased storage, but a concrete field like
	// `Stack[Item]` still carries `Item` for method result/argument recovery.
	generic_argument_type string
}

struct FastcInterfaceField {
	name       string
	typ        string
	is_mutable bool
}

struct FastcSourceHeader {
	module_name             string
	imports                 map[string]string
	import_order            []string
	blank_imports           []string
	has_globals             bool
	has_constants           bool
	has_global_declarations bool
	// Byte-level superset tests over the whole file (see fastc_source_scan_flags):
	// a false flag proves the file cannot hold that kind of declaration, so the
	// matching collection pass skips its scan.
	has_interfaces        bool
	has_comptime_if       bool
	has_type_keywords     bool
	has_generic_fn_syntax bool
}

struct FastcSourceFile {
	path          string
	source        string
	source_offset int
	header        FastcSourceHeader
}

struct FastcQueuedSource {
	path         string
	module_name  string
	is_canonical bool
	// listed sources came from a directory listing moments ago, so their
	// existence check is skipped.
	listed bool
}

struct FastcLoadedSource {
	path          string
	source        string
	header        FastcSourceHeader
	failed        bool
	error_message string
	// stamp is the file version the memo preload observed (zero when unknown).
	stamp FastcFileStamp
}

struct FastcHoistedCSource {
	directives       string
	conditional_code string
	body             string
}

struct FastcPartitionedCSource {
	source             string
	directive_ranges   []int
	conditional_ranges []int
	body_ranges        []int
	final_kind         int
}

struct FastcCDirectiveLine {
	start int
	end   int
	kind  int
}

// GenerationResult contains generated C and the complete resolved V source
// graph. `c_pieces` is the generated C in output order; every piece is an
// ordinary owned string, and writing them in sequence (write_c_pieces) or
// joining them (c_source) yields the program.
pub struct GenerationResult {
pub:
	c_pieces     []string
	source_paths []string
	uses_threads bool
	c_flags      []string
}

// c_source joins the generated C pieces into one string.
pub fn (g &GenerationResult) c_source() string {
	return fastc_join_c_pieces(g.c_pieces)
}

// c_size returns the size of the generated C in bytes.
pub fn (g &GenerationResult) c_size() int {
	mut size := 0
	for piece in g.c_pieces {
		size += piece.len
	}
	return size
}

// write_c_pieces writes generated C pieces to `path` without joining them.
pub fn write_c_pieces(path string, pieces []string) ! {
	mut file := os.create(path)!
	for piece in pieces {
		file.write_string(piece)!
	}
	file.close()
}

fn fastc_join_c_pieces(pieces []string) string {
	mut size := 0
	for piece in pieces {
		size += piece.len
	}
	mut out := strings.new_builder(size + 1)
	for piece in pieces {
		out.write_string(piece)
	}
	return out.str()
}

struct FastcGlobalDeclarations {
	declarations        string
	module_initializers map[string]string
	composite_types     map[string]bool
	fixed_array_types   map[string]string
}

struct FastcConstantDeclarations {
	macros              string
	declarations        string
	module_initializers map[string]string
	compile_time_values map[string]string
	composite_types     map[string]bool
	fixed_array_types   map[string]string
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
	// declarations_head precedes the composite typedefs (type ids and forward
	// typedefs) and declarations follows them (the type bodies).
	declarations_head   string
	declarations        string
	enum_string_helpers string
	alias_base_types    map[string]string
	enum_field_types    map[string]string
	// Declared C names of sum types (`type X = A | B`). They share the boxed
	// `{void*_object; u32 _typ;}` layout with interfaces; construction boxes a
	// variant and `match` dispatches on `_typ`.
	sum_types map[string]bool
	// Declared variants per sum type, keyed `"${sum_type_c_name}|${variant_c_name}"`
	// (see Parser.sum_type_variants).
	sum_type_variants map[string]bool
}

struct FastcLoopBlockResult {
	terminates          bool
	has_reachable_break bool
}

// FastcMemberSmartcast records the concrete pointer used for a boxed interface/sum-type
// member inside an `if holder.member is Concrete` branch. Unlike a local smart-cast, the
// member cannot be shadowed under its qualified source spelling, so member-chain rendering
// consults this table while the branch is active.
struct FastcMemberSmartcast {
	typ    string
	source string
}

struct Parser {
	prefs &pref.Preferences
	// Generic methods (own type param) not resolved by the source-level pass, kept so a
	// `recv.m(arg)` call can be monomorphized on demand at parse time (see anonfn/drain).
	generic_method_sources map[string]FastcGenericMethodSource
	// generic_method_names holds the bare names of the generic methods and
	// functions, so an expression can skip the monomorphization scan unless
	// its last name could be one.
	generic_method_names map[string]bool
	// A module imported under a second name that re-exports another (e.g. `json2` aliased
	// at `x.json2`): resolves `<alias>.<sym>` to the loaded `<target>.<sym>`.
	module_aliases map[string]string
	declared_types map[string]bool
	// Generated C spelling -> declared type key, precomputed once per program
	// (see fastc_declared_type_c_names); semantic_type_key resolves through it.
	declared_type_c_names     map[string]string
	declared_type_key_by_name map[string]string
	// `__v_fastc_`-prefixed generated C names, the only possible collisions for
	// temporary_namespace candidates (see fastc_reserved_temporary_c_names).
	fastc_prefixed_c_names []string
	declared_kinds         map[string]FastcDeclaredTypeKind
	enum_flags             map[string]bool
	enum_field_types       map[string]string
	alias_base_types       map[string]string
	sum_types              map[string]bool
	// Declared variants of each sum type, keyed `"${sum_type_c_name}|${variant_c_name}"`.
	// Lets an append distinguish push-many (`[]T << []T`) from boxing an array-valued
	// variant of a recursive sum type as one element (see sumtype_has_variant).
	sum_type_variants   map[string]bool
	struct_fields       map[string]map[string]string
	struct_field_info   map[string][]FastcStructField
	struct_field_lookup map[string]map[string]FastcStructField
	interface_fields    map[string]FastcInterfaceField
	constants           map[string]string
	constant_values     map[string]string
	public_constants    map[string]bool
	globals             map[string]string
	public_globals      map[string]bool
	used_function_names map[string]bool
	selfhost            bool
	has_startup_inits   bool
	has_cleanup_hooks   bool
mut:
	path          string
	source_offset int
	module_name   string
	imports       map[string]string
	s             scanner.Scanner
	tok           token.Token
	lit           string
	out           strings.Builder
	protos        strings.Builder
	indent        int
	in_main       bool
	has_main      bool
	unsafe_depth  int
	// Set while generating a `@[direct_array_access]` function body, so string
	// and array indexing skip the bounds-checked runtime accessors.
	direct_array_access         bool
	pending_direct_array_access bool
	temp_id                     int
	// Per-file memos for name lookups whose answers depend only on file-level
	// tables (module, imports, functions, constants, globals, declared types)
	// and are asked again for the same short name many times per file.
	unqualified_key_memo     map[string]string
	c_function_name_memo     map[string]string
	nonlocal_name_type_memo  map[string]string
	resolved_name_memo       map[string]string
	declared_type_key_memo   map[string]FastcMemoEntry
	locals                   map[string]FastcLocal
	local_scope_changes      []FastcLocalScopeChange
	local_scope_depth        int
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
	// While parsing an `or { ... }` block that yields a value, a trailing bare value
	// expression is captured here (typed by `or_value_expected_type`) instead of being
	// rejected as a value-only statement.
	or_value_capture        bool
	or_value_captured       string
	or_value_expected_type  string
	defer_depth             int
	captured_defer_lines    []string
	deferred_lines          []string
	deferred_block_starts   []int
	loop_defer_block_starts []int
	loop_has_breaks         []bool
	statement_reachable     bool
	last_expression_type    string
	last_expression         []FastcExpressionToken
	// Conditional expressions have no flat token list after their branches are
	// parsed. Preserve an Option payload type for their consuming declaration.
	last_option_value_type  string
	last_multi_return_types []string
	member_smartcasts       map[string]FastcMemberSmartcast
	// Set by `parse_type` to the wrapped value type when it parses an option type
	// (`?T`), so the caller can record it on the declared local.
	pending_option_value_type string
	// Set by `parse_type` when it parses a function type (`fn (...) R`): the C return
	// type (`Option`/`string`/…) and, for an `!`/`?R` return, the wrapped value type.
	// Lets a fn-typed parameter be declared as a real function pointer and its call
	// results be type-inferred. `pending_fn_pointer` gates whether the others apply.
	pending_fn_pointer           bool
	pending_fn_return_type       string
	pending_fn_option_value_type string
	c_flags                      []string
	fixed_array_types            map[string]string
	composite_types              map[string]bool
	expression_depth             int
	// True while reading a value inside an enum-keyed map literal, so a `.field`
	// on the next line is treated as the next key, not a member-chain continuation.
	in_enum_keyed_map_value bool
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
	// Declaration-initializer parsers (constants, globals, struct field
	// defaults) discard spawn registrations, so spawn is rejected there.
	declaration_initializer_mode bool
	// On-demand generic-method monomorphization state (see monomorphize.v drain): mono
	// instances still to generate, the set already generated (dedup), the signatures of the
	// instances (a PER-PARSER map — the shared `functions` is read-only across worker
	// threads and must never be mutated), and their C output.
	pending_mono     []FastcMonoRequest
	generated_mono   map[string]bool
	mono_functions   map[string]FastcFunctionSignature
	mono_definitions map[string]string
	// True while the drain re-parses a mono instance, so parse_function skips the
	// reachability prune (the instance is used by definition — that is why it was queued).
	in_mono_drain          bool
	in_generic_placeholder bool
}

// FastcMonoRequest is one queued on-demand generic method or free-function instantiation.
struct FastcMonoRequest {
	source_key string
	concrete   string
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
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: path
			source: source
			header: header
		},
	], map[string]string{}, prefs)!
	return c_source
}

// generate_files discovers imports from the input files and emits one C translation unit for
// the complete source graph. Discovery and generation use scanner tokens only.
pub fn generate_files(paths []string, prefs &pref.Preferences) !string {
	generation := generate_files_with_source_paths(paths, prefs)!
	return generation.c_source()
}

// generate_files_with_source_paths emits C and reports every source read during import discovery.
// FastcPhaseTimer reports per-phase generation timings to stderr when
// FASTC_BENCH_PHASES is set; it is a no-op otherwise.
struct FastcPhaseTimer {
mut:
	enabled bool
	sw      time.StopWatch
	last_us i64
}

fn fastc_new_phase_timer() FastcPhaseTimer {
	return FastcPhaseTimer{
		enabled: os.getenv('FASTC_BENCH_PHASES') != ''
		sw: time.new_stopwatch()
	}
}

fn (mut timer FastcPhaseTimer) mark(name string) {
	if !timer.enabled {
		return
	}
	now_us := timer.sw.elapsed().microseconds()
	eprintln('fastc-phase ${name} ${now_us - timer.last_us}us')
	timer.last_us = now_us
}

pub fn generate_files_with_source_paths(paths []string, prefs &pref.Preferences) !GenerationResult {
	mut timer := fastc_new_phase_timer()
	sources, module_aliases := fastc_resolve_source_files(paths, prefs)!
	timer.mark('resolve')
	mut source_paths := []string{cap: sources.len}
	for source_file in sources {
		source_paths << source_file.path
	}
	c_pieces, uses_threads, c_flags := generate_source_pieces(sources, module_aliases, prefs)!
	timer.mark('generate_total')
	return GenerationResult{
		c_pieces: c_pieces
		source_paths: source_paths
		uses_threads: uses_threads
		c_flags: c_flags
	}
}

// FastcFileGenContext bundles the read-only program tables every per-file
// code-generation Parser starts from. Workers share it across threads and
// return their per-file registration deltas to the stitch pass.
struct FastcFileGenContext {
	prefs                     &pref.Preferences = unsafe { nil }
	declared_types            map[string]bool
	declared_type_c_names     map[string]string
	declared_type_key_by_name map[string]string
	fastc_prefixed_c_names    []string
	has_c_functions           bool
	declared_kinds            map[string]FastcDeclaredTypeKind
	enum_flags                map[string]bool
	enum_field_types          map[string]string
	alias_base_types          map[string]string
	sum_types                 map[string]bool
	sum_type_variants         map[string]bool
	struct_fields             map[string]map[string]string
	struct_field_info         map[string][]FastcStructField
	struct_field_lookup       map[string]map[string]FastcStructField
	interface_fields          map[string]FastcInterfaceField
	constants                 map[string]string
	constant_values           map[string]string
	public_constants          map[string]bool
	globals                   map[string]string
	public_globals            map[string]bool
	used_function_names       map[string]bool
	has_startup_inits         bool
	has_cleanup_hooks         bool
	functions                 map[string]FastcFunctionSignature
	constant_types            map[string]string
	global_types              map[string]string
	fixed_array_types         map[string]string
	composite_types           map[string]bool
	generic_method_sources    map[string]FastcGenericMethodSource
	generic_method_names      map[string]bool
	module_aliases            map[string]string
}

// FastcFileGenOutput is one source file's generation result. The sequential
// stitch loop consumes them in file order, so parallel workers never touch
// the shared output builders or the merged registration maps.
// FastcFileGenResult is every file's generation output in file order with the
// union of the composite and fixed-array types they registered.
struct FastcFileGenResult {
	outputs           []FastcFileGenOutput
	composite_types   map[string]bool
	fixed_array_types map[string]string
}

// fastc_file_gen_result wraps serially generated outputs with their type
// registrations.
fn fastc_file_gen_result(outputs []FastcFileGenOutput) FastcFileGenResult {
	mut composite_types := map[string]bool{}
	mut fixed_array_types := map[string]string{}
	for output in outputs {
		for name, _ in output.composite_types {
			composite_types[name] = true
		}
		for name, array_type in output.fixed_array_types {
			fixed_array_types[name] = array_type
		}
	}
	return FastcFileGenResult{
		outputs: outputs
		composite_types: composite_types
		fixed_array_types: fixed_array_types
	}
}

struct FastcFileGenOutput {
mut:
	prototypes        string
	body              string
	directive_lines   []FastcCDirectiveLine
	has_main_entry    bool
	fixed_array_types map[string]string
	composite_types   map[string]bool
	spawn_typedefs    map[string]string
	spawn_helpers     map[string]string
	mono_definitions  map[string]string
	c_flags           []string
	failed            bool
	error_message     string
}

fn fastc_generate_single_file(ctx &FastcFileGenContext, source_file FastcSourceFile) FastcFileGenOutput {
	file := token.File.unindexed(source_file.path, source_file.source.len)
	prefs := ctx.prefs
	mut gen := Parser{
		prefs: unsafe { prefs }
		unqualified_key_memo: map[string]string{}
		c_function_name_memo: map[string]string{}
		nonlocal_name_type_memo: map[string]string{}
		resolved_name_memo: map[string]string{}
		declared_type_key_memo: map[string]FastcMemoEntry{}
		path: source_file.path
		source_offset: source_file.source_offset
		module_name: source_file.header.module_name
		imports: source_file.header.imports
		declared_types: ctx.declared_types
		declared_type_c_names: ctx.declared_type_c_names
		declared_type_key_by_name: ctx.declared_type_key_by_name
		fastc_prefixed_c_names: ctx.fastc_prefixed_c_names
		has_c_functions: ctx.has_c_functions
		comparison_memo: map[i64]FastcRenderedExpression{}
		member_smartcasts: map[string]FastcMemberSmartcast{}
		spawn_typedefs: map[string]string{}
		spawn_helpers: map[string]string{}
		thread_value_types: map[string]string{}
		declared_kinds: ctx.declared_kinds
		enum_flags: ctx.enum_flags
		enum_field_types: ctx.enum_field_types
		alias_base_types: ctx.alias_base_types
		sum_types: ctx.sum_types
		sum_type_variants: ctx.sum_type_variants
		struct_fields: ctx.struct_fields
		struct_field_info: ctx.struct_field_info
		struct_field_lookup: ctx.struct_field_lookup
		generic_method_sources: ctx.generic_method_sources
		generic_method_names: ctx.generic_method_names
		module_aliases: ctx.module_aliases
		generated_mono: map[string]bool{}
		mono_functions: map[string]FastcFunctionSignature{}
		mono_definitions: map[string]string{}
		interface_fields: ctx.interface_fields
		constants: ctx.constants
		constant_values: ctx.constant_values
		public_constants: ctx.public_constants
		globals: ctx.globals
		public_globals: ctx.public_globals
		used_function_names: ctx.used_function_names
		selfhost: prefs.building_v
		has_startup_inits: ctx.has_startup_inits
		has_cleanup_hooks: ctx.has_cleanup_hooks
		s: scanner.new_scanner(prefs, .normal)
		out: strings.new_builder(source_file.source.len)
		protos: strings.new_builder(256)
		functions: ctx.functions
		constant_types: ctx.constant_types
		global_types: ctx.global_types
		// These maps are per-file registration deltas. The stitch pass already owns
		// the declarations collected before file generation, so copying that shared
		// seed into every parser only adds allocation and hashing work.
		fixed_array_types: map[string]string{}
		composite_types: map[string]bool{}
		deferred_lines: []string{}
		deferred_block_starts: []int{}
		loop_defer_block_starts: []int{}
		loop_has_breaks: []bool{}
		statement_reachable: true
	}
	// The per-file lookup memos fill up quickly; size them once instead of
	// rehashing through the small capacities.
	gen.unqualified_key_memo.reserve(128)
	gen.c_function_name_memo.reserve(128)
	gen.nonlocal_name_type_memo.reserve(128)
	gen.resolved_name_memo.reserve(128)
	gen.declared_type_key_memo.reserve(128)
	gen.comparison_memo.reserve(64)
	gen.s.init(file, source_file.source)
	generated := gen.run() or {
		return FastcFileGenOutput{
			failed: true
			error_message: err.msg()
		}
	}
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return FastcFileGenOutput{
			failed: true
			error_message: 'fastc scanner error at byte ${diagnostic.offset + source_file.source_offset} in ${source_file.path}: ${diagnostic.message}'
		}
	}
	return FastcFileGenOutput{
		prototypes: gen.protos.str()
		body: generated
		directive_lines: fastc_scan_c_directive_lines(generated)
		has_main_entry: source_file.header.module_name in ['', 'main'] && gen.has_main
		fixed_array_types: gen.fixed_array_types
		composite_types: gen.composite_types
		spawn_typedefs: gen.spawn_typedefs
		spawn_helpers: gen.spawn_helpers
		mono_definitions: gen.mono_definitions
		c_flags: gen.c_flags
	}
}

// generate_source_files emits the program as one C string; tests and the
// single-source API use it. The driver takes the pieces directly.
fn generate_source_files(input_sources []FastcSourceFile, module_aliases map[string]string, prefs &pref.Preferences) !(string, bool, []string) {
	pieces, uses_threads, c_flags := generate_source_pieces(input_sources, module_aliases, prefs)!
	return fastc_join_c_pieces(pieces), uses_threads, c_flags
}

// generate_source_pieces emits the program as C pieces in output order; the
// per-file bodies are referenced, not copied, so the multi-megabyte output is
// never assembled into one buffer here.
fn generate_source_pieces(input_sources []FastcSourceFile, module_aliases map[string]string, prefs &pref.Preferences) !([]string, bool, []string) {
	// Source-level generic monomorphization runs first; it is a no-op when the
	// program has no generic function definitions (so the self-host is untouched).
	mut timer := fastc_new_phase_timer()
	mut sources := fastc_monomorphize_sources(input_sources, prefs)!
	timer.mark('monomorphize')
	mut declared_types := map[string]bool{}
	mut declared_kinds := map[string]FastcDeclaredTypeKind{}
	mut enum_flags := map[string]bool{}
	mut params_structs := map[string]bool{}
	mut constants := map[string]string{}
	mut public_constants := map[string]bool{}
	mut globals := map[string]string{}
	mut public_globals := map[string]bool{}
	mut type_source_paths := map[string]bool{}
	mut type_sources := map[string]string{}
	mut constant_sources := map[string]string{}
	mut constant_spans := map[string][]int{}
	mut global_sources := map[string]string{}
	generic_method_sources := fastc_collect_generic_and_declaration_indexes(mut sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
	timer.mark('declaration_indexes')
	// The type declarations depend only on the declaration index, so they are
	// rendered on a worker while the signatures are collected below; the
	// composite typedefs that precede them need both and are rendered after.
	mut pending_types := fastc_start_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants)
	// The sources are final now; split the oversized ones for parallel
	// generation while the declaration phases run.
	mut pending_fragments := fastc_start_generation_fragments(sources, prefs)
	declared_type_c_names := fastc_declared_type_c_names(declared_types)
	declared_type_key_by_name := fastc_declared_type_key_by_name(declared_types)
	mut functions := map[string]FastcFunctionSignature{}
	mut interface_methods := map[string]bool{}
	mut interface_fields := map[string]FastcInterfaceField{}
	// Interface embeds recorded as index-aligned parallel arrays (an embedder and the
	// interface it embeds), kept flat to stay in FastC's self-hostable subset.
	mut embed_embedders := []string{}
	mut embed_embeddeds := []string{}
	fastc_collect_signatures(sources, prefs, declared_types, declared_type_c_names, params_structs, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	timer.mark('signatures')
	// An interface that embeds another (`interface B { A; ... }`) inherits A's
	// methods. Copy them onto B (with the receiver re-keyed to B) so calls on a B
	// value resolve and B gets its own dispatch table entries.
	fastc_promote_embedded_interface_methods(embed_embedders, embed_embeddeds, mut functions, mut interface_methods)
	mut pending_references := fastc_start_referenced_function_names(sources, prefs, functions)
	has_c_functions := fastc_functions_declare_c(functions)
	fastc_prefixed_c_names := fastc_reserved_temporary_c_names(functions, globals)
	// Module dependency order is shared by the lifecycle, constant, global, and
	// startup-initializer passes below.
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	module_init_calls := fastc_module_init_calls(ordered_sources, functions)!
	module_cleanup_calls := fastc_module_cleanup_calls(ordered_sources, functions)!
	timer.mark('lifecycle_calls')
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
	mut type_result := fastc_wait_type_declarations(mut pending_types)!
	for name, _ in type_result.composite_types {
		composite_types[name] = true
	}
	composite_typedefs := fastc_composite_typedefs(composite_types)
	type_output := type_result.output
	struct_fields := type_result.struct_fields.move()
	mut struct_field_info := type_result.struct_field_info.move()
	timer.mark('type_declarations')
	declared_composite_types := composite_types.clone()
	type_declarations := type_output.declarations
	enum_field_types := type_output.enum_field_types.clone()
	mut constant_types := map[string]string{}
	mut global_types := map[string]string{}
	mut source_imports := map[string]map[string]string{}
	for source_file in sources {
		source_imports[source_file.path] = source_file.header.imports.clone()
	}
	fastc_render_struct_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.alias_base_types, struct_fields, mut struct_field_info, functions, constants, public_constants, constant_types, globals, public_globals, global_types, type_output.sum_types)!
	timer.mark('field_defaults')
	// The field lists are final; index them by name for generation while the
	// constant and global phases run.
	mut pending_field_lookup := fastc_start_struct_field_lookup(struct_field_info, prefs)
	constant_output := fastc_generate_constant_declarations(ordered_sources, constant_sources, constant_spans, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.alias_base_types, struct_fields, struct_field_info, functions, constants, public_constants, globals, public_globals, mut constant_types)!
	timer.mark('constant_declarations')
	for name, _ in constant_output.composite_types {
		composite_types[name] = true
	}
	global_output := fastc_generate_global_declarations(ordered_sources, global_sources, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.alias_base_types, struct_fields, struct_field_info, functions, constants, constant_output.compile_time_values, public_constants, constant_types, globals, public_globals, mut global_types)!
	timer.mark('global_declarations')
	for name, _ in global_output.composite_types {
		composite_types[name] = true
	}
	for constant_type in constant_types.values() {
		fastc_register_composite_type(constant_type, mut composite_types)
	}
	for global_type in global_types.values() {
		fastc_register_composite_type(global_type, mut composite_types)
	}
	startup_initializers := fastc_generate_startup_initializers(ordered_sources, constant_output.module_initializers, global_output.module_initializers, module_init_calls)!
	timer.mark('startup_initializers')
	used_function_names := fastc_wait_referenced_function_names(mut pending_references)
	timer.mark('wait_references')
	mut pending_interface_dispatches := fastc_start_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, prefs.building_v, prefs)
	struct_field_lookup := fastc_wait_struct_field_lookup(mut pending_field_lookup)
	mut prototypes := strings.new_builder(1024)
	// The per-file bodies are stitched by reference: the directive partition
	// works on their virtual concatenation and the final assembly copies each
	// range straight from the pieces, so the multi-megabyte body is copied once.
	mut body_pieces := []string{cap: sources.len + 16}
	mut body_len := 0
	mut body_directive_lines := []FastcCDirectiveLine{}
	mut fixed_array_types := constant_output.fixed_array_types.clone()
	for name, array_type in global_output.fixed_array_types {
		fixed_array_types[name] = array_type
	}
	mut has_entry_module := false
	for source_file in sources {
		if source_file.header.module_name in ['', 'main'] {
			has_entry_module = true
			break
		}
	}
	mut entry_has_main := false
	ctx := FastcFileGenContext{
		prefs: unsafe { prefs }
		declared_types: declared_types
		declared_type_c_names: declared_type_c_names
		declared_type_key_by_name: declared_type_key_by_name
		fastc_prefixed_c_names: fastc_prefixed_c_names
		has_c_functions: has_c_functions
		declared_kinds: declared_kinds
		enum_flags: enum_flags
		enum_field_types: enum_field_types
		alias_base_types: type_output.alias_base_types
		sum_types: type_output.sum_types
		sum_type_variants: type_output.sum_type_variants
		struct_fields: struct_fields
		struct_field_info: struct_field_info
		struct_field_lookup: struct_field_lookup
		generic_method_sources: generic_method_sources
		generic_method_names: fastc_generic_method_names(generic_method_sources)
		module_aliases: module_aliases
		interface_fields: interface_fields
		constants: constants
		constant_values: constant_output.compile_time_values
		public_constants: public_constants
		globals: globals
		public_globals: public_globals
		used_function_names: used_function_names
		has_startup_inits: startup_initializers.len > 0
		has_cleanup_hooks: module_cleanup_calls.len > 0
		functions: functions
		constant_types: constant_types
		global_types: global_types
		fixed_array_types: fixed_array_types
		composite_types: composite_types
	}
	mut spawn_typedefs := map[string]string{}
	mut spawn_helpers := map[string]string{}
	mut mono_definitions := map[string]string{}
	mut c_flags := []string{}
	generation_sources := fastc_wait_generation_fragments(mut pending_fragments)
	timer.mark('wait_fragments')
	generation := fastc_generate_file_outputs(&ctx, generation_sources)
	outputs := generation.outputs
	timer.mark('file_outputs')
	// Size the stitch buffers up front: the per-file bodies add up to several
	// megabytes, and growing the builders by doubling would copy that text
	// several times over.
	mut prototypes_size := 0
	for output in outputs {
		prototypes_size += output.prototypes.len
	}
	prototypes.ensure_cap(prototypes_size + 1024)
	timer.mark('stitch.size')
	for output in outputs {
		if output.failed {
			return error(output.error_message)
		}
		prototypes.write_string(output.prototypes)
		body_offset := body_len
		body_pieces << output.body
		body_len += output.body.len
		for line in output.directive_lines {
			body_directive_lines << FastcCDirectiveLine{
				start: body_offset + line.start
				end: body_offset + line.end
				kind: line.kind
			}
		}
		if output.mono_definitions.len > 0 {
			mut mono_names := output.mono_definitions.keys()
			mono_names.sort()
			for mono_name in mono_names {
				if mono_name !in mono_definitions {
					mono_definitions[mono_name] = output.mono_definitions[mono_name]
				}
			}
		}
		if output.has_main_entry {
			entry_has_main = true
		}
		for name, text in output.spawn_typedefs {
			spawn_typedefs[name] = text
		}
		for name, text in output.spawn_helpers {
			spawn_helpers[name] = text
		}
		c_flags << output.c_flags
	}
	for name, array_type in generation.fixed_array_types {
		fixed_array_types[name] = array_type
	}
	for name, _ in generation.composite_types {
		composite_types[name] = true
	}
	timer.mark('stitch.outputs')
	mut mono_names := mono_definitions.keys()
	mono_names.sort()
	timer.mark('stitch')
	for mono_name in mono_names {
		mono_definition := mono_definitions[mono_name]
		body_pieces << mono_definition
		body_len += mono_definition.len
	}
	synthesized_main := if has_entry_module && !entry_has_main {
		fastc_synthesized_main(prefs.building_v, startup_initializers.len > 0, module_cleanup_calls.len > 0)
	} else {
		''
	}
	mut late_composite_declarations := strings.new_builder(256)
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for index, composite_name in composite_names {
		// Composite types (`Array_x`, `Map_k_v`) can be sum-type variants, so give
		// each a stable type id in a range disjoint from the declared-type ids
		// (1..N) and the primitive ids (0x40000000+). Only unique/consistent values
		// matter: construction and `match` both reference `__v_typeid_<composite>`.
		late_composite_declarations.writeln('#define __v_typeid_${composite_name} ${u32(0x50000000) + u32(index)}')
		if composite_name !in declared_composite_types {
			declaration := 'typedef ${if composite_name.starts_with('Array_') {
				'array'
			} else {
				'map'
			}} ${composite_name};'
			late_composite_declarations.writeln(declaration)
		}
	}
	if late_composite_declarations.len > 0 {
		late_composite_declarations.writeln('')
	}
	interface_dispatches := fastc_wait_interface_dispatches(mut pending_interface_dispatches)
	timer.mark('wait_interface_dispatches')
	fixed_array_declarations := fastc_generate_fixed_array_declarations(fixed_array_types)
	preamble := if prefs.building_v { c_selfhost_preamble } else { c_preamble }
	hoisted_body := fastc_partition_c_directive_ranges(body_len, body_directive_lines)
	timer.mark('partition_directives')
	mut pieces := []string{cap: 64 + body_pieces.len * 3}
	pieces << preamble
	pieces << c_integer_comparison_helpers
	fastc_collect_c_piece_ranges(mut pieces, body_pieces, hoisted_body.directive_ranges)
	timer.mark('assemble.directives')
	if hoisted_body.final_kind == 1 {
		pieces << '\n'
	}
	if hoisted_body.directive_ranges.len > 0 {
		pieces << '\n'
	}
	if prefs.building_v {
		pieces << c_selfhost_post_directives
	}
	if spawn_typedefs.len > 0 {
		pieces << c_spawn_runtime
		pieces << '\n'
	}
	pieces << constant_output.macros
	pieces << type_output.declarations_head
	pieces << composite_typedefs
	pieces << type_declarations
	pieces << late_composite_declarations.str()
	pieces << fixed_array_declarations
	if spawn_typedefs.len > 0 {
		mut thread_type_names := spawn_typedefs.keys()
		thread_type_names.sort()
		for thread_type_name in thread_type_names {
			pieces << spawn_typedefs[thread_type_name]
			pieces << '\n'
		}
		pieces << '\n'
	}
	pieces << constant_output.declarations
	pieces << global_output.declarations
	pieces << prototypes.str()
	if startup_initializers.len > 0 {
		pieces << 'static void v_fastc_init_globals(void);'
		pieces << '\n'
	}
	if module_cleanup_calls.len > 0 {
		pieces << 'static void v_fastc_cleanup_modules(void);'
		pieces << '\n'
	}
	pieces << '\n'
	if prefs.building_v {
		pieces << c_selfhost_runtime
	}
	pieces << type_output.enum_string_helpers
	if spawn_helpers.len > 0 {
		mut spawn_helper_names := spawn_helpers.keys()
		spawn_helper_names.sort()
		for spawn_helper_name in spawn_helper_names {
			pieces << spawn_helpers[spawn_helper_name]
			pieces << '\n'
			pieces << '\n'
		}
	}
	pieces << interface_dispatches
	if startup_initializers.len > 0 {
		pieces << 'static void v_fastc_init_globals(void) {'
		pieces << '\n'
		pieces << startup_initializers
		pieces << '}'
		pieces << '\n'
		pieces << '\n'
	}
	if module_cleanup_calls.len > 0 {
		pieces << 'static void v_fastc_cleanup_modules(void) {'
		pieces << '\n'
		for cleanup_call in module_cleanup_calls {
			pieces << '\t${cleanup_call}();'
			pieces << '\n'
		}
		pieces << '}'
		pieces << '\n'
		pieces << '\n'
	}
	pieces << synthesized_main
	timer.mark('assemble.head')
	fastc_collect_c_piece_ranges(mut pieces, body_pieces, hoisted_body.conditional_ranges)
	timer.mark('assemble.conditional')
	if hoisted_body.final_kind == 2 {
		pieces << '\n'
	}
	if hoisted_body.conditional_ranges.len > 0 {
		pieces << '\n'
	}
	fastc_collect_c_piece_ranges(mut pieces, body_pieces, hoisted_body.body_ranges)
	if hoisted_body.final_kind == 0 {
		pieces << '\n'
	}
	timer.mark('assemble')
	return pieces, spawn_typedefs.len > 0, c_flags
}

// fastc_generic_method_names collects the bare method and function names of
// the generic sources, the last dotted component of their keys.
fn fastc_generic_method_names(generic_method_sources map[string]FastcGenericMethodSource) map[string]bool {
	mut names := map[string]bool{}
	for key, _ in generic_method_sources {
		names[key.all_after_last('.')] = true
	}
	return names
}

// fastc_build_struct_field_lookup indexes every struct's fields by name.
fn fastc_build_struct_field_lookup(struct_field_info map[string][]FastcStructField) map[string]map[string]FastcStructField {
	mut struct_field_lookup := map[string]map[string]FastcStructField{}
	for layout_type, fields in struct_field_info {
		mut fields_by_name := map[string]FastcStructField{}
		for field in fields {
			fields_by_name[field.name] = field
		}
		struct_field_lookup[layout_type] = fields_by_name.move()
	}
	return struct_field_lookup
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
	if candidate_signature.return_type != interface_signature.return_type || !fastc_string_types_equal(candidate_signature.return_types, interface_signature.return_types) || candidate_signature.option_type != interface_signature.option_type || candidate_signature.parameter_types.len != interface_signature.parameter_types.len {
		return false
	}
	for i in 0 .. interface_signature.parameter_types.len {
		if i > 0 && candidate_signature.parameter_types[i] != interface_signature.parameter_types[i] {
			return false
		}
		interface_parameter_is_mut := i < interface_signature.parameter_mutability.len && interface_signature.parameter_mutability[i]
		candidate_parameter_is_mut := i < candidate_signature.parameter_mutability.len && candidate_signature.parameter_mutability[i]
		if candidate_parameter_is_mut != interface_parameter_is_mut {
			return false
		}
	}
	return true
}

// fastc_promote_embedded_interface_methods copies the methods of each embedded
// interface onto the interface that embeds it, re-keyed to the embedder's receiver
// type. It runs to a fixpoint so a chain of embeds (`C { B }`, `B { A }`) inherits
// all the way down. A method the embedder already declares directly is left as-is.
fn fastc_promote_embedded_interface_methods(embed_embedders []string, embed_embeddeds []string, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool) {
	mut changed := true
	for changed {
		changed = false
		for i := 0; i < embed_embedders.len; i++ {
			embedder_key := embed_embedders[i]
			embedded_key := embed_embeddeds[i]
			embedder_type := fastc_c_declared_type_name(embedder_key)
			prefix := embedded_key + '.'
			for method_key in interface_methods.keys() {
				if !method_key.starts_with(prefix) {
					continue
				}
				method_name := method_key.all_after_last('.')
				promoted_key := '${embedder_key}.${method_name}'
				if promoted_key in interface_methods {
					continue
				}
				source := functions[method_key]
				mut promoted_params := source.parameter_types.clone()
				if promoted_params.len > 0 {
					promoted_params[0] = embedder_type
				}
				functions[promoted_key] = FastcFunctionSignature{
					parameter_types: promoted_params
					parameter_mutability: source.parameter_mutability.clone()
					return_type: source.return_type
					return_types: source.return_types.clone()
					option_type: source.option_type
					is_variadic: source.is_variadic
					last_parameter_is_params: source.last_parameter_is_params
					is_public: source.is_public
					is_disabled: source.is_disabled
					module_name: source.module_name
					path: source.path
				}
				interface_methods[promoted_key] = true
				changed = true
			}
		}
	}
}

fn fastc_generate_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool, used_function_names map[string]bool, selfhost bool) string {
	mut out := strings.new_builder(1024)
	mut function_keys := functions.keys()
	function_keys.sort()
	mut function_method_names := []string{cap: function_keys.len}
	for function_key in function_keys {
		function_method_names << function_key.all_after_last('.')
	}
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
			c_name := fastc_method_c_name(interface_signature.module_name, interface_type, method_name)
			out.writeln('${interface_signature.return_type} ${c_name}(${parameters.join(', ')}) {')
			out.writeln('\tswitch (value._typ) {')
			candidate_count := if selfhost && method_name !in used_function_names {
				0
			} else {
				function_keys.len
			}
			for candidate_index in 0 .. candidate_count {
				candidate_key := function_keys[candidate_index]
				if candidate_key == interface_method_key || function_method_names[candidate_index] != method_name {
					continue
				}
				receiver_key := candidate_key.all_before_last('.')
				if declared_kinds[receiver_key] in [.interface_, .enum_, .alias_] || receiver_key !in declared_kinds {
					continue
				}
				candidate_signature := functions[candidate_key]
				if !fastc_interface_method_signatures_match(interface_signature, candidate_signature) {
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
				call := '${fastc_method_c_name(candidate_signature.module_name, receiver_type, method_name)}(${receiver_argument}${call_arguments})'
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
	partitioned := fastc_partition_c_directives(source)
	mut directives := strings.new_builder(256)
	fastc_write_c_source_ranges(mut directives, partitioned.source, partitioned.directive_ranges)
	if partitioned.final_kind == 1 {
		directives.write_u8(`\n`)
	}
	if partitioned.directive_ranges.len > 0 {
		directives.writeln('')
	}
	mut conditional_code := strings.new_builder(256)
	fastc_write_c_source_ranges(mut conditional_code, partitioned.source, partitioned.conditional_ranges)
	if partitioned.final_kind == 2 {
		conditional_code.write_u8(`\n`)
	}
	if partitioned.conditional_ranges.len > 0 {
		conditional_code.writeln('')
	}
	mut body := strings.new_builder(source.len)
	fastc_write_c_source_ranges(mut body, partitioned.source, partitioned.body_ranges)
	if partitioned.final_kind == 0 {
		body.write_u8(`\n`)
	}
	return FastcHoistedCSource{
		directives: directives.str()
		conditional_code: conditional_code.str()
		body: body.str()
	}
}

fn fastc_partition_c_directives(source string) FastcPartitionedCSource {
	mut directive_ranges := []int{}
	mut conditional_ranges := []int{}
	mut body_ranges := []int{}
	mut conditional_depth := 0
	mut line_start := 0
	mut run_start := 0
	mut run_kind := -1
	for line_end := 0; line_end <= source.len; line_end++ {
		if line_end < source.len && source[line_end] != `\n` {
			continue
		}
		directive_kind := fastc_c_directive_kind(source, line_start, line_end)
		mut line_kind := 0
		if conditional_depth > 0 {
			line_kind = 2
			if directive_kind == 2 {
				conditional_depth++
			} else if directive_kind == 3 {
				conditional_depth--
			}
		} else if directive_kind == 2 {
			line_kind = 2
			conditional_depth = 1
		} else if directive_kind != 0 {
			line_kind = 1
		}
		if run_kind == -1 {
			run_kind = line_kind
		} else if line_kind != run_kind {
			fastc_append_c_source_range(run_start, line_start, run_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
			run_start = line_start
			run_kind = line_kind
		}
		if line_end == source.len {
			break
		}
		line_start = line_end + 1
	}
	fastc_append_c_source_range(run_start, source.len, run_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
	return FastcPartitionedCSource{
		source: source
		directive_ranges: directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges: body_ranges
		final_kind: run_kind
	}
}

fn fastc_scan_c_directive_lines(source string) []FastcCDirectiveLine {
	mut lines := []FastcCDirectiveLine{}
	mut line_start := 0
	for line_end := 0; line_end <= source.len; line_end++ {
		if line_end < source.len && source[line_end] != `\n` {
			continue
		}
		kind := fastc_c_directive_kind(source, line_start, line_end)
		if kind != 0 {
			lines << FastcCDirectiveLine{
				start: line_start
				end: if line_end < source.len { line_end + 1 } else { line_end }
				kind: kind
			}
		}
		line_start = line_end + 1
	}
	return lines
}

// fastc_partition_c_directive_ranges partitions a body of `total_len` bytes,
// known only through its directive lines, into directive, conditional and
// plain ranges (see fastc_partition_c_directive_lines).
fn fastc_partition_c_directive_ranges(total_len int, lines []FastcCDirectiveLine) FastcPartitionedCSource {
	mut directive_ranges := []int{cap: lines.len * 2}
	mut conditional_ranges := []int{cap: lines.len * 2}
	mut body_ranges := []int{cap: lines.len * 2 + 2}
	mut conditional_depth := 0
	mut cursor := 0
	mut final_kind := 0
	for line in lines {
		if cursor < line.start {
			final_kind = if conditional_depth > 0 { 2 } else { 0 }
			fastc_append_c_source_range(cursor, line.start, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
		}
		if conditional_depth > 0 {
			final_kind = 2
			if line.kind == 2 {
				conditional_depth++
			} else if line.kind == 3 {
				conditional_depth--
			}
		} else if line.kind == 2 {
			final_kind = 2
			conditional_depth = 1
		} else {
			final_kind = 1
		}
		fastc_append_c_source_range(line.start, line.end, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
		cursor = line.end
	}
	if cursor < total_len {
		final_kind = if conditional_depth > 0 { 2 } else { 0 }
		fastc_append_c_source_range(cursor, total_len, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
	}
	return FastcPartitionedCSource{
		directive_ranges: directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges: body_ranges
		final_kind: final_kind
	}
}

// fastc_collect_c_piece_ranges appends the ascending [start, end) `ranges` of
// the virtual concatenation of `pieces` to `out`. A range that covers a whole
// piece shares that string; a range that cuts a piece is copied out, so every
// output piece is an ordinary owned, NUL-terminated string. Only bodies that
// contain C directive lines are cut, so the copies are a small part of the
// output.
fn fastc_collect_c_piece_ranges(mut out []string, pieces []string, ranges []int) {
	mut piece_index := 0
	mut piece_start := 0
	mut range_index := 0
	for range_index + 1 < ranges.len {
		mut start := ranges[range_index]
		end := ranges[range_index + 1]
		range_index += 2
		for start < end {
			for piece_index < pieces.len && piece_start + pieces[piece_index].len <= start {
				piece_start += pieces[piece_index].len
				piece_index++
			}
			if piece_index >= pieces.len {
				return
			}
			piece := pieces[piece_index]
			local_start := start - piece_start
			mut local_end := end - piece_start
			if local_end > piece.len {
				local_end = piece.len
			}
			if local_start == 0 && local_end == piece.len {
				out << piece
			} else {
				out << piece.substr(local_start, local_end)
			}
			start = piece_start + local_end
		}
	}
}

fn fastc_partition_c_directive_lines(source string, lines []FastcCDirectiveLine) FastcPartitionedCSource {
	mut directive_ranges := []int{cap: lines.len * 2}
	mut conditional_ranges := []int{cap: lines.len * 2}
	mut body_ranges := []int{cap: lines.len * 2 + 2}
	mut conditional_depth := 0
	mut cursor := 0
	mut final_kind := 0
	for line in lines {
		if cursor < line.start {
			final_kind = if conditional_depth > 0 { 2 } else { 0 }
			fastc_append_c_source_range(cursor, line.start, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
		}
		if conditional_depth > 0 {
			final_kind = 2
			if line.kind == 2 {
				conditional_depth++
			} else if line.kind == 3 {
				conditional_depth--
			}
		} else if line.kind == 2 {
			final_kind = 2
			conditional_depth = 1
		} else {
			final_kind = 1
		}
		fastc_append_c_source_range(line.start, line.end, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
		cursor = line.end
	}
	if cursor < source.len {
		final_kind = if conditional_depth > 0 { 2 } else { 0 }
		fastc_append_c_source_range(cursor, source.len, final_kind, mut directive_ranges, mut conditional_ranges, mut body_ranges)
	}
	return FastcPartitionedCSource{
		source: source
		directive_ranges: directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges: body_ranges
		final_kind: final_kind
	}
}

fn fastc_append_c_source_range(start int, end int, kind int, mut directive_ranges []int, mut conditional_ranges []int, mut body_ranges []int) {
	match kind {
		1 {
			directive_ranges << start
			directive_ranges << end
		}
		2 {
			conditional_ranges << start
			conditional_ranges << end
		}
		else {
			body_ranges << start
			body_ranges << end
		}
	}
}

@[direct_array_access]
fn fastc_write_c_source_ranges(mut out strings.Builder, source string, ranges []int) {
	for i := 0; i < ranges.len; i += 2 {
		out.write_string(source[ranges[i]..ranges[i + 1]])
	}
}

// fastc_c_directive_kind classifies a source line without allocating a
// substring: 0 is ordinary code, 1 a plain directive, 2 a conditional opener,
// and 3 `#endif`.
fn fastc_c_directive_kind(source string, start int, end int) int {
	if start >= end || source[start] != `#` {
		return 0
	}
	mut name_start := start + 1
	for name_start < end && source[name_start] in [` `, `\t`, `\r`, `\v`, `\f`] {
		name_start++
	}
	if name_start == end {
		return 0
	}
	mut name_end := name_start
	for name_end < end && source[name_end] !in [` `, `\t`, `\r`, `\v`, `\f`] {
		name_end++
	}
	name_len := name_end - name_start
	if name_len == 2 && source[name_start] == `i` && source[name_start + 1] == `f` {
		return 2
	}
	if name_len == 5 && source[name_start] == `i` && source[name_start + 1] == `f` && source[name_start + 2] == `d` && source[name_start + 3] == `e` && source[name_start + 4] == `f` {
		return 2
	}
	if name_len == 6 && source[name_start] == `i` && source[name_start + 1] == `f` && source[name_start + 2] == `n` && source[name_start + 3] == `d` && source[name_start + 4] == `e` && source[name_start + 5] == `f` {
		return 2
	}
	if name_len == 5 && source[name_start] == `e` && source[name_start + 1] == `n` && source[name_start + 2] == `d` && source[name_start + 3] == `i` && source[name_start + 4] == `f` {
		return 3
	}
	return 1
}
