module fastc

import v.flat
import os
import strings
import time
import v.pref
import v.scanner
import v.token

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
typedef struct { void *data; int offset; int len; int cap; int flags; int element_size; } array;
/* Keep these private map-header declarations in sync with builtin/map.v. */
typedef struct {
	int key_bytes;
	int value_bytes;
	int cap;
	int len;
	u32 deletes;
	u8 *all_deleted;
	u8 *keys;
	u8 *values;
} DenseArray;
typedef u64 (*MapHashFn)(voidptr);
typedef bool (*MapEqFn)(voidptr, voidptr);
typedef void (*MapCloneFn)(voidptr, voidptr);
typedef void (*MapFreeFn)(voidptr);
typedef struct VMapData {
	int key_bytes;
	int value_bytes;
	u32 even_index;
	u8 cached_hashbits;
	u8 shift;
	DenseArray key_values;
	u32 *metas;
	u32 extra_metas;
	bool has_string_keys;
	MapHashFn hash_fn;
	MapEqFn key_eq_fn;
	MapCloneFn clone_fn;
	MapFreeFn free_fn;
	int count;
} VMapData;
typedef struct { VMapData *data; } map;
typedef struct { void *data; unsigned char state; } Option;
typedef struct { void *data; void *err; unsigned char state; } __v_result;
/* One multi-return component. Values up to 32 bytes are stored inline; larger
   ones are boxed and referenced through `ptr`, so no component size can
   overflow the slot. */
typedef union { uintptr_t word; long double alignment; void *ptr; unsigned char data[32]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) __vf_multi_value = (expression); MultiReturnValue __vf_multi_result = {0}; if (sizeof(__vf_multi_value) <= sizeof(__vf_multi_result.data)) memcpy(__vf_multi_result.data, &__vf_multi_value, sizeof(__vf_multi_value)); else __vf_multi_result.ptr = v_fastc_interface_box(&__vf_multi_value, sizeof(__vf_multi_value)); __vf_multi_result; })
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

// The allocator can itself return an Option. Small target payloads therefore
// travel in the wrapper, without allocating or storing a pointer into that wrapper.
const c_target_option_runtime = r'
#define V_FASTC_OPTION_SUCCESS(wrapper, type, expression) ({ \
    type __vf_inline_value = (expression); \
    wrapper __vf_inline_result = {0}; \
    if (sizeof(__vf_inline_value) <= sizeof(__vf_inline_result.inline_bits)) { \
        memcpy(&__vf_inline_result.inline_bits, &__vf_inline_value, sizeof(__vf_inline_value)); \
        __vf_inline_result.is_inline = 1; \
    } else { \
        __vf_inline_result.data = v_fastc_interface_box(&__vf_inline_value, sizeof(__vf_inline_value)); \
    } \
    __vf_inline_result; \
})
#define V_FASTC_OPTION_VALUE(type, expression) ({ \
    __typeof__(expression) __vf_inline_option = (expression); \
    type __vf_inline_payload; \
    memcpy(&__vf_inline_payload, __vf_inline_option.is_inline ? (const void *)&__vf_inline_option.inline_bits : __vf_inline_option.data, sizeof(__vf_inline_payload)); \
    __vf_inline_payload; \
})
/* Narrowed optional members can also be mutated. This accessor takes only
   original wrapper storage, never the temporary used by OPTION_VALUE. */
#define V_FASTC_OPTION_STORAGE(type, expression) ({ \
    __typeof__(&(expression)) __vf_inline_storage = &(expression); \
    typedef type __vf_inline_alias __attribute__((__may_alias__)); \
    (__vf_inline_alias *)(__vf_inline_storage->is_inline ? (void *)&__vf_inline_storage->inline_bits : __vf_inline_storage->data); \
})

'

// Keep V identifiers distinct from C keywords, library macros, and memory helpers.
const fastc_c_reserved_identifiers = {
	'array':    true
	'asm':      true
	'auto':     true
	'break':    true
	'map':      true
	'case':     true
	'char':     true
	'const':    true
	'continue': true
	'default':  true
	'do':       true
	'double':   true
	'else':     true
	'enum':     true
	'errno':    true
	'extern':   true
	'false':    true
	'float':    true
	'for':      true
	'goto':     true
	'if':       true
	'inline':   true
	'int':      true
	'long':     true
	'memcpy':   true
	'memmove':  true
	'memset':   true
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
	if name.len > 1 && name[0] == `@` {
		return fastc_c_identifier(name[1..])
	}
	if name.len < 2 || name.len > 8 {
		return name
	}
	first := name[0]
	// Bitset of the initial letters used by the reserved-identifier table (`a` is bit 0).
	if first < `a` || first > `w` || (u32(8_264_063) & (u32(1) << (first - `a`))) == 0 {
		return name
	}
	return if name in fastc_c_reserved_identifiers {
		'__vf_keyword_${name}'
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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* V __offsetof(T, field) */
#define __offsetof(st, m) offsetof(st, m)
#ifndef _WIN32
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <dirent.h>
#include <signal.h>
#include <utime.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
extern char **environ;
/* builtin declares these with `fn C.` and no header; the C backend provides them the same way. */
#if (defined(__linux__) && (defined(__GLIBC__) || defined(__GNU_LIBRARY__))) || defined(__APPLE__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__DragonFly__)
#include <execinfo.h>
#endif
#if defined(__linux__) && defined(__GLIBC__)
#include <sys/syscall.h>
#endif
#endif

/* `C.__V_architecture` (pref.get_host_arch) resolves the host arch at C-compile time;
 * its numeric values match pref.Arch (amd64=1, arm64=2, …). */
#ifndef __V_architecture
#define __V_architecture 0
#if defined(__x86_64__) || defined(_M_AMD64)
#undef __V_architecture
#define __V_architecture 1
#endif
#if defined(__aarch64__) || defined(__arm64__) || defined(_M_ARM64)
#undef __V_architecture
#define __V_architecture 2
#endif
#if defined(__arm__) || defined(_M_ARM)
#undef __V_architecture
#define __V_architecture 3
#endif
#if defined(__riscv) && __riscv_xlen == 64
#undef __V_architecture
#define __V_architecture 4
#endif
#if defined(__riscv) && __riscv_xlen == 32
#undef __V_architecture
#define __V_architecture 5
#endif
#if defined(__i386__) || defined(_M_IX86)
#undef __V_architecture
#define __V_architecture 6
#endif
#endif

/* `C.V_COMMIT_HASH` (util.vhash) is normally injected by the build; keep the same
 * placeholder the C backend uses when it is absent. */
#ifndef V_COMMIT_HASH
#define V_COMMIT_HASH "@@@"
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
struct sync__Channel;
typedef struct sync__Channel *chan;
/* One multi-return component. Values up to 32 bytes are stored inline; larger
   ones are boxed and referenced through `ptr`, so no component size can
   overflow the slot. */
typedef union { uintptr_t word; long double alignment; void *ptr; unsigned char data[32]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) __vf_multi_value = (expression); MultiReturnValue __vf_multi_result = {0}; if (sizeof(__vf_multi_value) <= sizeof(__vf_multi_result.data)) memcpy(__vf_multi_result.data, &__vf_multi_value, sizeof(__vf_multi_value)); else __vf_multi_result.ptr = v_fastc_interface_box(&__vf_multi_value, sizeof(__vf_multi_value)); __vf_multi_result; })
#define V_FASTC_MULTI_SOURCE(slot, size) (((size) <= sizeof((slot).data)) ? (const void *)(slot).data : (const void *)(slot).ptr)

#define _S(s) ((string){.str=(byteptr)("" s), .len=(sizeof(s)-1), .is_lit=1})
#define _SLIT0 _S("")

/* Option, result, and interface payloads are boxed on the heap and never
   freed. Boxing through malloc made every option-returning call pay for a
   locked, zero-filling allocation, so each thread bumps its boxes out of a
   private chunk instead; only oversized payloads still go through malloc. */
#if !defined(_WIN32) && !defined(V_FASTC_TARGET_LIBC)
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
static inline u64 wyhash64(u64 a, u64 b) { return _wymix(a ^ _wyp[0], 0xa0761d6478bd642full) ^ _wymix(b ^ _wyp[1], 0xe7037ed1a0b428dbull); }
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
// c_selfhost_preamble_includes is the header block of c_selfhost_preamble;
// a header-free build replaces it with the target's C ABI prelude.
const c_selfhost_preamble_includes = r'#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* V __offsetof(T, field) */
#define __offsetof(st, m) offsetof(st, m)
#ifndef _WIN32
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <dirent.h>
#include <signal.h>
#include <utime.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
extern char **environ;
/* builtin declares these with `fn C.` and no header; the C backend provides them the same way. */
#if (defined(__linux__) && (defined(__GLIBC__) || defined(__GNU_LIBRARY__))) || defined(__APPLE__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__DragonFly__)
#include <execinfo.h>
#endif
#if defined(__linux__) && defined(__GLIBC__)
#include <sys/syscall.h>
#endif
#endif
'

const c_selfhost_post_directives = r'#ifndef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP
#define pthread_rwlockattr_setkind_np(a, b) (0)
#endif
#if defined(__linux__)
/* These builtin APIs are not declared by the TinyCC C99 system headers. */
#if defined(__GLIBC__) || defined(__GNU_LIBRARY__)
extern int backtrace(void **, int);
extern char **backtrace_symbols(void *const *, int);
extern void backtrace_symbols_fd(void *const *, int, int);
#endif
#if defined(__GLIBC__)
/* glibc exports `gettid` only from 2.30 on; the system call works with any glibc.
   Strict feature modes hide `syscall` in <unistd.h>, so it is declared here. */
long syscall(long number, ...);
static inline pid_t v_fastc_gettid(void) { return (pid_t)syscall(SYS_gettid); }
#define gettid v_fastc_gettid
#else
extern int gettid(void);
#endif
extern void *aligned_alloc(size_t, size_t);
#endif
#ifndef memory_order_relaxed
#define memory_order_relaxed 0
#define memory_order_acquire 2
#define memory_order_release 3
#define memory_order_acq_rel 4
#define memory_order_seq_cst 5
#endif
#if defined(__TINYC__) && defined(__aarch64__)
#undef atomic_thread_fence
#undef __atomic_thread_fence
extern void _V_atomic_thread_fence(int order);
#define atomic_thread_fence(order) _V_atomic_thread_fence(order)
#define __atomic_thread_fence(order) _V_atomic_thread_fence(order)
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

static array builtin__array_sorted(array *values) {
	array result = *values;
	if (values != NULL && values->len > 0) {
		size_t bytes = (size_t)values->len * (size_t)values->element_size;
		result.data = malloc(bytes);
		if (result.data != NULL && values->data != NULL) memcpy(result.data, values->data, bytes);
		result.offset = 0;
		result.cap = values->len;
	}
	builtin__array_sort(&result);
	return result;
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
	parameter_types          flat.TypeTextList
	parameter_mutability     []bool
	parameter_constness      []bool
	parameter_generic_values []bool
	return_type              u32
	return_types             flat.TypeTextList
	option_type              u32
	// A directly returned function value is erased to voidptr in the C ABI.
	returned_fn_parameter_types   flat.TypeTextList
	returned_fn_return_type       u32
	returned_fn_option_value_type u32
	is_variadic                   bool
	last_parameter_is_params      bool
	is_public                     bool
	is_disabled                   bool
	is_c_extern                   bool
	is_markused                   bool
	is_global                     bool
	// Leading array dimensions before a stored function value's signature.
	callable_index_depth int
	export_name          string
	module_name          string
	path                 string
	// position is the byte offset of the declaration's `fn` in the file at `path`.
	position int
}

// fastc_multi_return_literal packs already-boxed components into a MultiReturn.
// Filling an uninitialized local slot by slot avoids the zero-fill a partially
// designated compound literal would perform on the whole aggregate.
fn fastc_multi_return_literal(packed_values []string) string {
	mut out := strings.new_builder(64)
	out.write_string('({ MultiReturn __vf_multi; ')
	for i, value in packed_values {
		out.write_string('__vf_multi.values[${i}] = ${value}; ')
	}
	out.write_string('__vf_multi; })')
	return out.str()
}

// multi_return_component stores one returned value in its multi-return slot as the
// component's declared type, which is how the caller reads it back: a number is converted
// (`return 1, 2` for `(f64, f32)`), and a concrete value for an interface or sum type
// component is boxed, as a single return value is.
fn (g &Parser) multi_return_component(value string, value_type u32, index int) string {
	mut expression := value
	if index < g.return_types.ids.len {
		declared_type := g.return_types.ids[index]
		declared_layout := g.underlying_alias_type(declared_type)
		is_number := fastc_is_integer_type(declared_layout)
			|| declared_layout == flat.source_type_f32 || declared_layout == flat.source_type_f64
		declared_kind := g.declared_kinds[flat.source_type_name(g.semantic_type_key(declared_type))]
		value_kind := g.declared_kinds[flat.source_type_name(g.semantic_type_key(value_type))]
		needs_box := g.selfhost && value_type != 0 && value_type != declared_type
			&& (g.should_box_variant(declared_type, value_type)
				|| (declared_kind == .interface_ && value_kind != .interface_))
		if is_number {
			expression = '((${fastc_output_c_type(flat.source_type_name(declared_type))})(${value}))'
		} else if needs_box {
			expression = g.interface_value_expression(declared_type, value_type, value)
		}
	}
	return 'V_FASTC_MULTI_VALUE((${expression}))'
}

fn fastc_type_ids_equal(left []u32, right []u32) bool {
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
	typ          u32
	// A smart-cast that narrows a local to a concrete variant may store the narrowed value
	// in a uniquely named C temporary rather than shadowing the local's own C name, so a
	// later `defer` body (rendered at function scope) still binds the original name. When set,
	// the local's spelling resolves to this name instead of `fastc_c_identifier(name)`.
	c_name string
	// For a variable declared with an option type (`b ?bool`), the C `typ` is the
	// type-erased `Option`; this keeps the wrapped value type so an `if x := b`
	// guard on the bare variable can unwrap it.
	option_value_type u32
	// For a function-pointer parameter (`f fn (int) !string`), the C `typ` is a
	// function pointer; these record the callee's return C type and, for an option
	// return, its wrapped value type, so `f(x)` / `f(x) or {…}` infer correctly.
	fn_return_type       u32
	fn_option_value_type u32
	fn_signature_key     string
	// For a channel local (`ch chan T` / `ch := chan T{…}`), the C `typ` is the erased
	// `chan`; this keeps the element C type so `<-ch` recovers the received value type.
	chan_element_type u32
	// For a bare-local `is` smart-cast whose `typ` is the narrowed variant, this records the
	// original boxed sum-type/interface type. A method defined on the whole sum type (`fn (e
	// Expr) pos()`) is not on the variant, so it dispatches on the still-live boxed original.
	smartcast_origin_type u32
	// Keep the declared slot mode distinct from a narrowed payload reference.
	smartcast_origin_is_reference bool
	// C expression that still names the original boxed value while this local is narrowed.
	// Mutable calls accepting the sum type must receive this storage, not the concrete payload
	// pointer, because their ABI expects the sum-type tag/object wrapper.
	smartcast_origin_source string
	// For a bool local assigned a top-level `&&` chain of `is` tests (`ok := x is A && y is B`),
	// the member smart-casts implied when the bool is true. A later `ok && x.field` then narrows
	// `x`/`y` for the rest of that `&&` chain. Scoped with the local, so it never leaks between
	// functions that reuse the bool's name.
	bool_implications []FastcBoolImplication
}

fn (local FastcLocal) original_is_reference() bool {
	if local.smartcast_origin_type != u32(0) {
		return local.smartcast_origin_is_reference
	}
	return local.is_reference
}

struct FastcLocalScopeChange {
	name         string
	previous     FastcLocal
	had_previous bool
}

struct FastcFunctionDeferBlock {
	flag  string
	lines []string
}

struct FastcExpressionToken {
	source               string
	unsafe_depth         int
	tok                  token.Token
	chan_element_type    u32
	option_value_type    u32
	is_mut_argument      bool
	is_statement         bool
	is_capturing_closure bool
	is_storage           bool
mut:
	typ u32
	lit string
}

struct FastcRenderedExpression {
	source string
	typ    u32
}

// FastcMemoEntry caches an optional lookup result, including a negative one.
struct FastcMemoEntry {
	found bool
	value u32
}

struct FastcInterpolationWidth {
	width      int
	left_align bool
}

struct FastcStructField {
	name                 string
	typ                  u32
	is_public            bool
	is_mutable           bool
	is_required          bool
	is_skip              bool
	is_function          bool
	is_optional_function bool
	// The field's type is spelled `voidptr`; fn types and erased type parameters are stored
	// as `voidptr` too, and V3 accepts `nil` outside `unsafe` only for a real one.
	is_declared_voidptr bool
	// Shared map fields keep pointer identity even though FastC currently erases locking.
	// Without this, copying their containing struct detaches subsequent map header updates.
	is_shared_pointer      bool
	module_name            string
	path                   string
	imports                map[string]string
	default_source         string
	default_source_offset  int
	default_source_origins []FastcSourceOrigin
	translated             bool
mut:
	default_value string
	storage_path  []string
	// For a `chan T` field, the erased C `typ` is `chan`; this keeps the element C type
	// so `<-x.field` recovers the received value type.
	chan_element_type u32
	// For an option field (`f ?T`), the erased C `typ` is `Option`; this keeps the
	// wrapped value C type so `if mut v := x.f {` binds `v` to `T` (not the default int).
	option_value_type u32
	// Function fields are stored as `voidptr`; retain their signature so member calls
	// can cast the erased storage back to a callable C function pointer.
	fn_parameter_types   flat.TypeTextList
	fn_return_type       u32
	fn_option_value_type u32
	// Imported generic structs retain erased storage, but a concrete field like
	// `Stack[Item]` still carries `Item` for method result/argument recovery.
	generic_argument_type u32
}

struct FastcInterfaceField {
	name       string
	typ        u32
	is_mutable bool
}

struct FastcSourceHeader {
	module_name             string
	imports                 map[string]string
	import_order            []string
	blank_imports           []string
	has_globals             bool
	translated              bool
	has_constants           bool
	has_global_declarations bool
	// Byte-level superset tests over the whole file (see fastc_source_scan_flags):
	// a false flag proves the file cannot hold that kind of declaration, so the
	// matching collection pass skips its scan.
	has_interfaces        bool
	has_comptime_if       bool
	has_type_keywords     bool
	has_generic_fn_syntax bool
	has_select            bool
	// body_spans lists the top-level function bodies as [start of `{`, offset
	// after `}`) pairs, recorded by the declaration pass, so the later token
	// passes skip them instead of lexing them again.
	body_spans []int
}

// FastcSourceOrigin maps a copied span in a rewritten or filtered buffer to its source file.
struct FastcSourceOrigin {
	filtered_offset int
	source_offset   int
}

struct FastcDeclarationSource {
	source  string
	origins []FastcSourceOrigin
}

struct FastcSourceFile {
	path                 string
	source               string
	source_offset        int
	source_origins       []FastcSourceOrigin
	source_line_offset   int
	source_column_offset int
	header               FastcSourceHeader
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
	// units describes how c_pieces split into translation units that the
	// drivers can compile in parallel (see fastc_write_c_units).
	units FastcUnitLayout
}

// FastcUnitLayout locates, in a generation's pieces, the shared head (types,
// constants, globals, prototypes, runtime), the pieces that belong to one
// unit only (the startup initializer, the cleanup hook and the synthesized
// main) and the per-file body units. extern_indexes name the head pieces
// whose definitions must become declarations in every unit but the first
// (the globals), with extern_texts holding those declarations.
pub struct FastcUnitLayout {
pub mut:
	head_end           int
	solo_end           int
	unit_starts        []int
	extern_indexes     []int
	extern_texts       []string
	prototype_start    int
	prototype_end      int
	prototype_texts    []string
	unit_ref_starts    []int
	unit_ref_ids       []int
	solo_prototype_ids []int
	tail_prototype_ids []int
	// define_texts are the same pieces with `static` dropped, for the first
	// unit: a static definition would not satisfy the other units' externs.
	define_texts []string
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

// fastc_take_string turns a builder's contents into a string without copying
// them: the builder's buffer becomes the string and the builder is left
// empty, exactly as `str()` leaves it.
// fastc_piece returns its argument unchanged. Pushing a call result onto a
// `[]string` shares the string, whereas pushing a variable or a field clones
// it, so the stitch and the assembly pass the generated bodies along without
// copying them.
fn fastc_piece(s string) string {
	return s
}

fn fastc_take_string(mut b strings.Builder) string {
	b << u8(0)
	taken := unsafe { (&u8(b.data)).vstring_with_len(b.len - 1) }
	b = strings.new_builder(0)
	return taken
}

// fastc_take_trimmed is fastc_take_string followed by trim_space, copying
// only when there is whitespace to trim.
fn fastc_take_trimmed(mut b strings.Builder) string {
	taken := fastc_take_string(mut b)
	left, right := taken.trim_indexes(' \n\t\v\f\r')
	if left == 0 && right == taken.len {
		return taken
	}
	return taken.substr(left, right)
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
	uses_pthread_tls    bool
	declarations        string
	module_initializers map[string]string
	composite_types     map[string]bool
	fixed_array_types   flat.TypeTextBindings
}

struct FastcConstantDeclarations {
	macros              string
	declarations        string
	module_initializers map[string]string
	compile_time_values map[string]string
	composite_types     map[string]bool
	fixed_array_types   flat.TypeTextBindings

mut:
	// The struct fields with their defaults rendered, and their by-name index:
	// the defaults are rendered on a worker while the constants are pre-parsed.
	struct_field_info   map[string][]FastcStructField
	struct_field_lookup map[string]map[string]&FastcStructField
}

// fastc_note_field_defaults_use records that the parser consulted rendered
// struct field defaults.
fn fastc_note_field_defaults_use(g &Parser) {
	mut w := unsafe { &Parser(g) }
	w.used_field_defaults = true
}

struct FastcConstantValue {
	key          string
	c_name       string
	module_name  string
	value        string
	typ          u32
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
	// offset is the byte offset of `source` in the source the block was scanned from.
	offset int
}

struct FastcDeclarationAttribute {
	tok            token.Token
	is_enabled     bool
	is_flag        bool
	is_params      bool
	is_typedef     bool
	is_c_extern    bool
	is_markused    bool
	export_name    string
	linker_section string
	alignment      string
	is_cinit       bool
	is_packed      bool
}

struct FastcEnumInfo {
	c_name  string
	name    u32
	fields  []string
	is_flag bool
}

struct FastcTypeDeclarations {
	// declarations_head precedes the composite typedefs (type ids and forward
	// typedefs) and declarations follows them (the type bodies).
	declarations_head string
	declarations      string
	// The enum `str`/print helpers, one piece each (parallel lists).
	enum_helper_names []string
	enum_helper_texts []string
	alias_base_types  flat.TypeTextBindings
	// Return type (C spelling) declared by each `type Name = fn (...) Ret` alias,
	// keyed by the alias C name. Lets a call through such a value infer its result.
	fn_alias_return_types flat.TypeTextBindings
	enum_field_types      flat.TypeTextBindings
	// Value names declared by each enum, keyed by the enum's C name, in source
	// order. Used to unroll `$for x in Enum.values { ... }`.
	enum_field_names map[string][]string
	// Declared C names of sum types (`type X = A | B`). They share the boxed
	// `{void*_object; uintptr_t _typ;}` layout with interfaces; construction boxes a
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
	typ u32
	// The tested value type distinguishes a reference variant from boxed value storage.
	value_type    u32
	source        string
	variants      flat.TypeTextList
	tag_source    string
	object_source string
	// The boxed member itself (`b->v`) and its sum or interface type. An assignment to the
	// narrowed member stores a new boxed value there, and ends the narrowing.
	subject_source string
	subject_type   u32
}

// FastcBoolImplication records that a bool local (assigned a top-level `&&` chain of `is` tests,
// `enum_operands := lhs is Enum && rhs is Enum`) implies a member smart-cast on `subject` whenever
// the bool is true — so a later `enum_operands && lhs.field` narrows `lhs` for the rest of the `&&`.
struct FastcBoolImplication {
	subject   string
	smartcast FastcMemberSmartcast
}

// FastcTypeDescriptorMemoEntry caches an immutable descriptor lookup. Zero
// keys are empty; each operation uses its own 64-slot direct-mapped cache.
struct FastcTypeDescriptorMemoEntry {
	key   u32
	value u32
}

struct FastcTypeDescriptorMemo {
mut:
	semantic_keys          [64]FastcTypeDescriptorMemoEntry
	aliases                [64]FastcTypeDescriptorMemoEntry
	direct_member_receiver u32
	direct_member_field    string
	direct_member_type     u32
	direct_member_known    bool
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
	declared_type_c_names     flat.TypeTextBindings
	declared_type_key_by_name flat.TypeTextBindings
	// Only final self-host file generation opts in: type names, aliases, kinds,
	// struct fields and interface fields stay immutable through module/mono switches.
	type_descriptor_memo &FastcTypeDescriptorMemo = unsafe { nil }
	// `__vf_`-prefixed generated C names, the only possible collisions for
	// temporary_namespace candidates (see fastc_reserved_temporary_c_names).
	fastc_prefixed_c_names []string
	declared_kinds         map[string]FastcDeclaredTypeKind
	enum_flags             map[string]bool
	enum_field_types       flat.TypeTextBindings
	enum_field_names       map[string][]string
	alias_base_types       flat.TypeTextBindings
	fn_alias_return_types  flat.TypeTextBindings
	sum_types              map[string]bool
	// Declared variants of each sum type, keyed `"${sum_type_c_name}|${variant_c_name}"`.
	// Lets an append distinguish push-many (`[]T << []T`) from boxing an array-valued
	// variant of a recursive sum type as one element (see sumtype_has_variant).
	sum_type_variants   map[string]bool
	struct_fields       map[string]flat.TypeTextBindings
	struct_field_info   map[string][]FastcStructField
	struct_field_lookup map[string]map[string]&FastcStructField
	interface_fields    map[string]FastcInterfaceField
	constants           map[string]string
	constant_values     map[string]string
	public_constants    map[string]bool
	globals             map[string]string
	global_suffix_index FastcGlobalSuffixIndex
	public_globals      map[string]bool
	used_function_names map[string]bool
	// function_c_names overrides the conventional C spelling for selected
	// source-level functions. It is immutable and shared by file generators.
	function_c_names map[string]string
	selfhost         bool
	header_free      bool
	// source_has_select is false only when the file provably holds no `select`
	// word (see fastc_source_scan_flags), so block pre-scans for channel
	// select statements can be skipped.
	source_has_select bool = true
	// Every builtin numeric receiver lacks a source-defined arithmetic method.
	numeric_ops_are_builtin bool
	primitive_cmps_builtin  bool
	has_startup_inits       bool
	has_cleanup_hooks       bool
mut:
	uses_pthread_tls     bool
	translated           bool
	path                 string
	source_offset        int
	source_origins       []FastcSourceOrigin
	source_line_offset   int
	source_column_offset int
	module_name          string
	imports              map[string]string
	s                    scanner.Scanner
	tok                  token.Token
	lit                  string
	out                  strings.Builder
	protos               strings.Builder
	// The function definitions of this file, recorded for the reachability
	// prune: see FastcFileGenOutput.
	function_id_table    map[string]int
	last_function_c_name string
	function_ids         []int
	function_spans       []int
	proto_spans          []int
	indent               int
	in_main              bool
	has_main             bool
	unsafe_depth         int
	// Set while generating a `@[direct_array_access]` function body, so string
	// and array indexing skip the bounds-checked runtime accessors.
	direct_array_access         bool
	pending_direct_array_access bool
	temp_id                     int
	// Per-file memos for name lookups whose answers depend only on file-level
	// tables (module, imports, functions, constants, globals, declared types)
	// and are asked again for the same short name many times per file.
	unqualified_key_memo       map[string]string
	global_key_memo            map[string]string
	c_function_name_memo       map[string]string
	nonlocal_name_type_memo    flat.TypeTextBindings
	resolved_name_memo         map[string]string
	declared_type_key_memo     map[string]FastcMemoEntry
	locals                     map[string]FastcLocal
	local_scope_changes        []FastcLocalScopeChange
	local_scope_depth          int
	functions                  map[string]FastcFunctionSignature
	constant_types             flat.TypeTextBindings
	global_types               flat.TypeTextBindings
	return_type                u32
	return_type_is_function    bool
	return_types_are_functions []bool
	return_types               flat.TypeTextList
	option_return_type         u32
	current_function           string
	current_receiver           u32
	current_method_is_static   bool
	expected_expression_type   u32
	// Scoped to a whole nil reset of a concrete local; erased function values are excluded.
	allows_nil_local_assignment bool
	capturing_defer             bool
	// next_declaration_is_unsafe records a `@[unsafe]` attribute seen just before a
	// top-level declaration, so a function so marked parses its body as unsafe.
	next_declaration_is_unsafe bool
	// While parsing an `or { ... }` block that yields a value, a trailing bare value
	// expression is captured here (typed by `or_value_expected_type`) instead of being
	// rejected as a value-only statement.
	or_value_capture            bool
	or_value_scope_depth        int
	or_value_captured           string
	or_value_expected_type      u32
	defer_depth                 int
	captured_defer_lines        []string
	deferred_lines              []string
	deferred_block_starts       []int
	function_defer_blocks       []FastcFunctionDeferBlock
	function_defer_declarations []string
	loop_defer_block_starts     []int
	loop_has_breaks             []bool
	loop_labels                 []string
	parsing_loop_labels         []string
	pending_loop_label          string
	statement_reachable         bool
	last_expression_type        u32
	last_expression             []FastcExpressionToken
	// Conditional expressions have no flat token list after their branches are
	// parsed. Preserve an Option payload type for their consuming declaration.
	last_option_value_type  u32
	last_multi_return_types flat.TypeTextList
	member_smartcasts       map[string]FastcMemberSmartcast
	// Set by `parse_type` to the wrapped value type when it parses an option type
	// (`?T`), so the caller can record it on the declared local.
	pending_option_value_type u32
	// Set by `parse_type` when it parses a function type (`fn (...) R`): the C return
	// type (`Option`/`string`/…), for an `!`/`?R` return the wrapped value type, and the
	// C prototype of its parameters. Lets a fn-typed parameter be declared as a real
	// function pointer and its call results be type-inferred. `pending_fn_pointer` gates
	// whether the others apply.
	pending_fn_pointer           bool
	pending_fn_return_type       u32
	pending_fn_option_value_type u32
	pending_fn_parameters        string
	c_flags                      []string
	fixed_array_types            flat.TypeTextBindings
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
	// Inferred types memoized per token subrange the same way, and cleared
	// whenever the locals or the registered functions change mid-expression.
	type_memo map[i64]u32
	// Method keys memoized per receiver type and method name; the key only
	// depends on the file-level tables, so it is reset with the other memos.
	method_key_memo map[u32]map[string]string
	// Promoted struct fields memoized per receiver type and field name (nil
	// records a miss); each record is allocated separately from the maps.
	field_memo map[u32]map[string]&FastcStructField
	// The render and inference passes commonly ask for the same field several
	// times in succession. Keep that entry outside the nested maps.
	last_field_receiver u32
	last_field_name     string
	last_field          &FastcStructField = unsafe { nil }
	last_field_known    bool
	// used_field_defaults records that a rendered struct field default was
	// consulted (see fastc_note_field_defaults_use).
	used_field_defaults bool
	has_c_functions     bool
	// Spawn lowering registrations (see spawn.v): thread struct typedefs,
	// creator/run/waiter helper definitions, and thread type -> value type.
	spawn_typedefs     map[string]string
	spawn_helpers      map[string]string
	thread_value_types flat.TypeTextBindings
	// Spawn pre-scans share one word check for the current scanner buffer.
	spawn_source_pointer  voidptr
	spawn_source_length   int
	spawn_source_checked  bool
	spawn_source_has_word bool
	// Declaration-initializer parsers (constants, globals, struct field
	// defaults) discard spawn registrations, so spawn is rejected there.
	declaration_initializer_mode bool
	c_global_static_initializer  bool
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
	concrete   u32
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

fn fastc_comparison_memo_range_key(tokens []FastcExpressionToken, start int, end int, tag i64) i64 {
	length := end - start
	if length >= 32768 {
		return 0
	}
	data := u64(tokens.data) + u64(start * int(sizeof(FastcExpressionToken)))
	return i64(((data >> 3) << 16) | (u64(length) & 0x7fff) | (u64(tag) << 62))
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
			path:   path
			source: source
			header: header
		},
	], map[string]string{}, prefs)!
	return c_source
}

// generate_files discovers imports from the input files or directories and emits one C unit for
// the complete source graph. Discovery and generation use scanner tokens only.
pub fn generate_files(paths []string, prefs &pref.Preferences) !string {
	generation := generate_files_with_source_paths(paths, prefs)!
	return generation.c_source()
}

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
		sw:      time.new_stopwatch()
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

// generate_files_with_source_paths emits C and reports every source read from entry files,
// entry directories, and their imports.
pub fn generate_files_with_source_paths(paths []string, prefs &pref.Preferences) !GenerationResult {
	mut timer := fastc_new_phase_timer()
	// The resolve memo is written while the program is generated and joined
	// once the C pieces exist.
	mut pending_memo_store := FastcPendingMemoStore{}
	scoped_prefs := fastc_preferences_for_entry_paths(paths, prefs)
	entries := fastc_expand_entry_paths(paths, &scoped_prefs)!
	sources, module_aliases := fastc_resolve_source_files_deferring_memo(entries, &scoped_prefs, mut pending_memo_store)!
	timer.mark('resolve')
	mut source_paths := []string{cap: sources.len}
	for source_file in sources {
		source_paths << source_file.path
	}
	// The layout comes back through a parameter: a struct of arrays as a
	// multi-return component is not carried correctly by the self-hosted
	// generator yet.
	mut units := FastcUnitLayout{}
	c_pieces, uses_threads, c_flags := generate_source_pieces(sources, module_aliases, &scoped_prefs,
		mut units)!
	fastc_wait_memo_store(mut pending_memo_store)
	timer.mark('generate_total')
	return GenerationResult{
		c_pieces:     c_pieces
		source_paths: source_paths
		uses_threads: uses_threads
		c_flags:      c_flags
		units:        units
	}
}

// FastcFileGenContext bundles the read-only program tables every per-file
// code-generation Parser starts from. Workers share it across threads and
// return their per-file registration deltas to the stitch pass.
struct FastcFileGenContext {
	prefs                     &pref.Preferences = unsafe { nil }
	declared_types            map[string]bool
	declared_type_c_names     flat.TypeTextBindings
	declared_type_key_by_name flat.TypeTextBindings
	fastc_prefixed_c_names    []string
	has_c_functions           bool
	numeric_ops_are_builtin   bool
	primitive_cmps_builtin    bool
	declared_kinds            map[string]FastcDeclaredTypeKind
	enum_flags                map[string]bool
	enum_field_types          flat.TypeTextBindings
	enum_field_names          map[string][]string
	alias_base_types          flat.TypeTextBindings
	fn_alias_return_types     flat.TypeTextBindings
	sum_types                 map[string]bool
	sum_type_variants         map[string]bool
	struct_fields             map[string]flat.TypeTextBindings
	struct_field_info         map[string][]FastcStructField
	struct_field_lookup       map[string]map[string]&FastcStructField
	interface_fields          map[string]FastcInterfaceField
	constants                 map[string]string
	constant_values           map[string]string
	public_constants          map[string]bool
	globals                   map[string]string
	global_suffix_index       FastcGlobalSuffixIndex
	public_globals            map[string]bool
	used_function_names       map[string]bool
	has_startup_inits         bool
	has_cleanup_hooks         bool
	functions                 map[string]FastcFunctionSignature
	function_c_names          map[string]string
	constant_types            flat.TypeTextBindings
	global_types              flat.TypeTextBindings
	fixed_array_types         flat.TypeTextBindings
	composite_types           map[string]bool
	generic_method_sources    map[string]FastcGenericMethodSource
	// function_ids numbers the C names of the indexed functions; a file
	// output reports its definitions and references by these ids so the
	// stitch can drop the functions nothing reachable refers to.
	function_ids         map[string]int
	prune_unreachable    bool
	generic_method_names map[string]bool
	module_aliases       map[string]string
}

// FastcFileGenOutput is one source file's generation result. The sequential
// stitch loop consumes them in file order, so parallel workers never touch
// the shared output builders or the merged registration maps.
// FastcFileGenResult is every file's generation output in file order with the
// union of the composite and fixed-array types they registered.
struct FastcFileGenResult {
	outputs           []FastcFileGenOutput
	composite_types   map[string]bool
	fixed_array_types flat.TypeTextBindings
}

// fastc_file_gen_result wraps serially generated outputs with their type
// registrations.
fn fastc_file_gen_result(outputs []FastcFileGenOutput) FastcFileGenResult {
	mut composite_types := map[string]bool{}
	mut fixed_array_types := flat.TypeTextBindings{}
	for output in outputs {
		for name, _ in output.composite_types {
			composite_types[name] = true
		}
		for name, array_type_id in output.fixed_array_types.ids {
			fixed_array_types.set_id(name, array_type_id)
		}
	}
	return FastcFileGenResult{
		outputs:           outputs
		composite_types:   composite_types
		fixed_array_types: fixed_array_types
	}
}

struct FastcFileGenOutput {
mut:
	prototypes      string
	body            string
	directive_lines []FastcCDirectiveLine
	// The functions defined in `body`: their id (-1 when not indexed), their
	// [start, end) span in `body` and in `prototypes`, and the ids they
	// reference (CSR: function i references refs[ref_starts[i]..ref_starts[i+1]]).
	// root_refs are the ids referenced by code that is always emitted (the
	// generic instances and spawn helpers of this file).
	function_ids      []int
	function_spans    []int
	proto_spans       []int
	ref_starts        []int
	refs              []int
	root_refs         []int
	has_main_entry    bool
	fixed_array_types flat.TypeTextBindings
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
	body_capacity := source_file.source.len * (if prefs.building_v { 4 } else { 2 }) + 1024
	mut gen := Parser{
		prefs:                     unsafe { prefs }
		unqualified_key_memo:      map[string]string{}
		global_key_memo:           map[string]string{}
		c_function_name_memo:      map[string]string{}
		nonlocal_name_type_memo:   flat.TypeTextBindings{}
		resolved_name_memo:        map[string]string{}
		declared_type_key_memo:    map[string]FastcMemoEntry{}
		path:                      source_file.path
		source_offset:             source_file.source_offset
		source_origins:            source_file.source_origins
		source_line_offset:        source_file.source_line_offset
		source_column_offset:      source_file.source_column_offset
		module_name:               source_file.header.module_name
		imports:                   source_file.header.imports
		declared_types:            ctx.declared_types
		declared_type_c_names:     ctx.declared_type_c_names
		declared_type_key_by_name: ctx.declared_type_key_by_name
		fastc_prefixed_c_names:    ctx.fastc_prefixed_c_names
		has_c_functions:           ctx.has_c_functions
		numeric_ops_are_builtin:   ctx.numeric_ops_are_builtin
		primitive_cmps_builtin:    ctx.primitive_cmps_builtin
		comparison_memo:           map[i64]FastcRenderedExpression{}
		type_memo:                 map[i64]u32{}
		method_key_memo:           map[u32]map[string]string{}
		field_memo:                map[u32]map[string]&FastcStructField{}
		member_smartcasts:         map[string]FastcMemberSmartcast{}
		spawn_typedefs:            map[string]string{}
		spawn_helpers:             map[string]string{}
		thread_value_types:        flat.TypeTextBindings{}
		declared_kinds:            ctx.declared_kinds
		enum_flags:                ctx.enum_flags
		enum_field_types:          ctx.enum_field_types
		enum_field_names:          ctx.enum_field_names
		alias_base_types:          ctx.alias_base_types
		fn_alias_return_types:     ctx.fn_alias_return_types
		sum_types:                 ctx.sum_types
		sum_type_variants:         ctx.sum_type_variants
		struct_fields:             ctx.struct_fields
		struct_field_info:         ctx.struct_field_info
		struct_field_lookup:       ctx.struct_field_lookup
		generic_method_sources:    ctx.generic_method_sources
		generic_method_names:      ctx.generic_method_names
		module_aliases:            ctx.module_aliases
		generated_mono:            map[string]bool{}
		mono_functions:            map[string]FastcFunctionSignature{}
		mono_definitions:          map[string]string{}
		interface_fields:          ctx.interface_fields
		constants:                 ctx.constants
		constant_values:           ctx.constant_values
		public_constants:          ctx.public_constants
		globals:                   ctx.globals
		global_suffix_index:       ctx.global_suffix_index
		public_globals:            ctx.public_globals
		used_function_names:       ctx.used_function_names
		function_c_names:          ctx.function_c_names
		selfhost:                  prefs.building_v
		type_descriptor_memo:      if prefs.building_v {
			&FastcTypeDescriptorMemo{}
		} else {
			unsafe { nil }
		}
		translated:                source_file.header.translated
		source_has_select:         source_file.header.has_select
		has_startup_inits:         ctx.has_startup_inits
		has_cleanup_hooks:         ctx.has_cleanup_hooks
		s:                         scanner.new_scanner(prefs, .normal)
		out:                       strings.new_builder(body_capacity)
		protos:                    strings.new_builder(4096)
		functions:                 ctx.functions
		function_id_table:         ctx.function_ids
		constant_types:            ctx.constant_types
		global_types:              ctx.global_types
		// These maps are per-file registration deltas. The stitch pass already owns
		// the declarations collected before file generation, so copying that shared
		// seed into every parser only adds allocation and hashing work.
		fixed_array_types:         flat.TypeTextBindings{}
		composite_types:           map[string]bool{}
		deferred_lines:            []string{}
		deferred_block_starts:     []int{}
		loop_defer_block_starts:   []int{}
		loop_has_breaks:           []bool{}
		loop_labels:               []string{}
		parsing_loop_labels:       []string{}
		statement_reachable:       true
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
			failed:        true
			error_message: err.msg()
		}
	}
	if gen.s.diagnostics.len > 0 {
		diagnostic := gen.s.diagnostics[0]
		return FastcFileGenOutput{
			failed:        true
			error_message: 'fastc scanner error at byte ${diagnostic.offset + source_file.source_offset} in ${source_file.path}: ${diagnostic.message}'
		}
	}
	mut ref_starts := []int{}
	mut refs := []int{}
	mut root_refs := []int{}
	if ctx.prune_unreachable {
		ref_starts = []int{cap: gen.function_ids.len + 1}
		for i in 0 .. gen.function_ids.len {
			ref_starts << refs.len
			fastc_collect_c_name_ids(generated, gen.function_spans[2 * i], gen.function_spans[2 * i + 1], ctx.function_ids, mut refs)
		}
		ref_starts << refs.len
		for _, definition in gen.mono_definitions {
			fastc_collect_c_name_ids(definition, 0, definition.len, ctx.function_ids, mut root_refs)
		}
		for _, helper in gen.spawn_helpers {
			fastc_collect_c_name_ids(helper, 0, helper.len, ctx.function_ids, mut root_refs)
		}
	}
	return FastcFileGenOutput{
		prototypes:        fastc_take_string(mut gen.protos)
		body:              generated
		directive_lines:   fastc_scan_c_directive_lines(generated)
		function_ids:      gen.function_ids
		function_spans:    gen.function_spans
		proto_spans:       gen.proto_spans
		ref_starts:        ref_starts
		refs:              refs
		root_refs:         root_refs
		has_main_entry:    source_file.header.module_name in ['', 'main'] && gen.has_main
		fixed_array_types: gen.fixed_array_types
		composite_types:   gen.composite_types
		spawn_typedefs:    gen.spawn_typedefs
		spawn_helpers:     gen.spawn_helpers
		mono_definitions:  gen.mono_definitions
		c_flags:           gen.c_flags
	}
}

// generate_source_files emits the program as one C string; tests and the
// single-source API use it. The driver takes the pieces directly.
fn generate_source_files(input_sources []FastcSourceFile, module_aliases map[string]string, prefs &pref.Preferences) !(string, bool, []string) {
	mut units := FastcUnitLayout{}
	pieces, uses_threads, c_flags := generate_source_pieces(input_sources, module_aliases, prefs, mut units)!
	return fastc_join_c_pieces(pieces), uses_threads, c_flags
}

// generate_source_pieces emits the program as C pieces in output order; the
// per-file bodies are referenced, not copied, so the multi-megabyte output is
// never assembled into one buffer here.
fn generate_source_pieces(input_sources []FastcSourceFile, module_aliases map[string]string, input_prefs &pref.Preferences, mut units FastcUnitLayout) !([]string, bool, []string) {
	scoped_prefs := fastc_target_runtime_preferences(input_prefs)
	prefs := &scoped_prefs
	// The `int` C spelling is fixed before any source is generated, so the
	// generation workers below all read the same width.
	fastc_set_platform_int_bits(prefs.target.pointer_bits)
	fastc_set_compact_v3_type_names(prefs.building_v)
	defer {
		// The compact spellings are private to a V3 self-host. Keep the module
		// generator's global target state from leaking into a later generation.
		fastc_set_compact_v3_type_names(false)
	}
	// Source-level generic monomorphization runs first; it is a no-op when the
	// program has no generic function definitions (so the self-host is untouched).
	mut timer := fastc_new_phase_timer()
	qualified_sources := fastc_sources_with_self_module_qualifiers(input_sources, prefs)!
	mut sources := fastc_monomorphize_sources(qualified_sources, prefs)!
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
	mut type_sources := map[string]FastcDeclarationSource{}
	mut constant_sources := map[string]FastcDeclarationSource{}
	mut constant_spans := map[string][]int{}
	mut global_sources := map[string]FastcDeclarationSource{}
	generic_method_sources := fastc_collect_generic_and_declaration_indexes(mut sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
	timer.mark('declaration_indexes')
	// builtin declares IError as `pub struct C.IError`, which the index does not record, so
	// signatures such as `fn f() IError` in selfhost sources would be undeclared. The arm64 path
	// records the same intrinsic error interface.
	declared_types['IError'] = true
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
	mut embed_embedders := []u32{}
	mut embed_embeddeds := []u32{}
	fastc_collect_signatures(sources, prefs, declared_types, declared_type_c_names, params_structs, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	if prefs.target_libc_headers {
		fastc_bind_legacy_native_aliases(mut functions)
	}
	timer.mark('signatures')
	// An interface that embeds another (`interface B { A; ... }`) inherits A's
	// methods. Copy them onto B (with the receiver re-keyed to B) so calls on a B
	// value resolve and B gets its own dispatch table entries.
	fastc_promote_embedded_interface_methods(embed_embedders, embed_embeddeds, mut functions, mut interface_methods)
	mut function_c_names := fastc_compact_function_c_names(functions,
		prefs.building_v && !prefs.target_libc_headers)
	if prefs.target_libc_headers {
		for key, signature in functions {
			if !key.starts_with('C.') {
				name := if key.contains('.') {
					fastc_signature_c_name(key, signature)
				} else {
					fastc_c_function_name(signature.module_name, key)
				}
				// A main-module function already has its `main__` name; a method of a
				// main-module type gets it here.
				function_c_names[key] = if key.contains('.')
					&& signature.module_name in ['', 'main'] {
					'main__${name}'
				} else {
					name
				}
			}
		}
	}
	for key, signature in functions {
		if key == 'main' || key.contains('.') || signature.module_name !in ['', 'main'] {
			continue
		}
		// A main-module function shares the `main__` namespace with the main module's
		// constants and the global `byte`. V accepts a constant and a function of the
		// same name, so such a function is named in FastC's own namespace instead.
		c_name := function_c_names[key] or { fastc_c_function_name_for_key(key) }
		if c_name.starts_with('main__')
			&& ((constants[key] or { '' }) == c_name || (globals[key] or { '' }) == c_name) {
			function_c_names[key] = '__vf_function_${c_name['main__'.len..]}'
		}
	}
	for key, signature in functions {
		if signature.export_name != '' && !signature.is_disabled {
			internal_name := function_c_names[key] or { fastc_c_function_name_for_key(key) }
			if internal_name == signature.export_name {
				function_c_names[key] = '__vf_export_body_${signature.export_name}'
			}
		}
	}
	for key, signature in functions {
		if signature.is_global {
			if c_name := globals[key] {
				function_c_names[key] = c_name
			}
		}
	}
	if prefs.is_test && 'main' in functions {
		// A test build's main runs its tests; the program's own is an ordinary function.
		function_c_names['main'] = 'v_fastc_program_main'
	}
	mut has_entry_module := false
	for source_file in sources {
		if source_file.header.module_name in ['', 'main'] {
			has_entry_module = true
			break
		}
	}
	mut pending_references := fastc_start_referenced_function_names(sources, prefs, functions)
	has_c_functions := fastc_functions_declare_c(functions)
	fastc_prefixed_c_names := fastc_reserved_temporary_c_names(functions, globals)
	// Module dependency order is shared by the lifecycle, constant, global, and
	// startup-initializer passes below.
	ordered_sources := fastc_sources_in_dependency_order(sources)!
	module_init_calls := fastc_module_init_calls(ordered_sources, functions, function_c_names)!
	module_cleanup_calls := fastc_module_cleanup_calls(ordered_sources, functions, function_c_names)!
	timer.mark('lifecycle_calls')
	mut composite_types := map[string]bool{}
	if prefs.building_v {
		// The OS exec helpers build their native argv arrays locally, so this
		// representation does not otherwise appear in a declaration signature.
		composite_types['Array_char_ptr'] = true
	}
	for signature in functions.values() {
		fastc_register_composite_type(signature.return_type_text(), mut composite_types)
		for parameter_type in signature.parameter_types_text() {
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
	mut constant_types := flat.TypeTextBindings{}
	mut global_types := flat.TypeTextBindings{}
	mut source_imports := map[string]map[string]string{}
	for source_file in sources {
		source_imports[source_file.path] = source_file.header.imports.clone()
	}
	// Candidates carry their filtered constant source as `source`, in module
	// dependency order, so both the parallel pre-pass and the in-order merge
	// see the same files.
	constant_candidates := fastc_constant_candidates(ordered_sources, constant_sources, constant_spans)
	mut parallel_constant_results := []FastcConstantFileResult{}
	if fastc_field_defaults_reference_constants(struct_field_info, constants) {
		seed_ctx := FastcConstantGenContext{
			prefs:                     unsafe { prefs }
			declared_types:            declared_types
			declared_type_c_names:     declared_type_c_names
			declared_type_key_by_name: declared_type_key_by_name
			fastc_prefixed_c_names:    fastc_prefixed_c_names
			has_c_functions:           fastc_functions_declare_c(functions)
			declared_kinds:            declared_kinds
			enum_flags:                enum_flags
			enum_field_types:          enum_field_types
			alias_base_types:          type_output.alias_base_types
			struct_fields:             struct_fields
			struct_field_info:         struct_field_info
			sum_types:                 type_output.sum_types
			sum_type_variants:         type_output.sum_type_variants
			functions:                 functions
			function_c_names:          function_c_names
			constants:                 constants
			public_constants:          public_constants
			globals:                   globals
			public_globals:            public_globals
		}
		parallel_constant_results = fastc_parse_constant_files_parallel(&seed_ctx, constant_candidates, constant_types)
		fastc_seed_constant_types(&seed_ctx, constant_candidates, parallel_constant_results, mut constant_types)
	}
	// The struct field defaults render on a worker while the constants are
	// pre-parsed; a constant file that consulted them is re-parsed after.
	mut pending_defaults := fastc_start_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.enum_field_names, type_output.alias_base_types, struct_fields, struct_field_info, functions, function_c_names, constants, public_constants, constant_types, globals, public_globals, global_types, type_output.sum_types)
	mut constant_output := fastc_generate_constant_declarations(constant_candidates, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.alias_base_types, struct_fields, struct_field_info, type_output.sum_types, type_output.sum_type_variants, mut pending_defaults, functions, function_c_names, constants, public_constants, globals, public_globals, parallel_constant_results, mut constant_types)!
	struct_field_info = constant_output.struct_field_info.move()
	timer.mark('constant_declarations')
	for name, _ in constant_output.composite_types {
		composite_types[name] = true
	}
	// Leave native headers to the C compiler, including compiler self-builds.
	header_free := false
	global_output := fastc_generate_global_declarations(ordered_sources, global_sources, prefs, header_free, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, type_output.alias_base_types, struct_fields, struct_field_info, functions, function_c_names, constants, constant_output.compile_time_values, public_constants, constant_types, globals, public_globals, mut global_types)!
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
	startup_initializers := fastc_generate_startup_initializers(ordered_sources, constant_output.module_initializers, global_output.module_initializers, module_init_calls, function_c_names)!
	timer.mark('startup_initializers')
	mut used_function_names := fastc_wait_referenced_function_names(mut pending_references)
	for key, signature in functions {
		if fastc_function_is_native_root(key, signature) {
			used_function_names[key.all_after_last('.')] = true
		}
	}
	if 'main' !in functions {
		// A program without an explicit main has no entry roots to walk from, so
		// every declared function must survive the earlier name-based body filter too.
		for key in functions.keys() {
			used_function_names[key.all_after_last('.')] = true
		}
	}
	timer.mark('wait_references')
	mut pending_interface_dispatches := fastc_start_interface_dispatches(declared_kinds, functions, function_c_names, interface_methods, struct_field_info, used_function_names, prefs.building_v, prefs)
	struct_field_lookup := constant_output.struct_field_lookup.move()
	// The per-file prototype blocks are emitted as pieces too, so they are
	// never concatenated into one buffer.
	mut prototype_pieces := []string{cap: sources.len + 16}
	// The per-file bodies are stitched by reference: the directive partition
	// works on their virtual concatenation and the final assembly copies each
	// range straight from the pieces, so the multi-megabyte body is copied once.
	mut body_pieces := []string{cap: sources.len + 16}
	mut body_len := 0
	mut proto_len := 0
	mut output_body_offsets := []int{cap: sources.len + 16}
	mut output_proto_offsets := []int{cap: sources.len + 16}
	mut body_directive_lines := []FastcCDirectiveLine{}
	mut fixed_array_types := constant_output.fixed_array_types.clone()
	for name, array_type_id in global_output.fixed_array_types.ids {
		fixed_array_types.set_id(name, array_type_id)
	}
	mut entry_has_main := false
	// Self-host builds drop the functions that nothing reachable refers to:
	// the source-level reachability keeps every function sharing a used name.
	prune_unreachable := prefs.building_v
	function_ids := if prune_unreachable {
		fastc_function_id_table(functions, function_c_names, declared_kinds)
	} else {
		map[string]int{}
	}
	ctx := FastcFileGenContext{
		prefs:                     unsafe { prefs }
		function_ids:              function_ids
		prune_unreachable:         prune_unreachable
		declared_types:            declared_types
		declared_type_c_names:     declared_type_c_names
		declared_type_key_by_name: declared_type_key_by_name
		fastc_prefixed_c_names:    fastc_prefixed_c_names
		has_c_functions:           has_c_functions
		numeric_ops_are_builtin:   prefs.building_v && fastc_numeric_arithmetic_has_no_overloads(functions, declared_type_c_names, type_output.alias_base_types, struct_fields)
		primitive_cmps_builtin:    prefs.building_v && fastc_primitive_comparisons_have_no_overloads(functions, declared_type_c_names, type_output.alias_base_types, struct_fields)
		declared_kinds:            declared_kinds
		enum_flags:                enum_flags
		enum_field_types:          enum_field_types
		enum_field_names:          type_output.enum_field_names
		alias_base_types:          type_output.alias_base_types
		fn_alias_return_types:     type_output.fn_alias_return_types
		sum_types:                 type_output.sum_types
		sum_type_variants:         type_output.sum_type_variants
		struct_fields:             struct_fields
		struct_field_info:         struct_field_info
		struct_field_lookup:       struct_field_lookup
		generic_method_sources:    generic_method_sources
		generic_method_names:      fastc_generic_method_names(generic_method_sources)
		module_aliases:            module_aliases
		interface_fields:          interface_fields
		constants:                 constants
		constant_values:           constant_output.compile_time_values
		public_constants:          public_constants
		globals:                   globals
		global_suffix_index:       fastc_global_suffix_index(globals, global_types)
		public_globals:            public_globals
		used_function_names:       used_function_names
		has_startup_inits:         startup_initializers.len > 0
		has_cleanup_hooks:         module_cleanup_calls.len > 0
		functions:                 functions
		function_c_names:          function_c_names
		constant_types:            constant_types
		global_types:              global_types
		fixed_array_types:         fixed_array_types
		composite_types:           composite_types
	}
	mut spawn_typedefs := map[string]string{}
	mut spawn_helpers := map[string]string{}
	mut mono_definitions := map[string]string{}
	mut c_flags := []string{}
	generation_sources := fastc_wait_generation_fragments(mut pending_fragments)
	timer.mark('wait_fragments')
	generation := fastc_generate_file_outputs(&ctx, generation_sources)
	mut outputs := generation.outputs.clone()
	fastc_deduplicate_fragment_prototypes(mut outputs, generation_sources)
	timer.mark('file_outputs')
	for output in outputs {
		if output.failed {
			return error(output.error_message)
		}
		output_proto_offsets << proto_len
		if output.prototypes.len > 0 {
			prototype_pieces << fastc_piece(output.prototypes)
			proto_len += output.prototypes.len
		}
		body_offset := body_len
		output_body_offsets << body_offset
		body_pieces << fastc_piece(output.body)
		body_len += output.body.len
		for line in output.directive_lines {
			kind := line.kind
			body_directive_lines << FastcCDirectiveLine{
				start: body_offset + line.start
				end:   body_offset + line.end
				kind:  kind
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
	for name, array_type_id in generation.fixed_array_types.ids {
		fixed_array_types.set_id(name, array_type_id)
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
		body_pieces << fastc_piece(mono_definition)
		body_len += mono_definition.len
	}
	if prefs.is_test {
		// The test main synthesized below is this program's entry.
		entry_has_main = true
	}
	synthesized_main := if prefs.is_test {
		fastc_synthesized_test_main(functions, function_c_names, prefs, startup_initializers.len > 0,
			module_cleanup_calls.len > 0)!
	} else if has_entry_module && !entry_has_main {
		fastc_synthesized_main(prefs, startup_initializers.len > 0, module_cleanup_calls.len > 0)
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
	mut interface_dispatches := fastc_wait_interface_dispatches(mut pending_interface_dispatches)
	timer.mark('wait_interface_dispatches')
	// Fixed arrays that occur only in declarations or signatures are not seen by
	// the expression renderer, so register their raw markers before emitting typedefs.
	// Function signatures are present in the prototypes; fixed arrays reached while
	// rendering function bodies are registered directly on the per-file parser.
	fastc_collect_referenced_fixed_array_types(type_output.declarations_head, mut fixed_array_types)
	fastc_collect_referenced_fixed_array_types(type_declarations, mut fixed_array_types)
	for prototype_piece in prototype_pieces {
		fastc_collect_referenced_fixed_array_types(prototype_piece, mut fixed_array_types)
	}
	for body_piece in body_pieces {
		fastc_collect_referenced_fixed_array_types(body_piece, mut fixed_array_types)
	}
	for array_name in fixed_array_types.keys() {
		array_type := fixed_array_types.value_id(array_name)
		fastc_register_fixed_array_type(array_type, mut fixed_array_types)
	}
	mut scalar_fixed_array_types := flat.TypeTextBindings{}
	mut aggregate_fixed_array_types := flat.TypeTextBindings{}
	mut callback_fixed_array_types := flat.TypeTextBindings{}
	for name, typ_id in fixed_array_types.ids {
		element_type := fastc_fixed_array_element_type(typ_id) or { continue }
		if flat.source_type_name(element_type).contains('__vf_fn_type_') {
			callback_fixed_array_types.set_id(name, typ_id)
		} else if fastc_primitive_c_type(flat.source_type_name(element_type)) != none
			&& flat.source_type_name(element_type) !in ['string', 'array', 'map', 'Option', '__v_result'] {
			scalar_fixed_array_types.set_id(name, typ_id)
		} else {
			aggregate_fixed_array_types.set_id(name, typ_id)
		}
	}
	// Scalar wrappers must precede aliases such as `type Gf = [16]i64`.
	// Wrappers containing V aggregates wait for those aggregate definitions.
	scalar_fixed_array_declarations := fastc_generate_fixed_array_declarations(scalar_fixed_array_types)
	fixed_array_declarations := fastc_generate_fixed_array_declarations(aggregate_fixed_array_types)
	callback_fixed_array_declarations := fastc_generate_fixed_array_declarations(callback_fixed_array_types)
	preamble := if prefs.target_libc_headers {
		fastc_target_libc_preamble() + c_target_option_runtime
	} else if header_free {
		c_selfhost_preamble.replace(c_selfhost_preamble_includes, fastc_c_abi_prelude(prefs.target.os, prefs.target.arch, ''))
	} else if prefs.building_v {
		c_selfhost_preamble
	} else {
		c_preamble + c_collection_runtime
	}
	hoisted_body := fastc_partition_c_directive_ranges(body_len, body_directive_lines)
	timer.mark('partition_directives')
	mut kept_body_ranges := hoisted_body.body_ranges.clone()
	mut kept_conditional_ranges := hoisted_body.conditional_ranges.clone()
	mut kept_proto_ranges := [0, proto_len]
	mut solo_prototype_ids := []int{}
	mut enum_helpers_len := 0
	for helper_text in type_output.enum_helper_texts {
		enum_helpers_len += helper_text.len
	}
	mut kept_helper_ranges := [0, enum_helpers_len]
	// A program without `main` (a module generated on its own) has no roots
	// to walk from, so it keeps every function.
	if ctx.prune_unreachable && entry_has_main {
		// Everything outside the function bodies is emitted as is, so the
		// functions it names are the roots, with `main` and the lifecycle hooks.
		mut root_ids := []int{}
		fastc_collect_c_name_ids(preamble, 0, preamble.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(c_integer_comparison_helpers, 0, c_integer_comparison_helpers.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(c_selfhost_post_directives, 0, c_selfhost_post_directives.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(c_spawn_runtime, 0, c_spawn_runtime.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(c_selfhost_runtime, 0, c_selfhost_runtime.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(constant_output.macros, 0, constant_output.macros.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(constant_output.declarations, 0, constant_output.declarations.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(global_output.declarations, 0, global_output.declarations.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(startup_initializers, 0, startup_initializers.len, function_ids, mut root_ids)
		fastc_collect_c_name_ids(synthesized_main, 0, synthesized_main.len, function_ids, mut root_ids)
		for _, text in spawn_typedefs {
			fastc_collect_c_name_ids(text, 0, text.len, function_ids, mut root_ids)
		}
		for _, text in spawn_helpers {
			fastc_collect_c_name_ids(text, 0, text.len, function_ids, mut root_ids)
		}
		for call in module_init_calls {
			if id := function_ids[call] {
				root_ids << id
			}
		}
		for call in module_cleanup_calls {
			if id := function_ids[call] {
				root_ids << id
			}
		}
		if id := function_ids['main'] {
			root_ids << id
		}
		for key, signature in functions {
			if fastc_function_is_native_root(key, signature) {
				c_name := function_c_names[key] or { fastc_c_function_name_for_key(key) }
				if id := function_ids[c_name] {
					root_ids << id
				}
			}
		}
		solo_prototype_ids = root_ids.clone()
		// The enum helpers join the walk as one more output placed after the
		// bodies, so their unreachable pieces are dropped the same way.
		mut helper_output := FastcFileGenOutput{}
		mut helper_len := 0
		for i, helper_name in type_output.enum_helper_names {
			helper_text := type_output.enum_helper_texts[i]
			helper_output.function_ids << function_ids[helper_name] or { -1 }
			helper_output.function_spans << helper_len
			helper_output.function_spans << helper_len + helper_text.len
			helper_output.proto_spans << 0
			helper_output.proto_spans << 0
			helper_output.ref_starts << helper_output.refs.len
			fastc_collect_c_name_ids(helper_text, 0, helper_text.len, function_ids, mut helper_output.refs)
			helper_len += helper_text.len
		}
		helper_output.ref_starts << helper_output.refs.len
		// Interface dispatchers are ordinary callees, not entry roots. An unused
		// dispatcher must not retain every native driver implementing its interface.
		for definition in interface_dispatches.split('\n}\n\n') {
			if definition == '' {
				continue
			}
			text := definition + '\n}\n\n'
			name := definition.all_before('(').all_after_last(' ')
			helper_output.function_ids << function_ids[name] or { -1 }
			helper_output.function_spans << helper_len
			helper_output.function_spans << helper_len + text.len
			helper_output.proto_spans << 0
			helper_output.proto_spans << 0
			fastc_collect_c_name_ids(text, 0, text.len, function_ids, mut helper_output.refs)
			helper_output.ref_starts << helper_output.refs.len
			helper_len += text.len
		}
		mut walk_outputs := outputs.clone()
		walk_outputs << helper_output
		mut walk_body_offsets := output_body_offsets.clone()
		walk_body_offsets << body_len
		mut walk_proto_offsets := output_proto_offsets.clone()
		walk_proto_offsets << proto_len
		dead_body_ranges, dead_proto_ranges := fastc_unreachable_function_ranges(walk_outputs, walk_body_offsets, walk_proto_offsets, function_ids.len, root_ids)
		mut dead_file_ranges := []int{cap: dead_body_ranges.len}
		mut dead_helper_ranges := []int{}
		mut dead_dispatch_ranges := []int{}
		for i := 0; i + 1 < dead_body_ranges.len; i += 2 {
			if dead_body_ranges[i] >= body_len + enum_helpers_len {
				dead_dispatch_ranges << dead_body_ranges[i] - body_len - enum_helpers_len
				dead_dispatch_ranges << dead_body_ranges[i + 1] - body_len - enum_helpers_len
			} else if dead_body_ranges[i] >= body_len {
				dead_helper_ranges << dead_body_ranges[i] - body_len
				dead_helper_ranges << dead_body_ranges[i + 1] - body_len
			} else {
				dead_file_ranges << dead_body_ranges[i]
				dead_file_ranges << dead_body_ranges[i + 1]
			}
		}
		kept_body_ranges = fastc_subtract_ranges(kept_body_ranges, dead_file_ranges)
		kept_conditional_ranges = fastc_subtract_ranges(kept_conditional_ranges, dead_file_ranges)
		kept_proto_ranges = fastc_subtract_ranges(kept_proto_ranges, dead_proto_ranges)
		kept_helper_ranges = fastc_subtract_ranges(kept_helper_ranges, dead_helper_ranges)
		mut dispatch_pieces := []string{}
		fastc_collect_c_piece_ranges(mut dispatch_pieces, [interface_dispatches],
			fastc_subtract_ranges([0, interface_dispatches.len], dead_dispatch_ranges))
		interface_dispatches = dispatch_pieces.join('')
		fastc_collect_c_name_ids(interface_dispatches, 0, interface_dispatches.len, function_ids,
			mut solo_prototype_ids)
		mut helper_offset := 0
		for helper_text in type_output.enum_helper_texts {
			if fastc_range_is_kept(kept_helper_ranges, helper_offset, helper_offset + helper_text.len) {
				fastc_collect_c_name_ids(helper_text, 0, helper_text.len, function_ids,
					mut solo_prototype_ids)
			}
			helper_offset += helper_text.len
		}
		timer.mark('prune')
	}
	mut prototype_texts := []string{}
	mut unit_ref_starts := []int{}
	mut unit_ref_ids := []int{}
	mut tail_prototype_ids := []int{}
	mut unindexed_prototypes := strings.new_builder(256)
	if prune_unreachable {
		prototype_texts = []string{len: function_ids.len}
		unit_ref_starts = []int{cap: outputs.len + 1}
		for oi, output in outputs {
			unit_ref_starts << unit_ref_ids.len
			tail_prototype_ids << output.root_refs
			mut proto_cursor := 0
			for fi, id in output.function_ids {
				proto_start := output.proto_spans[2 * fi]
				proto_end := output.proto_spans[2 * fi + 1]
				if proto_cursor < proto_start {
					// On-demand monomorphizations append declarations outside the
					// ordinary function spans, so they remain shared by every unit.
					unindexed_prototypes.write_string(output.prototypes[proto_cursor..proto_start])
				}
				if proto_end > proto_cursor {
					proto_cursor = proto_end
				}
				kept_prototype := proto_start < proto_end
					&& fastc_range_is_kept(kept_proto_ranges, output_proto_offsets[oi] + proto_start, output_proto_offsets[oi] + proto_end)
				if proto_start >= proto_end || kept_prototype {
					// A removed function cannot require declarations in this unit.
					// Keep definitions without an indexed prototype conservative;
					// retained spans also contain their own definition's reference.
					unit_ref_ids << output.refs[output.ref_starts[fi]..output.ref_starts[fi + 1]]
				}
				if !kept_prototype {
					continue
				}
				prototype := output.prototypes[proto_start..proto_end]
				if id >= 0 {
					prototype_texts[id] = prototype
				} else {
					unindexed_prototypes.write_string(prototype)
				}
			}
			if proto_cursor < output.prototypes.len {
				unindexed_prototypes.write_string(output.prototypes[proto_cursor..])
			}
		}
		unit_ref_starts << unit_ref_ids.len
	}
	mut pieces := []string{cap: 64 + body_pieces.len * 3}
	mut extern_indexes := []int{}
	mut extern_texts := []string{}
	mut define_texts := []string{}
	fastc_append_split_head_piece(mut pieces, mut extern_indexes, mut extern_texts, mut define_texts, preamble)
	fastc_append_split_head_piece(mut pieces, mut extern_indexes, mut extern_texts, mut define_texts, c_integer_comparison_helpers)
	fastc_collect_c_piece_ranges(mut pieces, body_pieces, hoisted_body.directive_ranges)
	timer.mark('assemble.directives')
	if hoisted_body.final_kind == 1 {
		pieces << '\n'
	}
	if hoisted_body.directive_ranges.len > 0 {
		pieces << '\n'
	}
	if prefs.building_v && !prefs.target_libc_headers {
		fastc_append_split_head_piece(mut pieces, mut extern_indexes, mut extern_texts, mut define_texts, c_selfhost_post_directives)
	}
	if spawn_typedefs.len > 0 {
		if !header_free {
			pieces << fastc_piece(c_spawn_runtime)
			pieces << '\n'
		}
		// A `thread` handle can appear as a struct field, so its typedef must precede the
		// aggregate type declarations that embed it (and the `Array_`/typeid composites
		// derived from it), not follow them.
		mut thread_type_names := spawn_typedefs.keys()
		thread_type_names.sort()
		for thread_type_name in thread_type_names {
			if thread_type_name.starts_with('__vf_fn_type_') { continue }
			pieces << spawn_typedefs[thread_type_name]
			pieces << '\n'
		}
		pieces << '\n'
	}
	pieces << fastc_piece(constant_output.macros)
	pieces << fastc_piece(type_output.declarations_head)
	pieces << fastc_piece(composite_typedefs)
	pieces << scalar_fixed_array_declarations
	pieces << fastc_piece(type_declarations)
	pieces << late_composite_declarations.str()
	pieces << fastc_piece(fixed_array_declarations)
	// Function signatures can refer to aliases, composite types, and fixed arrays.
	mut function_type_names := spawn_typedefs.keys().filter(it.starts_with('__vf_fn_type_'))
	function_type_names.sort()
	for function_type_name in function_type_names {
		pieces << spawn_typedefs[function_type_name]
	}
	pieces << fastc_piece(callback_fixed_array_declarations)
	native_context := if prefs.target_libc_headers || c_flags.any(it.ends_with('.c') || it.ends_with('.o')) {
		fastc_native_source_context(sources, prefs)
	} else {
		FastcNativeSourceContext{}
	}
	pieces << fastc_c_extern_prototypes_for_source_inputs(functions, c_flags, native_context)
	if prefs.target_libc_headers {
		pieces << fastc_target_c_extern_prototypes_for_context(functions, used_function_names, prefs, native_context)
	}
	extern_indexes << pieces.len
	extern_texts << fastc_extern_declarations(constant_output.declarations, true)
	define_texts << fastc_extern_declarations(constant_output.declarations, false)
	pieces << fastc_piece(constant_output.declarations)
	extern_indexes << pieces.len
	extern_texts << fastc_extern_declarations(global_output.declarations, true)
	define_texts << fastc_extern_declarations(global_output.declarations, false)
	pieces << fastc_piece(global_output.declarations)
	// Helper implementations are emitted once in the first translation unit.
	// Keep only their declarations in the shared head parsed by every TinyCC job.
	runtime_definitions := if prefs.building_v {
		c_selfhost_runtime.replace('static ', '')
	} else {
		''
	}
	pieces << fastc_definition_prototypes(runtime_definitions)
	target_startup := fastc_target_startup_definition(prefs, startup_initializers.len > 0)
	pieces << fastc_definition_prototypes(target_startup)
	for helper_text in type_output.enum_helper_texts {
		pieces << fastc_definition_prototypes(helper_text)
	}
	// Generated formatting helpers can be registered while parsing a function
	// that reachability later removes, even though another live helper calls
	// them. Keep their declarations outside the per-function prototype ranges.
	mut spawn_helper_names := spawn_helpers.keys()
	spawn_helper_names.sort()
	mut spawn_helper_prototypes := strings.new_builder(256)
	for spawn_helper_name in spawn_helper_names {
		helper_definitions := spawn_helpers[spawn_helper_name].replace('static ', '')
		if helper_definitions.starts_with('typedef ') && !helper_definitions.contains(') {') {
			spawn_helper_prototypes.write_string(helper_definitions)
			spawn_helpers[spawn_helper_name] = ''
			continue
		}
		spawn_helpers[spawn_helper_name] = helper_definitions
		spawn_helper_prototypes.write_string(fastc_definition_prototypes(helper_definitions))
	}
	pieces << fastc_take_string(mut spawn_helper_prototypes)
	pieces << fastc_take_string(mut unindexed_prototypes)
	prototype_start := pieces.len
	fastc_collect_c_piece_ranges(mut pieces, prototype_pieces, kept_proto_ranges)
	prototype_end := pieces.len
	if startup_initializers.len > 0 {
		pieces << 'void v_fastc_init_globals(void);'
		pieces << '\n'
	}
	if module_cleanup_calls.len > 0 {
		pieces << 'void v_fastc_cleanup_modules(void);'
		pieces << '\n'
	}
	pieces << '\n'
	// The interface dispatch functions are definitions, so they belong to
	// one unit; every unit sees their prototypes.
	pieces << fastc_definition_prototypes(interface_dispatches)
	head_end := pieces.len
	pieces << fastc_piece(runtime_definitions)
	fastc_collect_c_piece_ranges(mut pieces, type_output.enum_helper_texts, kept_helper_ranges)
	if spawn_helpers.len > 0 {
		for spawn_helper_name in spawn_helper_names {
			pieces << spawn_helpers[spawn_helper_name]
			pieces << '\n'
			pieces << '\n'
		}
	}
	pieces << fastc_piece(interface_dispatches)
	if startup_initializers.len > 0 {
		pieces << 'void v_fastc_init_globals(void) {'
		pieces << '\n'
		pieces << fastc_piece(startup_initializers)
		pieces << '}'
		pieces << '\n'
		pieces << '\n'
	}
	if module_cleanup_calls.len > 0 {
		pieces << 'void v_fastc_cleanup_modules(void) {'
		pieces << '\n'
		for cleanup_call in module_cleanup_calls {
			pieces << '\t${cleanup_call}();'
			pieces << '\n'
		}
		pieces << '}'
		pieces << '\n'
		pieces << '\n'
	}
	pieces << fastc_piece(synthesized_main)
	pieces << fastc_piece(target_startup)
	solo_end := pieces.len
	timer.mark('assemble.head')
	fastc_collect_c_piece_ranges(mut pieces, body_pieces, kept_conditional_ranges)
	timer.mark('assemble.conditional')
	if hoisted_body.final_kind == 2 {
		pieces << '\n'
	}
	if hoisted_body.conditional_ranges.len > 0 {
		pieces << '\n'
	}
	// The bodies are appended file by file, so each file's pieces form a
	// unit; the generic instances (after the last file) join the last unit.
	// Conditional blocks would be body text shared by every unit, so a
	// program with any keeps a single unit.
	mut unit_starts := []int{cap: outputs.len + 2}
	if kept_conditional_ranges.len == 0 && outputs.len > 0 {
		for i, _ in outputs {
			window_start := output_body_offsets[i]
			window_end := if i + 1 < outputs.len { output_body_offsets[i + 1] } else { body_len }
			unit_starts << pieces.len
			fastc_collect_c_piece_ranges(mut pieces, body_pieces, fastc_window_ranges(kept_body_ranges, window_start, window_end))
		}
	} else {
		fastc_collect_c_piece_ranges(mut pieces, body_pieces, kept_body_ranges)
	}
	if hoisted_body.final_kind == 0 {
		pieces << '\n'
	}
	unit_starts << pieces.len
	units.head_end = head_end
	units.solo_end = solo_end
	units.unit_starts = unit_starts
	units.extern_indexes = extern_indexes
	units.extern_texts = extern_texts
	units.define_texts = define_texts
	if prune_unreachable {
		units.prototype_start = prototype_start
		units.prototype_end = prototype_end
		units.prototype_texts = prototype_texts
		units.unit_ref_starts = unit_ref_starts
		units.unit_ref_ids = unit_ref_ids
		units.solo_prototype_ids = solo_prototype_ids
		units.tail_prototype_ids = tail_prototype_ids
	}
	timer.mark('assemble')
	if header_free {
		// A C function without a prototype in the ABI table must fail the
		// build rather than compile against an implicit `int` declaration.
		c_flags << '-Werror=implicit-function-declaration'
	}
	if prune_unreachable && entry_has_main {
		// A module imported only for V types must not compile its unused native
		// driver. This also keeps Linux-only CAN sources out of host-side tools.
		mut native_module_ids := map[string]int{}
		mut native_symbol_ids := map[string]int{}
		for key, signature in functions {
			if !key.starts_with('C.') {
				continue
			}
			if signature.module_name !in native_module_ids {
				native_module_ids[signature.module_name] = native_module_ids.len
			}
			native_symbol_ids[key['C.'.len..]] = native_module_ids[signature.module_name]
		}
		mut live_modules := []bool{len: native_module_ids.len}
		for source in generation_sources {
			for imported_module in source.header.blank_imports {
				if module_id := native_module_ids[imported_module] {
					live_modules[module_id] = true
				}
			}
		}
		for i, output in outputs {
			module_name := generation_sources[i].header.module_name
			module_id := native_module_ids[module_name] or { continue }
			// Live V functions may use native globals or header macros instead of
			// a declared C function. Keep their module's native inputs as well.
			for f := 0; f + 1 < output.function_spans.len; f += 2 {
				if fastc_range_is_kept(kept_body_ranges,
					output_body_offsets[i] + output.function_spans[f],
					output_body_offsets[i] + output.function_spans[f + 1]) {
					live_modules[module_id] = true
					break
				}
			}
			if module_name in global_output.module_initializers
				|| module_name in constant_output.module_initializers {
				live_modules[module_id] = true
			}
		}
		// Only unresolved owners of C source inputs need a symbol-use scan.
		// V bodies, blank imports and initializers already retain the other
		// modules, and binary inputs are retained independently below.
		mut pending_native_modules := []bool{len: native_module_ids.len}
		mut has_pending_native_modules := false
		for i, output in outputs {
			module_name := generation_sources[i].header.module_name
			module_id := native_module_ids[module_name] or { continue }
			if live_modules[module_id] {
				continue
			}
			for flag in output.c_flags {
				if fastc_flag_is_native_input(flag) && flag.ends_with('.c') {
					pending_native_modules[module_id] = true
					has_pending_native_modules = true
					break
				}
			}
		}
		if has_pending_native_modules {
			mut pending_native_symbol_ids := map[string]int{}
			for name, id in native_symbol_ids {
				if pending_native_modules[id] {
					pending_native_symbol_ids[name] = id
				}
			}
			mut native_refs := []int{}
			mut live_texts := [startup_initializers, constant_output.macros,
				constant_output.declarations, global_output.declarations, interface_dispatches]
			fastc_collect_c_piece_ranges(mut live_texts, body_pieces, kept_body_ranges)
			for _, helper in spawn_helpers {
				live_texts << helper
			}
			for text in live_texts {
				fastc_collect_native_name_ids(text, pending_native_symbol_ids, mut native_refs)
			}
			for id in native_refs {
				live_modules[id] = true
			}
		}
		mut required_native_inputs := map[string]bool{}
		for i, output in outputs {
			module_name := generation_sources[i].header.module_name
			module_id := native_module_ids[module_name] or { -1 }
			for flag in output.c_flags {
				if fastc_flag_is_native_input(flag) {
					// Binary artifacts can carry constructors or registration data
					// independently of the module's declared C functions.
					if flag.ends_with('.c') && module_id >= 0 && !live_modules[module_id] {
						continue
					}
					required_native_inputs[flag] = true
				}
			}
		}
		c_flags = c_flags.filter(!fastc_flag_is_native_input(it) || it in required_native_inputs)
	}
	return pieces, spawn_typedefs.len > 0 || global_output.uses_pthread_tls, c_flags
}

@[direct_array_access]
fn fastc_collect_native_name_ids(text string, ids map[string]int, mut refs []int) {
	mut i := 0
	for i < text.len {
		if text[i] in [`'`, `"`] {
			quote := text[i]
			i++
			for i < text.len {
				if text[i] == `\\` {
					i += 2
				} else if text[i] == quote {
					i++
					break
				} else {
					i++
				}
			}
		} else if fastc_identifier_byte(text[i]) {
			start := i
			for i < text.len && fastc_identifier_byte(text[i]) {
				i++
			}
			// The lookup does not retain the key, so borrow the live C text.
			name := unsafe { tos(text.str + start, i - start) }
			if id := ids[name] {
				refs << id
			}
		} else {
			i++
		}
	}
}

// fastc_definition_prototypes returns a prototype for every function
// definition of `text` (a definition starts a line and contains `) {`).
fn fastc_definition_prototypes(text string) string {
	if text.len == 0 {
		return ''
	}
	mut out := strings.new_builder(text.len / 8 + 64)
	for line in text.split_into_lines() {
		if line.len > 4 && !line[0].is_space() && !line.starts_with('static ')
			&& !line.starts_with('#') {
			if body_start := line.index(') {') {
				out.writeln(line[..body_start + 1] + ';')
			}
		}
	}
	return out.str()
}

// fastc_c_extern_prototypes emits declarations explicitly requested with `@[c_extern]`.
// Such symbols come from linked system libraries rather than an included header, so C99 must
// see their V-declared ABI before a call (matching the main C backend's behavior).
fn fastc_c_extern_prototypes(functions map[string]FastcFunctionSignature) string {
	return fastc_c_extern_prototypes_for_inputs(functions, [])
}

fn fastc_c_declaration_has_native_input(path string, inputs []string) bool {
	if !path.ends_with('.c.v') {
		return false
	}
	stem := path[..path.len - 4]
	mut platform_stem := stem
	for suffix in ['_nix', '_windows', '_linux', '_macos', '_darwin', '_default', '_freebsd', '_netbsd',
		'_openbsd'] {
		if stem.ends_with(suffix) {
			platform_stem = stem[..stem.len - suffix.len]
			break
		}
	}
	for input in inputs {
		if (input.ends_with('.c') || input.ends_with('.o'))
			&& input[..input.len - 2] in [stem, platform_stem] {
			return true
		}
	}
	return false
}

fn fastc_c_extern_prototypes_for_inputs(functions map[string]FastcFunctionSignature, inputs []string) string {
	return fastc_c_extern_prototypes_for_source_inputs(functions, inputs, FastcNativeSourceContext{})
}

fn fastc_c_extern_prototypes_for_source_inputs(functions map[string]FastcFunctionSignature, inputs []string, context FastcNativeSourceContext) string {
	mut keys := functions.keys().filter(it.starts_with('C.'))
	keys.sort()
	mut out := strings.new_builder(128)
	for key in keys {
		signature := functions[key]
		if signature.is_disabled {
			continue
		}
		name := key.all_after_first('C.')
		paths := context.declaration_paths[name] or { [signature.path] }
		if !signature.is_c_extern && !paths.any(!context.files_with_headers[it]
			&& fastc_c_declaration_has_native_input(it, inputs)) {
			continue
		}
		mut parameters := []string{cap: signature.parameter_types.ids.len + 1}
		for i, parameter_type in signature.parameter_types_text() {
			parameters << if i < signature.parameter_constness.len
				&& signature.parameter_constness[i] {
				'const ${parameter_type}'
			} else {
				parameter_type
			}
		}
		if signature.is_variadic {
			parameters << '...'
		} else if parameters.len == 0 {
			parameters << 'void'
		}
		// The selected pthread helper header owns these native callback signatures.
		// Keep explicit declarations available when that header is absent.
		if name in ['v3_pthread_zero', 'v3_pthread_create', 'v3_pthread_is_current'] {
			out.writeln('#if !defined(${name}) && !defined(V3_PTHREAD_HELPER_H)')
		} else {
			out.writeln('#ifndef ${name}')
		}
		out.writeln('extern ${signature.return_type_text()} ${name}(${parameters.join(', ')});')
		out.writeln('#endif')
	}
	return out.str()
}

// fastc_window_ranges returns the parts of the ascending [start, end)
// `ranges` inside [window_start, window_end).
fn fastc_window_ranges(ranges []int, window_start int, window_end int) []int {
	mut out := []int{}
	for i := 0; i + 1 < ranges.len; i += 2 {
		start := ranges[i]
		end := ranges[i + 1]
		if end <= window_start {
			continue
		}
		if start >= window_end {
			break
		}
		out << if start < window_start { window_start } else { start }
		out << if end > window_end { window_end } else { end }
	}
	return out
}

fn fastc_range_is_kept(ranges []int, start int, end int) bool {
	for i := 0; i + 1 < ranges.len; i += 2 {
		if ranges[i] <= start && end <= ranges[i + 1] {
			return true
		}
		if ranges[i] > start {
			break
		}
	}
	return false
}

fn fastc_append_split_head_piece(mut pieces []string, mut extern_indexes []int, mut extern_texts []string, mut define_texts []string, text string) {
	// TinyCC recognizes its private atomic helpers only while compiling these
	// static-inline wrappers; keep that header local to every translation unit.
	if text.contains('__atomic_exchange_4') {
		pieces << fastc_piece(text)
		return
	}
	definitions := fastc_extern_declarations(text, false)
	declarations := fastc_extern_declarations(text, true)
	if definitions != text || declarations != text {
		extern_indexes << pieces.len
		extern_texts << declarations
		define_texts << definitions
	}
	pieces << fastc_piece(text)
}

fn fastc_external_function_header(line string, as_extern bool) string {
	body_start := line.index(') {') or { return line }
	mut header := line['static '.len..body_start + 1]
	if header.starts_with('inline ') {
		header = header['inline '.len..]
	}
	return if as_extern { 'extern ${header};' } else { header + line[body_start + 1..] }
}

// fastc_extern_declarations rewrites top-level `static` definitions for a
// split build: as `extern` declarations (`as_extern`) for units that share
// them, or as external definitions in the first unit. TinyCC does not inline,
// so emitting helper bodies once avoids parsing and generating them per unit.
fn fastc_extern_declarations(text string, as_extern bool) string {
	if !fastc_contains(text, 'static ') {
		return text
	}
	mut out := strings.new_builder(text.len)
	lines := text.split_into_lines()
	mut i := 0
	for i < lines.len {
		line := lines[i]
		if line.starts_with('static ') && line.contains(') {') {
			out.writeln(fastc_external_function_header(line, as_extern))
			mut depth := line.count('{') - line.count('}')
			i++
			for i < lines.len && depth > 0 {
				if !as_extern {
					out.writeln(lines[i])
				}
				depth += lines[i].count('{') - lines[i].count('}')
				i++
			}
			continue
		}
		if line.starts_with('static ') && !fastc_contains(line, '(') && line.ends_with(';') {
			mut declaration := line['static '.len..]
			if as_extern {
				if assign := declaration.index(' = ') {
					declaration = declaration[..assign] + ';'
				}
				out.writeln('extern ' + declaration)
			} else {
				out.writeln(declaration)
			}
			i++
			continue
		}
		out.writeln(line)
		i++
	}
	return out.str()
}

// fastc_parallel_worker_limit is the number of worker threads or compiler
// processes a parallel phase may run at once: the CPU count, overridden by
// VJOBS, and 1 when parallelism is disabled.
fn fastc_parallel_worker_limit(prefs &pref.Preferences) int {
	if prefs.no_parallel {
		return 1
	}
	mut jobs := fastc_nr_cpus()
	vjobs := os.getenv('VJOBS').int()
	if vjobs > 0 {
		jobs = vjobs
	}
	if os.getenv('V3_FASTC_NO_PARALLEL') != '' {
		jobs = 1
	}
	return jobs
}

// fastc_tcc_job_count is the number of TinyCC processes a driver compiles a
// program's translation units with.
pub fn fastc_tcc_job_count(prefs &pref.Preferences) int {
	mut jobs := fastc_parallel_worker_limit(prefs)
	if jobs > 12 {
		jobs = 12
	}
	if jobs < 1 {
		jobs = 1
	}
	return jobs
}

// fastc_unit_group_starts partitions consecutive body units while accounting
// for the C text prepended to each translation unit. The first unit also owns
// the runtime/startup/main definitions, so balancing only the bodies makes it
// the slowest TinyCC job. Dynamic programming finds the partition whose
// largest emitted source is smallest; there are at most 16 groups and a few
// hundred source files, so this costs much less than writing one unit.
fn fastc_unit_group_starts(unit_sizes []int, groups int, first_overhead int, shared_overhead int) []int {
	unit_count := unit_sizes.len
	if groups < 1 || unit_count < groups {
		return []int{}
	}
	mut prefix := []int{len: unit_count + 1}
	for i, size in unit_sizes {
		prefix[i + 1] = prefix[i] + size
	}
	mut previous := []int{len: unit_count + 1, init: max_int}
	previous[0] = 0
	mut splits := [][]int{len: groups + 1}
	for count in 1 .. groups + 1 {
		mut current := []int{len: unit_count + 1, init: max_int}
		splits[count] = []int{len: unit_count + 1, init: -1}
		overhead := if count == 1 { first_overhead } else { shared_overhead }
		for end in count .. unit_count + 1 {
			for start in count - 1 .. end {
				if previous[start] == max_int {
					continue
				}
				group_size := overhead + prefix[end] - prefix[start]
				largest := if previous[start] > group_size { previous[start] } else { group_size }
				if largest < current[end] {
					current[end] = largest
					splits[count][end] = start
				}
			}
		}
		previous = current.clone()
	}
	mut starts := []int{len: groups + 1}
	starts[groups] = unit_count
	mut end := unit_count
	for count := groups; count > 0; count-- {
		start := splits[count][end]
		if start < 0 {
			return []int{}
		}
		starts[count - 1] = start
		end = start
	}
	return starts
}

struct FastcCUnitPlan {
	paths       []string
	first_units []int
	sizes       []int
}

fn fastc_c_unit_plan(prefix string, pieces []string, units FastcUnitLayout, jobs int) FastcCUnitPlan {
	unit_count := units.unit_starts.len - 1
	if jobs < 2 || unit_count < 2 || units.head_end <= 0 || units.solo_end > pieces.len {
		return FastcCUnitPlan{}
	}
	mut unit_sizes := []int{cap: unit_count}
	for u in 0 .. unit_count {
		mut size := 0
		for k in units.unit_starts[u] .. units.unit_starts[u + 1] {
			size += pieces[k].len
		}
		unit_sizes << size
	}
	groups := if jobs < unit_count { jobs } else { unit_count }
	mut first_overhead := 1
	mut shared_overhead := 1
	for k in 0 .. units.head_end {
		if units.prototype_start < units.prototype_end && k >= units.prototype_start
			&& k < units.prototype_end {
			continue
		}
		mut first_text := pieces[k]
		mut shared_text := pieces[k]
		for e, index in units.extern_indexes {
			if index == k {
				first_text = units.define_texts[e]
				shared_text = units.extern_texts[e]
				break
			}
		}
		first_overhead += first_text.len
		shared_overhead += shared_text.len
	}
	for k in units.head_end .. units.solo_end {
		first_overhead += pieces[k].len
	}
	first_units := fastc_unit_group_starts(unit_sizes, groups, first_overhead, shared_overhead)
	if first_units.len == 0 {
		return FastcCUnitPlan{}
	}
	mut paths := []string{cap: groups}
	mut sizes := []int{cap: groups}
	for g in 0 .. groups {
		paths << '${prefix}.unit${g}.c'
		mut size := if g == 0 { first_overhead } else { shared_overhead }
		for u in first_units[g] .. first_units[g + 1] {
			size += unit_sizes[u]
		}
		sizes << size
	}
	return FastcCUnitPlan{
		paths:       paths
		first_units: first_units
		sizes:       sizes
	}
}

// fastc_write_c_units writes the translation units of a generation for
// `jobs` parallel TinyCC processes: `prefix.unit<k>.c` files, each with the
// shared head (the globals as extern declarations after the first), the
// first also with the startup, cleanup and main pieces, and consecutive body
// units grouped to balance their sizes. It returns the paths, or none when
// the program does not split.
pub fn fastc_write_c_units(prefix string, pieces []string, units FastcUnitLayout, jobs int) ![]string {
	plan := fastc_c_unit_plan(prefix, pieces, units, jobs)
	if plan.paths.len == 0 {
		return []string{}
	}
	// The files are written concurrently; they add up to several megabytes.
	mut writers := [
		spawn fastc_write_c_unit(plan.paths[0], pieces, &units, 0, plan.first_units[0], plan.first_units[1]),
	]
	for g in 1 .. plan.paths.len {
		writers << spawn fastc_write_c_unit(plan.paths[g], pieces, &units, g, plan.first_units[g], plan.first_units[g + 1])
	}
	mut failure := ''
	for writer in writers {
		message := writer.wait()
		if message != '' && failure == '' {
			failure = message
		}
	}
	if failure != '' {
		return error(failure)
	}
	return plan.paths
}

// fastc_write_c_unit writes one translation unit: the head (with the shared
// globals as definitions in the first unit and as externs elsewhere), the
// pieces every program has once in the first unit, and the bodies of the
// units `first_unit` to `end_unit`. It returns an error message or ''.
fn fastc_write_c_unit(path string, pieces []string, units &FastcUnitLayout, g int, first_unit int, end_unit int) string {
	mut file := os.create(path) or { return 'could not create ${path}: ${err.msg()}' }
	text := fastc_render_c_unit(pieces, units, g, first_unit, end_unit)
	file.write_string(text) or {
		file.close()
		return 'could not write ${path}: ${err.msg()}'
	}
	file.close()
	return ''
}

// FastcRenderedCUnits holds split translation units in memory so an uncached
// build can stream them to TinyCC without temporary C files.
pub struct FastcRenderedCUnits {
pub:
	paths   []string
	sources []string
}

// FastcRenderingCUnits holds translation-unit render workers. Keeping the
// workers live lets an uncached build start TinyCC while the C text is still
// being assembled.
pub struct FastcRenderingCUnits {
pub:
	paths []string
mut:
	workers []thread string
	order   []int
}

// FastcPrestartedCUnits holds TinyCC processes started before FastC generation
// completes. They wait on stdin until the balanced translation units are ready.
pub struct FastcPrestartedCUnits {
pub:
	build_dir  string
	base_args  []string
	unit_count int
mut:
	paths     []string
	objects   []string
	pids      []int
	read_fds  []int
	write_fds []int
	active    bool
}

// FastcFeedingCUnits holds the workers that copy rendered translation units
// into prestarted TinyCC processes.
pub struct FastcFeedingCUnits {
pub:
	paths []string
mut:
	writers []thread
}

// fastc_prestarted_c_units_match reports whether `units` can consume a build
// with the final compile arguments and translation-unit count.
pub fn fastc_prestarted_c_units_match(units &FastcPrestartedCUnits, base_args []string, unit_count int) bool {
	return units.active && units.unit_count == unit_count && units.base_args == base_args
}

fn fastc_render_c_unit(pieces []string, units &FastcUnitLayout, g int, first_unit int, end_unit int) string {
	mut out := strings.new_builder(1024 * 1024)
	for k in 0 .. units.head_end {
		if units.prototype_start < units.prototype_end && k >= units.prototype_start
			&& k < units.prototype_end {
			continue
		}
		mut text := pieces[k]
		for e, index in units.extern_indexes {
			if index == k {
				text = if g > 0 { units.extern_texts[e] } else { units.define_texts[e] }
			}
		}
		out.write_string(text)
	}
	if units.prototype_start < units.prototype_end && units.prototype_texts.len > 0
		&& units.unit_ref_starts.len == units.unit_starts.len {
		mut needed := []bool{len: units.prototype_texts.len}
		if g == 0 {
			for id in units.solo_prototype_ids {
				needed[id] = true
			}
		}
		for u in first_unit .. end_unit {
			for r in units.unit_ref_starts[u] .. units.unit_ref_starts[u + 1] {
				needed[units.unit_ref_ids[r]] = true
			}
		}
		if end_unit == units.unit_starts.len - 1 {
			for id in units.tail_prototype_ids {
				needed[id] = true
			}
		}
		for id, include_prototype in needed {
			if include_prototype {
				out.write_string(units.prototype_texts[id])
			}
		}
	}
	if g == 0 {
		for k in units.head_end .. units.solo_end {
			out.write_string(pieces[k])
		}
	}
	out.write_string('\n')
	for k in units.unit_starts[first_unit] .. units.unit_starts[end_unit] {
		out.write_string(pieces[k])
	}
	return fastc_take_string(mut out)
}

fn fastc_descending_size_order(sizes []int) []int {
	mut order := []int{cap: sizes.len}
	mut ordered_sizes := []int{cap: sizes.len}
	for i, size in sizes {
		order << i
		ordered_sizes << size
		mut at := order.len - 1
		for at > 0 && ordered_sizes[at - 1] < size {
			order[at] = order[at - 1]
			ordered_sizes[at] = ordered_sizes[at - 1]
			at--
		}
		order[at] = i
		ordered_sizes[at] = size
	}
	return order
}

// fastc_begin_render_c_units starts rendering balanced split translation
// units and returns immediately, so their consumers can start concurrently.
pub fn fastc_begin_render_c_units(prefix string, pieces []string, units FastcUnitLayout, jobs int) FastcRenderingCUnits {
	plan := fastc_c_unit_plan(prefix, pieces, units, jobs)
	if plan.paths.len == 0 {
		return FastcRenderingCUnits{}
	}
	mut workers := []thread string{cap: plan.paths.len}
	for g in 0 .. plan.paths.len {
		workers << spawn fastc_render_c_unit(pieces, &units, g, plan.first_units[g], plan.first_units[g + 1])
	}
	return FastcRenderingCUnits{
		paths:   plan.paths
		workers: workers
		order:   fastc_descending_size_order(plan.sizes)
	}
}

// fastc_finish_render_c_units waits for an in-progress unit render.
pub fn fastc_finish_render_c_units(mut rendering FastcRenderingCUnits) FastcRenderedCUnits {
	mut sources := []string{cap: rendering.paths.len}
	for worker in rendering.workers {
		sources << worker.wait()
	}
	return FastcRenderedCUnits{
		paths:   rendering.paths
		sources: sources
	}
}

// fastc_render_c_units renders balanced split translation units in parallel.
pub fn fastc_render_c_units(prefix string, pieces []string, units FastcUnitLayout, jobs int) FastcRenderedCUnits {
	mut rendering := fastc_begin_render_c_units(prefix, pieces, units, jobs)
	return fastc_finish_render_c_units(mut rendering)
}

struct FastcUnitCacheEntry {
	object       string
	cache_object string
	hit          bool
}

// FastcPreparedUnits holds the ordered TinyCC objects and their content cache
// entries. Preparing them restores object-cache hits before compilation starts.
pub struct FastcPreparedUnits {
	entries []FastcUnitCacheEntry
pub:
	objects   []string
	cache_key string
}

// cache_objects maps build-local objects to their persistent cache entries for
// reporting linker failures, including units published during this build.
pub fn (prepared &FastcPreparedUnits) cache_objects() map[string]string {
	mut cached := map[string]string{}
	for entry in prepared.entries {
		if entry.cache_object != '' {
			cached[entry.object] = entry.cache_object
		}
	}
	return cached
}

// fastc_unit_compile_order returns the indexes of uncached C units largest
// first. TinyCC processes start one at a time, so launching longer jobs first
// reduces the time spent waiting for the final unit.
fn fastc_unit_compile_order(unit_paths []string, prepared FastcPreparedUnits) []int {
	mut order := []int{cap: unit_paths.len}
	mut sizes := []u64{cap: unit_paths.len}
	for i, path in unit_paths {
		if prepared.entries[i].hit {
			continue
		}
		size := os.file_size(path)
		order << i
		sizes << size
		mut at := order.len - 1
		for at > 0 && sizes[at - 1] < size {
			order[at] = order[at - 1]
			sizes[at] = sizes[at - 1]
			at--
		}
		order[at] = i
		sizes[at] = size
	}
	return order
}

fn fastc_unit_cache_entry(unit_path string, configuration string, cache_dir string) FastcUnitCacheEntry {
	object := unit_path[..unit_path.len - 2] + '.o'
	source := os.read_file(unit_path) or {
		return FastcUnitCacheEntry{
			object: object
		}
	}
	configuration_hash := fastc_unit_cache_hash(configuration, u64(0x6a09e667f3bcc909))
	source_hash := fastc_unit_cache_hash(source, configuration_hash)
	cache_object := os.join_path_single(cache_dir, '${source_hash.hex()}.o')
	if os.is_file(cache_object) && os.file_size(cache_object) > 0 {
		os.rm(object) or {}
		os.link(cache_object, object) or {
			os.cp(cache_object, object) or {
				return FastcUnitCacheEntry{
					object:       object
					cache_object: cache_object
				}
			}
		}
		return FastcUnitCacheEntry{
			object:       object
			cache_object: cache_object
			hit:          true
		}
	}
	return FastcUnitCacheEntry{
		object:       object
		cache_object: cache_object
	}
}

fn fastc_unit_cache_configuration(tcc string, base_args []string) string {
	build_hash := os.read_file(os.join_path_single(os.dir(tcc), 'build_source_hash.txt')) or { '' }
	return 'fastc-unit-v1\x00${tcc}\x00${os.file_size(tcc)}\x00${os.file_last_mod_unix(tcc)}\x00${build_hash}\x00${base_args.join('\x00')}'
}

fn fastc_unit_cache_entries(tcc string, base_args []string, unit_paths []string, enabled bool) []FastcUnitCacheEntry {
	mut entries := []FastcUnitCacheEntry{len: unit_paths.len}
	for i, unit_path in unit_paths {
		entries[i] = FastcUnitCacheEntry{
			object: unit_path[..unit_path.len - 2] + '.o'
		}
	}
	if !enabled || unit_paths.len == 0 {
		return entries
	}
	cache_dir := os.join_path_single(os.vtmp_dir(), 'v3_fastc_unit_cache')
	os.mkdir_all(cache_dir) or { return entries }
	configuration := fastc_unit_cache_configuration(tcc, base_args)
	mut workers := []thread FastcUnitCacheEntry{cap: unit_paths.len}
	for unit_path in unit_paths {
		workers << spawn fastc_unit_cache_entry(unit_path, configuration, cache_dir)
	}
	for i, worker in workers {
		entries[i] = worker.wait()
	}
	return entries
}

fn fastc_publish_unit_cache(entry FastcUnitCacheEntry) {
	if entry.hit || entry.cache_object == '' || !os.is_file(entry.object)
		|| os.file_size(entry.object) == 0 || os.is_file(entry.cache_object) {
		return
	}
	os.link(entry.object, entry.cache_object) or {
		if os.is_file(entry.cache_object) {
			return
		}
		staged := '${entry.cache_object}.tmp.${os.getpid()}'
		os.cp(entry.object, staged) or { return }
		os.mv(staged, entry.cache_object) or { os.rm(staged) or {} }
	}
}

fn fastc_unit_cache_key(entries []FastcUnitCacheEntry) string {
	mut text := strings.new_builder(entries.len * 24)
	for entry in entries {
		if entry.cache_object == '' {
			return ''
		}
		text.write_string(os.file_name(entry.cache_object))
		text.write_u8(0)
	}
	return fastc_unit_cache_hash(text.str(), u64(0xbb67ae8584caa73b)).hex()
}

// fastc_prepare_c_units calculates the object cache keys and restores hits.
pub fn fastc_prepare_c_units(tcc string, base_args []string, unit_paths []string, cache_enabled bool) FastcPreparedUnits {
	entries := fastc_unit_cache_entries(tcc, base_args, unit_paths, cache_enabled)
	return FastcPreparedUnits{
		entries:   entries
		objects:   entries.map(it.object)
		cache_key: fastc_unit_cache_key(entries)
	}
}

// fastc_link_cache_key identifies a link of content-addressed unit objects.
pub fn fastc_link_cache_key(unit_key string, final_args []string) string {
	if unit_key == '' {
		return ''
	}
	text := 'fastc-link-v1\x00${unit_key}\x00${final_args.join('\x00')}'
	return fastc_unit_cache_hash(text, u64(0x3c6ef372fe94f82b)).hex()
}

// fastc_generation_link_cache_key identifies the final executable directly
// from the generated pieces and every input that affects unit compilation or
// linking. A warm build can therefore restore the executable before writing
// and re-reading the split C files.
pub fn fastc_generation_link_cache_key(tcc string, compile_args []string, final_args []string, pieces []string, units FastcUnitLayout, jobs int, enabled bool) string {
	if !enabled || pieces.len == 0 {
		return ''
	}
	mut stable_final_args := []string{cap: final_args.len}
	for arg in final_args {
		if arg.ends_with('.tbd') && os.is_file(arg) {
			stable_final_args << 'tbd-content:' + (os.read_file(arg) or { arg })
		} else {
			stable_final_args << arg
		}
	}
	configuration := 'fastc-generation-link-v1\x00${fastc_unit_cache_configuration(tcc, compile_args)}\x00${stable_final_args.join('\x00')}\x00${jobs}\x00${units.head_end}\x00${units.solo_end}\x00${units.prototype_start}\x00${units.prototype_end}\x00${units.unit_starts}\x00${units.extern_indexes}\x00${units.unit_ref_starts}\x00${units.unit_ref_ids}\x00${units.solo_prototype_ids}\x00${units.tail_prototype_ids}'
	mut hash := fastc_unit_cache_hash(configuration, u64(0x510e527fade682d1))
	for piece in pieces {
		hash = fastc_unit_cache_hash(piece, hash ^ u64(piece.len))
	}
	for text in units.extern_texts {
		hash = fastc_unit_cache_hash(text, hash ^ u64(text.len))
	}
	for text in units.define_texts {
		hash = fastc_unit_cache_hash(text, hash ^ u64(text.len))
	}
	for text in units.prototype_texts {
		hash = fastc_unit_cache_hash(text, hash ^ u64(text.len))
	}
	return hash.hex()
}

fn fastc_link_cache_path(key string) string {
	return os.join_path(os.vtmp_dir(), 'v3_fastc_link_cache', '${key}.bin')
}

// fastc_restore_link_cache copies a cached linked self-host to output.
pub fn fastc_restore_link_cache(key string, output string) bool {
	if key == '' {
		return false
	}
	cached := fastc_link_cache_path(key)
	if !os.is_file(cached) || os.file_size(cached) == 0 {
		return false
	}
	fastc_copy_cached_link(cached, output) or { return false }
	return os.is_executable(output)
}

// fastc_publish_link_cache atomically caches a linked and signed self-host.
pub fn fastc_publish_link_cache(key string, output string) {
	if key == '' || !os.is_executable(output) {
		return
	}
	cached := fastc_link_cache_path(key)
	if os.is_file(cached) {
		return
	}
	os.mkdir_all(os.dir(cached)) or { return }
	staged := '${cached}.tmp.${os.getpid()}'
	fastc_copy_cached_link(output, staged) or { return }
	os.mv(staged, cached) or { os.rm(staged) or {} }
}

// FastcUnitCompile is one TinyCC process compiling a translation unit.
// fastc_remove_c_units deletes the translation unit sources and their
// objects.
pub fn fastc_remove_c_units(unit_paths []string) {
	for unit_path in unit_paths {
		os.rm(unit_path) or {}
		os.rm(unit_path[..unit_path.len - 2] + '.o') or {}
	}
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

// fastc_build_struct_field_lookup owns the immutable records indexed by name.
// Separate allocations keep pointers stable when the index maps grow.
fn fastc_build_struct_field_lookup(struct_field_info map[string][]FastcStructField) map[string]map[string]&FastcStructField {
	mut struct_field_lookup := map[string]map[string]&FastcStructField{}
	for layout_type, fields in struct_field_info {
		mut fields_by_name := map[string]&FastcStructField{}
		for field in fields {
			fields_by_name[field.name] = &FastcStructField{ ...field }
		}
		struct_field_lookup[layout_type] = fields_by_name.move()
	}
	return struct_field_lookup
}

// fastc_c_main_name returns the C entry point name for the target. Windows
// executables link with `-municode` (v.driver's Windows linker flags), whose CRT
// startup calls `wmain` instead of `main`; the C backend emits the same entry.
fn fastc_c_main_name(prefs &pref.Preferences) string {
	return if prefs.normalized_target_os() == 'windows' { 'wmain' } else { 'main' }
}

// fastc_c_main_parameters returns the entry point parameter list. Selfhost builds
// store argv in g_main_argv, which builtin.arguments decodes as wide strings on
// Windows, so the Windows entry receives `wchar_t **argv`.
fn fastc_c_main_parameters(prefs &pref.Preferences) string {
	if !prefs.building_v {
		return 'void'
	}
	return if prefs.normalized_target_os() == 'windows' {
		'int argc, wchar_t **argv'
	} else {
		'int argc, char **argv'
	}
}

fn fastc_synthesized_main(prefs &pref.Preferences, has_startup_inits bool, has_cleanup_hooks bool) string {
	mut result := strings.new_builder(256)
	selfhost := prefs.building_v
	target_libc_headers := prefs.target_libc_headers
	result.writeln('int ${fastc_c_main_name(prefs)}(${fastc_c_main_parameters(prefs)}) {')
	if !target_libc_headers {
		result.writeln('\tsetvbuf(stdout, NULL, _IONBF, 0);')
	}
	if selfhost {
		result.writeln('\tg_main_argc = argc;')
		result.writeln('\tg_main_argv = argv;')
	}
	if has_startup_inits && !target_libc_headers {
		result.writeln('\tv_fastc_init_globals();')
	}
	if has_cleanup_hooks && !target_libc_headers {
		result.writeln('\tatexit(v_fastc_cleanup_modules);')
	}
	result.writeln('\treturn 0;')
	result.writeln('}')
	result.writeln('')
	return result.str()
}

fn fastc_interface_method_signatures_match(interface_signature FastcFunctionSignature, candidate_signature FastcFunctionSignature) bool {
	if candidate_signature.return_type != interface_signature.return_type || !fastc_type_ids_equal(candidate_signature.return_types.ids.clone(), interface_signature.return_types.ids.clone()) || candidate_signature.option_type != interface_signature.option_type || candidate_signature.parameter_types.ids.len != interface_signature.parameter_types.ids.len {
		return false
	}
	for i in 0 .. interface_signature.parameter_types.ids.len {
		if i > 0 && candidate_signature.parameter_types.ids[i] != interface_signature.parameter_types.ids[i] {
			return false
		}
		interface_parameter_is_mut := i < interface_signature.parameter_mutability.len && interface_signature.parameter_mutability[i]
		candidate_parameter_is_mut := i < candidate_signature.parameter_mutability.len && candidate_signature.parameter_mutability[i]
		// A mutable interface can call an implementation that only reads its receiver.
		if candidate_parameter_is_mut != interface_parameter_is_mut
			&& !(i == 0 && interface_parameter_is_mut) {
			return false
		}
	}
	return true
}

// fastc_promote_embedded_interface_methods copies the methods of each embedded
// interface onto the interface that embeds it, re-keyed to the embedder's receiver
// type. It runs to a fixpoint so a chain of embeds (`C { B }`, `B { A }`) inherits
// all the way down. A method the embedder already declares directly is left as-is.
fn fastc_promote_embedded_interface_methods(embed_embedders []u32, embed_embeddeds []u32, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool) {
	mut changed := true
	for changed {
		changed = false
		for i := 0; i < embed_embedders.len; i++ {
			embedder_key := flat.source_type_name(embed_embedders[i])
			embedded_key := flat.source_type_name(embed_embeddeds[i])
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
				mut promoted_params := source.parameter_types.ids.clone()
				if promoted_params.len > 0 {
					promoted_params[0] = flat.source_type_id(embedder_type)
				}
				functions[promoted_key] = FastcFunctionSignature{
					parameter_types:          flat.TypeTextList{ ids: promoted_params.clone() }
					parameter_mutability:     source.parameter_mutability.clone()
					return_type:              source.return_type
					return_types:             source.return_types.clone()
					option_type:              source.option_type
					is_variadic:              source.is_variadic
					last_parameter_is_params: source.last_parameter_is_params
					is_public:                source.is_public
					is_disabled:              source.is_disabled
					module_name:              source.module_name
					path:                     source.path
				}
				interface_methods[promoted_key] = true
				changed = true
			}
		}
	}
}

fn fastc_generate_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, function_c_names map[string]string, interface_methods map[string]bool, struct_field_info map[string][]FastcStructField, used_function_names map[string]bool, selfhost bool) string {
	mut out := strings.new_builder(1024)
	mut function_keys := functions.keys()
	function_keys.sort()
	mut function_method_names := []string{cap: function_keys.len}
	for function_key in function_keys {
		function_method_names << function_key.all_after_last('.')
	}
	mut interface_method_keys := interface_methods.keys()
	interface_method_keys.sort()
	// Structs may implement an interface through an embedded struct's methods.
	mut struct_keys_by_c_type := map[string]string{}
	mut embedding_structs := []string{}
	for key, kind in declared_kinds {
		if kind != .struct_ {
			continue
		}
		c_type := fastc_c_declared_type_name(key)
		struct_keys_by_c_type[c_type] = key
		fields := struct_field_info[c_type] or { []FastcStructField{} }
		if fields.any(it.name.starts_with('__embedded_')) {
			embedding_structs << key
		}
	}
	embedding_structs.sort()
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
			if interface_signature.parameter_types.ids.len == 0 {
				continue
			}
			interface_type := fastc_c_declared_type_name(interface_key)
			if interface_signature.parameter_types.at(0) != interface_type {
				continue
			}
			method_name := interface_method_key.all_after_last('.')
			mut parameters := ['${interface_type} value']
			mut arguments := []string{}
			for i in 1 .. interface_signature.parameter_types.ids.len {
				parameters << '${fastc_output_c_type(interface_signature.parameter_types_text()[i])} arg${i}'
				arguments << 'arg${i}'
			}
			c_name := if compact_name := function_c_names[interface_method_key] {
				compact_name
			} else {
				fastc_method_c_name(interface_signature.module_name, interface_type, method_name)
			}
			// Spelled like the methods it dispatches to: a V `int` is 64 bits wide.
			return_c_type := fastc_output_c_type(interface_signature.return_type_text())
			out.writeln('${return_c_type} ${c_name}(${parameters.join(', ')}) {')
			out.writeln('\tswitch (value._typ) {')
			candidate_count := if selfhost && method_name !in used_function_names {
				0
			} else {
				function_keys.len
			}
			mut implemented := map[string]bool{}
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
				implemented[receiver_key] = true
				receiver_type := fastc_c_declared_type_name(receiver_key)
				expected_receiver := candidate_signature.parameter_types.at(0)
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
				candidate_c_name := if compact_name := function_c_names[candidate_key] {
					compact_name
				} else {
					fastc_method_c_name(candidate_signature.module_name, receiver_type, method_name)
				}
				call := '${candidate_c_name}(${receiver_argument}${call_arguments})'
				out.writeln('\tcase __v_typeid_${receiver_type}: ${if interface_signature.return_type_text() == 'void' {
					call + '; return;'
				} else {
					'return ' + call + ';'
				}}')
			}
			if candidate_count > 0 {
				for struct_key in embedding_structs {
					if struct_key in implemented {
						continue
					}
					struct_type := fastc_c_declared_type_name(struct_key)
					embedded := fastc_embedded_method(struct_type, method_name, interface_signature, functions, struct_field_info, struct_keys_by_c_type, 0) or {
						continue
					}
					embedded_signature := functions[embedded.key]
					field := '((${struct_type} *)value._object)->${embedded.path}'
					receiver_argument := if embedded_signature.parameter_types.at(0).ends_with('*') {
						'&(${field})'
					} else {
						field
					}
					call_arguments := if arguments.len > 0 {
						',' + arguments.join(',')
					} else {
						''
					}
					embedded_c_name := if compact_name := function_c_names[embedded.key] {
						compact_name
					} else {
						fastc_method_c_name(embedded_signature.module_name, fastc_c_declared_type_name(embedded.key.all_before_last('.')), method_name)
					}
					call := '${embedded_c_name}(${receiver_argument}${call_arguments})'
					out.writeln('\tcase __v_typeid_${struct_type}: ${if interface_signature.return_type_text() == 'void' {
						call + '; return;'
					} else {
						'return ' + call + ';'
					}}')
				}
			}
			out.writeln('\tdefault:')
			if selfhost {
				// No case matched: the value's type was not recognised as implementing
				// the interface. Stop loudly rather than return a zero value.
				out.writeln('\t\tbuiltin__panic(_S("fastc: no implementation of `${interface_key}.${method_name}` for this value"));')
			}
			out.writeln('\t\t${if return_c_type == 'void' {
				'return;'
			} else {
				'return (' + return_c_type + '){0};'
			}}')
			out.writeln('\t}')
			out.writeln('}')
			out.writeln('')
		}
	}
	return out.str()
}

struct FastcEmbeddedMethod {
	key  string
	path string
}

// fastc_embedded_method finds the method an interface call reaches through the embedded
// struct fields of `c_type`: its key and the path of `__embedded_N` fields leading to it.
fn fastc_embedded_method(c_type string, method_name string, interface_signature FastcFunctionSignature, functions map[string]FastcFunctionSignature, struct_field_info map[string][]FastcStructField, struct_keys_by_c_type map[string]string, depth int) ?FastcEmbeddedMethod {
	if depth > 8 {
		return none
	}
	fields := struct_field_info[c_type] or { return none }
	for field in fields {
		if !field.name.starts_with('__embedded_') {
			continue
		}
		field_type := field.typ_text()
		embedded_key := struct_keys_by_c_type[field_type] or { continue }
		method_key := '${embedded_key}.${method_name}'
		if signature := functions[method_key] {
			if fastc_interface_method_signatures_match(interface_signature, signature) {
				return FastcEmbeddedMethod{
					key:  method_key
					path: field.name
				}
			}
		}
		if nested := fastc_embedded_method(field_type, method_name, interface_signature, functions, struct_field_info, struct_keys_by_c_type, depth + 1) {
			return FastcEmbeddedMethod{
				key:  nested.key
				path: '${field.name}.${nested.path}'
			}
		}
	}
	return none
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
		directives:       directives.str()
		conditional_code: conditional_code.str()
		body:             body.str()
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
		source:             source
		directive_ranges:   directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges:        body_ranges
		final_kind:         run_kind
	}
}

@[direct_array_access]
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
				end:   if line_end < source.len { line_end + 1 } else { line_end }
				kind:  kind
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
		if line.kind == 4 {
			// An omitted directive (an `#include` of a header-free build).
			cursor = line.end
			continue
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
		directive_ranges:   directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges:        body_ranges
		final_kind:         final_kind
	}
}

// fastc_function_id_table numbers the C names of the indexed functions and
// methods (the keys of the signature index), as the emitted definitions and
// calls spell them.
fn fastc_function_id_table(functions map[string]FastcFunctionSignature, function_c_names map[string]string, declared_kinds map[string]FastcDeclaredTypeKind) map[string]int {
	mut ids := map[string]int{}
	ids.reserve(u32(functions.len))
	for key, signature in functions {
		if key.starts_with('C.') {
			continue
		}
		c_name := if compact_name := function_c_names[key] {
			compact_name
		} else if !key.contains('.') {
			// A main-module function stays out of the index, so its definition is
			// always kept.
			continue
		} else {
			fastc_signature_c_name(key, signature)
		}
		// Only names the reference scan recognizes (a `__` separator or the
		// `v_fastc_` prefix) are indexed; a bare main-module method name stays
		// out and its definition is always kept.
		if !fastc_c_name_is_indexed(c_name) {
			continue
		}
		if c_name !in ids {
			ids[c_name] = ids.len
		}
	}
	for type_key, kind in declared_kinds {
		if kind != .enum_ {
			continue
		}
		c_name := fastc_c_declared_type_name(type_key)
		for helper in ['v_fastc_enum_str_${c_name}', 'v_fastc_print_enum_${c_name}'] {
			if helper !in ids {
				ids[helper] = ids.len
			}
		}
	}
	// Native adapters share their implementation's emitted span. References to
	// either spelling must retain that span without adding another graph node.
	for key, signature in functions {
		if key.starts_with('C.') || signature.return_type_text() != 'MultiReturn'
			|| signature.return_types.ids.len != 2 {
			continue
		}
		c_name := function_c_names[key] or { fastc_signature_c_name(key, signature) }
		if id := ids[c_name] {
			ids[fastc_native_pair_wrapper_name(c_name)] = id
		}
	}
	return ids
}

// fastc_c_name_is_indexed reports whether a C name carries a module or type
// separator (`__`) or one of the generated helper prefixes, the spellings the
// reference scan looks up.
fn fastc_c_name_is_indexed(c_name string) bool {
	return fastc_contains(c_name, '__') || c_name.starts_with('v_fastc_')
		|| c_name.starts_with('v_f')
}

fn fastc_c_identifier_start_byte(value u8) bool {
	return value == `_` || (value >= `a` && value <= `z`) || (value >= `A` && value <= `Z`)
}

// fastc_collect_c_name_ids appends the ids of the indexed functions named in
// text[start..end]. Every indexed C name carries a module or type separator
// (`__`), `v_fastc_`, or compact `v_f` prefix, so only underscores can begin a candidate.
// Expanding from those markers avoids classifying every ordinary identifier.
@[direct_array_access]
fn fastc_collect_c_name_ids(text string, start int, end int, ids map[string]int, mut out []int) {
	mut i := start
	for i < end {
		i = fastc_next_underscore(text, i, end)
		if i >= end {
			break
		}
		mut word_start := i
		mut indexed := i + 1 < end && text[i + 1] == `_`
		if !indexed && i > start && text[i - 1] == `v` && i + 6 < end && text[i + 1] == `f`
			&& text[i + 2] == `a` && text[i + 3] == `s` && text[i + 4] == `t`
			&& text[i + 5] == `c` && text[i + 6] == `_` {
			word_start = i - 1
			indexed = word_start == start || !fastc_identifier_byte(text[word_start - 1])
		} else if !indexed && i > start && text[i - 1] == `v` && i + 1 < end
			&& text[i + 1] == `f` {
			word_start = i - 1
			indexed = word_start == start || !fastc_identifier_byte(text[word_start - 1])
		}
		if !indexed {
			i++
			continue
		}
		for word_start > start && fastc_identifier_byte(text[word_start - 1]) {
			word_start--
		}
		mut word_end := i + 2
		for word_end < end && fastc_identifier_byte(text[word_end]) {
			word_end++
		}
		candidate := unsafe { tos(text.str + word_start, word_end - word_start) }
		if id := ids[candidate] {
			out << id
		}
		i = word_end
	}
}

// fastc_unreachable_function_ranges walks the references from `root_ids`
// through the file outputs and returns the body and prototype ranges (in
// the outputs' virtual concatenations) of the indexed functions never reached.
fn fastc_unreachable_function_ranges(outputs []FastcFileGenOutput, body_offsets []int, proto_offsets []int, function_count int, root_ids []int) ([]int, []int) {
	mut reachable := []bool{len: function_count}
	mut def_output := []int{len: function_count}
	mut def_index := []int{len: function_count}
	for id in 0 .. function_count {
		def_output[id] = -1
		def_index[id] = -1
	}
	mut work := root_ids.clone()
	for oi, output in outputs {
		for id in output.root_refs {
			work << id
		}
		for fi, id in output.function_ids {
			if id >= 0 {
				def_output[id] = oi
				def_index[id] = fi
			} else {
				// A definition outside the index is always emitted, so what
				// it references is reachable too.
				for k in output.ref_starts[fi] .. output.ref_starts[fi + 1] {
					work << output.refs[k]
				}
			}
		}
	}
	mut cursor := 0
	for cursor < work.len {
		id := work[cursor]
		cursor++
		if reachable[id] {
			continue
		}
		reachable[id] = true
		oi := def_output[id]
		if oi < 0 {
			continue
		}
		fi := def_index[id]
		output := outputs[oi]
		for k in output.ref_starts[fi] .. output.ref_starts[fi + 1] {
			r := output.refs[k]
			if !reachable[r] {
				work << r
			}
		}
	}
	mut dead_body := []int{}
	mut dead_proto := []int{}
	for oi, output in outputs {
		for fi, id in output.function_ids {
			if id < 0 || reachable[id] {
				continue
			}
			dead_body << body_offsets[oi] + output.function_spans[2 * fi]
			dead_body << body_offsets[oi] + output.function_spans[2 * fi + 1]
			dead_proto << proto_offsets[oi] + output.proto_spans[2 * fi]
			dead_proto << proto_offsets[oi] + output.proto_spans[2 * fi + 1]
		}
	}
	return dead_body, dead_proto
}

// fastc_subtract_ranges returns the parts of the ascending [start, end)
// `ranges` not covered by the ascending, disjoint `dead` ranges.
fn fastc_subtract_ranges(ranges []int, dead []int) []int {
	if dead.len == 0 {
		return ranges
	}
	mut kept := []int{cap: ranges.len + dead.len}
	mut d := 0
	for i := 0; i + 1 < ranges.len; i += 2 {
		mut start := ranges[i]
		end := ranges[i + 1]
		for d + 1 < dead.len && dead[d + 1] <= start {
			d += 2
		}
		mut k := d
		for k + 1 < dead.len && dead[k] < end {
			if dead[k + 1] > dead[k] {
				if dead[k] > start {
					kept << start
					kept << dead[k]
				}
				if dead[k + 1] > start {
					start = dead[k + 1]
				}
			}
			k += 2
		}
		if start < end {
			kept << start
			kept << end
		}
	}
	return kept
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
				out << fastc_piece(piece)
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
		source:             source
		directive_ranges:   directive_ranges
		conditional_ranges: conditional_ranges
		body_ranges:        body_ranges
		final_kind:         final_kind
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

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcExpressionToken) typ_text() string {
	return flat.source_type_name(value.typ)
}

// chan_element_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcExpressionToken) chan_element_type_text() string {
	return flat.source_type_name(value.chan_element_type)
}

// option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcExpressionToken) option_value_type_text() string {
	return flat.source_type_name(value.option_value_type)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcRenderedExpression) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcInterfaceField) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcConstantValue) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcMemberSmartcast) typ_text() string {
	return flat.source_type_name(value.typ)
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcFunctionSignature) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// option_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcFunctionSignature) option_type_text() string {
	return flat.source_type_name(value.option_type)
}

// chan_element_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) chan_element_type_text() string {
	return flat.source_type_name(value.chan_element_type)
}

// fn_option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) fn_option_value_type_text() string {
	return flat.source_type_name(value.fn_option_value_type)
}

// fn_return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) fn_return_type_text() string {
	return flat.source_type_name(value.fn_return_type)
}

// option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) option_value_type_text() string {
	return flat.source_type_name(value.option_value_type)
}

// smartcast_origin_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcLocal) smartcast_origin_type_text() string {
	return flat.source_type_name(value.smartcast_origin_type)
}

// chan_element_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) chan_element_type_text() string {
	return flat.source_type_name(value.chan_element_type)
}

// fn_option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) fn_option_value_type_text() string {
	return flat.source_type_name(value.fn_option_value_type)
}

// fn_return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) fn_return_type_text() string {
	return flat.source_type_name(value.fn_return_type)
}

// generic_argument_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) generic_argument_type_text() string {
	return flat.source_type_name(value.generic_argument_type)
}

// option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcStructField) option_value_type_text() string {
	return flat.source_type_name(value.option_value_type)
}

// expected_expression_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) expected_expression_type_text() string {
	return flat.source_type_name(value.expected_expression_type)
}

// last_expression_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) last_expression_type_text() string {
	return flat.source_type_name(value.last_expression_type)
}

// last_option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) last_option_value_type_text() string {
	return flat.source_type_name(value.last_option_value_type)
}

// option_return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) option_return_type_text() string {
	return flat.source_type_name(value.option_return_type)
}

// or_value_expected_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) or_value_expected_type_text() string {
	return flat.source_type_name(value.or_value_expected_type)
}

// pending_fn_option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) pending_fn_option_value_type_text() string {
	return flat.source_type_name(value.pending_fn_option_value_type)
}

// pending_fn_return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) pending_fn_return_type_text() string {
	return flat.source_type_name(value.pending_fn_return_type)
}

// pending_option_value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) pending_option_value_type_text() string {
	return flat.source_type_name(value.pending_option_value_type)
}

// parameter_types_text resolves indexed types at a text integration boundary.
fn (value &FastcFunctionSignature) parameter_types_text() []string {
	return value.parameter_types.names()
}

// return_types_text resolves indexed types at a text integration boundary.
fn (value &FastcFunctionSignature) return_types_text() []string {
	return value.return_types.names()
}

// fn_parameter_types_text resolves indexed types at a text integration boundary.
fn (value &FastcStructField) fn_parameter_types_text() []string {
	return value.fn_parameter_types.names()
}

// last_multi_return_types_text resolves indexed types at a text integration boundary.
fn (value &Parser) last_multi_return_types_text() []string {
	return value.last_multi_return_types.names()
}

// return_types_text resolves indexed types at a text integration boundary.
fn (value &Parser) return_types_text() []string {
	return value.return_types.names()
}

// current_receiver_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) current_receiver_text() string {
	return flat.source_type_name(value.current_receiver)
}

// last_field_receiver_text resolves the indexed source type spelling.
@[inline]
fn (value &Parser) last_field_receiver_text() string {
	return flat.source_type_name(value.last_field_receiver)
}

fn (smartcast &FastcMemberSmartcast) variants_text() []string {
	return smartcast.variants.names()
}

// value_type_text resolves the tested value's indexed type spelling.
@[inline]
fn (smartcast &FastcMemberSmartcast) value_type_text() string {
	return flat.source_type_name(smartcast.value_type)
}

// concrete_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcMonoRequest) concrete_text() string {
	return flat.source_type_name(value.concrete)
}

// name_text resolves the enum display type spelling.
@[inline]
fn (info &FastcEnumInfo) name_text() string {
	return flat.source_type_name(info.name)
}
