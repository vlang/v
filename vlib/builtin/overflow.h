/*

This header file implements the necessary functions for integer arithmetic overflow checks, 
compatible with functions such as __builtin_add_overflow(), __builtin_sub_overflow(), and 
__builtin_mul_overflow() in compilers like GCC and Clang.

*/
#ifndef _BUILTIN_OVERFLOW_H
#define _BUILTIN_OVERFLOW_H

#if defined(__TINYC__) || defined(_MSC_VER)
// TODO: remove this when TCC support `__builtin_add_overflow`, `__builtin_sub_overflow`,`__builtin_mul_overflow`
// TODO: msvc does not support `_Generic` yet
#include <stdbool.h>
#include <limits.h>

static inline bool __builtin_add_overflow_i8(i8 x, i8 y, i8* res) {
    *res = x + y;
    return ((x > 0 && y > 0 && *res < 0) ||
            (x < 0 && y < 0 && *res > 0));
}

static inline bool __builtin_add_overflow_u8(u8 x, u8 y, u8* res) {
    *res = x + y;
    return *res < x;
}

static inline bool __builtin_sub_overflow_i8(i8 x, i8 y, i8* res) {
    *res = x - y;
    return ((x >= 0 && y < 0 && *res < 0) ||
            (x < 0 && y > 0 && *res > 0));
}

static inline bool __builtin_sub_overflow_u8(u8 x, u8 y, u8* res) {
    *res = x - y;
    return x < y;
}

static inline bool __builtin_mul_overflow_i8(i8 x, i8 y, i8* res) {
    *res = x * y;

    if (x == 0 || y == 0) {
        return false;
    }

    if (x == SCHAR_MIN || y == SCHAR_MIN) {
        if ((x == SCHAR_MIN && y == 1) || (y == SCHAR_MIN && x == 1)) {
            return false;
        }
        return true;
    }

    if (x > 0) {
        if (y > 0) {
            return x > SCHAR_MAX / y;
        } else {
            return y < SCHAR_MIN / x;
        }
    } else {
        if (y > 0) {
            return x < SCHAR_MIN / y;
        } else {
            return y < SCHAR_MAX / x;
        }
    }
}

static inline bool __builtin_mul_overflow_u8(u8 x, u8 y, u8* res) {
    *res = x * y;
    return (y != 0) && (x > UCHAR_MAX / y);
}

static inline bool __builtin_add_overflow_i16(i16 x, i16 y, i16* res) {
    *res = x + y;
    return ((x > 0 && y > 0 && *res < 0) ||
            (x < 0 && y < 0 && *res > 0));
}

static inline bool __builtin_add_overflow_u16(u16 x, u16 y, u16* res) {
    *res = x + y;
    return *res < x;
}

static inline bool __builtin_sub_overflow_i16(i16 x, i16 y, i16* res) {
    *res = x - y;
    return ((x >= 0 && y < 0 && *res < 0) ||
            (x < 0 && y > 0 && *res > 0));
}

static inline bool __builtin_sub_overflow_u16(u16 x, u16 y, u16* res) {
    *res = x - y;
    return x < y;
}

static inline bool __builtin_mul_overflow_i16(i16 x, i16 y, i16* res) {
    *res = x * y;

    if (x == 0 || y == 0) {
        return false;
    }

    if (x == SHRT_MIN || y == SHRT_MIN) {
        if ((x == SHRT_MIN && y == 1) || (y == SHRT_MIN && x == 1)) {
            return false;
        }
        return true;
    }

    if (x > 0) {
        if (y > 0) {
            return x > SHRT_MAX / y;
        } else {
            return y < SHRT_MIN / x;
        }
    } else {
        if (y > 0) {
            return x < SHRT_MIN / y;
        } else {
            return y < SHRT_MAX / x;
        }
    }
}

static inline bool __builtin_mul_overflow_u16(u16 x, u16 y, u16* res) {
    *res = x * y;
    return (y != 0) && (x > USHRT_MAX / y);
}

static inline bool __builtin_add_overflow_i32(i32 x, i32 y, i32* res) {
    *res = x + y;
    return ((x > 0 && y > 0 && *res < 0) ||
            (x < 0 && y < 0 && *res > 0));
}

static inline bool __builtin_add_overflow_u32(u32 x, u32 y, u32* res) {
    *res = x + y;
    return *res < x;
}

static inline bool __builtin_sub_overflow_i32(i32 x, i32 y, i32* res) {
    *res = x - y;
    return ((x >= 0 && y < 0 && *res < 0) ||
            (x < 0 && y > 0 && *res > 0));
}

static inline bool __builtin_sub_overflow_u32(u32 x, u32 y, u32* res) {
    *res = x - y;
    return x < y;
}

static inline bool __builtin_mul_overflow_i32(i32 x, i32 y, i32* res) {
    *res = x * y;

    if (x == 0 || y == 0) {
        return false;
    }

    if (x == INT_MIN || y == INT_MIN) {
        if ((x == INT_MIN && y == 1) || (y == INT_MIN && x == 1)) {
            return false;
        }
        return true;
    }

    if (x > 0) {
        if (y > 0) {
            return x > INT_MAX / y;
        } else {
            return y < INT_MIN / x;
        }
    } else {
        if (y > 0) {
            return x < INT_MIN / y;
        } else {
            return y < INT_MAX / x;
        }
    }
}

static inline bool __builtin_mul_overflow_u32(u32 x, u32 y, u32* res) {
    *res = x * y;
    return (y != 0) && (x > UINT_MAX / y);
}

static inline bool __builtin_add_overflow_i64(i64 x, i64 y, i64* res) {
    *res = x + y;
    return ((x > 0 && y > 0 && *res < 0) ||
            (x < 0 && y < 0 && *res > 0));
}

static inline bool __builtin_add_overflow_u64(u64 x, u64 y, u64* res) {
    *res = x + y;
    return *res < x;
}

static inline bool __builtin_sub_overflow_i64(i64 x, i64 y, i64* res) {
    *res = x - y;
    return ((x >= 0 && y < 0 && *res < 0) || 
            (x < 0 && y > 0 && *res > 0));
}

static inline bool __builtin_sub_overflow_u64(u64 x, u64 y, u64* res) {
    *res = x - y;
    return x < y;
}

static inline bool __builtin_mul_overflow_i64(i64 x, i64 y, i64* res) {
    *res = x * y;

    if (x == 0 || y == 0) {
        return false;
    }

    if (x == LLONG_MIN || y == LLONG_MIN) {
        if ((x == LLONG_MIN && y == 1) || (y == LLONG_MIN && x == 1)) {
            return false;
        }
        return true;
    }

    if (x > 0) {
        if (y > 0) {
            return x > LLONG_MAX / y;
        } else {
            return y < LLONG_MIN / x;
        }
    } else {
        if (y > 0) {
            return x < LLONG_MIN / y;
        } else {
            return y < LLONG_MAX / x;
        }
    }
}

static inline bool __builtin_mul_overflow_u64(u64 x, u64 y, u64* res) {
    *res = x * y;
    return (y != 0) && (x > ULLONG_MAX / y);
}

#if defined(CUSTOM_DEFINE_new_int) && (defined(__x86_64__) || defined(_M_AMD64) || defined(__aarch64__) || defined(__arm64__) || defined(_M_ARM64) || (defined(__riscv_xlen) && __riscv_xlen == 64) || defined(__s390x__) || (defined(__powerpc64__) && defined(__LITTLE_ENDIAN__)) || defined(__loongarch64))
	#define __builtin_add_overflow_int __builtin_add_overflow_i64
	#define __builtin_sub_overflow_int __builtin_sub_overflow_i64
	#define __builtin_mul_overflow_int __builtin_mul_overflow_i64
#else
	#define __builtin_add_overflow_int __builtin_add_overflow_i32
	#define __builtin_sub_overflow_int __builtin_sub_overflow_i32
	#define __builtin_mul_overflow_int __builtin_mul_overflow_i32
#endif

#define __builtin_add_overflow_int_literal __builtin_add_overflow_i64
#define __builtin_sub_overflow_int_literal __builtin_sub_overflow_i64
#define __builtin_mul_overflow_int_literal __builtin_mul_overflow_i64
#define __builtin_add_overflow_rune __builtin_add_overflow_u32
#define __builtin_sub_overflow_rune __builtin_sub_overflow_u32
#define __builtin_mul_overflow_rune __builtin_mul_overflow_u32

#if INTPTR_MAX == INT64_MAX
	#define __builtin_add_overflow_isize __builtin_add_overflow_i64
	#define __builtin_sub_overflow_isize __builtin_sub_overflow_i64
	#define __builtin_mul_overflow_isize __builtin_mul_overflow_i64
	#define __builtin_add_overflow_usize __builtin_add_overflow_u64
	#define __builtin_sub_overflow_usize __builtin_sub_overflow_u64
	#define __builtin_mul_overflow_usize __builtin_mul_overflow_u64
#else
	#define __builtin_add_overflow_isize __builtin_add_overflow_i32
	#define __builtin_sub_overflow_isize __builtin_sub_overflow_i32
	#define __builtin_mul_overflow_isize __builtin_mul_overflow_i32
	#define __builtin_add_overflow_usize __builtin_add_overflow_u32
	#define __builtin_sub_overflow_usize __builtin_sub_overflow_u32
	#define __builtin_mul_overflow_usize __builtin_mul_overflow_u32
#endif

#endif // __TINYC__ || _MSC_VER

#endif // _BUILTIN_OVERFLOW_H
