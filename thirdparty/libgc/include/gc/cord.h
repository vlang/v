/*
 * Copyright (c) 1993-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/*
 * Cords are immutable character strings.  A number of operations on long
 * cords are much more efficient than their counterpart in platform
 * `strings.h` file.  In particular, concatenation takes constant time
 * independent of the length of the arguments.  (Cords are represented as
 * trees, with internal nodes representing concatenation and leaves
 * consisting of either C strings or a functional description of the string.)
 *
 * The following are reasonable applications of cords.  They would perform
 * unacceptably if C strings were used:
 *   - A compiler that produces assembly language output by repeatedly
 *     concatenating instructions onto a cord representing the output file;
 *   - A text editor that converts the input file to a cord, and then
 *     performs editing operations by producing a new cord representing the
 *     file after each character change (and keeping the old ones in an edit
 *     history).
 *
 * For optimal performance, cords should be built by concatenating short
 * sections.  This interface is designed for maximum compatibility with
 * C strings.  ASCII NUL characters may be embedded in cords using
 * `CORD_from_fn`.  This is handled correctly, but `CORD_to_char_star` will
 * produce a string with embedded NUL characters when given such a cord.
 *
 * This interface is fairly big, largely for performance reasons.
 * The most basic constants, functions and types:
 *
 *   - `CORD` - the type of a cord;
 *   - `CORD_EMPTY` - empty cord;
 *   - `CORD_len(cord)` - length of a cord;
 *   - `CORD_cat(cord1, cord2)` - concatenation of two cords;
 *   - `CORD_substr(cord, start, len)` - substring (or subcord);
 *   - `CORD_pos i; CORD_FOR(i, cord) { ... CORD_pos_fetch(i) ... }` - examine
 *     each character in a cord (`CORD_pos_fetch(i)` is the `char`);
 *   - `CORD_fetch(i)` - retrieve `i`'th character (slowly);
 *   - `CORD_cmp(cord1, cord2)` - compare two cords;
 *   - `CORD_from_file(FILE *f)` - turn a read-only file into a cord;
 *   - `CORD_to_char_star(cord)` - convert to C string (non-`NULL` C constant
 *     strings are cords);
 *   - `CORD_printf(CORD format, ...)` and friends - cord's variant of
 *     `printf` (use "%r" for cords).
 */

#ifndef CORD_H
#define CORD_H

#include <stddef.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(GC_DLL) && !defined(CORD_NOT_DLL) && !defined(CORD_API)
/* Same as for `GC_API` in `gc_config_macros.h` file. */
#  ifdef CORD_BUILD
#    if defined(__MINGW32__) && !defined(__cplusplus) || defined(__CEGCC__)
#      define CORD_API __declspec(dllexport)
#    elif defined(_MSC_VER) || defined(__DMC__) || defined(__BORLANDC__) \
        || defined(__CYGWIN__) || defined(__MINGW32__)                   \
        || defined(__WATCOMC__)
#      define CORD_API extern __declspec(dllexport)
#    elif defined(__GNUC__) && !defined(GC_NO_VISIBILITY) \
        && (__GNUC__ >= 4 || defined(GC_VISIBILITY_HIDDEN_SET))
/* Only matters if used in conjunction with `-fvisibility=hidden` option. */
#      define CORD_API extern __attribute__((__visibility__("default")))
#    endif
#  else /* !CORD_BUILD */
#    if defined(__BORLANDC__) || defined(__CEGCC__) || defined(__CYGWIN__) \
        || defined(__DMC__) || defined(_MSC_VER)
#      define CORD_API __declspec(dllimport)
#    elif defined(__MINGW32__) || defined(__WATCOMC__)
#      define CORD_API extern __declspec(dllimport)
#    endif
#  endif
#endif /* GC_DLL */

#ifndef CORD_API
#  define CORD_API extern
#endif

/**
 * Cords have type `const char *`.  This is cheating quite a bit, and not
 * 100% portable.  But it means that nonempty character string constants
 * may be used as cords directly, provided the string is never modified in
 * place.  The empty cord is represented by, and can be written as, `NULL`.
 */
typedef const char *CORD;

/** An empty cord is always represented as nil. */
#define CORD_EMPTY 0

/** Is a nonempty cord represented as a C string? */
#define CORD_IS_STRING(s) (*(s) != '\0')

/**
 * Concatenate two cords.  If the arguments are C strings, they may not
 * be subsequently altered.
 */
CORD_API CORD CORD_cat(CORD, CORD);

/**
 * Concatenate a cord and a C string with known length.  Except for the
 * empty string case, this is a special case of `CORD_cat`.  Since the
 * length is known, it can be faster.  The string `y` is shared with
 * the resulting cord.  Hence it should not be altered by the caller.
 */
CORD_API CORD CORD_cat_char_star(CORD /* `x` */, const char * /* `y` */,
                                 size_t /* `y_len` */);

/** Compute the length of a cord. */
CORD_API size_t CORD_len(CORD);

/** Cords may be represented by functions defining the `i`-th character. */
typedef char (*CORD_fn)(size_t /* `i` */, void * /* `client_data` */);

/** Turn a functional description into a cord. */
CORD_API CORD CORD_from_fn(CORD_fn, void * /* `client_data` */,
                           size_t /* `len` */);

/**
 * Return the substring (subcord really) of `x` with length at most `n`,
 * starting at position `i`.  (The initial character has position zero.)
 */
CORD_API CORD CORD_substr(CORD, size_t /* `i` */, size_t /* `n` */);

/**
 * Return the argument, but rebalanced to allow more efficient
 * character retrieval, substring operations, and comparisons.
 * This is useful only for cords that were built using repeated
 * concatenation.  Guarantees log time access to the result, unless
 * the argument was obtained through a large number of repeated
 * substring operations or the embedded functional descriptions take
 * longer to evaluate.  May reallocate significant parts of the cord.
 * The argument is not modified; only the result is balanced.
 */
CORD_API CORD CORD_balance(CORD);

/*
 * The following traverse a cord by applying a function to each
 * character.  This is occasionally appropriate, especially where
 * speed is crucial.  But, since C does not have nested functions,
 * clients of this sort of traversal are clumsy to write.  Consider
 * the functions that operate on cord positions instead.
 */

/** Function to iteratively apply to individual characters in cord. */
typedef int (*CORD_iter_fn)(char, void * /* `client_data` */);

/**
 * Function to apply to substrings of a cord.  Each substring is
 * a C character string, not a general cord.
 */
typedef int (*CORD_batched_iter_fn)(const char *, void * /* `client_data` */);

#define CORD_NO_FN ((CORD_batched_iter_fn)0)

/**
 * Apply `f1` to each character in the cord, in ascending order, starting at
 * position `i`.  If `f2` is not `CORD_NO_FN`, then multiple calls to `f1`
 * may be replaced by a single call to `f2`.  The latter is provided only
 * to allow some optimization by the client.  This terminates when the right
 * end of this string is reached, or when `f1` or `f2` return a nonzero value.
 * In the latter case this function returns a nonzero value; otherwise
 * it returns 0.  The specified value of `i` must be less than `CORD_len(x)`.
 */
CORD_API int CORD_iter5(CORD, size_t /* `i` */, CORD_iter_fn /* `f1` */,
                        CORD_batched_iter_fn /* `f2` */,
                        void * /* `client_data` */);

/** A simpler variant of `CORD_iter5` that starts at 0, and without `f2`. */
CORD_API int CORD_iter(CORD, CORD_iter_fn /* `f1` */,
                       void * /* `client_data` */);
#define CORD_iter(x, f1, cd) CORD_iter5(x, 0, f1, CORD_NO_FN, cd)

/**
 * Similar to `CORD_iter5`, but end-to-beginning.  No provisions for
 * `CORD_batched_iter_fn`.
 */
CORD_API int CORD_riter4(CORD, size_t /* `i` */, CORD_iter_fn /* `f1` */,
                         void * /* `client_data` */);

/** A simpler variant of `CORD_riter4` that starts at the end. */
CORD_API int CORD_riter(CORD, CORD_iter_fn /* `f1` */,
                        void * /* `client_data` */);

#ifdef __cplusplus
} /* extern "C" */
#endif

/*
 * Functions that operate on cord positions.  The easy way to traverse cords.
 * A cord position is logically a pair consisting of a cord and an index
 * into that cord.  But it is much faster to retrieve a character based on
 * a position than on an index.  Unfortunately, positions are big (order of
 * a few 100 bytes), so allocate them with caution.  Things in `cord_pos.h`
 * file should be treated as opaque, except as described below.  Also, note
 * that `CORD_pos_fetch`, `CORD_next` and `CORD_prev` have both macro and
 * function definitions.  The former may evaluate their argument more than
 * once.
 */
#include "cord_pos.h"

/*
 * Visible definitions from above:
 *   - `typedef <OPAQUE_but_fairly_big> CORD_pos[1]`;
 *   - `CORD CORD_pos_to_cord(CORD_pos p)` - extract the cord from
 *     a position;
 *   - `size_t CORD_pos_to_index(CORD_pos p)` - extract the current index
 *     from a position;
 *   - `char CORD_pos_fetch(CORD_pos p)` - fetch the character located at
 *     the given position;
 *   - `void CORD_set_pos(CORD_pos p, CORD x, size_t i)` - initialize the
 *     position to refer to the given cord and index;
 *   - `void CORD_next(CORD_pos p)` - advance the position to the next
 *     character;
 *   - `void CORD_prev(CORD_pos p)` - move the position to the preceding
 *     character;
 *   - `int CORD_pos_valid(CORD_pos p)` - check whether the position is
 *     valid, i.e. inside the cord.
 */

#ifdef __cplusplus
extern "C" {
#endif

#define CORD_FOR(pos, cord) \
  for (CORD_set_pos(pos, cord, 0); CORD_pos_valid(pos); CORD_next(pos))

/**
 * An out-of-memory handler to call.  Zero value means do nothing special,
 * just abort.  Use of the setter and getter is preferred over the direct
 * access to the global variable.
 */
typedef void (*CORD_oom_fn_t)(void);
CORD_API void CORD_set_oom_fn(CORD_oom_fn_t);
CORD_API CORD_oom_fn_t CORD_get_oom_fn(void);
#ifndef CORD_DONT_DECLARE_OOM_FN
CORD_API CORD_oom_fn_t CORD_oom_fn;
#endif
#ifdef CORD_BUILD
/* no export */ void CORD__call_oom_fn(void);
#endif

/**
 * Dump the representation of `x` to `stdout` in an implementation
 * defined manner.  Intended for debugging only.
 */
CORD_API void CORD_dump(CORD /* `x` */);

/*
 * The following could easily be implemented by the client.  They are
 * provided by the cord library for convenience.
 */

/** Concatenate a character to the end of a cord. */
CORD_API CORD CORD_cat_char(CORD, char);

/** Concatenate `n` cords. */
CORD_API CORD CORD_catn(int /* `n` */, /* `CORD` */...);

/** Return the character in `CORD_substr(x, i, 1)`. */
CORD_API char CORD_fetch(CORD /* `x` */, size_t /* `i` */);

/**
 * Return a negative value, zero, or a positive value, depending on
 * whether `x < y`, `x == y`, or `x > y`, respectively.
 */
CORD_API int CORD_cmp(CORD /* `x` */, CORD /* `y` */);

/**
 * A generalization that takes both starting positions for the comparison,
 * and a limit on the number of characters to be compared.
 */
CORD_API int CORD_ncmp(CORD /* `x` */, size_t /* `x_start` */, CORD /* `y` */,
                       size_t /* `y_start` */, size_t /* `len` */);

/**
 * Find the first occurrence of `s` in `x` at position `start` or later.
 * Return the position of the first character of `s` in `x`, or
 * `CORD_NOT_FOUND` if there is none.
 */
CORD_API size_t CORD_str(CORD /* `x` */, size_t /* `start` */, CORD /* `s` */);

/**
 * Return a cord consisting of `i` copies of `c`.  Dangerous in
 * conjunction with `CORD_to_char_star`.  `c` could be a NUL character.
 * The resulting representation takes constant space, independent of `i`.
 */
CORD_API CORD CORD_chars(char /* `c` */, size_t /* `i` */);

#define CORD_nul(i) CORD_chars('\0', i)

/**
 * Turn a file `f` into cord.  The file must be seekable.  Its contents
 * must remain constant.  The file may be accessed as an immediate
 * result of this call and/or as a result of subsequent accesses to
 * the cord.  Short files are likely to be immediately read, but
 * long files are likely to be read on demand, possibly relying on
 * `stdio` for buffering.  We must have exclusive access to the
 * descriptor `f`, i.e. we may read it at any time, and expect the file
 * pointer to be where we left it.  Normally this should be invoked as
 * `CORD_from_file(fopen(...))`.  The latter (`CORD_from_file`)
 * arranges to close the file descriptor when it is no longer needed
 * (e.g. when the result becomes inaccessible).  The file `f` must be
 * such that `ftell` reflects the actual character position in the
 * file, i.e. the number of characters that can be or were read with
 * `fread`.  On UNIX systems this is always true.  On Windows systems,
 * `f` must be opened in binary mode.
 */
CORD_API CORD CORD_from_file(FILE * /* `f` */);

/**
 * Equivalent to `CORD_from_file`, except that the entire file will be
 * read and the file pointer will be closed immediately.  The binary mode
 * restriction of `CORD_from_file` does not apply.
 */
CORD_API CORD CORD_from_file_eager(FILE *);

/**
 * Equivalent to `CORD_from_file`, except that the file will be read on
 * demand.  The binary mode restriction applies.
 */
CORD_API CORD CORD_from_file_lazy(FILE *);

/**
 * Turn a cord into a C string.  The result shares no structure with `x`,
 * and is thus modifiable.
 */
CORD_API char *CORD_to_char_star(CORD /* `x` */);

/**
 * Turn a C string into a `CORD`.  The C string is copied, and so may
 * subsequently be modified.
 */
CORD_API CORD CORD_from_char_star(const char *);

/**
 * Identical to `CORD_from_char_star`, but the result may share structure
 * with the argument and is thus not modifiable.
 */
CORD_API const char *CORD_to_const_char_star(CORD);

/**
 * Write a cord to a file, starting at the current position.
 * No trailing NUL characters and newlines are added.  Returns `EOF`
 * if a write error occurs, 1 otherwise.
 */
CORD_API int CORD_put(CORD, FILE *);

/** "Not found" result for `CORD_chr()` and `CORD_rchr()`. */
#define CORD_NOT_FOUND ((size_t)(-1))

/**
 * A vague analog of `strchr`.  Returns the position (an integer, not
 * a pointer) of the first occurrence of `char` `c` inside `x` at
 * position `i` or later.  The value `i` must be less than `CORD_len(x)`.
 */
CORD_API size_t CORD_chr(CORD /* `x` */, size_t /* `i` */, int /* `c` */);

/**
 * A vague analog of `strrchr`.  Returns index of the last occurrence
 * of `char` `c` inside `x` at position `i` or earlier.  The value `i` must
 * be less than `CORD_len(x)`.
 */
CORD_API size_t CORD_rchr(CORD /* `x` */, size_t /* `i` */, int /* `c` */);

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifndef CORD_NO_IO

#  include <stdarg.h>

#  ifdef __cplusplus
extern "C" {
#  endif

/**
 * The following ones provide functionality similar to the ANSI C functions
 * with corresponding names, but with the following additions and changes:
 *   1. A "%r" conversion specification specifies a `CORD` argument.
 *      Field width, precision, etc. have the same semantics as for "%s".
 *      (Note that "%c", "%C", and "%S" were already taken.)
 *   2. The format string is represented as a `CORD`.
 *   3. `CORD_sprintf` and `CORD_vsprintf` assign the result through
 *      the 1st argument.  Unlike their ANSI C variants, there is no need
 *      to guess the correct buffer size.
 *   4. Most of the conversions are implemented through the native
 *      `vsprintf`.  Hence they are usually no faster, and idiosyncrasies
 *      of the native `printf` are preserved.  However, `CORD` arguments
 *      to `CORD_sprintf`, `CORD_vsprintf` are *not* copied; the result
 *      shares the original structure.  This may make them very efficient
 *      in some unusual applications.  The format string is copied.
 *
 * The functions return the number of characters generated or -1 on error.
 * This complies with the ANSI standard, but is inconsistent with some
 * older implementations of `sprintf`.
 */
CORD_API int CORD_sprintf(CORD * /* `out` */, CORD /* `format` */, ...);
CORD_API int CORD_vsprintf(CORD * /* `out` */, CORD /* `format` */, va_list);
CORD_API int CORD_fprintf(FILE *, CORD /* `format` */, ...);
CORD_API int CORD_vfprintf(FILE *, CORD /* `format` */, va_list);
CORD_API int CORD_printf(CORD /* `format` */, ...);
CORD_API int CORD_vprintf(CORD /* `format` */, va_list);

#  ifdef __cplusplus
} /* extern "C" */
#  endif

#endif /* CORD_NO_IO */

#endif /* CORD_H */
