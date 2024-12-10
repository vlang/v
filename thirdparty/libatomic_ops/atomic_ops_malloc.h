/*
 * Copyright (c) 2005 Hewlett-Packard Development Company, L.P.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* Almost lock-free malloc implementation based on stack implementation. */
/* See README_malloc.txt file for detailed usage rules.                  */

#ifndef AO_MALLOC_H
#define AO_MALLOC_H

#include "atomic_ops_stack.h"

#include <stddef.h> /* for size_t */

#ifdef __cplusplus
  extern "C" {
#endif

#ifdef AO_STACK_IS_LOCK_FREE
# define AO_MALLOC_IS_LOCK_FREE
#endif

#ifndef AO_ATTR_MALLOC
# if AO_GNUC_PREREQ(3, 1)
#   define AO_ATTR_MALLOC __attribute__((__malloc__))
# elif defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(__EDG__)
#   define AO_ATTR_MALLOC \
                __declspec(allocator) __declspec(noalias) __declspec(restrict)
# elif defined(_MSC_VER) && _MSC_VER >= 1400
#   define AO_ATTR_MALLOC __declspec(noalias) __declspec(restrict)
# else
#   define AO_ATTR_MALLOC /* empty */
# endif
#endif

#ifndef AO_ATTR_ALLOC_SIZE
# ifdef __clang__
#   if __has_attribute(__alloc_size__)
#     define AO_ATTR_ALLOC_SIZE(argnum) \
                __attribute__((__alloc_size__(argnum)))
#   else
#     define AO_ATTR_ALLOC_SIZE(argnum) /* empty */
#   endif
# elif AO_GNUC_PREREQ(4, 3) && !defined(__ICC)
#   define AO_ATTR_ALLOC_SIZE(argnum) __attribute__((__alloc_size__(argnum)))
# else
#   define AO_ATTR_ALLOC_SIZE(argnum) /* empty */
# endif
#endif

AO_API void AO_free(void *);

AO_API AO_ATTR_MALLOC AO_ATTR_ALLOC_SIZE(1)
void * AO_malloc(size_t);

/* Allow use of mmap to grow the heap.  No-op on some platforms.        */
AO_API void AO_malloc_enable_mmap(void);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* !AO_MALLOC_H */
