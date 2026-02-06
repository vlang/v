/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
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
 *
 * Derived from the corresponding header file for gcc.
 */

#include "../loadstore/atomic_load.h"
#include "../loadstore/atomic_store.h"

/* Some architecture set descriptions include special "ordered" memory  */
/* operations.  As far as we can tell, no existing processors actually  */
/* require those.  Nor does it appear likely that future processors     */
/* will.                                                                */
/* FIXME: The PA emulator on Itanium may obey weaker restrictions.      */
/* There should be a mode in which we don't assume sequential           */
/* consistency here.                                                    */
#include "../ordered.h"

#include <machine/inline.h>

/* GCC will not guarantee the alignment we need, use four lock words    */
/* and select the correctly aligned datum. See the glibc 2.3.2          */
/* linuxthread port for the original implementation.                    */
struct AO_pa_clearable_loc {
  int data[4];
};

#undef AO_TS_INITIALIZER
#define AO_TS_t struct AO_pa_clearable_loc
#define AO_TS_INITIALIZER {1,1,1,1}
/* Switch meaning of set and clear, since we only have an atomic clear  */
/* instruction.                                                         */
typedef enum {AO_PA_TS_set = 0, AO_PA_TS_clear = 1} AO_PA_TS_val;
#define AO_TS_VAL_t AO_PA_TS_val
#define AO_TS_CLEAR AO_PA_TS_clear
#define AO_TS_SET AO_PA_TS_set

/* The hppa only has one atomic read and modify memory operation,       */
/* load and clear, so hppa spinlocks must use zero to signify that      */
/* someone is holding the lock.  The address used for the ldcw          */
/* semaphore must be 16-byte aligned.                                   */
#define AO_ldcw(a, ret) \
  _LDCWX(0 /* index */, 0 /* s */, a /* base */, ret)

/* Because malloc only guarantees 8-byte alignment for malloc'd data,   */
/* and GCC only guarantees 8-byte alignment for stack locals, we can't  */
/* be assured of 16-byte alignment for atomic lock data even if we      */
/* specify "__attribute ((aligned(16)))" in the type declaration.  So,  */
/* we use a struct containing an array of four ints for the atomic lock */
/* type and dynamically select the 16-byte aligned int from the array   */
/* for the semaphore.                                                   */
#define AO_PA_LDCW_ALIGNMENT 16
#define AO_ldcw_align(addr) \
            ((volatile unsigned *)(((unsigned long)(addr) \
                                        + (AO_PA_LDCW_ALIGNMENT - 1)) \
                                   & ~(AO_PA_LDCW_ALIGNMENT - 1)))

/* Works on PA 1.1 and PA 2.0 systems */
AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t * addr)
{
  register unsigned int ret;
  register unsigned long a = (unsigned long)AO_ldcw_align(addr);

# if defined(CPPCHECK)
    ret = 0; /* to void 'uninitialized variable' warning */
# endif
  AO_ldcw(a, ret);
  return (AO_TS_VAL_t)ret;
}
#define AO_HAVE_test_and_set_full

AO_INLINE void
AO_pa_clear(volatile AO_TS_t * addr)
{
  volatile unsigned *a = AO_ldcw_align(addr);

  AO_compiler_barrier();
  *a = 1;
}
#define AO_CLEAR(addr) AO_pa_clear(addr)
#define AO_HAVE_CLEAR

#undef AO_PA_LDCW_ALIGNMENT
#undef AO_ldcw
#undef AO_ldcw_align
