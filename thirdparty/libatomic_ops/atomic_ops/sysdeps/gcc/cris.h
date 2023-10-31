/*
 * Copyright (c) 2004 Hewlett-Packard Development Company, L.P.
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

/* FIXME: seems to be untested.         */

#include "../all_atomic_load_store.h"

#include "../ordered.h"  /* There are no multiprocessor implementations. */

#include "../test_and_set_t_is_ao_t.h"

/*
 * The architecture apparently supports an "f" flag which is
 * set on preemption.  This essentially gives us load-locked,
 * store-conditional primitives, though I'm not quite sure how
 * this would work on a hypothetical multiprocessor.  -HB
 *
 * For details, see
 * http://developer.axis.com/doc/hardware/etrax100lx/prog_man/
 *      1_architectural_description.pdf
 *
 * TODO: Presumably many other primitives (notably CAS, including the double-
 * width versions) could be implemented in this manner, if someone got
 * around to it.
 */

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
    /* Ripped from linuxthreads/sysdeps/cris/pt-machine.h */
    register unsigned long int ret;

    /* Note the use of a dummy output of *addr to expose the write.  The
       memory barrier is to stop *other* writes being moved past this code.  */
      __asm__ __volatile__("clearf\n"
                           "0:\n\t"
                           "movu.b [%2],%0\n\t"
                           "ax\n\t"
                           "move.b %3,[%2]\n\t"
                           "bwf 0b\n\t"
                           "clearf"
                           : "=&r" (ret), "=m" (*addr)
                           : "r" (addr), "r" ((int) 1), "m" (*addr)
                           : "memory");
    return ret;
}
#define AO_HAVE_test_and_set_full
