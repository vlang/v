/*
 * Copyright (c) 2003-2004 Hewlett-Packard Development Company, L.P.
 * Copyright (c) 2011-2018 Ivan Maidanski
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

#ifndef AO_ATOMIC_OPS_H
# error This file should not be included directly.
#endif

/* The policy regarding version numbers: development code has odd       */
/* "minor" number (and "micro" part is 0); when development is finished */
/* and a release is prepared, "minor" number is incremented (keeping    */
/* "micro" number still zero), whenever a defect is fixed a new release */
/* is prepared incrementing "micro" part to odd value (the most stable  */
/* release has the biggest "micro" number).                             */

/* The version here should match that in configure.ac and README.       */
#define AO_VERSION_MAJOR 7
#define AO_VERSION_MINOR 6
#define AO_VERSION_MICRO 12 /* 7.6.12 */
