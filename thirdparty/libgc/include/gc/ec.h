/*
 * Copyright (c) 1993-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#ifndef EC_H
#define EC_H

# ifndef CORD_H
#  include "cord.h"
# endif

#ifdef __cplusplus
  extern "C" {
#endif

/* Extensible cords are strings that may be destructively appended to.  */
/* They allow fast construction of cords from characters that are       */
/* being read from a stream.                                            */
/*
 * A client might look like:
 *
 *      {
 *          CORD_ec x;
 *          CORD result;
 *          char c;
 *          FILE *f;
 *
 *          ...
 *          CORD_ec_init(x);
 *          while(...) {
 *              c = getc(f);
 *              ...
 *              CORD_ec_append(x, c);
 *          }
 *          result = CORD_balance(CORD_ec_to_cord(x));
 *
 * If a C string is desired as the final result, the call to CORD_balance
 * may be replaced by a call to CORD_to_char_star.
 */

# ifndef CORD_BUFSZ
#   define CORD_BUFSZ 128
# endif

typedef struct CORD_ec_struct {
    CORD ec_cord;
    char * ec_bufptr;
    char ec_buf[CORD_BUFSZ+1];
} CORD_ec[1];

/* This structure represents the concatenation of ec_cord with  */
/* ec_buf[0 ... (ec_bufptr-ec_buf-1)]                           */

/* Flush the buffer part of the extended cord into ec_cord.     */
/* Note that this is almost the only real function, and it is   */
/* implemented in 6 lines in cordxtra.c                         */
void CORD_ec_flush_buf(CORD_ec x);

/* Convert an extensible cord to a cord. */
# define CORD_ec_to_cord(x) (CORD_ec_flush_buf(x), (x)[0].ec_cord)

/* Initialize an extensible cord. */
#define CORD_ec_init(x) \
                ((x)[0].ec_cord = 0, (void)((x)[0].ec_bufptr = (x)[0].ec_buf))

/* Append a character to an extensible cord.    */
#define CORD_ec_append(x, c) \
                ((void)((x)[0].ec_bufptr == (x)[0].ec_buf + CORD_BUFSZ \
                        ? (CORD_ec_flush_buf(x), 0) : 0), \
                 (void)(*(x)[0].ec_bufptr++ = (c)))

/* Append a cord to an extensible cord.  Structure remains shared with  */
/* original.                                                            */
void CORD_ec_append_cord(CORD_ec x, CORD s);

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif /* EC_H */
