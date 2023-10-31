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

/* This should never be included directly; included only from cord.h.   */
#if !defined(CORD_POSITION_H) && defined(CORD_H)
#define CORD_POSITION_H

#ifdef __cplusplus
  extern "C" {
#endif

/* The representation of CORD_position.  This is private to the */
/* implementation, but the size is known to clients.  Also      */
/* the implementation of some exported macros relies on it.     */
/* Don't use anything defined here and not in cord.h.           */

# define MAX_DEPTH 48
        /* The maximum depth of a balanced cord + 1.            */
        /* We don't let cords get deeper than MAX_DEPTH.        */

struct CORD_pe {
    CORD pe_cord;
    size_t pe_start_pos;
};

/* A structure describing an entry on the path from the root    */
/* to current position.                                         */
typedef struct CORD_Pos {
    size_t cur_pos;
    int path_len;
#       define CORD_POS_INVALID (0x55555555)
                /* path_len == INVALID <==> position invalid */
    const char *cur_leaf;       /* Current leaf, if it is a string.     */
                                /* If the current leaf is a function,   */
                                /* then this may point to function_buf  */
                                /* containing the next few characters.  */
                                /* Always points to a valid string      */
                                /* containing the current character     */
                                /* unless cur_end is 0.                 */
    size_t cur_start;   /* Start position of cur_leaf   */
    size_t cur_end;     /* Ending position of cur_leaf  */
                        /* 0 if cur_leaf is invalid.    */
    struct CORD_pe path[MAX_DEPTH + 1];
        /* path[path_len] is the leaf corresponding to cur_pos  */
        /* path[0].pe_cord is the cord we point to.             */
#   define FUNCTION_BUF_SZ 8
    char function_buf[FUNCTION_BUF_SZ]; /* Space for next few chars     */
                                        /* from function node.          */
} CORD_pos[1];

/* Extract the cord from a position:    */
CORD_API CORD CORD_pos_to_cord(CORD_pos p);

/* Extract the current index from a position:   */
CORD_API size_t CORD_pos_to_index(CORD_pos p);

/* Fetch the character located at the given position:   */
CORD_API char CORD_pos_fetch(CORD_pos p);

/* Initialize the position to refer to the give cord and index. */
/* Note that this is the most expensive function on positions:  */
CORD_API void CORD_set_pos(CORD_pos p, CORD x, size_t i);

/* Advance the position to the next character.  */
/* P must be initialized and valid.             */
/* Invalidates p if past end:                   */
CORD_API void CORD_next(CORD_pos p);

/* Move the position to the preceding character.        */
/* P must be initialized and valid.                     */
/* Invalidates p if past beginning:                     */
CORD_API void CORD_prev(CORD_pos p);

/* Is the position valid, i.e. inside the cord?         */
CORD_API int CORD_pos_valid(CORD_pos p);

CORD_API char CORD__pos_fetch(CORD_pos);
CORD_API void CORD__next(CORD_pos);
CORD_API void CORD__prev(CORD_pos);

#define CORD_pos_fetch(p)       \
    (((p)[0].cur_end != 0)? \
        (p)[0].cur_leaf[(p)[0].cur_pos - (p)[0].cur_start] \
        : CORD__pos_fetch(p))

#define CORD_next(p)    \
    (((p)[0].cur_pos + 1 < (p)[0].cur_end)? \
        (p)[0].cur_pos++ \
        : (CORD__next(p), 0))

#define CORD_prev(p)    \
    (((p)[0].cur_end != 0 && (p)[0].cur_pos > (p)[0].cur_start)? \
        (p)[0].cur_pos-- \
        : (CORD__prev(p), 0))

#define CORD_pos_to_index(p) ((p)[0].cur_pos)

#define CORD_pos_to_cord(p) ((p)[0].path[0].pe_cord)

#define CORD_pos_valid(p) ((p)[0].path_len != CORD_POS_INVALID)

/* Some grubby stuff for performance-critical friends:  */
#define CORD_pos_chars_left(p) ((long)((p)[0].cur_end) - (long)((p)[0].cur_pos))
        /* Number of characters in cache.  <= 0 ==> none        */

#define CORD_pos_advance(p,n) ((p)[0].cur_pos += (n) - 1, CORD_next(p))
        /* Advance position by n characters     */
        /* 0 < n < CORD_pos_chars_left(p)       */

#define CORD_pos_cur_char_addr(p) \
        (p)[0].cur_leaf + ((p)[0].cur_pos - (p)[0].cur_start)
        /* address of current character in cache.       */

#ifdef __cplusplus
  } /* extern "C" */
#endif

#endif
