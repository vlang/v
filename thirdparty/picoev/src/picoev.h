/*
 * Copyright (c) 2009, Cybozu Labs, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the name of the <ORGANIZATION> nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef picoev_h
#define picoev_h

#ifdef __cplusplus
extern "C" {
# define PICOEV_INLINE inline
#else
# define PICOEV_INLINE static __inline__
#endif

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define PICOEV_IS_INITED (picoev.max_fd != 0)  
#define PICOEV_IS_INITED_AND_FD_IN_RANGE(fd) \
  (((unsigned)fd) < (unsigned)picoev.max_fd)
#define PICOEV_TOO_MANY_LOOPS (picoev.num_loops != 0) /* use after ++ */
#define PICOEV_FD_BELONGS_TO_LOOP(loop, fd) \
  ((loop)->loop_id == picoev.fds[fd].loop_id)

#define PICOEV_TIMEOUT_VEC_OF(loop, idx) \
  ((loop)->timeout.vec + (idx) * picoev.timeout_vec_size)
#define PICOEV_TIMEOUT_VEC_OF_VEC_OF(loop, idx) \
  ((loop)->timeout.vec_of_vec + (idx) * picoev.timeout_vec_of_vec_size)
#define PICOEV_RND_UP(v, d) (((v) + (d) - 1) / (d) * (d))

#define PICOEV_PAGE_SIZE 4096
#define PICOEV_CACHE_LINE_SIZE 32 /* in bytes, ok if greater than the actual */
#define PICOEV_SIMD_BITS 128
#define PICOEV_TIMEOUT_VEC_SIZE 128
#define PICOEV_SHORT_BITS (sizeof(short) * 8)

#define PICOEV_READ 1
#define PICOEV_WRITE 2
#define PICOEV_TIMEOUT 4
#define PICOEV_ADD 0x40000000
#define PICOEV_DEL 0x20000000
#define PICOEV_READWRITE (PICOEV_READ | PICOEV_WRITE)
  
#define PICOEV_TIMEOUT_IDX_UNUSED (UCHAR_MAX)
  
  typedef unsigned short picoev_loop_id_t;
  
  typedef struct picoev_loop_st picoev_loop;
  
  typedef void picoev_handler(picoev_loop* loop, int fd, int revents,
			      void* cb_arg);
  
  typedef struct picoev_fd_st {
    /* use accessors! */
    /* TODO adjust the size to match that of a cache line */
    picoev_handler* callback;
    void* cb_arg;
    picoev_loop_id_t loop_id;
    char events;
    unsigned char timeout_idx; /* PICOEV_TIMEOUT_IDX_UNUSED if not used */
    int _backend; /* can be used by backends (never modified by core) */
  } picoev_fd;
  
  struct picoev_loop_st {
    /* read only */
    picoev_loop_id_t loop_id;
    struct {
      short* vec;
      short* vec_of_vec;
      size_t base_idx;
      time_t base_time;
      int resolution;
      void* _free_addr;
    } timeout;
    time_t now;
  };
  
  typedef struct picoev_globals_st {
    /* read only */
    picoev_fd* fds;
    void* _fds_free_addr;
    int max_fd;
    int num_loops;
    size_t timeout_vec_size; /* # of elements in picoev_loop.timeout.vec[0] */
    size_t timeout_vec_of_vec_size; /* ... in timeout.vec_of_vec[0] */
  } picoev_globals;
  
  extern picoev_globals picoev;
  
  /* creates a new event loop (defined by each backend) */
  picoev_loop* picoev_create_loop(int max_timeout);
  
  /* destroys a loop (defined by each backend) */
  int picoev_destroy_loop(picoev_loop* loop);
  
  /* internal: updates events to be watched (defined by each backend) */
  int picoev_update_events_internal(picoev_loop* loop, int fd, int events);
  
  /* internal: poll once and call the handlers (defined by each backend) */
  int picoev_poll_once_internal(picoev_loop* loop, int max_wait);
  
  /* internal, aligned allocator with address scrambling to avoid cache
     line contention */
  PICOEV_INLINE
  void* picoev_memalign(size_t sz, void** orig_addr, int clear) {
    sz = sz + PICOEV_PAGE_SIZE + PICOEV_CACHE_LINE_SIZE;
    if ((*orig_addr = malloc(sz)) == NULL) {
      return NULL;
    }
    if (clear != 0) {
      memset(*orig_addr, 0, sz);
    }
    return
      (void*)PICOEV_RND_UP((unsigned long)*orig_addr
			   + (rand() % PICOEV_PAGE_SIZE),
			   PICOEV_CACHE_LINE_SIZE);
  }
  
  /* initializes picoev */
  PICOEV_INLINE
  int picoev_init(int max_fd) {
    assert(! PICOEV_IS_INITED);
    assert(max_fd > 0);
    if ((picoev.fds = (picoev_fd*)picoev_memalign(sizeof(picoev_fd) * max_fd,
						  &picoev._fds_free_addr, 1))
	== NULL) {
      return -1;
    }
    picoev.max_fd = max_fd;
    picoev.num_loops = 0;
    picoev.timeout_vec_size
      = PICOEV_RND_UP(picoev.max_fd, PICOEV_SIMD_BITS) / PICOEV_SHORT_BITS;
    picoev.timeout_vec_of_vec_size
      = PICOEV_RND_UP(picoev.timeout_vec_size, PICOEV_SIMD_BITS)
      / PICOEV_SHORT_BITS;
    return 0;
  }
  
  /* deinitializes picoev */
  PICOEV_INLINE
  int picoev_deinit(void) {
    assert(PICOEV_IS_INITED);
    free(picoev._fds_free_addr);
    picoev.fds = NULL;
    picoev._fds_free_addr = NULL;
    picoev.max_fd = 0;
    picoev.num_loops = 0;
    return 0;
  }
  
  /* updates timeout */
  PICOEV_INLINE
  void picoev_set_timeout(picoev_loop* loop, int fd, int secs) {
    picoev_fd* target;
    short* vec, * vec_of_vec;
    size_t vi = fd / PICOEV_SHORT_BITS, delta;
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    assert(PICOEV_FD_BELONGS_TO_LOOP(loop, fd));
    target = picoev.fds + fd;
    /* clear timeout */
    if (target->timeout_idx != PICOEV_TIMEOUT_IDX_UNUSED) {
      vec = PICOEV_TIMEOUT_VEC_OF(loop, target->timeout_idx);
      if ((vec[vi] &= ~((unsigned short)SHRT_MIN >> (fd % PICOEV_SHORT_BITS)))
	  == 0) {
	vec_of_vec = PICOEV_TIMEOUT_VEC_OF_VEC_OF(loop, target->timeout_idx);
	vec_of_vec[vi / PICOEV_SHORT_BITS]
	  &= ~((unsigned short)SHRT_MIN >> (vi % PICOEV_SHORT_BITS));
      }
      target->timeout_idx = PICOEV_TIMEOUT_IDX_UNUSED;
    }
    if (secs != 0) {
      delta = (loop->now + secs - loop->timeout.base_time)
	/ loop->timeout.resolution;
      if (delta >= PICOEV_TIMEOUT_VEC_SIZE) {
	delta = PICOEV_TIMEOUT_VEC_SIZE - 1;
      }
      target->timeout_idx =
	(loop->timeout.base_idx + delta) % PICOEV_TIMEOUT_VEC_SIZE;
      vec = PICOEV_TIMEOUT_VEC_OF(loop, target->timeout_idx);
      vec[vi] |= (unsigned short)SHRT_MIN >> (fd % PICOEV_SHORT_BITS);
      vec_of_vec = PICOEV_TIMEOUT_VEC_OF_VEC_OF(loop, target->timeout_idx);
      vec_of_vec[vi / PICOEV_SHORT_BITS]
	|= (unsigned short)SHRT_MIN >> (vi % PICOEV_SHORT_BITS);
    }
  }
  
  /* registers a file descriptor and callback argument to a event loop */
  PICOEV_INLINE
  int picoev_add(picoev_loop* loop, int fd, int events, int timeout_in_secs,
		 picoev_handler* callback, void* cb_arg) {
    picoev_fd* target;
    if (!PICOEV_IS_INITED_AND_FD_IN_RANGE(fd)) { return -1; }
    target = picoev.fds + fd;
    assert(target->loop_id == 0);
    target->callback = callback;
    target->cb_arg = cb_arg;
    target->loop_id = loop->loop_id;
    target->events = 0;
    target->timeout_idx = PICOEV_TIMEOUT_IDX_UNUSED;
    if (picoev_update_events_internal(loop, fd, events | PICOEV_ADD) != 0) {
      target->loop_id = 0;
      return -1;
    }
    picoev_set_timeout(loop, fd, timeout_in_secs);
    return 0;
  }
  
  /* unregisters a file descriptor from event loop */
  PICOEV_INLINE
  int picoev_del(picoev_loop* loop, int fd) {
    picoev_fd* target;
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    target = picoev.fds + fd;
    if (picoev_update_events_internal(loop, fd, PICOEV_DEL) != 0) {
      return -1;
    }
    picoev_set_timeout(loop, fd, 0);
    target->loop_id = 0;
    return 0;
  }
  
  /* check if fd is registered (checks all loops if loop == NULL) */
  PICOEV_INLINE
  int picoev_is_active(picoev_loop* loop, int fd) {
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    return loop != NULL
      ? picoev.fds[fd].loop_id == loop->loop_id
      : picoev.fds[fd].loop_id != 0;
  }
  
  /* returns events being watched for given descriptor */
  PICOEV_INLINE
  int picoev_get_events(picoev_loop* loop __attribute__((unused)), int fd) {
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    return picoev.fds[fd].events & PICOEV_READWRITE;
  }
  
  /* sets events to be watched for given desriptor */
  PICOEV_INLINE
  int picoev_set_events(picoev_loop* loop, int fd, int events) {
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    if (picoev.fds[fd].events != events
	&& picoev_update_events_internal(loop, fd, events) != 0) {
      return -1;
    }
    return 0;
  }
  
  /* returns callback for given descriptor */
  PICOEV_INLINE
  picoev_handler* picoev_get_callback(picoev_loop* loop __attribute__((unused)),
				      int fd, void** cb_arg) {
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    if (cb_arg != NULL) {
      *cb_arg = picoev.fds[fd].cb_arg;
    }
    return picoev.fds[fd].callback;
  }
  
  /* sets callback for given descriptor */
  PICOEV_INLINE
  void picoev_set_callback(picoev_loop* loop __attribute__((unused)), int fd,
			   picoev_handler* callback, void** cb_arg) {
    assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
    if (cb_arg != NULL) {
      picoev.fds[fd].cb_arg = *cb_arg;
    }
    picoev.fds[fd].callback = callback;
  }
  
  /* function to iterate registered information. To start iteration, set curfd
     to -1 and call the function until -1 is returned */
  PICOEV_INLINE
  int picoev_next_fd(picoev_loop* loop, int curfd) {
    if (curfd != -1) {
      assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(curfd));
    }
    while (++curfd < picoev.max_fd) {
      if (loop->loop_id == picoev.fds[curfd].loop_id) {
	return curfd;
      }
    }
    return -1;
  }
  
  /* internal function */
  PICOEV_INLINE
  int picoev_init_loop_internal(picoev_loop* loop, int max_timeout) {
    loop->loop_id = ++picoev.num_loops;
    assert(PICOEV_TOO_MANY_LOOPS);
    if ((loop->timeout.vec_of_vec
	 = (short*)picoev_memalign((picoev.timeout_vec_of_vec_size
				    + picoev.timeout_vec_size)
				   * sizeof(short) * PICOEV_TIMEOUT_VEC_SIZE,
				   &loop->timeout._free_addr, 1))
	== NULL) {
      --picoev.num_loops;
      return -1;
    }
    loop->timeout.vec = loop->timeout.vec_of_vec
      + picoev.timeout_vec_of_vec_size * PICOEV_TIMEOUT_VEC_SIZE;
    loop->timeout.base_idx = 0;
    loop->timeout.base_time = time(NULL);
    loop->timeout.resolution
      = PICOEV_RND_UP(max_timeout, PICOEV_TIMEOUT_VEC_SIZE)
      / PICOEV_TIMEOUT_VEC_SIZE;
    return 0;
  }
  
  /* internal function */
  PICOEV_INLINE
  void picoev_deinit_loop_internal(picoev_loop* loop) {
    free(loop->timeout._free_addr);
  }
  
  /* internal function */
  PICOEV_INLINE
  void picoev_handle_timeout_internal(picoev_loop* loop) {
    size_t i, j, k;
    for (;
	 loop->timeout.base_time <= loop->now - loop->timeout.resolution; 
	 loop->timeout.base_idx
	   = (loop->timeout.base_idx + 1) % PICOEV_TIMEOUT_VEC_SIZE,
	   loop->timeout.base_time += loop->timeout.resolution) {
      /* TODO use SIMD instructions */
      short* vec = PICOEV_TIMEOUT_VEC_OF(loop, loop->timeout.base_idx);
      short* vec_of_vec
	= PICOEV_TIMEOUT_VEC_OF_VEC_OF(loop, loop->timeout.base_idx);
      for (i = 0; i < picoev.timeout_vec_of_vec_size; ++i) {
	short vv = vec_of_vec[i];
	if (vv != 0) {
	  for (j = i * PICOEV_SHORT_BITS; vv != 0; j++, vv <<= 1) {
	    if (vv < 0) {
	      short v = vec[j];
	      assert(v != 0);
	      for (k = j * PICOEV_SHORT_BITS; v != 0; k++, v <<= 1) {
		if (v < 0) {
		  picoev_fd* fd = picoev.fds + k;
		  assert(fd->loop_id == loop->loop_id);
		  fd->timeout_idx = PICOEV_TIMEOUT_IDX_UNUSED;
		  (*fd->callback)(loop, k, PICOEV_TIMEOUT, fd->cb_arg);
		}
	      }
	      vec[j] = 0;
	    }
	  }
	  vec_of_vec[i] = 0;
	}
      }
    }
  }
  
  /* loop once */
  PICOEV_INLINE
  int picoev_loop_once(picoev_loop* loop, int max_wait) {
    loop->now = time(NULL);
    if (max_wait > loop->timeout.resolution) {
      max_wait = loop->timeout.resolution;
    }
    if (picoev_poll_once_internal(loop, max_wait) != 0) {
      return -1;
    }
    if (max_wait != 0) {
      loop->now = time(NULL);
    }
    picoev_handle_timeout_internal(loop);
    return 0;
  }
  
#undef PICOEV_INLINE

#ifdef __cplusplus
}
#endif

#endif
