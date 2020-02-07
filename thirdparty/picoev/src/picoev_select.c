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

#ifndef _WIN32
# include <sys/select.h>
#else
# include <ws2tcpip.h>
#endif

#include "picoev.h"

#ifdef _WIN32
# define PICOEV_W32_INTERNAL
# include "picoev_w32.h"
# define PICOEV_FD_SET(x, y) FD_SET(picoev_w32_fd2sock(x), y)
# define PICOEV_FD_ISSET(x, y) FD_ISSET(picoev_w32_fd2sock(x), y)

typedef struct picoev_w32_globals_st {
  int* fds;
  void* _fds_free_addr;
} picoev_w32_globals;

picoev_w32_globals picoev_w32;

int picoev_w32_sock2fd(int sock) {
  int i;
  for (i = 0; i < picoev.max_fd && picoev_w32.fds[i]; ++i)
    if (picoev_w32.fds[i] == sock) return i;
  assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(i));
  picoev_w32.fds[i] = sock;
  return i;
}

int picoev_w32_fd2sock(int fd) {
  assert(PICOEV_IS_INITED_AND_FD_IN_RANGE(fd));
  return picoev_w32.fds[fd];
}

extern int picoev_w32_deinit(void);

int picoev_w32_init(int max_fd) {
  int r = picoev_init(max_fd);
  if ((picoev_w32.fds = (int*)picoev_memalign(sizeof(int) * max_fd,
						&picoev_w32._fds_free_addr, 1))
	== NULL) {
    picoev_deinit();
    return -1;
  }
}

int picoev_w32_deinit(void) {
  free(picoev_w32._fds_free_addr);
  picoev_w32.fds = NULL;
  picoev_w32._fds_free_addr = NULL;
  return picoev_deinit();
}

#else
# define PICOEV_FD_SET(x, y) FD_SET(x, y)
# define PICOEV_FD_ISSET(x, y) FD_ISSET(x, y)
#endif

picoev_globals picoev;

picoev_loop* picoev_create_loop(int max_timeout)
{
  picoev_loop* loop;
  
  assert(PICOEV_IS_INITED);
  if ((loop = (picoev_loop*)malloc(sizeof(picoev_loop))) == NULL) {
    return NULL;
  }
  if (picoev_init_loop_internal(loop, max_timeout) != 0) {
    free(loop);
    return NULL;
  }
  
  loop->now = time(NULL);
  return loop;
}

int picoev_destroy_loop(picoev_loop* loop)
{
  picoev_deinit_loop_internal(loop);
  free(loop);
  return 0;
}

int picoev_update_events_internal(picoev_loop* loop, int fd, int events)
{
  picoev.fds[fd].events = events & PICOEV_READWRITE;
  return 0;
}

int picoev_poll_once_internal(picoev_loop* loop, int max_wait)
{
  fd_set readfds, writefds, errorfds;
  struct timeval tv;
  int i, r, maxfd = 0;
  
  /* setup */
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&errorfds);
  for (i = 0; i < picoev.max_fd; ++i) {
    picoev_fd* fd = picoev.fds + i;
    if (fd->loop_id == loop->loop_id) {
      if ((fd->events & PICOEV_READ) != 0) {
	PICOEV_FD_SET(i, &readfds);
	if (maxfd < i) {
	  maxfd = i;
	}
      }
      if ((fd->events & PICOEV_WRITE) != 0) {
	PICOEV_FD_SET(i, &writefds);
	if (maxfd < i) {
	  maxfd = i;
	}
      }
    }
  }
  
  /* select and handle if any */
  tv.tv_sec = max_wait;
  tv.tv_usec = 0;
  r = select(maxfd + 1, &readfds, &writefds, &errorfds, &tv);
  if (r == -1) {
    return -1;
  } else if (r > 0) {
    for (i = 0; i < picoev.max_fd; ++i) {
      picoev_fd* target = picoev.fds + i;
      if (target->loop_id == loop->loop_id) {
	int revents = (PICOEV_FD_ISSET(i, &readfds) ? PICOEV_READ : 0)
	  | (PICOEV_FD_ISSET(i, &writefds) ? PICOEV_WRITE : 0);
	if (revents != 0) {
	  (*target->callback)(loop, i, revents, target->cb_arg);
	}
      }
    }
  }
  
  return 0;
}
