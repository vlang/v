#ifndef picoev_w32_h
#define picoev_w32_h

#include "picoev.h"

#ifndef PICOEV_W32_INTERNAL
extern int picoev_w32_init(int);
extern int picoev_w32_deinit(void);
extern int picoev_w32_sock2fd(int);
extern int picoev_w32_fd2sock(int);
# define picoev_init picoev_w32_init
# define picoev_deinit picoev_w32_deinit
#endif

#endif
