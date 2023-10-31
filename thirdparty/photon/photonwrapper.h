#ifndef C_PHOTONWRAPPER_H_
#define C_PHOTONWRAPPER_H_


#ifdef __cplusplus

#include <fcntl.h>
//#include <vector>

#include <photon/thread/std-compat.h>
#include <photon/common/alog.h>
#include <photon/common/iovector.h>
#include <photon/fs/localfs.h>
#include <photon/net/socket.h>

#include <iostream>

extern "C" {
#else

#endif

int photon_init_default();
void photon_thread_create(void* (* f)(void*), void* arg);
void photon_sleep_s(int n);
void photon_sleep_ms(int n);
void set_photon_thread_stack_allocator(
    void* (*alloc_func)(void*, size_t),
    void (*dealloc_func)(void*, void*, size_t)
);

#ifdef __cplusplus
}
#endif


#endif
