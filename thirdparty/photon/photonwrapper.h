


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
    typedef struct CppClass CppClass;
#endif

//CppClass *cpp_class_create();
//void cpp_class_destroy(CppClass *c);
//void cpp_class_do_work(CppClass *c);

int photon_init_default();
void photon_thread_create11(void* (* f)(void*));

void photon_sleep_s(int n);

void photon_sleep_ms(int n);



#ifdef __cplusplus
}
#endif


#endif
