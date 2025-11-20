// NOTE: tcc does not support yet __attribute__ ((__packed__)) properly, 
// so the __EPOLL_PACKED macro that /usr/include/bits/epoll.h uses does not work :-| .
// However, it *does support* the older `#pragma pack(push, 1)`
#if defined(__TINYC__) && defined(__V_amd64)
#pragma pack(push, 1)
#endif

#include <sys/epoll.h>

#if defined(__TINYC__) && defined(__V_amd64)
#pragma pack(pop)
#endif
