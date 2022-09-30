// NOTE: tcc does not support yet __attribute__ ((__packed__)) properly, 
// so the __EPOLL_PACKED macro that /usr/include/bits/epoll.h uses does not work :-| .
// However, it *does support* the older `#pragma pack(push, 1)`
#pragma pack(push, 1)
#include <sys/epoll.h>
#pragma pack(pop)
