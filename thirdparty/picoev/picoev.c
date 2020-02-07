#ifdef __linux__
	#include "src/picoev_epoll.c"
#elif __APPLE__
	#include "src/picoev_kqueue.c"
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
	#include "src/picoev_kqueue.c"
#else
	#include "src/picoev_select.c"
#endif
