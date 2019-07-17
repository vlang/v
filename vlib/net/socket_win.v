module net

#ifdef _WIN32 
#flag -lws2_32
#include <winsock2.h>
#include <Ws2tcpip.h>
#endif 