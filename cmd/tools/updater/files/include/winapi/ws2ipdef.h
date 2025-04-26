#ifndef _WS2IPDEF_H
#define _WS2IPDEF_H

#if __GNUC__ >=3
#pragma GCC system_header
#endif

#include <winsock2.h>

struct ip_mreq {
  struct in_addr imr_multiaddr;
  struct in_addr imr_interface;
};

struct ip_mreq_source {
  struct in_addr imr_multiaddr;
  struct in_addr imr_sourceaddr;
  struct in_addr imr_interface;
};

#endif
