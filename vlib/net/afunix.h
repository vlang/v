/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the mingw-w64 runtime package.
 * No warranty is given; refer to the file DISCLAIMER.PD within this package.
 */

#ifndef _AFUNIX_
#define _AFUNIX_

#define UNIX_PATH_MAX 108

#if !defined(ADDRESS_FAMILY)
#define UNDEF_ADDRESS_FAMILY
#define ADDRESS_FAMILY unsigned short
#endif

typedef struct sockaddr_un {
  ADDRESS_FAMILY sun_family;
  char sun_path[UNIX_PATH_MAX];
} SOCKADDR_UN, *PSOCKADDR_UN;

#if defined(UNDEF_ADDRESS_FAMILY)
#undef ADDRESS_FAMILY
#endif

#endif /* _AFUNIX_ */
