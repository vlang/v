/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __SECEXT_H__
#define __SECEXT_H__

#include "sspi.h"

#ifdef __cplusplus
extern "C" {
#endif

  typedef enum {
    NameUnknown = 0,NameFullyQualifiedDN = 1,NameSamCompatible = 2,NameDisplay = 3,NameUniqueId = 6,NameCanonical = 7,NameUserPrincipal = 8,
    NameCanonicalEx = 9,NameServicePrincipal = 10,NameDnsDomain = 12
  } EXTENDED_NAME_FORMAT,*PEXTENDED_NAME_FORMAT;

#ifdef UNICODE
#define GetUserNameEx GetUserNameExW
#define GetComputerObjectName GetComputerObjectNameW
#define TranslateName TranslateNameW
#else
#define GetUserNameEx GetUserNameExA
#define GetComputerObjectName GetComputerObjectNameA
#define TranslateName TranslateNameA
#endif

  BOOLEAN SEC_ENTRY GetUserNameExA(EXTENDED_NAME_FORMAT NameFormat,LPSTR lpNameBuffer,PULONG nSize);
  BOOLEAN SEC_ENTRY GetUserNameExW(EXTENDED_NAME_FORMAT NameFormat,LPWSTR lpNameBuffer,PULONG nSize);
  BOOLEAN SEC_ENTRY GetComputerObjectNameA(EXTENDED_NAME_FORMAT NameFormat,LPSTR lpNameBuffer,PULONG nSize);
  BOOLEAN SEC_ENTRY GetComputerObjectNameW(EXTENDED_NAME_FORMAT NameFormat,LPWSTR lpNameBuffer,PULONG nSize);
  BOOLEAN SEC_ENTRY TranslateNameA(LPCSTR lpAccountName,EXTENDED_NAME_FORMAT AccountNameFormat,EXTENDED_NAME_FORMAT DesiredNameFormat,LPSTR lpTranslatedName,PULONG nSize);
  BOOLEAN SEC_ENTRY TranslateNameW(LPCWSTR lpAccountName,EXTENDED_NAME_FORMAT AccountNameFormat,EXTENDED_NAME_FORMAT DesiredNameFormat,LPWSTR lpTranslatedName,PULONG nSize);

#ifdef __cplusplus
}
#endif
#endif
