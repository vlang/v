/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef NTLMSP_NAME_A
#define NTLMSP_NAME_A "NTLM"
#define NTLMSP_NAME L"NTLM"
#endif

#ifndef MICROSOFT_KERBEROS_NAME_A
#define MICROSOFT_KERBEROS_NAME_A "Kerberos"
#define MICROSOFT_KERBEROS_NAME_W L"Kerberos"
#ifdef WIN32_CHICAGO
#define MICROSOFT_KERBEROS_NAME MICROSOFT_KERBEROS_NAME_A
#else
#define MICROSOFT_KERBEROS_NAME MICROSOFT_KERBEROS_NAME_W
#endif
#endif

#ifndef NEGOSSP_NAME
#define NEGOSSP_NAME_W L"Negotiate"
#define NEGOSSP_NAME_A "Negotiate"

#ifdef UNICODE
#define NEGOSSP_NAME NEGOSSP_NAME_W
#else
#define NEGOSSP_NAME NEGOSSP_NAME_A
#endif
#endif

#include <sspi.h>

#if defined(SECURITY_WIN32) || defined(SECURITY_KERNEL)
#include <secext.h>
#endif

#if ISSP_LEVEL==16
#include <issper16.h>
#endif
