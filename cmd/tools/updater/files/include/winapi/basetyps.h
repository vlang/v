/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#if !defined(_BASETYPS_H_)
#define _BASETYPS_H_

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#define STDMETHODCALLTYPE WINAPI
#define STDMETHODVCALLTYPE __cdecl

#define STDAPICALLTYPE WINAPI
#define STDAPIVCALLTYPE __cdecl

#define STDAPI EXTERN_C HRESULT WINAPI
#define STDAPI_(type) EXTERN_C type WINAPI

#define STDMETHODIMP HRESULT WINAPI
#define STDMETHODIMP_(type) type WINAPI

#define STDAPIV EXTERN_C HRESULT STDAPIVCALLTYPE
#define STDAPIV_(type) EXTERN_C type STDAPIVCALLTYPE

#define STDMETHODIMPV HRESULT STDMETHODVCALLTYPE
#define STDMETHODIMPV_(type) type STDMETHODVCALLTYPE

#if defined(__cplusplus) && !defined(CINTERFACE)

#define __STRUCT__ struct
#define STDMETHOD(method) virtual HRESULT WINAPI method
#define STDMETHOD_(type,method) virtual type WINAPI method
#define STDMETHODV(method) virtual HRESULT STDMETHODVCALLTYPE method
#define STDMETHODV_(type,method) virtual type STDMETHODVCALLTYPE method
#define PURE = 0
#define THIS_
#define THIS void
#define DECLARE_INTERFACE(iface) __STRUCT__ iface
#define DECLARE_INTERFACE_(iface,baseiface) __STRUCT__ iface : public baseiface
#else

#ifndef __OBJC__
#define interface struct
#endif

#define STDMETHOD(method) HRESULT (WINAPI *method)
#define STDMETHOD_(type,method) type (WINAPI *method)
#define STDMETHODV(method) HRESULT (STDMETHODVCALLTYPE *method)
#define STDMETHODV_(type,method) type (STDMETHODVCALLTYPE *method)

#define PURE
#define THIS_ INTERFACE *This,
#define THIS INTERFACE *This
#ifdef CONST_VTABLE
#define DECLARE_INTERFACE(iface) typedef struct iface { \
  const struct iface##Vtbl *lpVtbl; } iface; \
  typedef const struct iface##Vtbl iface##Vtbl; \
  const struct iface##Vtbl
#else
#define DECLARE_INTERFACE(iface) typedef struct iface { \
    struct iface##Vtbl *lpVtbl; \
  } iface; \
  typedef struct iface##Vtbl iface##Vtbl; \
  struct iface##Vtbl
#endif
#define DECLARE_INTERFACE_(iface,baseiface) DECLARE_INTERFACE(iface)
#endif

#include <guiddef.h>

#ifndef _ERROR_STATUS_T_DEFINED
#define _ERROR_STATUS_T_DEFINED
typedef unsigned long error_status_t;
#endif

#ifndef _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
#define _WCHAR_T_DEFINED
#endif
#endif
