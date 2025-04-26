/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef RPC_NO_WINDOWS_H
#include <windows.h>
#endif

#ifndef __RPC_H__
#define __RPC_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <basetsd.h>

#if defined(__ia64__) || defined(__x86_64)
#define __RPC_WIN64__
#else
#define __RPC_WIN32__
#endif

#ifdef __RPC_WIN64__
#include <pshpack8.h>
#endif

#ifndef __MIDL_USER_DEFINED
#define __MIDL_USER_DEFINED
#define midl_user_allocate MIDL_user_allocate
#define midl_user_free MIDL_user_free
#endif

  typedef void *I_RPC_HANDLE;
  typedef long RPC_STATUS;

#define RPC_UNICODE_SUPPORTED
#define __RPC_FAR
#define __RPC_API __stdcall
#define __RPC_USER __RPC_API
#define __RPC_STUB __RPC_API
#define RPC_ENTRY __RPC_API

#ifndef DECLSPEC_IMPORT
#define DECLSPEC_IMPORT __declspec(dllimport)
#endif

#ifndef _RPCRT4_
#define RPCRTAPI DECLSPEC_IMPORT
#else
#define RPCRTAPI
#endif

#ifndef _RPCNS4_
#define RPCNSAPI DECLSPEC_IMPORT
#else
#define RPCNSAPI
#endif

#include <rpcdce.h>
#include <rpcnsi.h>
#include <rpcnterr.h>
#include <excpt.h>
#include <winerror.h>

#define RpcTryExcept __try {
#define RpcExcept(expr) } __except(expr) {
#define RpcEndExcept }
#define RpcTryFinally __try {
#define RpcFinally } __finally {
#define RpcEndFinally }

#define RpcExceptionCode() GetExceptionCode()
#define RpcAbnormalTermination() AbnormalTermination()

#ifndef RPC_NO_WINDOWS_H
#include <rpcasync.h>
#endif

#ifdef __RPC_WIN64__
#include <poppack.h>
#endif

#ifdef __cplusplus
}
#endif
#endif
