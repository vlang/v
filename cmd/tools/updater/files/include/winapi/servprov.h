/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include <_mingw.h>
#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error This stub requires an updated version of <rpcndr.h>
#endif

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif

#ifndef __servprov_h__
#define __servprov_h__

#ifndef __IServiceProvider_FWD_DEFINED__
#define __IServiceProvider_FWD_DEFINED__
typedef struct IServiceProvider IServiceProvider;
#endif

#include "objidl.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __MIDL_user_allocate_free_DEFINED__
#define __MIDL_user_allocate_free_DEFINED__
  void *__RPC_API MIDL_user_allocate(size_t);
  void __RPC_API MIDL_user_free(void *);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_servprov_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_servprov_0000_v0_0_s_ifspec;

#ifndef __IServiceProvider_INTERFACE_DEFINED__
#define __IServiceProvider_INTERFACE_DEFINED__
  typedef IServiceProvider *LPSERVICEPROVIDER;

#if defined(__cplusplus) && !defined(CINTERFACE)
  EXTERN_C const IID IID_IServiceProvider;
  extern "C++" {
    struct IServiceProvider : public IUnknown {
    public:
      virtual HRESULT WINAPI QueryService(REFGUID guidService,REFIID riid,void **ppvObject) = 0;
#if USE___UUIDOF != 0
      template <class Q> HRESULT WINAPI QueryService(REFGUID guidService,Q **pp) { return QueryService(guidService,__uuidof(Q),(void **)pp); }
#endif
    };
  }
  HRESULT WINAPI IServiceProvider_RemoteQueryService_Proxy(IServiceProvider *This,REFGUID guidService,REFIID riid,IUnknown **ppvObject);
  void __RPC_STUB IServiceProvider_RemoteQueryService_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#else
  EXTERN_C const IID IID_IServiceProvider;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IServiceProvider : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryService(REFGUID guidService,REFIID riid,void **ppvObject) = 0;
  };
#else
  typedef struct IServiceProviderVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IServiceProvider *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IServiceProvider *This);
      ULONG (WINAPI *Release)(IServiceProvider *This);
      HRESULT (WINAPI *QueryService)(IServiceProvider *This,REFGUID guidService,REFIID riid,void **ppvObject);
    END_INTERFACE
  } IServiceProviderVtbl;
  struct IServiceProvider {
    CONST_VTBL struct IServiceProviderVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IServiceProvider_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IServiceProvider_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IServiceProvider_Release(This) (This)->lpVtbl->Release(This)
#define IServiceProvider_QueryService(This,guidService,riid,ppvObject) (This)->lpVtbl->QueryService(This,guidService,riid,ppvObject)
#endif
#endif
  HRESULT WINAPI IServiceProvider_RemoteQueryService_Proxy(IServiceProvider *This,REFGUID guidService,REFIID riid,IUnknown **ppvObject);
  void __RPC_STUB IServiceProvider_RemoteQueryService_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

  extern RPC_IF_HANDLE __MIDL_itf_servprov_0093_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_servprov_0093_v0_0_s_ifspec;

  HRESULT WINAPI IServiceProvider_QueryService_Proxy(IServiceProvider *This,REFGUID guidService,REFIID riid,void **ppvObject);
  HRESULT WINAPI IServiceProvider_QueryService_Stub(IServiceProvider *This,REFGUID guidService,REFIID riid,IUnknown **ppvObject);

#ifdef __cplusplus
}
#endif
#endif
