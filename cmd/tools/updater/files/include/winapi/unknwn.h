/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
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

#ifndef __unknwn_h__
#define __unknwn_h__

#ifndef __IUnknown_FWD_DEFINED__
#define __IUnknown_FWD_DEFINED__
typedef struct IUnknown IUnknown;
#endif

#ifndef __AsyncIUnknown_FWD_DEFINED__
#define __AsyncIUnknown_FWD_DEFINED__
typedef struct AsyncIUnknown AsyncIUnknown;
#endif

#ifndef __IClassFactory_FWD_DEFINED__
#define __IClassFactory_FWD_DEFINED__
typedef struct IClassFactory IClassFactory;
#endif

#include "wtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __MIDL_user_allocate_free_DEFINED__
#define __MIDL_user_allocate_free_DEFINED__
  void *__RPC_API MIDL_user_allocate(size_t);
  void __RPC_API MIDL_user_free(void *);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_unknwn_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_unknwn_0000_v0_0_s_ifspec;

#ifndef __IUnknown_INTERFACE_DEFINED__
#define __IUnknown_INTERFACE_DEFINED__

  typedef IUnknown *LPUNKNOWN;

#if defined(__cplusplus) && !defined(CINTERFACE)
  EXTERN_C const IID IID_IUnknown;
  extern "C++" {
    struct IUnknown {
    public:
      BEGIN_INTERFACE
	virtual HRESULT WINAPI QueryInterface(REFIID riid,void **ppvObject) = 0;
	virtual ULONG WINAPI AddRef(void) = 0;
	virtual ULONG WINAPI Release(void) = 0;
#if USE___UUIDOF != 0
	template<class Q> HRESULT WINAPI QueryInterface(Q **pp) { return QueryInterface(__uuidof(Q),(void **)pp); }
#endif
      END_INTERFACE
    };
  }
  HRESULT WINAPI IUnknown_QueryInterface_Proxy(IUnknown *This,REFIID riid,void **ppvObject);
  void __RPC_STUB IUnknown_QueryInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IUnknown_AddRef_Proxy(IUnknown *This);
  void __RPC_STUB IUnknown_AddRef_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IUnknown_Release_Proxy(IUnknown *This);
  void __RPC_STUB IUnknown_Release_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#else
  EXTERN_C const IID IID_IUnknown;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IUnknown {
  public:
    BEGIN_INTERFACE
      virtual HRESULT WINAPI QueryInterface(REFIID riid,void **ppvObject) = 0;
      virtual ULONG WINAPI AddRef(void) = 0;
      virtual ULONG WINAPI Release(void) = 0;
    END_INTERFACE
  };
#else
  typedef struct IUnknownVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IUnknown *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IUnknown *This);
      ULONG (WINAPI *Release)(IUnknown *This);
    END_INTERFACE
  } IUnknownVtbl;
  struct IUnknown {
    CONST_VTBL struct IUnknownVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IUnknown_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IUnknown_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IUnknown_Release(This) (This)->lpVtbl->Release(This)
#endif
#endif
  HRESULT WINAPI IUnknown_QueryInterface_Proxy(IUnknown *This,REFIID riid,void **ppvObject);
  void __RPC_STUB IUnknown_QueryInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IUnknown_AddRef_Proxy(IUnknown *This);
  void __RPC_STUB IUnknown_AddRef_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IUnknown_Release_Proxy(IUnknown *This);
  void __RPC_STUB IUnknown_Release_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

  extern RPC_IF_HANDLE __MIDL_itf_unknwn_0005_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_unknwn_0005_v0_0_s_ifspec;
#ifndef __AsyncIUnknown_INTERFACE_DEFINED__
#define __AsyncIUnknown_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIUnknown;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIUnknown : public IUnknown {
  public:
    virtual HRESULT WINAPI Begin_QueryInterface(REFIID riid) = 0;
    virtual HRESULT WINAPI Finish_QueryInterface(void **ppvObject) = 0;
    virtual HRESULT WINAPI Begin_AddRef(void) = 0;
    virtual ULONG WINAPI Finish_AddRef(void) = 0;
    virtual HRESULT WINAPI Begin_Release(void) = 0;
    virtual ULONG WINAPI Finish_Release(void) = 0;
  };
#else
  typedef struct AsyncIUnknownVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIUnknown *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIUnknown *This);
      ULONG (WINAPI *Release)(AsyncIUnknown *This);
      HRESULT (WINAPI *Begin_QueryInterface)(AsyncIUnknown *This,REFIID riid);
      HRESULT (WINAPI *Finish_QueryInterface)(AsyncIUnknown *This,void **ppvObject);
      HRESULT (WINAPI *Begin_AddRef)(AsyncIUnknown *This);
      ULONG (WINAPI *Finish_AddRef)(AsyncIUnknown *This);
      HRESULT (WINAPI *Begin_Release)(AsyncIUnknown *This);
      ULONG (WINAPI *Finish_Release)(AsyncIUnknown *This);
    END_INTERFACE
  } AsyncIUnknownVtbl;
  struct AsyncIUnknown {
    CONST_VTBL struct AsyncIUnknownVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIUnknown_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIUnknown_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIUnknown_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIUnknown_Begin_QueryInterface(This,riid) (This)->lpVtbl->Begin_QueryInterface(This,riid)
#define AsyncIUnknown_Finish_QueryInterface(This,ppvObject) (This)->lpVtbl->Finish_QueryInterface(This,ppvObject)
#define AsyncIUnknown_Begin_AddRef(This) (This)->lpVtbl->Begin_AddRef(This)
#define AsyncIUnknown_Finish_AddRef(This) (This)->lpVtbl->Finish_AddRef(This)
#define AsyncIUnknown_Begin_Release(This) (This)->lpVtbl->Begin_Release(This)
#define AsyncIUnknown_Finish_Release(This) (This)->lpVtbl->Finish_Release(This)
#endif
#endif
  HRESULT WINAPI AsyncIUnknown_Begin_QueryInterface_Proxy(AsyncIUnknown *This,REFIID riid);
  void __RPC_STUB AsyncIUnknown_Begin_QueryInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIUnknown_Finish_QueryInterface_Proxy(AsyncIUnknown *This,void **ppvObject);
  void __RPC_STUB AsyncIUnknown_Finish_QueryInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIUnknown_Begin_AddRef_Proxy(AsyncIUnknown *This);
  void __RPC_STUB AsyncIUnknown_Begin_AddRef_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI AsyncIUnknown_Finish_AddRef_Proxy(AsyncIUnknown *This);
  void __RPC_STUB AsyncIUnknown_Finish_AddRef_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIUnknown_Begin_Release_Proxy(AsyncIUnknown *This);
  void __RPC_STUB AsyncIUnknown_Begin_Release_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI AsyncIUnknown_Finish_Release_Proxy(AsyncIUnknown *This);
  void __RPC_STUB AsyncIUnknown_Finish_Release_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IClassFactory_INTERFACE_DEFINED__
#define __IClassFactory_INTERFACE_DEFINED__
  typedef IClassFactory *LPCLASSFACTORY;

  EXTERN_C const IID IID_IClassFactory;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IClassFactory : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateInstance(IUnknown *pUnkOuter,REFIID riid,void **ppvObject) = 0;
    virtual HRESULT WINAPI LockServer(WINBOOL fLock) = 0;
  };
#else
  typedef struct IClassFactoryVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IClassFactory *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IClassFactory *This);
      ULONG (WINAPI *Release)(IClassFactory *This);
      HRESULT (WINAPI *CreateInstance)(IClassFactory *This,IUnknown *pUnkOuter,REFIID riid,void **ppvObject);
      HRESULT (WINAPI *LockServer)(IClassFactory *This,WINBOOL fLock);
    END_INTERFACE
  } IClassFactoryVtbl;
  struct IClassFactory {
    CONST_VTBL struct IClassFactoryVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IClassFactory_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IClassFactory_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IClassFactory_Release(This) (This)->lpVtbl->Release(This)
#define IClassFactory_CreateInstance(This,pUnkOuter,riid,ppvObject) (This)->lpVtbl->CreateInstance(This,pUnkOuter,riid,ppvObject)
#define IClassFactory_LockServer(This,fLock) (This)->lpVtbl->LockServer(This,fLock)
#endif
#endif
  HRESULT WINAPI IClassFactory_RemoteCreateInstance_Proxy(IClassFactory *This,REFIID riid,IUnknown **ppvObject);
  void __RPC_STUB IClassFactory_RemoteCreateInstance_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IClassFactory_RemoteLockServer_Proxy(IClassFactory *This,WINBOOL fLock);
  void __RPC_STUB IClassFactory_RemoteLockServer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IClassFactory_CreateInstance_Proxy(IClassFactory *This,IUnknown *pUnkOuter,REFIID riid,void **ppvObject);
  HRESULT WINAPI IClassFactory_CreateInstance_Stub(IClassFactory *This,REFIID riid,IUnknown **ppvObject);
  HRESULT WINAPI IClassFactory_LockServer_Proxy(IClassFactory *This,WINBOOL fLock);
  HRESULT WINAPI IClassFactory_LockServer_Stub(IClassFactory *This,WINBOOL fLock);
#endif

#ifdef __cplusplus
}
#endif
#endif
