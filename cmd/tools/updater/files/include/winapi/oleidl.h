/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error This stub requires an updated version of <rpcndr.h>
#endif

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif

#ifndef __oleidl_h__
#define __oleidl_h__

#ifndef __IOleAdviseHolder_FWD_DEFINED__
#define __IOleAdviseHolder_FWD_DEFINED__
typedef struct IOleAdviseHolder IOleAdviseHolder;
#endif

#ifndef __IOleCache_FWD_DEFINED__
#define __IOleCache_FWD_DEFINED__
typedef struct IOleCache IOleCache;
#endif

#ifndef __IOleCache2_FWD_DEFINED__
#define __IOleCache2_FWD_DEFINED__
typedef struct IOleCache2 IOleCache2;
#endif

#ifndef __IOleCacheControl_FWD_DEFINED__
#define __IOleCacheControl_FWD_DEFINED__
typedef struct IOleCacheControl IOleCacheControl;
#endif

#ifndef __IParseDisplayName_FWD_DEFINED__
#define __IParseDisplayName_FWD_DEFINED__
typedef struct IParseDisplayName IParseDisplayName;
#endif

#ifndef __IOleContainer_FWD_DEFINED__
#define __IOleContainer_FWD_DEFINED__
typedef struct IOleContainer IOleContainer;
#endif

#ifndef __IOleClientSite_FWD_DEFINED__
#define __IOleClientSite_FWD_DEFINED__
typedef struct IOleClientSite IOleClientSite;
#endif

#ifndef __IOleObject_FWD_DEFINED__
#define __IOleObject_FWD_DEFINED__
typedef struct IOleObject IOleObject;
#endif

#ifndef __IOleWindow_FWD_DEFINED__
#define __IOleWindow_FWD_DEFINED__
typedef struct IOleWindow IOleWindow;
#endif

#ifndef __IOleLink_FWD_DEFINED__
#define __IOleLink_FWD_DEFINED__
typedef struct IOleLink IOleLink;
#endif

#ifndef __IOleItemContainer_FWD_DEFINED__
#define __IOleItemContainer_FWD_DEFINED__
typedef struct IOleItemContainer IOleItemContainer;
#endif

#ifndef __IOleInPlaceUIWindow_FWD_DEFINED__
#define __IOleInPlaceUIWindow_FWD_DEFINED__
typedef struct IOleInPlaceUIWindow IOleInPlaceUIWindow;
#endif

#ifndef __IOleInPlaceActiveObject_FWD_DEFINED__
#define __IOleInPlaceActiveObject_FWD_DEFINED__
typedef struct IOleInPlaceActiveObject IOleInPlaceActiveObject;
#endif

#ifndef __IOleInPlaceFrame_FWD_DEFINED__
#define __IOleInPlaceFrame_FWD_DEFINED__
typedef struct IOleInPlaceFrame IOleInPlaceFrame;
#endif

#ifndef __IOleInPlaceObject_FWD_DEFINED__
#define __IOleInPlaceObject_FWD_DEFINED__
typedef struct IOleInPlaceObject IOleInPlaceObject;
#endif

#ifndef __IOleInPlaceSite_FWD_DEFINED__
#define __IOleInPlaceSite_FWD_DEFINED__
typedef struct IOleInPlaceSite IOleInPlaceSite;
#endif

#ifndef __IContinue_FWD_DEFINED__
#define __IContinue_FWD_DEFINED__
typedef struct IContinue IContinue;
#endif

#ifndef __IViewObject_FWD_DEFINED__
#define __IViewObject_FWD_DEFINED__
typedef struct IViewObject IViewObject;
#endif

#ifndef __IViewObject2_FWD_DEFINED__
#define __IViewObject2_FWD_DEFINED__
typedef struct IViewObject2 IViewObject2;
#endif

#ifndef __IDropSource_FWD_DEFINED__
#define __IDropSource_FWD_DEFINED__
typedef struct IDropSource IDropSource;
#endif

#ifndef __IDropTarget_FWD_DEFINED__
#define __IDropTarget_FWD_DEFINED__
typedef struct IDropTarget IDropTarget;
#endif

#ifndef __IEnumOLEVERB_FWD_DEFINED__
#define __IEnumOLEVERB_FWD_DEFINED__
typedef struct IEnumOLEVERB IEnumOLEVERB;
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

  extern RPC_IF_HANDLE __MIDL_itf_oleidl_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_oleidl_0000_v0_0_s_ifspec;

#ifndef __IOleAdviseHolder_INTERFACE_DEFINED__
#define __IOleAdviseHolder_INTERFACE_DEFINED__

  typedef IOleAdviseHolder *LPOLEADVISEHOLDER;
  EXTERN_C const IID IID_IOleAdviseHolder;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleAdviseHolder : public IUnknown {
  public:
    virtual HRESULT WINAPI Advise(IAdviseSink *pAdvise,DWORD *pdwConnection) = 0;
    virtual HRESULT WINAPI Unadvise(DWORD dwConnection) = 0;
    virtual HRESULT WINAPI EnumAdvise(IEnumSTATDATA **ppenumAdvise) = 0;
    virtual HRESULT WINAPI SendOnRename(IMoniker *pmk) = 0;
    virtual HRESULT WINAPI SendOnSave(void) = 0;
    virtual HRESULT WINAPI SendOnClose(void) = 0;
  };
#else
  typedef struct IOleAdviseHolderVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleAdviseHolder *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleAdviseHolder *This);
      ULONG (WINAPI *Release)(IOleAdviseHolder *This);
      HRESULT (WINAPI *Advise)(IOleAdviseHolder *This,IAdviseSink *pAdvise,DWORD *pdwConnection);
      HRESULT (WINAPI *Unadvise)(IOleAdviseHolder *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumAdvise)(IOleAdviseHolder *This,IEnumSTATDATA **ppenumAdvise);
      HRESULT (WINAPI *SendOnRename)(IOleAdviseHolder *This,IMoniker *pmk);
      HRESULT (WINAPI *SendOnSave)(IOleAdviseHolder *This);
      HRESULT (WINAPI *SendOnClose)(IOleAdviseHolder *This);
    END_INTERFACE
  } IOleAdviseHolderVtbl;
  struct IOleAdviseHolder {
    CONST_VTBL struct IOleAdviseHolderVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleAdviseHolder_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleAdviseHolder_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleAdviseHolder_Release(This) (This)->lpVtbl->Release(This)
#define IOleAdviseHolder_Advise(This,pAdvise,pdwConnection) (This)->lpVtbl->Advise(This,pAdvise,pdwConnection)
#define IOleAdviseHolder_Unadvise(This,dwConnection) (This)->lpVtbl->Unadvise(This,dwConnection)
#define IOleAdviseHolder_EnumAdvise(This,ppenumAdvise) (This)->lpVtbl->EnumAdvise(This,ppenumAdvise)
#define IOleAdviseHolder_SendOnRename(This,pmk) (This)->lpVtbl->SendOnRename(This,pmk)
#define IOleAdviseHolder_SendOnSave(This) (This)->lpVtbl->SendOnSave(This)
#define IOleAdviseHolder_SendOnClose(This) (This)->lpVtbl->SendOnClose(This)
#endif
#endif
  HRESULT WINAPI IOleAdviseHolder_Advise_Proxy(IOleAdviseHolder *This,IAdviseSink *pAdvise,DWORD *pdwConnection);
  void __RPC_STUB IOleAdviseHolder_Advise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleAdviseHolder_Unadvise_Proxy(IOleAdviseHolder *This,DWORD dwConnection);
  void __RPC_STUB IOleAdviseHolder_Unadvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleAdviseHolder_EnumAdvise_Proxy(IOleAdviseHolder *This,IEnumSTATDATA **ppenumAdvise);
  void __RPC_STUB IOleAdviseHolder_EnumAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleAdviseHolder_SendOnRename_Proxy(IOleAdviseHolder *This,IMoniker *pmk);
  void __RPC_STUB IOleAdviseHolder_SendOnRename_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleAdviseHolder_SendOnSave_Proxy(IOleAdviseHolder *This);
  void __RPC_STUB IOleAdviseHolder_SendOnSave_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleAdviseHolder_SendOnClose_Proxy(IOleAdviseHolder *This);
  void __RPC_STUB IOleAdviseHolder_SendOnClose_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleCache_INTERFACE_DEFINED__
#define __IOleCache_INTERFACE_DEFINED__
  typedef IOleCache *LPOLECACHE;

  EXTERN_C const IID IID_IOleCache;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleCache : public IUnknown {
  public:
    virtual HRESULT WINAPI Cache(FORMATETC *pformatetc,DWORD advf,DWORD *pdwConnection) = 0;
    virtual HRESULT WINAPI Uncache(DWORD dwConnection) = 0;
    virtual HRESULT WINAPI EnumCache(IEnumSTATDATA **ppenumSTATDATA) = 0;
    virtual HRESULT WINAPI InitCache(IDataObject *pDataObject) = 0;
    virtual HRESULT WINAPI SetData(FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease) = 0;
  };
#else
  typedef struct IOleCacheVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleCache *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleCache *This);
      ULONG (WINAPI *Release)(IOleCache *This);
      HRESULT (WINAPI *Cache)(IOleCache *This,FORMATETC *pformatetc,DWORD advf,DWORD *pdwConnection);
      HRESULT (WINAPI *Uncache)(IOleCache *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumCache)(IOleCache *This,IEnumSTATDATA **ppenumSTATDATA);
      HRESULT (WINAPI *InitCache)(IOleCache *This,IDataObject *pDataObject);
      HRESULT (WINAPI *SetData)(IOleCache *This,FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease);
    END_INTERFACE
  } IOleCacheVtbl;
  struct IOleCache {
    CONST_VTBL struct IOleCacheVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleCache_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleCache_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleCache_Release(This) (This)->lpVtbl->Release(This)
#define IOleCache_Cache(This,pformatetc,advf,pdwConnection) (This)->lpVtbl->Cache(This,pformatetc,advf,pdwConnection)
#define IOleCache_Uncache(This,dwConnection) (This)->lpVtbl->Uncache(This,dwConnection)
#define IOleCache_EnumCache(This,ppenumSTATDATA) (This)->lpVtbl->EnumCache(This,ppenumSTATDATA)
#define IOleCache_InitCache(This,pDataObject) (This)->lpVtbl->InitCache(This,pDataObject)
#define IOleCache_SetData(This,pformatetc,pmedium,fRelease) (This)->lpVtbl->SetData(This,pformatetc,pmedium,fRelease)
#endif
#endif
  HRESULT WINAPI IOleCache_Cache_Proxy(IOleCache *This,FORMATETC *pformatetc,DWORD advf,DWORD *pdwConnection);
  void __RPC_STUB IOleCache_Cache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCache_Uncache_Proxy(IOleCache *This,DWORD dwConnection);
  void __RPC_STUB IOleCache_Uncache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCache_EnumCache_Proxy(IOleCache *This,IEnumSTATDATA **ppenumSTATDATA);
  void __RPC_STUB IOleCache_EnumCache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCache_InitCache_Proxy(IOleCache *This,IDataObject *pDataObject);
  void __RPC_STUB IOleCache_InitCache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCache_SetData_Proxy(IOleCache *This,FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease);
  void __RPC_STUB IOleCache_SetData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleCache2_INTERFACE_DEFINED__
#define __IOleCache2_INTERFACE_DEFINED__
  typedef IOleCache2 *LPOLECACHE2;

#define UPDFCACHE_NODATACACHE (0x1)
#define UPDFCACHE_ONSAVECACHE (0x2)
#define UPDFCACHE_ONSTOPCACHE (0x4)
#define UPDFCACHE_NORMALCACHE (0x8)
#define UPDFCACHE_IFBLANK (0x10)
#define UPDFCACHE_ONLYIFBLANK (0x80000000)
#define UPDFCACHE_IFBLANKORONSAVECACHE (UPDFCACHE_IFBLANK | UPDFCACHE_ONSAVECACHE)
#define UPDFCACHE_ALL ((DWORD)~UPDFCACHE_ONLYIFBLANK)
#define UPDFCACHE_ALLBUTNODATACACHE (UPDFCACHE_ALL & (DWORD)~UPDFCACHE_NODATACACHE)

  typedef enum tagDISCARDCACHE {
    DISCARDCACHE_SAVEIFDIRTY = 0,DISCARDCACHE_NOSAVE = 1
  } DISCARDCACHE;

  EXTERN_C const IID IID_IOleCache2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleCache2 : public IOleCache {
  public:
    virtual HRESULT WINAPI UpdateCache(LPDATAOBJECT pDataObject,DWORD grfUpdf,LPVOID pReserved) = 0;
    virtual HRESULT WINAPI DiscardCache(DWORD dwDiscardOptions) = 0;
  };
#else
  typedef struct IOleCache2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleCache2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleCache2 *This);
      ULONG (WINAPI *Release)(IOleCache2 *This);
      HRESULT (WINAPI *Cache)(IOleCache2 *This,FORMATETC *pformatetc,DWORD advf,DWORD *pdwConnection);
      HRESULT (WINAPI *Uncache)(IOleCache2 *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumCache)(IOleCache2 *This,IEnumSTATDATA **ppenumSTATDATA);
      HRESULT (WINAPI *InitCache)(IOleCache2 *This,IDataObject *pDataObject);
      HRESULT (WINAPI *SetData)(IOleCache2 *This,FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease);
      HRESULT (WINAPI *UpdateCache)(IOleCache2 *This,LPDATAOBJECT pDataObject,DWORD grfUpdf,LPVOID pReserved);
      HRESULT (WINAPI *DiscardCache)(IOleCache2 *This,DWORD dwDiscardOptions);
    END_INTERFACE
  } IOleCache2Vtbl;
  struct IOleCache2 {
    CONST_VTBL struct IOleCache2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleCache2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleCache2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleCache2_Release(This) (This)->lpVtbl->Release(This)
#define IOleCache2_Cache(This,pformatetc,advf,pdwConnection) (This)->lpVtbl->Cache(This,pformatetc,advf,pdwConnection)
#define IOleCache2_Uncache(This,dwConnection) (This)->lpVtbl->Uncache(This,dwConnection)
#define IOleCache2_EnumCache(This,ppenumSTATDATA) (This)->lpVtbl->EnumCache(This,ppenumSTATDATA)
#define IOleCache2_InitCache(This,pDataObject) (This)->lpVtbl->InitCache(This,pDataObject)
#define IOleCache2_SetData(This,pformatetc,pmedium,fRelease) (This)->lpVtbl->SetData(This,pformatetc,pmedium,fRelease)
#define IOleCache2_UpdateCache(This,pDataObject,grfUpdf,pReserved) (This)->lpVtbl->UpdateCache(This,pDataObject,grfUpdf,pReserved)
#define IOleCache2_DiscardCache(This,dwDiscardOptions) (This)->lpVtbl->DiscardCache(This,dwDiscardOptions)
#endif
#endif
  HRESULT WINAPI IOleCache2_RemoteUpdateCache_Proxy(IOleCache2 *This,LPDATAOBJECT pDataObject,DWORD grfUpdf,LONG_PTR pReserved);
  void __RPC_STUB IOleCache2_RemoteUpdateCache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCache2_DiscardCache_Proxy(IOleCache2 *This,DWORD dwDiscardOptions);
  void __RPC_STUB IOleCache2_DiscardCache_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleCacheControl_INTERFACE_DEFINED__
#define __IOleCacheControl_INTERFACE_DEFINED__
  typedef IOleCacheControl *LPOLECACHECONTROL;
  EXTERN_C const IID IID_IOleCacheControl;

#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleCacheControl : public IUnknown {
  public:
    virtual HRESULT WINAPI OnRun(LPDATAOBJECT pDataObject) = 0;
    virtual HRESULT WINAPI OnStop(void) = 0;
  };
#else
  typedef struct IOleCacheControlVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleCacheControl *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleCacheControl *This);
      ULONG (WINAPI *Release)(IOleCacheControl *This);
      HRESULT (WINAPI *OnRun)(IOleCacheControl *This,LPDATAOBJECT pDataObject);
      HRESULT (WINAPI *OnStop)(IOleCacheControl *This);
    END_INTERFACE
  } IOleCacheControlVtbl;
  struct IOleCacheControl {
    CONST_VTBL struct IOleCacheControlVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleCacheControl_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleCacheControl_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleCacheControl_Release(This) (This)->lpVtbl->Release(This)
#define IOleCacheControl_OnRun(This,pDataObject) (This)->lpVtbl->OnRun(This,pDataObject)
#define IOleCacheControl_OnStop(This) (This)->lpVtbl->OnStop(This)
#endif
#endif
  HRESULT WINAPI IOleCacheControl_OnRun_Proxy(IOleCacheControl *This,LPDATAOBJECT pDataObject);
  void __RPC_STUB IOleCacheControl_OnRun_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleCacheControl_OnStop_Proxy(IOleCacheControl *This);
  void __RPC_STUB IOleCacheControl_OnStop_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IParseDisplayName_INTERFACE_DEFINED__
#define __IParseDisplayName_INTERFACE_DEFINED__
  typedef IParseDisplayName *LPPARSEDISPLAYNAME;

  EXTERN_C const IID IID_IParseDisplayName;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IParseDisplayName : public IUnknown {
  public:
    virtual HRESULT WINAPI ParseDisplayName(IBindCtx *pbc,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut) = 0;
  };
#else
  typedef struct IParseDisplayNameVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IParseDisplayName *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IParseDisplayName *This);
      ULONG (WINAPI *Release)(IParseDisplayName *This);
      HRESULT (WINAPI *ParseDisplayName)(IParseDisplayName *This,IBindCtx *pbc,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
    END_INTERFACE
  } IParseDisplayNameVtbl;
  struct IParseDisplayName {
    CONST_VTBL struct IParseDisplayNameVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IParseDisplayName_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IParseDisplayName_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IParseDisplayName_Release(This) (This)->lpVtbl->Release(This)
#define IParseDisplayName_ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut) (This)->lpVtbl->ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut)
#endif
#endif
  HRESULT WINAPI IParseDisplayName_ParseDisplayName_Proxy(IParseDisplayName *This,IBindCtx *pbc,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
  void __RPC_STUB IParseDisplayName_ParseDisplayName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleContainer_INTERFACE_DEFINED__
#define __IOleContainer_INTERFACE_DEFINED__
  typedef IOleContainer *LPOLECONTAINER;

  EXTERN_C const IID IID_IOleContainer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleContainer : public IParseDisplayName {
  public:
    virtual HRESULT WINAPI EnumObjects(DWORD grfFlags,IEnumUnknown **ppenum) = 0;
    virtual HRESULT WINAPI LockContainer(WINBOOL fLock) = 0;
  };
#else
  typedef struct IOleContainerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleContainer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleContainer *This);
      ULONG (WINAPI *Release)(IOleContainer *This);
      HRESULT (WINAPI *ParseDisplayName)(IOleContainer *This,IBindCtx *pbc,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
      HRESULT (WINAPI *EnumObjects)(IOleContainer *This,DWORD grfFlags,IEnumUnknown **ppenum);
      HRESULT (WINAPI *LockContainer)(IOleContainer *This,WINBOOL fLock);
    END_INTERFACE
  } IOleContainerVtbl;
  struct IOleContainer {
    CONST_VTBL struct IOleContainerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleContainer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleContainer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleContainer_Release(This) (This)->lpVtbl->Release(This)
#define IOleContainer_ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut) (This)->lpVtbl->ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut)
#define IOleContainer_EnumObjects(This,grfFlags,ppenum) (This)->lpVtbl->EnumObjects(This,grfFlags,ppenum)
#define IOleContainer_LockContainer(This,fLock) (This)->lpVtbl->LockContainer(This,fLock)
#endif
#endif
  HRESULT WINAPI IOleContainer_EnumObjects_Proxy(IOleContainer *This,DWORD grfFlags,IEnumUnknown **ppenum);
  void __RPC_STUB IOleContainer_EnumObjects_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleContainer_LockContainer_Proxy(IOleContainer *This,WINBOOL fLock);
  void __RPC_STUB IOleContainer_LockContainer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleClientSite_INTERFACE_DEFINED__
#define __IOleClientSite_INTERFACE_DEFINED__
  typedef IOleClientSite *LPOLECLIENTSITE;

  EXTERN_C const IID IID_IOleClientSite;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleClientSite : public IUnknown {
  public:
    virtual HRESULT WINAPI SaveObject(void) = 0;
    virtual HRESULT WINAPI GetMoniker(DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk) = 0;
    virtual HRESULT WINAPI GetContainer(IOleContainer **ppContainer) = 0;
    virtual HRESULT WINAPI ShowObject(void) = 0;
    virtual HRESULT WINAPI OnShowWindow(WINBOOL fShow) = 0;
    virtual HRESULT WINAPI RequestNewObjectLayout(void) = 0;
  };
#else
  typedef struct IOleClientSiteVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleClientSite *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleClientSite *This);
      ULONG (WINAPI *Release)(IOleClientSite *This);
      HRESULT (WINAPI *SaveObject)(IOleClientSite *This);
      HRESULT (WINAPI *GetMoniker)(IOleClientSite *This,DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk);
      HRESULT (WINAPI *GetContainer)(IOleClientSite *This,IOleContainer **ppContainer);
      HRESULT (WINAPI *ShowObject)(IOleClientSite *This);
      HRESULT (WINAPI *OnShowWindow)(IOleClientSite *This,WINBOOL fShow);
      HRESULT (WINAPI *RequestNewObjectLayout)(IOleClientSite *This);
    END_INTERFACE
  } IOleClientSiteVtbl;
  struct IOleClientSite {
    CONST_VTBL struct IOleClientSiteVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleClientSite_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleClientSite_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleClientSite_Release(This) (This)->lpVtbl->Release(This)
#define IOleClientSite_SaveObject(This) (This)->lpVtbl->SaveObject(This)
#define IOleClientSite_GetMoniker(This,dwAssign,dwWhichMoniker,ppmk) (This)->lpVtbl->GetMoniker(This,dwAssign,dwWhichMoniker,ppmk)
#define IOleClientSite_GetContainer(This,ppContainer) (This)->lpVtbl->GetContainer(This,ppContainer)
#define IOleClientSite_ShowObject(This) (This)->lpVtbl->ShowObject(This)
#define IOleClientSite_OnShowWindow(This,fShow) (This)->lpVtbl->OnShowWindow(This,fShow)
#define IOleClientSite_RequestNewObjectLayout(This) (This)->lpVtbl->RequestNewObjectLayout(This)
#endif
#endif
  HRESULT WINAPI IOleClientSite_SaveObject_Proxy(IOleClientSite *This);
  void __RPC_STUB IOleClientSite_SaveObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleClientSite_GetMoniker_Proxy(IOleClientSite *This,DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk);
  void __RPC_STUB IOleClientSite_GetMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleClientSite_GetContainer_Proxy(IOleClientSite *This,IOleContainer **ppContainer);
  void __RPC_STUB IOleClientSite_GetContainer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleClientSite_ShowObject_Proxy(IOleClientSite *This);
  void __RPC_STUB IOleClientSite_ShowObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleClientSite_OnShowWindow_Proxy(IOleClientSite *This,WINBOOL fShow);
  void __RPC_STUB IOleClientSite_OnShowWindow_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleClientSite_RequestNewObjectLayout_Proxy(IOleClientSite *This);
  void __RPC_STUB IOleClientSite_RequestNewObjectLayout_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleObject_INTERFACE_DEFINED__
#define __IOleObject_INTERFACE_DEFINED__
#ifndef _LPOLEOBJECT_DEFINED
#define _LPOLEOBJECT_DEFINED
  typedef IOleObject *LPOLEOBJECT;
#endif

  typedef enum tagOLEGETMONIKER {
    OLEGETMONIKER_ONLYIFTHERE = 1,OLEGETMONIKER_FORCEASSIGN = 2,OLEGETMONIKER_UNASSIGN = 3,OLEGETMONIKER_TEMPFORUSER = 4
  } OLEGETMONIKER;
  typedef enum tagOLEWHICHMK {
    OLEWHICHMK_CONTAINER = 1,OLEWHICHMK_OBJREL = 2,OLEWHICHMK_OBJFULL = 3
  } OLEWHICHMK;

  typedef enum tagUSERCLASSTYPE {
    USERCLASSTYPE_FULL = 1,USERCLASSTYPE_SHORT = 2,USERCLASSTYPE_APPNAME = 3
  } USERCLASSTYPE;

  typedef enum tagOLEMISC {
    OLEMISC_RECOMPOSEONRESIZE = 0x1,OLEMISC_ONLYICONIC = 0x2,OLEMISC_INSERTNOTREPLACE = 0x4,OLEMISC_STATIC = 0x8,OLEMISC_CANTLINKINSIDE = 0x10,
    OLEMISC_CANLINKBYOLE1 = 0x20,OLEMISC_ISLINKOBJECT = 0x40,OLEMISC_INSIDEOUT = 0x80,OLEMISC_ACTIVATEWHENVISIBLE = 0x100,
    OLEMISC_RENDERINGISDEVICEINDEPENDENT = 0x200,OLEMISC_INVISIBLEATRUNTIME = 0x400,OLEMISC_ALWAYSRUN = 0x800,OLEMISC_ACTSLIKEBUTTON = 0x1000,
    OLEMISC_ACTSLIKELABEL = 0x2000,OLEMISC_NOUIACTIVATE = 0x4000,OLEMISC_ALIGNABLE = 0x8000,OLEMISC_SIMPLEFRAME = 0x10000,
    OLEMISC_SETCLIENTSITEFIRST = 0x20000,OLEMISC_IMEMODE = 0x40000,OLEMISC_IGNOREACTIVATEWHENVISIBLE = 0x80000,OLEMISC_WANTSTOMENUMERGE = 0x100000,
    OLEMISC_SUPPORTSMULTILEVELUNDO = 0x200000
  } OLEMISC;
  typedef enum tagOLECLOSE {
    OLECLOSE_SAVEIFDIRTY = 0,OLECLOSE_NOSAVE = 1,OLECLOSE_PROMPTSAVE = 2
  } OLECLOSE;

  EXTERN_C const IID IID_IOleObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleObject : public IUnknown {
  public:
    virtual HRESULT WINAPI SetClientSite(IOleClientSite *pClientSite) = 0;
    virtual HRESULT WINAPI GetClientSite(IOleClientSite **ppClientSite) = 0;
    virtual HRESULT WINAPI SetHostNames(LPCOLESTR szContainerApp,LPCOLESTR szContainerObj) = 0;
    virtual HRESULT WINAPI Close(DWORD dwSaveOption) = 0;
    virtual HRESULT WINAPI SetMoniker(DWORD dwWhichMoniker,IMoniker *pmk) = 0;
    virtual HRESULT WINAPI GetMoniker(DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk) = 0;
    virtual HRESULT WINAPI InitFromData(IDataObject *pDataObject,WINBOOL fCreation,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI GetClipboardData(DWORD dwReserved,IDataObject **ppDataObject) = 0;
    virtual HRESULT WINAPI DoVerb(LONG iVerb,LPMSG lpmsg,IOleClientSite *pActiveSite,LONG lindex,HWND hwndParent,LPCRECT lprcPosRect) = 0;
    virtual HRESULT WINAPI EnumVerbs(IEnumOLEVERB **ppEnumOleVerb) = 0;
    virtual HRESULT WINAPI Update(void) = 0;
    virtual HRESULT WINAPI IsUpToDate(void) = 0;
    virtual HRESULT WINAPI GetUserClassID(CLSID *pClsid) = 0;
    virtual HRESULT WINAPI GetUserType(DWORD dwFormOfType,LPOLESTR *pszUserType) = 0;
    virtual HRESULT WINAPI SetExtent(DWORD dwDrawAspect,SIZEL *psizel) = 0;
    virtual HRESULT WINAPI GetExtent(DWORD dwDrawAspect,SIZEL *psizel) = 0;
    virtual HRESULT WINAPI Advise(IAdviseSink *pAdvSink,DWORD *pdwConnection) = 0;
    virtual HRESULT WINAPI Unadvise(DWORD dwConnection) = 0;
    virtual HRESULT WINAPI EnumAdvise(IEnumSTATDATA **ppenumAdvise) = 0;
    virtual HRESULT WINAPI GetMiscStatus(DWORD dwAspect,DWORD *pdwStatus) = 0;
    virtual HRESULT WINAPI SetColorScheme(LOGPALETTE *pLogpal) = 0;
  };
#else
  typedef struct IOleObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleObject *This);
      ULONG (WINAPI *Release)(IOleObject *This);
      HRESULT (WINAPI *SetClientSite)(IOleObject *This,IOleClientSite *pClientSite);
      HRESULT (WINAPI *GetClientSite)(IOleObject *This,IOleClientSite **ppClientSite);
      HRESULT (WINAPI *SetHostNames)(IOleObject *This,LPCOLESTR szContainerApp,LPCOLESTR szContainerObj);
      HRESULT (WINAPI *Close)(IOleObject *This,DWORD dwSaveOption);
      HRESULT (WINAPI *SetMoniker)(IOleObject *This,DWORD dwWhichMoniker,IMoniker *pmk);
      HRESULT (WINAPI *GetMoniker)(IOleObject *This,DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk);
      HRESULT (WINAPI *InitFromData)(IOleObject *This,IDataObject *pDataObject,WINBOOL fCreation,DWORD dwReserved);
      HRESULT (WINAPI *GetClipboardData)(IOleObject *This,DWORD dwReserved,IDataObject **ppDataObject);
      HRESULT (WINAPI *DoVerb)(IOleObject *This,LONG iVerb,LPMSG lpmsg,IOleClientSite *pActiveSite,LONG lindex,HWND hwndParent,LPCRECT lprcPosRect);
      HRESULT (WINAPI *EnumVerbs)(IOleObject *This,IEnumOLEVERB **ppEnumOleVerb);
      HRESULT (WINAPI *Update)(IOleObject *This);
      HRESULT (WINAPI *IsUpToDate)(IOleObject *This);
      HRESULT (WINAPI *GetUserClassID)(IOleObject *This,CLSID *pClsid);
      HRESULT (WINAPI *GetUserType)(IOleObject *This,DWORD dwFormOfType,LPOLESTR *pszUserType);
      HRESULT (WINAPI *SetExtent)(IOleObject *This,DWORD dwDrawAspect,SIZEL *psizel);
      HRESULT (WINAPI *GetExtent)(IOleObject *This,DWORD dwDrawAspect,SIZEL *psizel);
      HRESULT (WINAPI *Advise)(IOleObject *This,IAdviseSink *pAdvSink,DWORD *pdwConnection);
      HRESULT (WINAPI *Unadvise)(IOleObject *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumAdvise)(IOleObject *This,IEnumSTATDATA **ppenumAdvise);
      HRESULT (WINAPI *GetMiscStatus)(IOleObject *This,DWORD dwAspect,DWORD *pdwStatus);
      HRESULT (WINAPI *SetColorScheme)(IOleObject *This,LOGPALETTE *pLogpal);
    END_INTERFACE
  } IOleObjectVtbl;
  struct IOleObject {
    CONST_VTBL struct IOleObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleObject_Release(This) (This)->lpVtbl->Release(This)
#define IOleObject_SetClientSite(This,pClientSite) (This)->lpVtbl->SetClientSite(This,pClientSite)
#define IOleObject_GetClientSite(This,ppClientSite) (This)->lpVtbl->GetClientSite(This,ppClientSite)
#define IOleObject_SetHostNames(This,szContainerApp,szContainerObj) (This)->lpVtbl->SetHostNames(This,szContainerApp,szContainerObj)
#define IOleObject_Close(This,dwSaveOption) (This)->lpVtbl->Close(This,dwSaveOption)
#define IOleObject_SetMoniker(This,dwWhichMoniker,pmk) (This)->lpVtbl->SetMoniker(This,dwWhichMoniker,pmk)
#define IOleObject_GetMoniker(This,dwAssign,dwWhichMoniker,ppmk) (This)->lpVtbl->GetMoniker(This,dwAssign,dwWhichMoniker,ppmk)
#define IOleObject_InitFromData(This,pDataObject,fCreation,dwReserved) (This)->lpVtbl->InitFromData(This,pDataObject,fCreation,dwReserved)
#define IOleObject_GetClipboardData(This,dwReserved,ppDataObject) (This)->lpVtbl->GetClipboardData(This,dwReserved,ppDataObject)
#define IOleObject_DoVerb(This,iVerb,lpmsg,pActiveSite,lindex,hwndParent,lprcPosRect) (This)->lpVtbl->DoVerb(This,iVerb,lpmsg,pActiveSite,lindex,hwndParent,lprcPosRect)
#define IOleObject_EnumVerbs(This,ppEnumOleVerb) (This)->lpVtbl->EnumVerbs(This,ppEnumOleVerb)
#define IOleObject_Update(This) (This)->lpVtbl->Update(This)
#define IOleObject_IsUpToDate(This) (This)->lpVtbl->IsUpToDate(This)
#define IOleObject_GetUserClassID(This,pClsid) (This)->lpVtbl->GetUserClassID(This,pClsid)
#define IOleObject_GetUserType(This,dwFormOfType,pszUserType) (This)->lpVtbl->GetUserType(This,dwFormOfType,pszUserType)
#define IOleObject_SetExtent(This,dwDrawAspect,psizel) (This)->lpVtbl->SetExtent(This,dwDrawAspect,psizel)
#define IOleObject_GetExtent(This,dwDrawAspect,psizel) (This)->lpVtbl->GetExtent(This,dwDrawAspect,psizel)
#define IOleObject_Advise(This,pAdvSink,pdwConnection) (This)->lpVtbl->Advise(This,pAdvSink,pdwConnection)
#define IOleObject_Unadvise(This,dwConnection) (This)->lpVtbl->Unadvise(This,dwConnection)
#define IOleObject_EnumAdvise(This,ppenumAdvise) (This)->lpVtbl->EnumAdvise(This,ppenumAdvise)
#define IOleObject_GetMiscStatus(This,dwAspect,pdwStatus) (This)->lpVtbl->GetMiscStatus(This,dwAspect,pdwStatus)
#define IOleObject_SetColorScheme(This,pLogpal) (This)->lpVtbl->SetColorScheme(This,pLogpal)
#endif
#endif
  HRESULT WINAPI IOleObject_SetClientSite_Proxy(IOleObject *This,IOleClientSite *pClientSite);
  void __RPC_STUB IOleObject_SetClientSite_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetClientSite_Proxy(IOleObject *This,IOleClientSite **ppClientSite);
  void __RPC_STUB IOleObject_GetClientSite_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_SetHostNames_Proxy(IOleObject *This,LPCOLESTR szContainerApp,LPCOLESTR szContainerObj);
  void __RPC_STUB IOleObject_SetHostNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_Close_Proxy(IOleObject *This,DWORD dwSaveOption);
  void __RPC_STUB IOleObject_Close_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_SetMoniker_Proxy(IOleObject *This,DWORD dwWhichMoniker,IMoniker *pmk);
  void __RPC_STUB IOleObject_SetMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetMoniker_Proxy(IOleObject *This,DWORD dwAssign,DWORD dwWhichMoniker,IMoniker **ppmk);
  void __RPC_STUB IOleObject_GetMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_InitFromData_Proxy(IOleObject *This,IDataObject *pDataObject,WINBOOL fCreation,DWORD dwReserved);
  void __RPC_STUB IOleObject_InitFromData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetClipboardData_Proxy(IOleObject *This,DWORD dwReserved,IDataObject **ppDataObject);
  void __RPC_STUB IOleObject_GetClipboardData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_DoVerb_Proxy(IOleObject *This,LONG iVerb,LPMSG lpmsg,IOleClientSite *pActiveSite,LONG lindex,HWND hwndParent,LPCRECT lprcPosRect);
  void __RPC_STUB IOleObject_DoVerb_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_EnumVerbs_Proxy(IOleObject *This,IEnumOLEVERB **ppEnumOleVerb);
  void __RPC_STUB IOleObject_EnumVerbs_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_Update_Proxy(IOleObject *This);
  void __RPC_STUB IOleObject_Update_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_IsUpToDate_Proxy(IOleObject *This);
  void __RPC_STUB IOleObject_IsUpToDate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetUserClassID_Proxy(IOleObject *This,CLSID *pClsid);
  void __RPC_STUB IOleObject_GetUserClassID_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetUserType_Proxy(IOleObject *This,DWORD dwFormOfType,LPOLESTR *pszUserType);
  void __RPC_STUB IOleObject_GetUserType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_SetExtent_Proxy(IOleObject *This,DWORD dwDrawAspect,SIZEL *psizel);
  void __RPC_STUB IOleObject_SetExtent_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetExtent_Proxy(IOleObject *This,DWORD dwDrawAspect,SIZEL *psizel);
  void __RPC_STUB IOleObject_GetExtent_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_Advise_Proxy(IOleObject *This,IAdviseSink *pAdvSink,DWORD *pdwConnection);
  void __RPC_STUB IOleObject_Advise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_Unadvise_Proxy(IOleObject *This,DWORD dwConnection);
  void __RPC_STUB IOleObject_Unadvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_EnumAdvise_Proxy(IOleObject *This,IEnumSTATDATA **ppenumAdvise);
  void __RPC_STUB IOleObject_EnumAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_GetMiscStatus_Proxy(IOleObject *This,DWORD dwAspect,DWORD *pdwStatus);
  void __RPC_STUB IOleObject_GetMiscStatus_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleObject_SetColorScheme_Proxy(IOleObject *This,LOGPALETTE *pLogpal);
  void __RPC_STUB IOleObject_SetColorScheme_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOLETypes_INTERFACE_DEFINED__
#define __IOLETypes_INTERFACE_DEFINED__
  typedef enum tagOLERENDER {
    OLERENDER_NONE = 0,OLERENDER_DRAW = 1,OLERENDER_FORMAT = 2,OLERENDER_ASIS = 3
  } OLERENDER;

  typedef OLERENDER *LPOLERENDER;

  typedef struct tagOBJECTDESCRIPTOR {
    ULONG cbSize;
    CLSID clsid;
    DWORD dwDrawAspect;
    SIZEL sizel;
    POINTL pointl;
    DWORD dwStatus;
    DWORD dwFullUserTypeName;
    DWORD dwSrcOfCopy;
  } OBJECTDESCRIPTOR;

  typedef struct tagOBJECTDESCRIPTOR *POBJECTDESCRIPTOR;
  typedef struct tagOBJECTDESCRIPTOR *LPOBJECTDESCRIPTOR;
  typedef struct tagOBJECTDESCRIPTOR LINKSRCDESCRIPTOR;
  typedef struct tagOBJECTDESCRIPTOR *PLINKSRCDESCRIPTOR;
  typedef struct tagOBJECTDESCRIPTOR *LPLINKSRCDESCRIPTOR;

  extern RPC_IF_HANDLE IOLETypes_v0_0_c_ifspec;
  extern RPC_IF_HANDLE IOLETypes_v0_0_s_ifspec;
#endif

#ifndef __IOleWindow_INTERFACE_DEFINED__
#define __IOleWindow_INTERFACE_DEFINED__
  typedef IOleWindow *LPOLEWINDOW;

  EXTERN_C const IID IID_IOleWindow;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleWindow : public IUnknown {
  public:
    virtual HRESULT WINAPI GetWindow(HWND *phwnd) = 0;
    virtual HRESULT WINAPI ContextSensitiveHelp(WINBOOL fEnterMode) = 0;
  };
#else
  typedef struct IOleWindowVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleWindow *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleWindow *This);
      ULONG (WINAPI *Release)(IOleWindow *This);
      HRESULT (WINAPI *GetWindow)(IOleWindow *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleWindow *This,WINBOOL fEnterMode);
    END_INTERFACE
  } IOleWindowVtbl;
  struct IOleWindow {
    CONST_VTBL struct IOleWindowVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleWindow_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleWindow_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleWindow_Release(This) (This)->lpVtbl->Release(This)
#define IOleWindow_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleWindow_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#endif
#endif
  HRESULT WINAPI IOleWindow_GetWindow_Proxy(IOleWindow *This,HWND *phwnd);
  void __RPC_STUB IOleWindow_GetWindow_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleWindow_ContextSensitiveHelp_Proxy(IOleWindow *This,WINBOOL fEnterMode);
  void __RPC_STUB IOleWindow_ContextSensitiveHelp_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleLink_INTERFACE_DEFINED__
#define __IOleLink_INTERFACE_DEFINED__
  typedef IOleLink *LPOLELINK;

  typedef enum tagOLEUPDATE {
    OLEUPDATE_ALWAYS = 1,OLEUPDATE_ONCALL = 3
  } OLEUPDATE;

  typedef OLEUPDATE *LPOLEUPDATE;
  typedef OLEUPDATE *POLEUPDATE;

  typedef enum tagOLELINKBIND {
    OLELINKBIND_EVENIFCLASSDIFF = 1
  } OLELINKBIND;

  EXTERN_C const IID IID_IOleLink;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleLink : public IUnknown {
  public:
    virtual HRESULT WINAPI SetUpdateOptions(DWORD dwUpdateOpt) = 0;
    virtual HRESULT WINAPI GetUpdateOptions(DWORD *pdwUpdateOpt) = 0;
    virtual HRESULT WINAPI SetSourceMoniker(IMoniker *pmk,REFCLSID rclsid) = 0;
    virtual HRESULT WINAPI GetSourceMoniker(IMoniker **ppmk) = 0;
    virtual HRESULT WINAPI SetSourceDisplayName(LPCOLESTR pszStatusText) = 0;
    virtual HRESULT WINAPI GetSourceDisplayName(LPOLESTR *ppszDisplayName) = 0;
    virtual HRESULT WINAPI BindToSource(DWORD bindflags,IBindCtx *pbc) = 0;
    virtual HRESULT WINAPI BindIfRunning(void) = 0;
    virtual HRESULT WINAPI GetBoundSource(IUnknown **ppunk) = 0;
    virtual HRESULT WINAPI UnbindSource(void) = 0;
    virtual HRESULT WINAPI Update(IBindCtx *pbc) = 0;
  };
#else
  typedef struct IOleLinkVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleLink *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleLink *This);
      ULONG (WINAPI *Release)(IOleLink *This);
      HRESULT (WINAPI *SetUpdateOptions)(IOleLink *This,DWORD dwUpdateOpt);
      HRESULT (WINAPI *GetUpdateOptions)(IOleLink *This,DWORD *pdwUpdateOpt);
      HRESULT (WINAPI *SetSourceMoniker)(IOleLink *This,IMoniker *pmk,REFCLSID rclsid);
      HRESULT (WINAPI *GetSourceMoniker)(IOleLink *This,IMoniker **ppmk);
      HRESULT (WINAPI *SetSourceDisplayName)(IOleLink *This,LPCOLESTR pszStatusText);
      HRESULT (WINAPI *GetSourceDisplayName)(IOleLink *This,LPOLESTR *ppszDisplayName);
      HRESULT (WINAPI *BindToSource)(IOleLink *This,DWORD bindflags,IBindCtx *pbc);
      HRESULT (WINAPI *BindIfRunning)(IOleLink *This);
      HRESULT (WINAPI *GetBoundSource)(IOleLink *This,IUnknown **ppunk);
      HRESULT (WINAPI *UnbindSource)(IOleLink *This);
      HRESULT (WINAPI *Update)(IOleLink *This,IBindCtx *pbc);
    END_INTERFACE
  } IOleLinkVtbl;
  struct IOleLink {
    CONST_VTBL struct IOleLinkVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleLink_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleLink_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleLink_Release(This) (This)->lpVtbl->Release(This)
#define IOleLink_SetUpdateOptions(This,dwUpdateOpt) (This)->lpVtbl->SetUpdateOptions(This,dwUpdateOpt)
#define IOleLink_GetUpdateOptions(This,pdwUpdateOpt) (This)->lpVtbl->GetUpdateOptions(This,pdwUpdateOpt)
#define IOleLink_SetSourceMoniker(This,pmk,rclsid) (This)->lpVtbl->SetSourceMoniker(This,pmk,rclsid)
#define IOleLink_GetSourceMoniker(This,ppmk) (This)->lpVtbl->GetSourceMoniker(This,ppmk)
#define IOleLink_SetSourceDisplayName(This,pszStatusText) (This)->lpVtbl->SetSourceDisplayName(This,pszStatusText)
#define IOleLink_GetSourceDisplayName(This,ppszDisplayName) (This)->lpVtbl->GetSourceDisplayName(This,ppszDisplayName)
#define IOleLink_BindToSource(This,bindflags,pbc) (This)->lpVtbl->BindToSource(This,bindflags,pbc)
#define IOleLink_BindIfRunning(This) (This)->lpVtbl->BindIfRunning(This)
#define IOleLink_GetBoundSource(This,ppunk) (This)->lpVtbl->GetBoundSource(This,ppunk)
#define IOleLink_UnbindSource(This) (This)->lpVtbl->UnbindSource(This)
#define IOleLink_Update(This,pbc) (This)->lpVtbl->Update(This,pbc)
#endif
#endif
  HRESULT WINAPI IOleLink_SetUpdateOptions_Proxy(IOleLink *This,DWORD dwUpdateOpt);
  void __RPC_STUB IOleLink_SetUpdateOptions_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_GetUpdateOptions_Proxy(IOleLink *This,DWORD *pdwUpdateOpt);
  void __RPC_STUB IOleLink_GetUpdateOptions_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_SetSourceMoniker_Proxy(IOleLink *This,IMoniker *pmk,REFCLSID rclsid);
  void __RPC_STUB IOleLink_SetSourceMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_GetSourceMoniker_Proxy(IOleLink *This,IMoniker **ppmk);
  void __RPC_STUB IOleLink_GetSourceMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_SetSourceDisplayName_Proxy(IOleLink *This,LPCOLESTR pszStatusText);
  void __RPC_STUB IOleLink_SetSourceDisplayName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_GetSourceDisplayName_Proxy(IOleLink *This,LPOLESTR *ppszDisplayName);
  void __RPC_STUB IOleLink_GetSourceDisplayName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_BindToSource_Proxy(IOleLink *This,DWORD bindflags,IBindCtx *pbc);
  void __RPC_STUB IOleLink_BindToSource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_BindIfRunning_Proxy(IOleLink *This);
  void __RPC_STUB IOleLink_BindIfRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_GetBoundSource_Proxy(IOleLink *This,IUnknown **ppunk);
  void __RPC_STUB IOleLink_GetBoundSource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_UnbindSource_Proxy(IOleLink *This);
  void __RPC_STUB IOleLink_UnbindSource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleLink_Update_Proxy(IOleLink *This,IBindCtx *pbc);
  void __RPC_STUB IOleLink_Update_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleItemContainer_INTERFACE_DEFINED__
#define __IOleItemContainer_INTERFACE_DEFINED__
  typedef IOleItemContainer *LPOLEITEMCONTAINER;

  typedef enum tagBINDSPEED {
    BINDSPEED_INDEFINITE = 1,BINDSPEED_MODERATE = 2,BINDSPEED_IMMEDIATE = 3
  } BINDSPEED;

  typedef enum tagOLECONTF {
    OLECONTF_EMBEDDINGS = 1,OLECONTF_LINKS = 2,OLECONTF_OTHERS = 4,OLECONTF_ONLYUSER = 8,OLECONTF_ONLYIFRUNNING = 16
  } OLECONTF;

  EXTERN_C const IID IID_IOleItemContainer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleItemContainer : public IOleContainer {
  public:
    virtual HRESULT WINAPI GetObject(LPOLESTR pszItem,DWORD dwSpeedNeeded,IBindCtx *pbc,REFIID riid,void **ppvObject) = 0;
    virtual HRESULT WINAPI GetObjectStorage(LPOLESTR pszItem,IBindCtx *pbc,REFIID riid,void **ppvStorage) = 0;
    virtual HRESULT WINAPI IsRunning(LPOLESTR pszItem) = 0;
  };
#else
  typedef struct IOleItemContainerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleItemContainer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleItemContainer *This);
      ULONG (WINAPI *Release)(IOleItemContainer *This);
      HRESULT (WINAPI *ParseDisplayName)(IOleItemContainer *This,IBindCtx *pbc,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
      HRESULT (WINAPI *EnumObjects)(IOleItemContainer *This,DWORD grfFlags,IEnumUnknown **ppenum);
      HRESULT (WINAPI *LockContainer)(IOleItemContainer *This,WINBOOL fLock);
      HRESULT (WINAPI *GetObject)(IOleItemContainer *This,LPOLESTR pszItem,DWORD dwSpeedNeeded,IBindCtx *pbc,REFIID riid,void **ppvObject);
      HRESULT (WINAPI *GetObjectStorage)(IOleItemContainer *This,LPOLESTR pszItem,IBindCtx *pbc,REFIID riid,void **ppvStorage);
      HRESULT (WINAPI *IsRunning)(IOleItemContainer *This,LPOLESTR pszItem);
    END_INTERFACE
  } IOleItemContainerVtbl;
  struct IOleItemContainer {
    CONST_VTBL struct IOleItemContainerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleItemContainer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleItemContainer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleItemContainer_Release(This) (This)->lpVtbl->Release(This)
#define IOleItemContainer_ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut) (This)->lpVtbl->ParseDisplayName(This,pbc,pszDisplayName,pchEaten,ppmkOut)
#define IOleItemContainer_EnumObjects(This,grfFlags,ppenum) (This)->lpVtbl->EnumObjects(This,grfFlags,ppenum)
#define IOleItemContainer_LockContainer(This,fLock) (This)->lpVtbl->LockContainer(This,fLock)
#define IOleItemContainer_GetObject(This,pszItem,dwSpeedNeeded,pbc,riid,ppvObject) (This)->lpVtbl->GetObject(This,pszItem,dwSpeedNeeded,pbc,riid,ppvObject)
#define IOleItemContainer_GetObjectStorage(This,pszItem,pbc,riid,ppvStorage) (This)->lpVtbl->GetObjectStorage(This,pszItem,pbc,riid,ppvStorage)
#define IOleItemContainer_IsRunning(This,pszItem) (This)->lpVtbl->IsRunning(This,pszItem)
#endif
#endif
  HRESULT WINAPI IOleItemContainer_GetObject_Proxy(IOleItemContainer *This,LPOLESTR pszItem,DWORD dwSpeedNeeded,IBindCtx *pbc,REFIID riid,void **ppvObject);
  void __RPC_STUB IOleItemContainer_GetObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleItemContainer_GetObjectStorage_Proxy(IOleItemContainer *This,LPOLESTR pszItem,IBindCtx *pbc,REFIID riid,void **ppvStorage);
  void __RPC_STUB IOleItemContainer_GetObjectStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleItemContainer_IsRunning_Proxy(IOleItemContainer *This,LPOLESTR pszItem);
  void __RPC_STUB IOleItemContainer_IsRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleInPlaceUIWindow_INTERFACE_DEFINED__
#define __IOleInPlaceUIWindow_INTERFACE_DEFINED__
  typedef IOleInPlaceUIWindow *LPOLEINPLACEUIWINDOW;

  typedef RECT BORDERWIDTHS;
  typedef LPRECT LPBORDERWIDTHS;
  typedef LPCRECT LPCBORDERWIDTHS;

  EXTERN_C const IID IID_IOleInPlaceUIWindow;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleInPlaceUIWindow : public IOleWindow {
  public:
    virtual HRESULT WINAPI GetBorder(LPRECT lprectBorder) = 0;
    virtual HRESULT WINAPI RequestBorderSpace(LPCBORDERWIDTHS pborderwidths) = 0;
    virtual HRESULT WINAPI SetBorderSpace(LPCBORDERWIDTHS pborderwidths) = 0;
    virtual HRESULT WINAPI SetActiveObject(IOleInPlaceActiveObject *pActiveObject,LPCOLESTR pszObjName) = 0;
  };
#else
  typedef struct IOleInPlaceUIWindowVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleInPlaceUIWindow *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleInPlaceUIWindow *This);
      ULONG (WINAPI *Release)(IOleInPlaceUIWindow *This);
      HRESULT (WINAPI *GetWindow)(IOleInPlaceUIWindow *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleInPlaceUIWindow *This,WINBOOL fEnterMode);
      HRESULT (WINAPI *GetBorder)(IOleInPlaceUIWindow *This,LPRECT lprectBorder);
      HRESULT (WINAPI *RequestBorderSpace)(IOleInPlaceUIWindow *This,LPCBORDERWIDTHS pborderwidths);
      HRESULT (WINAPI *SetBorderSpace)(IOleInPlaceUIWindow *This,LPCBORDERWIDTHS pborderwidths);
      HRESULT (WINAPI *SetActiveObject)(IOleInPlaceUIWindow *This,IOleInPlaceActiveObject *pActiveObject,LPCOLESTR pszObjName);
    END_INTERFACE
  } IOleInPlaceUIWindowVtbl;
  struct IOleInPlaceUIWindow {
    CONST_VTBL struct IOleInPlaceUIWindowVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleInPlaceUIWindow_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleInPlaceUIWindow_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleInPlaceUIWindow_Release(This) (This)->lpVtbl->Release(This)
#define IOleInPlaceUIWindow_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleInPlaceUIWindow_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#define IOleInPlaceUIWindow_GetBorder(This,lprectBorder) (This)->lpVtbl->GetBorder(This,lprectBorder)
#define IOleInPlaceUIWindow_RequestBorderSpace(This,pborderwidths) (This)->lpVtbl->RequestBorderSpace(This,pborderwidths)
#define IOleInPlaceUIWindow_SetBorderSpace(This,pborderwidths) (This)->lpVtbl->SetBorderSpace(This,pborderwidths)
#define IOleInPlaceUIWindow_SetActiveObject(This,pActiveObject,pszObjName) (This)->lpVtbl->SetActiveObject(This,pActiveObject,pszObjName)
#endif
#endif
  HRESULT WINAPI IOleInPlaceUIWindow_GetBorder_Proxy(IOleInPlaceUIWindow *This,LPRECT lprectBorder);
  void __RPC_STUB IOleInPlaceUIWindow_GetBorder_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceUIWindow_RequestBorderSpace_Proxy(IOleInPlaceUIWindow *This,LPCBORDERWIDTHS pborderwidths);
  void __RPC_STUB IOleInPlaceUIWindow_RequestBorderSpace_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceUIWindow_SetBorderSpace_Proxy(IOleInPlaceUIWindow *This,LPCBORDERWIDTHS pborderwidths);
  void __RPC_STUB IOleInPlaceUIWindow_SetBorderSpace_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceUIWindow_SetActiveObject_Proxy(IOleInPlaceUIWindow *This,IOleInPlaceActiveObject *pActiveObject,LPCOLESTR pszObjName);
  void __RPC_STUB IOleInPlaceUIWindow_SetActiveObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleInPlaceActiveObject_INTERFACE_DEFINED__
#define __IOleInPlaceActiveObject_INTERFACE_DEFINED__
  typedef IOleInPlaceActiveObject *LPOLEINPLACEACTIVEOBJECT;

  EXTERN_C const IID IID_IOleInPlaceActiveObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleInPlaceActiveObject : public IOleWindow {
  public:
    virtual HRESULT WINAPI TranslateAccelerator(LPMSG lpmsg) = 0;
    virtual HRESULT WINAPI OnFrameWindowActivate(WINBOOL fActivate) = 0;
    virtual HRESULT WINAPI OnDocWindowActivate(WINBOOL fActivate) = 0;
    virtual HRESULT WINAPI ResizeBorder(LPCRECT prcBorder,IOleInPlaceUIWindow *pUIWindow,WINBOOL fFrameWindow) = 0;
    virtual HRESULT WINAPI EnableModeless(WINBOOL fEnable) = 0;
  };
#else
  typedef struct IOleInPlaceActiveObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleInPlaceActiveObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleInPlaceActiveObject *This);
      ULONG (WINAPI *Release)(IOleInPlaceActiveObject *This);
      HRESULT (WINAPI *GetWindow)(IOleInPlaceActiveObject *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleInPlaceActiveObject *This,WINBOOL fEnterMode);
      HRESULT (WINAPI *TranslateAccelerator)(IOleInPlaceActiveObject *This,LPMSG lpmsg);
      HRESULT (WINAPI *OnFrameWindowActivate)(IOleInPlaceActiveObject *This,WINBOOL fActivate);
      HRESULT (WINAPI *OnDocWindowActivate)(IOleInPlaceActiveObject *This,WINBOOL fActivate);
      HRESULT (WINAPI *ResizeBorder)(IOleInPlaceActiveObject *This,LPCRECT prcBorder,IOleInPlaceUIWindow *pUIWindow,WINBOOL fFrameWindow);
      HRESULT (WINAPI *EnableModeless)(IOleInPlaceActiveObject *This,WINBOOL fEnable);
    END_INTERFACE
  } IOleInPlaceActiveObjectVtbl;
  struct IOleInPlaceActiveObject {
    CONST_VTBL struct IOleInPlaceActiveObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleInPlaceActiveObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleInPlaceActiveObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleInPlaceActiveObject_Release(This) (This)->lpVtbl->Release(This)
#define IOleInPlaceActiveObject_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleInPlaceActiveObject_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#define IOleInPlaceActiveObject_TranslateAccelerator(This,lpmsg) (This)->lpVtbl->TranslateAccelerator(This,lpmsg)
#define IOleInPlaceActiveObject_OnFrameWindowActivate(This,fActivate) (This)->lpVtbl->OnFrameWindowActivate(This,fActivate)
#define IOleInPlaceActiveObject_OnDocWindowActivate(This,fActivate) (This)->lpVtbl->OnDocWindowActivate(This,fActivate)
#define IOleInPlaceActiveObject_ResizeBorder(This,prcBorder,pUIWindow,fFrameWindow) (This)->lpVtbl->ResizeBorder(This,prcBorder,pUIWindow,fFrameWindow)
#define IOleInPlaceActiveObject_EnableModeless(This,fEnable) (This)->lpVtbl->EnableModeless(This,fEnable)
#endif
#endif
  HRESULT WINAPI IOleInPlaceActiveObject_RemoteTranslateAccelerator_Proxy(IOleInPlaceActiveObject *This);
  void __RPC_STUB IOleInPlaceActiveObject_RemoteTranslateAccelerator_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceActiveObject_OnFrameWindowActivate_Proxy(IOleInPlaceActiveObject *This,WINBOOL fActivate);
  void __RPC_STUB IOleInPlaceActiveObject_OnFrameWindowActivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceActiveObject_OnDocWindowActivate_Proxy(IOleInPlaceActiveObject *This,WINBOOL fActivate);
  void __RPC_STUB IOleInPlaceActiveObject_OnDocWindowActivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceActiveObject_RemoteResizeBorder_Proxy(IOleInPlaceActiveObject *This,LPCRECT prcBorder,REFIID riid,IOleInPlaceUIWindow *pUIWindow,WINBOOL fFrameWindow);
  void __RPC_STUB IOleInPlaceActiveObject_RemoteResizeBorder_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceActiveObject_EnableModeless_Proxy(IOleInPlaceActiveObject *This,WINBOOL fEnable);
  void __RPC_STUB IOleInPlaceActiveObject_EnableModeless_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleInPlaceFrame_INTERFACE_DEFINED__
#define __IOleInPlaceFrame_INTERFACE_DEFINED__
  typedef IOleInPlaceFrame *LPOLEINPLACEFRAME;

  typedef struct tagOIFI {
    UINT cb;
    WINBOOL fMDIApp;
    HWND hwndFrame;
    HACCEL haccel;
    UINT cAccelEntries;
  } OLEINPLACEFRAMEINFO;

  typedef struct tagOIFI *LPOLEINPLACEFRAMEINFO;

  typedef struct tagOleMenuGroupWidths {
    LONG width[6 ];
  } OLEMENUGROUPWIDTHS;

  typedef struct tagOleMenuGroupWidths *LPOLEMENUGROUPWIDTHS;
  typedef HGLOBAL HOLEMENU;

  EXTERN_C const IID IID_IOleInPlaceFrame;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleInPlaceFrame : public IOleInPlaceUIWindow {
  public:
    virtual HRESULT WINAPI InsertMenus(HMENU hmenuShared,LPOLEMENUGROUPWIDTHS lpMenuWidths) = 0;
    virtual HRESULT WINAPI SetMenu(HMENU hmenuShared,HOLEMENU holemenu,HWND hwndActiveObject) = 0;
    virtual HRESULT WINAPI RemoveMenus(HMENU hmenuShared) = 0;
    virtual HRESULT WINAPI SetStatusText(LPCOLESTR pszStatusText) = 0;
    virtual HRESULT WINAPI EnableModeless(WINBOOL fEnable) = 0;
    virtual HRESULT WINAPI TranslateAccelerator(LPMSG lpmsg,WORD wID) = 0;
  };
#else
  typedef struct IOleInPlaceFrameVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleInPlaceFrame *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleInPlaceFrame *This);
      ULONG (WINAPI *Release)(IOleInPlaceFrame *This);
      HRESULT (WINAPI *GetWindow)(IOleInPlaceFrame *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleInPlaceFrame *This,WINBOOL fEnterMode);
      HRESULT (WINAPI *GetBorder)(IOleInPlaceFrame *This,LPRECT lprectBorder);
      HRESULT (WINAPI *RequestBorderSpace)(IOleInPlaceFrame *This,LPCBORDERWIDTHS pborderwidths);
      HRESULT (WINAPI *SetBorderSpace)(IOleInPlaceFrame *This,LPCBORDERWIDTHS pborderwidths);
      HRESULT (WINAPI *SetActiveObject)(IOleInPlaceFrame *This,IOleInPlaceActiveObject *pActiveObject,LPCOLESTR pszObjName);
      HRESULT (WINAPI *InsertMenus)(IOleInPlaceFrame *This,HMENU hmenuShared,LPOLEMENUGROUPWIDTHS lpMenuWidths);
      HRESULT (WINAPI *SetMenu)(IOleInPlaceFrame *This,HMENU hmenuShared,HOLEMENU holemenu,HWND hwndActiveObject);
      HRESULT (WINAPI *RemoveMenus)(IOleInPlaceFrame *This,HMENU hmenuShared);
      HRESULT (WINAPI *SetStatusText)(IOleInPlaceFrame *This,LPCOLESTR pszStatusText);
      HRESULT (WINAPI *EnableModeless)(IOleInPlaceFrame *This,WINBOOL fEnable);
      HRESULT (WINAPI *TranslateAccelerator)(IOleInPlaceFrame *This,LPMSG lpmsg,WORD wID);
    END_INTERFACE
  } IOleInPlaceFrameVtbl;
  struct IOleInPlaceFrame {
    CONST_VTBL struct IOleInPlaceFrameVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleInPlaceFrame_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleInPlaceFrame_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleInPlaceFrame_Release(This) (This)->lpVtbl->Release(This)
#define IOleInPlaceFrame_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleInPlaceFrame_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#define IOleInPlaceFrame_GetBorder(This,lprectBorder) (This)->lpVtbl->GetBorder(This,lprectBorder)
#define IOleInPlaceFrame_RequestBorderSpace(This,pborderwidths) (This)->lpVtbl->RequestBorderSpace(This,pborderwidths)
#define IOleInPlaceFrame_SetBorderSpace(This,pborderwidths) (This)->lpVtbl->SetBorderSpace(This,pborderwidths)
#define IOleInPlaceFrame_SetActiveObject(This,pActiveObject,pszObjName) (This)->lpVtbl->SetActiveObject(This,pActiveObject,pszObjName)
#define IOleInPlaceFrame_InsertMenus(This,hmenuShared,lpMenuWidths) (This)->lpVtbl->InsertMenus(This,hmenuShared,lpMenuWidths)
#define IOleInPlaceFrame_SetMenu(This,hmenuShared,holemenu,hwndActiveObject) (This)->lpVtbl->SetMenu(This,hmenuShared,holemenu,hwndActiveObject)
#define IOleInPlaceFrame_RemoveMenus(This,hmenuShared) (This)->lpVtbl->RemoveMenus(This,hmenuShared)
#define IOleInPlaceFrame_SetStatusText(This,pszStatusText) (This)->lpVtbl->SetStatusText(This,pszStatusText)
#define IOleInPlaceFrame_EnableModeless(This,fEnable) (This)->lpVtbl->EnableModeless(This,fEnable)
#define IOleInPlaceFrame_TranslateAccelerator(This,lpmsg,wID) (This)->lpVtbl->TranslateAccelerator(This,lpmsg,wID)
#endif
#endif
  HRESULT WINAPI IOleInPlaceFrame_InsertMenus_Proxy(IOleInPlaceFrame *This,HMENU hmenuShared,LPOLEMENUGROUPWIDTHS lpMenuWidths);
  void __RPC_STUB IOleInPlaceFrame_InsertMenus_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceFrame_SetMenu_Proxy(IOleInPlaceFrame *This,HMENU hmenuShared,HOLEMENU holemenu,HWND hwndActiveObject);
  void __RPC_STUB IOleInPlaceFrame_SetMenu_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceFrame_RemoveMenus_Proxy(IOleInPlaceFrame *This,HMENU hmenuShared);
  void __RPC_STUB IOleInPlaceFrame_RemoveMenus_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceFrame_SetStatusText_Proxy(IOleInPlaceFrame *This,LPCOLESTR pszStatusText);
  void __RPC_STUB IOleInPlaceFrame_SetStatusText_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceFrame_EnableModeless_Proxy(IOleInPlaceFrame *This,WINBOOL fEnable);
  void __RPC_STUB IOleInPlaceFrame_EnableModeless_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceFrame_TranslateAccelerator_Proxy(IOleInPlaceFrame *This,LPMSG lpmsg,WORD wID);
  void __RPC_STUB IOleInPlaceFrame_TranslateAccelerator_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleInPlaceObject_INTERFACE_DEFINED__
#define __IOleInPlaceObject_INTERFACE_DEFINED__
  typedef IOleInPlaceObject *LPOLEINPLACEOBJECT;

  EXTERN_C const IID IID_IOleInPlaceObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleInPlaceObject : public IOleWindow {
  public:
    virtual HRESULT WINAPI InPlaceDeactivate(void) = 0;
    virtual HRESULT WINAPI UIDeactivate(void) = 0;
    virtual HRESULT WINAPI SetObjectRects(LPCRECT lprcPosRect,LPCRECT lprcClipRect) = 0;
    virtual HRESULT WINAPI ReactivateAndUndo(void) = 0;
  };
#else
  typedef struct IOleInPlaceObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleInPlaceObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleInPlaceObject *This);
      ULONG (WINAPI *Release)(IOleInPlaceObject *This);
      HRESULT (WINAPI *GetWindow)(IOleInPlaceObject *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleInPlaceObject *This,WINBOOL fEnterMode);
      HRESULT (WINAPI *InPlaceDeactivate)(IOleInPlaceObject *This);
      HRESULT (WINAPI *UIDeactivate)(IOleInPlaceObject *This);
      HRESULT (WINAPI *SetObjectRects)(IOleInPlaceObject *This,LPCRECT lprcPosRect,LPCRECT lprcClipRect);
      HRESULT (WINAPI *ReactivateAndUndo)(IOleInPlaceObject *This);
    END_INTERFACE
  } IOleInPlaceObjectVtbl;
  struct IOleInPlaceObject {
    CONST_VTBL struct IOleInPlaceObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleInPlaceObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleInPlaceObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleInPlaceObject_Release(This) (This)->lpVtbl->Release(This)
#define IOleInPlaceObject_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleInPlaceObject_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#define IOleInPlaceObject_InPlaceDeactivate(This) (This)->lpVtbl->InPlaceDeactivate(This)
#define IOleInPlaceObject_UIDeactivate(This) (This)->lpVtbl->UIDeactivate(This)
#define IOleInPlaceObject_SetObjectRects(This,lprcPosRect,lprcClipRect) (This)->lpVtbl->SetObjectRects(This,lprcPosRect,lprcClipRect)
#define IOleInPlaceObject_ReactivateAndUndo(This) (This)->lpVtbl->ReactivateAndUndo(This)
#endif
#endif
  HRESULT WINAPI IOleInPlaceObject_InPlaceDeactivate_Proxy(IOleInPlaceObject *This);
  void __RPC_STUB IOleInPlaceObject_InPlaceDeactivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceObject_UIDeactivate_Proxy(IOleInPlaceObject *This);
  void __RPC_STUB IOleInPlaceObject_UIDeactivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceObject_SetObjectRects_Proxy(IOleInPlaceObject *This,LPCRECT lprcPosRect,LPCRECT lprcClipRect);
  void __RPC_STUB IOleInPlaceObject_SetObjectRects_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceObject_ReactivateAndUndo_Proxy(IOleInPlaceObject *This);
  void __RPC_STUB IOleInPlaceObject_ReactivateAndUndo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOleInPlaceSite_INTERFACE_DEFINED__
#define __IOleInPlaceSite_INTERFACE_DEFINED__
  typedef IOleInPlaceSite *LPOLEINPLACESITE;

  EXTERN_C const IID IID_IOleInPlaceSite;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOleInPlaceSite : public IOleWindow {
  public:
    virtual HRESULT WINAPI CanInPlaceActivate(void) = 0;
    virtual HRESULT WINAPI OnInPlaceActivate(void) = 0;
    virtual HRESULT WINAPI OnUIActivate(void) = 0;
    virtual HRESULT WINAPI GetWindowContext(IOleInPlaceFrame **ppFrame,IOleInPlaceUIWindow **ppDoc,LPRECT lprcPosRect,LPRECT lprcClipRect,LPOLEINPLACEFRAMEINFO lpFrameInfo) = 0;
    virtual HRESULT WINAPI Scroll(SIZE scrollExtant) = 0;
    virtual HRESULT WINAPI OnUIDeactivate(WINBOOL fUndoable) = 0;
    virtual HRESULT WINAPI OnInPlaceDeactivate(void) = 0;
    virtual HRESULT WINAPI DiscardUndoState(void) = 0;
    virtual HRESULT WINAPI DeactivateAndUndo(void) = 0;
    virtual HRESULT WINAPI OnPosRectChange(LPCRECT lprcPosRect) = 0;
  };
#else
  typedef struct IOleInPlaceSiteVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOleInPlaceSite *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOleInPlaceSite *This);
      ULONG (WINAPI *Release)(IOleInPlaceSite *This);
      HRESULT (WINAPI *GetWindow)(IOleInPlaceSite *This,HWND *phwnd);
      HRESULT (WINAPI *ContextSensitiveHelp)(IOleInPlaceSite *This,WINBOOL fEnterMode);
      HRESULT (WINAPI *CanInPlaceActivate)(IOleInPlaceSite *This);
      HRESULT (WINAPI *OnInPlaceActivate)(IOleInPlaceSite *This);
      HRESULT (WINAPI *OnUIActivate)(IOleInPlaceSite *This);
      HRESULT (WINAPI *GetWindowContext)(IOleInPlaceSite *This,IOleInPlaceFrame **ppFrame,IOleInPlaceUIWindow **ppDoc,LPRECT lprcPosRect,LPRECT lprcClipRect,LPOLEINPLACEFRAMEINFO lpFrameInfo);
      HRESULT (WINAPI *Scroll)(IOleInPlaceSite *This,SIZE scrollExtant);
      HRESULT (WINAPI *OnUIDeactivate)(IOleInPlaceSite *This,WINBOOL fUndoable);
      HRESULT (WINAPI *OnInPlaceDeactivate)(IOleInPlaceSite *This);
      HRESULT (WINAPI *DiscardUndoState)(IOleInPlaceSite *This);
      HRESULT (WINAPI *DeactivateAndUndo)(IOleInPlaceSite *This);
      HRESULT (WINAPI *OnPosRectChange)(IOleInPlaceSite *This,LPCRECT lprcPosRect);
    END_INTERFACE
  } IOleInPlaceSiteVtbl;
  struct IOleInPlaceSite {
    CONST_VTBL struct IOleInPlaceSiteVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOleInPlaceSite_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOleInPlaceSite_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOleInPlaceSite_Release(This) (This)->lpVtbl->Release(This)
#define IOleInPlaceSite_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IOleInPlaceSite_ContextSensitiveHelp(This,fEnterMode) (This)->lpVtbl->ContextSensitiveHelp(This,fEnterMode)
#define IOleInPlaceSite_CanInPlaceActivate(This) (This)->lpVtbl->CanInPlaceActivate(This)
#define IOleInPlaceSite_OnInPlaceActivate(This) (This)->lpVtbl->OnInPlaceActivate(This)
#define IOleInPlaceSite_OnUIActivate(This) (This)->lpVtbl->OnUIActivate(This)
#define IOleInPlaceSite_GetWindowContext(This,ppFrame,ppDoc,lprcPosRect,lprcClipRect,lpFrameInfo) (This)->lpVtbl->GetWindowContext(This,ppFrame,ppDoc,lprcPosRect,lprcClipRect,lpFrameInfo)
#define IOleInPlaceSite_Scroll(This,scrollExtant) (This)->lpVtbl->Scroll(This,scrollExtant)
#define IOleInPlaceSite_OnUIDeactivate(This,fUndoable) (This)->lpVtbl->OnUIDeactivate(This,fUndoable)
#define IOleInPlaceSite_OnInPlaceDeactivate(This) (This)->lpVtbl->OnInPlaceDeactivate(This)
#define IOleInPlaceSite_DiscardUndoState(This) (This)->lpVtbl->DiscardUndoState(This)
#define IOleInPlaceSite_DeactivateAndUndo(This) (This)->lpVtbl->DeactivateAndUndo(This)
#define IOleInPlaceSite_OnPosRectChange(This,lprcPosRect) (This)->lpVtbl->OnPosRectChange(This,lprcPosRect)
#endif
#endif
  HRESULT WINAPI IOleInPlaceSite_CanInPlaceActivate_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_CanInPlaceActivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_OnInPlaceActivate_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_OnInPlaceActivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_OnUIActivate_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_OnUIActivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_GetWindowContext_Proxy(IOleInPlaceSite *This,IOleInPlaceFrame **ppFrame,IOleInPlaceUIWindow **ppDoc,LPRECT lprcPosRect,LPRECT lprcClipRect,LPOLEINPLACEFRAMEINFO lpFrameInfo);
  void __RPC_STUB IOleInPlaceSite_GetWindowContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_Scroll_Proxy(IOleInPlaceSite *This,SIZE scrollExtant);
  void __RPC_STUB IOleInPlaceSite_Scroll_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_OnUIDeactivate_Proxy(IOleInPlaceSite *This,WINBOOL fUndoable);
  void __RPC_STUB IOleInPlaceSite_OnUIDeactivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_OnInPlaceDeactivate_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_OnInPlaceDeactivate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_DiscardUndoState_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_DiscardUndoState_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_DeactivateAndUndo_Proxy(IOleInPlaceSite *This);
  void __RPC_STUB IOleInPlaceSite_DeactivateAndUndo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOleInPlaceSite_OnPosRectChange_Proxy(IOleInPlaceSite *This,LPCRECT lprcPosRect);
  void __RPC_STUB IOleInPlaceSite_OnPosRectChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IContinue_INTERFACE_DEFINED__
#define __IContinue_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IContinue;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IContinue : public IUnknown {
  public:
    virtual HRESULT WINAPI FContinue(void) = 0;
  };
#else
  typedef struct IContinueVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IContinue *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IContinue *This);
      ULONG (WINAPI *Release)(IContinue *This);
      HRESULT (WINAPI *FContinue)(IContinue *This);
    END_INTERFACE
  } IContinueVtbl;
  struct IContinue {
    CONST_VTBL struct IContinueVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IContinue_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IContinue_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IContinue_Release(This) (This)->lpVtbl->Release(This)
#define IContinue_FContinue(This) (This)->lpVtbl->FContinue(This)
#endif
#endif
  HRESULT WINAPI IContinue_FContinue_Proxy(IContinue *This);
  void __RPC_STUB IContinue_FContinue_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IViewObject_INTERFACE_DEFINED__
#define __IViewObject_INTERFACE_DEFINED__
  typedef IViewObject *LPVIEWOBJECT;

  EXTERN_C const IID IID_IViewObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IViewObject : public IUnknown {
  public:
    virtual HRESULT WINAPI Draw(DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hdcTargetDev,HDC hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,WINBOOL (WINAPI *pfnContinue)(ULONG_PTR dwContinue),ULONG_PTR dwContinue) = 0;
    virtual HRESULT WINAPI GetColorSet(DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hicTargetDev,LOGPALETTE **ppColorSet) = 0;
    virtual HRESULT WINAPI Freeze(DWORD dwDrawAspect,LONG lindex,void *pvAspect,DWORD *pdwFreeze) = 0;
    virtual HRESULT WINAPI Unfreeze(DWORD dwFreeze) = 0;
    virtual HRESULT WINAPI SetAdvise(DWORD aspects,DWORD advf,IAdviseSink *pAdvSink) = 0;
    virtual HRESULT WINAPI GetAdvise(DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink) = 0;
  };
#else
  typedef struct IViewObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IViewObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IViewObject *This);
      ULONG (WINAPI *Release)(IViewObject *This);
      HRESULT (WINAPI *Draw)(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hdcTargetDev,HDC hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,WINBOOL (WINAPI *pfnContinue)(ULONG_PTR dwContinue),ULONG_PTR dwContinue);
      HRESULT (WINAPI *GetColorSet)(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hicTargetDev,LOGPALETTE **ppColorSet);
      HRESULT (WINAPI *Freeze)(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DWORD *pdwFreeze);
      HRESULT (WINAPI *Unfreeze)(IViewObject *This,DWORD dwFreeze);
      HRESULT (WINAPI *SetAdvise)(IViewObject *This,DWORD aspects,DWORD advf,IAdviseSink *pAdvSink);
      HRESULT (WINAPI *GetAdvise)(IViewObject *This,DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink);
    END_INTERFACE
  } IViewObjectVtbl;
  struct IViewObject {
    CONST_VTBL struct IViewObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IViewObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IViewObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IViewObject_Release(This) (This)->lpVtbl->Release(This)
#define IViewObject_Draw(This,dwDrawAspect,lindex,pvAspect,ptd,hdcTargetDev,hdcDraw,lprcBounds,lprcWBounds,pfnContinue,dwContinue) (This)->lpVtbl->Draw(This,dwDrawAspect,lindex,pvAspect,ptd,hdcTargetDev,hdcDraw,lprcBounds,lprcWBounds,pfnContinue,dwContinue)
#define IViewObject_GetColorSet(This,dwDrawAspect,lindex,pvAspect,ptd,hicTargetDev,ppColorSet) (This)->lpVtbl->GetColorSet(This,dwDrawAspect,lindex,pvAspect,ptd,hicTargetDev,ppColorSet)
#define IViewObject_Freeze(This,dwDrawAspect,lindex,pvAspect,pdwFreeze) (This)->lpVtbl->Freeze(This,dwDrawAspect,lindex,pvAspect,pdwFreeze)
#define IViewObject_Unfreeze(This,dwFreeze) (This)->lpVtbl->Unfreeze(This,dwFreeze)
#define IViewObject_SetAdvise(This,aspects,advf,pAdvSink) (This)->lpVtbl->SetAdvise(This,aspects,advf,pAdvSink)
#define IViewObject_GetAdvise(This,pAspects,pAdvf,ppAdvSink) (This)->lpVtbl->GetAdvise(This,pAspects,pAdvf,ppAdvSink)
#endif
#endif
  HRESULT WINAPI IViewObject_RemoteDraw_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DVTARGETDEVICE *ptd,ULONG_PTR hdcTargetDev,ULONG_PTR hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,IContinue *pContinue);
  void __RPC_STUB IViewObject_RemoteDraw_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IViewObject_RemoteGetColorSet_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DVTARGETDEVICE *ptd,ULONG_PTR hicTargetDev,LOGPALETTE **ppColorSet);
  void __RPC_STUB IViewObject_RemoteGetColorSet_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IViewObject_RemoteFreeze_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DWORD *pdwFreeze);
  void __RPC_STUB IViewObject_RemoteFreeze_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IViewObject_Unfreeze_Proxy(IViewObject *This,DWORD dwFreeze);
  void __RPC_STUB IViewObject_Unfreeze_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IViewObject_SetAdvise_Proxy(IViewObject *This,DWORD aspects,DWORD advf,IAdviseSink *pAdvSink);
  void __RPC_STUB IViewObject_SetAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IViewObject_RemoteGetAdvise_Proxy(IViewObject *This,DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink);
  void __RPC_STUB IViewObject_RemoteGetAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IViewObject2_INTERFACE_DEFINED__
#define __IViewObject2_INTERFACE_DEFINED__
  typedef IViewObject2 *LPVIEWOBJECT2;

  EXTERN_C const IID IID_IViewObject2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IViewObject2 : public IViewObject {
  public:
    virtual HRESULT WINAPI GetExtent(DWORD dwDrawAspect,LONG lindex,DVTARGETDEVICE *ptd,LPSIZEL lpsizel) = 0;
  };
#else
  typedef struct IViewObject2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IViewObject2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IViewObject2 *This);
      ULONG (WINAPI *Release)(IViewObject2 *This);
      HRESULT (WINAPI *Draw)(IViewObject2 *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hdcTargetDev,HDC hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,WINBOOL (WINAPI *pfnContinue)(ULONG_PTR dwContinue),ULONG_PTR dwContinue);
      HRESULT (WINAPI *GetColorSet)(IViewObject2 *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hicTargetDev,LOGPALETTE **ppColorSet);
      HRESULT (WINAPI *Freeze)(IViewObject2 *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DWORD *pdwFreeze);
      HRESULT (WINAPI *Unfreeze)(IViewObject2 *This,DWORD dwFreeze);
      HRESULT (WINAPI *SetAdvise)(IViewObject2 *This,DWORD aspects,DWORD advf,IAdviseSink *pAdvSink);
      HRESULT (WINAPI *GetAdvise)(IViewObject2 *This,DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink);
      HRESULT (WINAPI *GetExtent)(IViewObject2 *This,DWORD dwDrawAspect,LONG lindex,DVTARGETDEVICE *ptd,LPSIZEL lpsizel);
    END_INTERFACE
  } IViewObject2Vtbl;
  struct IViewObject2 {
    CONST_VTBL struct IViewObject2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IViewObject2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IViewObject2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IViewObject2_Release(This) (This)->lpVtbl->Release(This)
#define IViewObject2_Draw(This,dwDrawAspect,lindex,pvAspect,ptd,hdcTargetDev,hdcDraw,lprcBounds,lprcWBounds,pfnContinue,dwContinue) (This)->lpVtbl->Draw(This,dwDrawAspect,lindex,pvAspect,ptd,hdcTargetDev,hdcDraw,lprcBounds,lprcWBounds,pfnContinue,dwContinue)
#define IViewObject2_GetColorSet(This,dwDrawAspect,lindex,pvAspect,ptd,hicTargetDev,ppColorSet) (This)->lpVtbl->GetColorSet(This,dwDrawAspect,lindex,pvAspect,ptd,hicTargetDev,ppColorSet)
#define IViewObject2_Freeze(This,dwDrawAspect,lindex,pvAspect,pdwFreeze) (This)->lpVtbl->Freeze(This,dwDrawAspect,lindex,pvAspect,pdwFreeze)
#define IViewObject2_Unfreeze(This,dwFreeze) (This)->lpVtbl->Unfreeze(This,dwFreeze)
#define IViewObject2_SetAdvise(This,aspects,advf,pAdvSink) (This)->lpVtbl->SetAdvise(This,aspects,advf,pAdvSink)
#define IViewObject2_GetAdvise(This,pAspects,pAdvf,ppAdvSink) (This)->lpVtbl->GetAdvise(This,pAspects,pAdvf,ppAdvSink)
#define IViewObject2_GetExtent(This,dwDrawAspect,lindex,ptd,lpsizel) (This)->lpVtbl->GetExtent(This,dwDrawAspect,lindex,ptd,lpsizel)
#endif
#endif
  HRESULT WINAPI IViewObject2_GetExtent_Proxy(IViewObject2 *This,DWORD dwDrawAspect,LONG lindex,DVTARGETDEVICE *ptd,LPSIZEL lpsizel);
  void __RPC_STUB IViewObject2_GetExtent_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDropSource_INTERFACE_DEFINED__
#define __IDropSource_INTERFACE_DEFINED__
  typedef IDropSource *LPDROPSOURCE;

  EXTERN_C const IID IID_IDropSource;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDropSource : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryContinueDrag(WINBOOL fEscapePressed,DWORD grfKeyState) = 0;
    virtual HRESULT WINAPI GiveFeedback(DWORD dwEffect) = 0;
  };
#else
  typedef struct IDropSourceVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDropSource *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDropSource *This);
      ULONG (WINAPI *Release)(IDropSource *This);
      HRESULT (WINAPI *QueryContinueDrag)(IDropSource *This,WINBOOL fEscapePressed,DWORD grfKeyState);
      HRESULT (WINAPI *GiveFeedback)(IDropSource *This,DWORD dwEffect);
    END_INTERFACE
  } IDropSourceVtbl;
  struct IDropSource {
    CONST_VTBL struct IDropSourceVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDropSource_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDropSource_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDropSource_Release(This) (This)->lpVtbl->Release(This)
#define IDropSource_QueryContinueDrag(This,fEscapePressed,grfKeyState) (This)->lpVtbl->QueryContinueDrag(This,fEscapePressed,grfKeyState)
#define IDropSource_GiveFeedback(This,dwEffect) (This)->lpVtbl->GiveFeedback(This,dwEffect)
#endif
#endif
  HRESULT WINAPI IDropSource_QueryContinueDrag_Proxy(IDropSource *This,WINBOOL fEscapePressed,DWORD grfKeyState);
  void __RPC_STUB IDropSource_QueryContinueDrag_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDropSource_GiveFeedback_Proxy(IDropSource *This,DWORD dwEffect);
  void __RPC_STUB IDropSource_GiveFeedback_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDropTarget_INTERFACE_DEFINED__
#define __IDropTarget_INTERFACE_DEFINED__
  typedef IDropTarget *LPDROPTARGET;

#define MK_ALT (0x20)

#define DROPEFFECT_NONE (0)
#define DROPEFFECT_COPY (1)
#define DROPEFFECT_MOVE (2)
#define DROPEFFECT_LINK (4)
#define DROPEFFECT_SCROLL (0x80000000)
#define DD_DEFSCROLLINSET (11)
#define DD_DEFSCROLLDELAY (50)
#define DD_DEFSCROLLINTERVAL (50)
#define DD_DEFDRAGDELAY (200)
#define DD_DEFDRAGMINDIST (2)

  EXTERN_C const IID IID_IDropTarget;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDropTarget : public IUnknown {
  public:
    virtual HRESULT WINAPI DragEnter(IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect) = 0;
    virtual HRESULT WINAPI DragOver(DWORD grfKeyState,POINTL pt,DWORD *pdwEffect) = 0;
    virtual HRESULT WINAPI DragLeave(void) = 0;
    virtual HRESULT WINAPI Drop(IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect) = 0;
  };
#else
  typedef struct IDropTargetVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDropTarget *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDropTarget *This);
      ULONG (WINAPI *Release)(IDropTarget *This);
      HRESULT (WINAPI *DragEnter)(IDropTarget *This,IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
      HRESULT (WINAPI *DragOver)(IDropTarget *This,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
      HRESULT (WINAPI *DragLeave)(IDropTarget *This);
      HRESULT (WINAPI *Drop)(IDropTarget *This,IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
    END_INTERFACE
  } IDropTargetVtbl;
  struct IDropTarget {
    CONST_VTBL struct IDropTargetVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDropTarget_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDropTarget_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDropTarget_Release(This) (This)->lpVtbl->Release(This)
#define IDropTarget_DragEnter(This,pDataObj,grfKeyState,pt,pdwEffect) (This)->lpVtbl->DragEnter(This,pDataObj,grfKeyState,pt,pdwEffect)
#define IDropTarget_DragOver(This,grfKeyState,pt,pdwEffect) (This)->lpVtbl->DragOver(This,grfKeyState,pt,pdwEffect)
#define IDropTarget_DragLeave(This) (This)->lpVtbl->DragLeave(This)
#define IDropTarget_Drop(This,pDataObj,grfKeyState,pt,pdwEffect) (This)->lpVtbl->Drop(This,pDataObj,grfKeyState,pt,pdwEffect)
#endif
#endif
  HRESULT WINAPI IDropTarget_DragEnter_Proxy(IDropTarget *This,IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
  void __RPC_STUB IDropTarget_DragEnter_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDropTarget_DragOver_Proxy(IDropTarget *This,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
  void __RPC_STUB IDropTarget_DragOver_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDropTarget_DragLeave_Proxy(IDropTarget *This);
  void __RPC_STUB IDropTarget_DragLeave_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDropTarget_Drop_Proxy(IDropTarget *This,IDataObject *pDataObj,DWORD grfKeyState,POINTL pt,DWORD *pdwEffect);
  void __RPC_STUB IDropTarget_Drop_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumOLEVERB_INTERFACE_DEFINED__
#define __IEnumOLEVERB_INTERFACE_DEFINED__
  typedef IEnumOLEVERB *LPENUMOLEVERB;

  typedef struct tagOLEVERB {
    LONG lVerb;
    LPOLESTR lpszVerbName;
    DWORD fuFlags;
    DWORD grfAttribs;
  } OLEVERB;

  typedef struct tagOLEVERB *LPOLEVERB;

  typedef enum tagOLEVERBATTRIB {
    OLEVERBATTRIB_NEVERDIRTIES = 1,OLEVERBATTRIB_ONCONTAINERMENU = 2
  } OLEVERBATTRIB;

  EXTERN_C const IID IID_IEnumOLEVERB;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumOLEVERB : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,LPOLEVERB rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumOLEVERB **ppenum) = 0;
  };
#else
  typedef struct IEnumOLEVERBVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumOLEVERB *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumOLEVERB *This);
      ULONG (WINAPI *Release)(IEnumOLEVERB *This);
      HRESULT (WINAPI *Next)(IEnumOLEVERB *This,ULONG celt,LPOLEVERB rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumOLEVERB *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumOLEVERB *This);
      HRESULT (WINAPI *Clone)(IEnumOLEVERB *This,IEnumOLEVERB **ppenum);
    END_INTERFACE
  } IEnumOLEVERBVtbl;
  struct IEnumOLEVERB {
    CONST_VTBL struct IEnumOLEVERBVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumOLEVERB_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumOLEVERB_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumOLEVERB_Release(This) (This)->lpVtbl->Release(This)
#define IEnumOLEVERB_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumOLEVERB_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumOLEVERB_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumOLEVERB_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumOLEVERB_RemoteNext_Proxy(IEnumOLEVERB *This,ULONG celt,LPOLEVERB rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumOLEVERB_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumOLEVERB_Skip_Proxy(IEnumOLEVERB *This,ULONG celt);
  void __RPC_STUB IEnumOLEVERB_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumOLEVERB_Reset_Proxy(IEnumOLEVERB *This);
  void __RPC_STUB IEnumOLEVERB_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumOLEVERB_Clone_Proxy(IEnumOLEVERB *This,IEnumOLEVERB **ppenum);
  void __RPC_STUB IEnumOLEVERB_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  unsigned long __RPC_API CLIPFORMAT_UserSize(unsigned long *,unsigned long,CLIPFORMAT *);
  unsigned char *__RPC_API CLIPFORMAT_UserMarshal(unsigned long *,unsigned char *,CLIPFORMAT *);
  unsigned char *__RPC_API CLIPFORMAT_UserUnmarshal(unsigned long *,unsigned char *,CLIPFORMAT *);
  void __RPC_API CLIPFORMAT_UserFree(unsigned long *,CLIPFORMAT *);
  unsigned long __RPC_API HACCEL_UserSize(unsigned long *,unsigned long,HACCEL *);
  unsigned char *__RPC_API HACCEL_UserMarshal(unsigned long *,unsigned char *,HACCEL *);
  unsigned char *__RPC_API HACCEL_UserUnmarshal(unsigned long *,unsigned char *,HACCEL *);
  void __RPC_API HACCEL_UserFree(unsigned long *,HACCEL *);
  unsigned long __RPC_API HGLOBAL_UserSize(unsigned long *,unsigned long,HGLOBAL *);
  unsigned char *__RPC_API HGLOBAL_UserMarshal(unsigned long *,unsigned char *,HGLOBAL *);
  unsigned char *__RPC_API HGLOBAL_UserUnmarshal(unsigned long *,unsigned char *,HGLOBAL *);
  void __RPC_API HGLOBAL_UserFree(unsigned long *,HGLOBAL *);
  unsigned long __RPC_API HMENU_UserSize(unsigned long *,unsigned long,HMENU *);
  unsigned char *__RPC_API HMENU_UserMarshal(unsigned long *,unsigned char *,HMENU *);
  unsigned char *__RPC_API HMENU_UserUnmarshal(unsigned long *,unsigned char *,HMENU *);
  void __RPC_API HMENU_UserFree(unsigned long *,HMENU *);
  unsigned long __RPC_API HWND_UserSize(unsigned long *,unsigned long,HWND *);
  unsigned char *__RPC_API HWND_UserMarshal(unsigned long *,unsigned char *,HWND *);
  unsigned char *__RPC_API HWND_UserUnmarshal(unsigned long *,unsigned char *,HWND *);
  void __RPC_API HWND_UserFree(unsigned long *,HWND *);
  unsigned long __RPC_API STGMEDIUM_UserSize(unsigned long *,unsigned long,STGMEDIUM *);
  unsigned char *__RPC_API STGMEDIUM_UserMarshal(unsigned long *,unsigned char *,STGMEDIUM *);
  unsigned char *__RPC_API STGMEDIUM_UserUnmarshal(unsigned long *,unsigned char *,STGMEDIUM *);
  void __RPC_API STGMEDIUM_UserFree(unsigned long *,STGMEDIUM *);

  HRESULT WINAPI IOleCache2_UpdateCache_Proxy(IOleCache2 *This,LPDATAOBJECT pDataObject,DWORD grfUpdf,LPVOID pReserved);
  HRESULT WINAPI IOleCache2_UpdateCache_Stub(IOleCache2 *This,LPDATAOBJECT pDataObject,DWORD grfUpdf,LONG_PTR pReserved);
  HRESULT WINAPI IOleInPlaceActiveObject_TranslateAccelerator_Proxy(IOleInPlaceActiveObject *This,LPMSG lpmsg);
  HRESULT WINAPI IOleInPlaceActiveObject_TranslateAccelerator_Stub(IOleInPlaceActiveObject *This);
  HRESULT WINAPI IOleInPlaceActiveObject_ResizeBorder_Proxy(IOleInPlaceActiveObject *This,LPCRECT prcBorder,IOleInPlaceUIWindow *pUIWindow,WINBOOL fFrameWindow);
  HRESULT WINAPI IOleInPlaceActiveObject_ResizeBorder_Stub(IOleInPlaceActiveObject *This,LPCRECT prcBorder,REFIID riid,IOleInPlaceUIWindow *pUIWindow,WINBOOL fFrameWindow);
  HRESULT WINAPI IViewObject_Draw_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hdcTargetDev,HDC hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,WINBOOL (WINAPI *pfnContinue)(ULONG_PTR dwContinue),ULONG_PTR dwContinue);
  HRESULT WINAPI IViewObject_Draw_Stub(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DVTARGETDEVICE *ptd,ULONG_PTR hdcTargetDev,ULONG_PTR hdcDraw,LPCRECTL lprcBounds,LPCRECTL lprcWBounds,IContinue *pContinue);
  HRESULT WINAPI IViewObject_GetColorSet_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DVTARGETDEVICE *ptd,HDC hicTargetDev,LOGPALETTE **ppColorSet);
  HRESULT WINAPI IViewObject_GetColorSet_Stub(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DVTARGETDEVICE *ptd,ULONG_PTR hicTargetDev,LOGPALETTE **ppColorSet);
  HRESULT WINAPI IViewObject_Freeze_Proxy(IViewObject *This,DWORD dwDrawAspect,LONG lindex,void *pvAspect,DWORD *pdwFreeze);
  HRESULT WINAPI IViewObject_Freeze_Stub(IViewObject *This,DWORD dwDrawAspect,LONG lindex,ULONG_PTR pvAspect,DWORD *pdwFreeze);
  HRESULT WINAPI IViewObject_GetAdvise_Proxy(IViewObject *This,DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink);
  HRESULT WINAPI IViewObject_GetAdvise_Stub(IViewObject *This,DWORD *pAspects,DWORD *pAdvf,IAdviseSink **ppAdvSink);
  HRESULT WINAPI IEnumOLEVERB_Next_Proxy(IEnumOLEVERB *This,ULONG celt,LPOLEVERB rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumOLEVERB_Next_Stub(IEnumOLEVERB *This,ULONG celt,LPOLEVERB rgelt,ULONG *pceltFetched);

#ifdef __cplusplus
}
#endif
#endif
