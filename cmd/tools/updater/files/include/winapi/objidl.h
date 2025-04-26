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

#ifndef __objidl_h__
#define __objidl_h__

#ifndef __IMarshal_FWD_DEFINED__
#define __IMarshal_FWD_DEFINED__
typedef struct IMarshal IMarshal;
#endif

#ifndef __IMarshal2_FWD_DEFINED__
#define __IMarshal2_FWD_DEFINED__
typedef struct IMarshal2 IMarshal2;
#endif

#ifndef __IMalloc_FWD_DEFINED__
#define __IMalloc_FWD_DEFINED__
typedef struct IMalloc IMalloc;
#endif

#ifndef __IMallocSpy_FWD_DEFINED__
#define __IMallocSpy_FWD_DEFINED__
typedef struct IMallocSpy IMallocSpy;
#endif

#ifndef __IStdMarshalInfo_FWD_DEFINED__
#define __IStdMarshalInfo_FWD_DEFINED__
typedef struct IStdMarshalInfo IStdMarshalInfo;
#endif

#ifndef __IExternalConnection_FWD_DEFINED__
#define __IExternalConnection_FWD_DEFINED__
typedef struct IExternalConnection IExternalConnection;
#endif

#ifndef __IMultiQI_FWD_DEFINED__
#define __IMultiQI_FWD_DEFINED__
typedef struct IMultiQI IMultiQI;
#endif

#ifndef __AsyncIMultiQI_FWD_DEFINED__
#define __AsyncIMultiQI_FWD_DEFINED__
typedef struct AsyncIMultiQI AsyncIMultiQI;
#endif

#ifndef __IInternalUnknown_FWD_DEFINED__
#define __IInternalUnknown_FWD_DEFINED__
typedef struct IInternalUnknown IInternalUnknown;
#endif

#ifndef __IEnumUnknown_FWD_DEFINED__
#define __IEnumUnknown_FWD_DEFINED__
typedef struct IEnumUnknown IEnumUnknown;
#endif

#ifndef __IBindCtx_FWD_DEFINED__
#define __IBindCtx_FWD_DEFINED__
typedef struct IBindCtx IBindCtx;
#endif

#ifndef __IEnumMoniker_FWD_DEFINED__
#define __IEnumMoniker_FWD_DEFINED__
typedef struct IEnumMoniker IEnumMoniker;
#endif

#ifndef __IRunnableObject_FWD_DEFINED__
#define __IRunnableObject_FWD_DEFINED__
typedef struct IRunnableObject IRunnableObject;
#endif

#ifndef __IRunningObjectTable_FWD_DEFINED__
#define __IRunningObjectTable_FWD_DEFINED__
typedef struct IRunningObjectTable IRunningObjectTable;
#endif

#ifndef __IPersist_FWD_DEFINED__
#define __IPersist_FWD_DEFINED__
typedef struct IPersist IPersist;
#endif

#ifndef __IPersistStream_FWD_DEFINED__
#define __IPersistStream_FWD_DEFINED__
typedef struct IPersistStream IPersistStream;
#endif

#ifndef __IMoniker_FWD_DEFINED__
#define __IMoniker_FWD_DEFINED__
typedef struct IMoniker IMoniker;
#endif

#ifndef __IROTData_FWD_DEFINED__
#define __IROTData_FWD_DEFINED__
typedef struct IROTData IROTData;
#endif

#ifndef __IEnumString_FWD_DEFINED__
#define __IEnumString_FWD_DEFINED__
typedef struct IEnumString IEnumString;
#endif

#ifndef __ISequentialStream_FWD_DEFINED__
#define __ISequentialStream_FWD_DEFINED__
typedef struct ISequentialStream ISequentialStream;
#endif

#ifndef __IStream_FWD_DEFINED__
#define __IStream_FWD_DEFINED__
typedef struct IStream IStream;
#endif

#ifndef __IEnumSTATSTG_FWD_DEFINED__
#define __IEnumSTATSTG_FWD_DEFINED__
typedef struct IEnumSTATSTG IEnumSTATSTG;
#endif

#ifndef __IStorage_FWD_DEFINED__
#define __IStorage_FWD_DEFINED__
typedef struct IStorage IStorage;
#endif

#ifndef __IPersistFile_FWD_DEFINED__
#define __IPersistFile_FWD_DEFINED__
typedef struct IPersistFile IPersistFile;
#endif

#ifndef __IPersistStorage_FWD_DEFINED__
#define __IPersistStorage_FWD_DEFINED__
typedef struct IPersistStorage IPersistStorage;
#endif

#ifndef __ILockBytes_FWD_DEFINED__
#define __ILockBytes_FWD_DEFINED__
typedef struct ILockBytes ILockBytes;
#endif

#ifndef __IEnumFORMATETC_FWD_DEFINED__
#define __IEnumFORMATETC_FWD_DEFINED__
typedef struct IEnumFORMATETC IEnumFORMATETC;
#endif

#ifndef __IEnumSTATDATA_FWD_DEFINED__
#define __IEnumSTATDATA_FWD_DEFINED__
typedef struct IEnumSTATDATA IEnumSTATDATA;
#endif

#ifndef __IRootStorage_FWD_DEFINED__
#define __IRootStorage_FWD_DEFINED__
typedef struct IRootStorage IRootStorage;
#endif

#ifndef __IAdviseSink_FWD_DEFINED__
#define __IAdviseSink_FWD_DEFINED__
typedef struct IAdviseSink IAdviseSink;
#endif

#ifndef __AsyncIAdviseSink_FWD_DEFINED__
#define __AsyncIAdviseSink_FWD_DEFINED__
typedef struct AsyncIAdviseSink AsyncIAdviseSink;
#endif

#ifndef __IAdviseSink2_FWD_DEFINED__
#define __IAdviseSink2_FWD_DEFINED__
typedef struct IAdviseSink2 IAdviseSink2;
#endif

#ifndef __AsyncIAdviseSink2_FWD_DEFINED__
#define __AsyncIAdviseSink2_FWD_DEFINED__
typedef struct AsyncIAdviseSink2 AsyncIAdviseSink2;
#endif

#ifndef __IDataObject_FWD_DEFINED__
#define __IDataObject_FWD_DEFINED__
typedef struct IDataObject IDataObject;
#endif

#ifndef __IDataAdviseHolder_FWD_DEFINED__
#define __IDataAdviseHolder_FWD_DEFINED__
typedef struct IDataAdviseHolder IDataAdviseHolder;
#endif

#ifndef __IMessageFilter_FWD_DEFINED__
#define __IMessageFilter_FWD_DEFINED__
typedef struct IMessageFilter IMessageFilter;
#endif

#ifndef __IRpcChannelBuffer_FWD_DEFINED__
#define __IRpcChannelBuffer_FWD_DEFINED__
typedef struct IRpcChannelBuffer IRpcChannelBuffer;
#endif

#ifndef __IRpcChannelBuffer2_FWD_DEFINED__
#define __IRpcChannelBuffer2_FWD_DEFINED__
typedef struct IRpcChannelBuffer2 IRpcChannelBuffer2;
#endif

#ifndef __IAsyncRpcChannelBuffer_FWD_DEFINED__
#define __IAsyncRpcChannelBuffer_FWD_DEFINED__
typedef struct IAsyncRpcChannelBuffer IAsyncRpcChannelBuffer;
#endif

#ifndef __IRpcChannelBuffer3_FWD_DEFINED__
#define __IRpcChannelBuffer3_FWD_DEFINED__
typedef struct IRpcChannelBuffer3 IRpcChannelBuffer3;
#endif

#ifndef __IRpcSyntaxNegotiate_FWD_DEFINED__
#define __IRpcSyntaxNegotiate_FWD_DEFINED__
typedef struct IRpcSyntaxNegotiate IRpcSyntaxNegotiate;
#endif

#ifndef __IRpcProxyBuffer_FWD_DEFINED__
#define __IRpcProxyBuffer_FWD_DEFINED__
typedef struct IRpcProxyBuffer IRpcProxyBuffer;
#endif

#ifndef __IRpcStubBuffer_FWD_DEFINED__
#define __IRpcStubBuffer_FWD_DEFINED__
typedef struct IRpcStubBuffer IRpcStubBuffer;
#endif

#ifndef __IPSFactoryBuffer_FWD_DEFINED__
#define __IPSFactoryBuffer_FWD_DEFINED__
typedef struct IPSFactoryBuffer IPSFactoryBuffer;
#endif

#ifndef __IChannelHook_FWD_DEFINED__
#define __IChannelHook_FWD_DEFINED__
typedef struct IChannelHook IChannelHook;
#endif

#ifndef __IClientSecurity_FWD_DEFINED__
#define __IClientSecurity_FWD_DEFINED__
typedef struct IClientSecurity IClientSecurity;
#endif

#ifndef __IServerSecurity_FWD_DEFINED__
#define __IServerSecurity_FWD_DEFINED__
typedef struct IServerSecurity IServerSecurity;
#endif

#ifndef __IClassActivator_FWD_DEFINED__
#define __IClassActivator_FWD_DEFINED__
typedef struct IClassActivator IClassActivator;
#endif

#ifndef __IRpcOptions_FWD_DEFINED__
#define __IRpcOptions_FWD_DEFINED__
typedef struct IRpcOptions IRpcOptions;
#endif

#ifndef __IFillLockBytes_FWD_DEFINED__
#define __IFillLockBytes_FWD_DEFINED__
typedef struct IFillLockBytes IFillLockBytes;
#endif

#ifndef __IProgressNotify_FWD_DEFINED__
#define __IProgressNotify_FWD_DEFINED__
typedef struct IProgressNotify IProgressNotify;
#endif

#ifndef __ILayoutStorage_FWD_DEFINED__
#define __ILayoutStorage_FWD_DEFINED__
typedef struct ILayoutStorage ILayoutStorage;
#endif

#ifndef __IBlockingLock_FWD_DEFINED__
#define __IBlockingLock_FWD_DEFINED__
typedef struct IBlockingLock IBlockingLock;
#endif

#ifndef __ITimeAndNoticeControl_FWD_DEFINED__
#define __ITimeAndNoticeControl_FWD_DEFINED__
typedef struct ITimeAndNoticeControl ITimeAndNoticeControl;
#endif

#ifndef __IOplockStorage_FWD_DEFINED__
#define __IOplockStorage_FWD_DEFINED__
typedef struct IOplockStorage IOplockStorage;
#endif

#ifndef __ISurrogate_FWD_DEFINED__
#define __ISurrogate_FWD_DEFINED__
typedef struct ISurrogate ISurrogate;
#endif

#ifndef __IGlobalInterfaceTable_FWD_DEFINED__
#define __IGlobalInterfaceTable_FWD_DEFINED__
typedef struct IGlobalInterfaceTable IGlobalInterfaceTable;
#endif

#ifndef __IDirectWriterLock_FWD_DEFINED__
#define __IDirectWriterLock_FWD_DEFINED__
typedef struct IDirectWriterLock IDirectWriterLock;
#endif

#ifndef __ISynchronize_FWD_DEFINED__
#define __ISynchronize_FWD_DEFINED__
typedef struct ISynchronize ISynchronize;
#endif

#ifndef __ISynchronizeHandle_FWD_DEFINED__
#define __ISynchronizeHandle_FWD_DEFINED__
typedef struct ISynchronizeHandle ISynchronizeHandle;
#endif

#ifndef __ISynchronizeEvent_FWD_DEFINED__
#define __ISynchronizeEvent_FWD_DEFINED__
typedef struct ISynchronizeEvent ISynchronizeEvent;
#endif

#ifndef __ISynchronizeContainer_FWD_DEFINED__
#define __ISynchronizeContainer_FWD_DEFINED__
typedef struct ISynchronizeContainer ISynchronizeContainer;
#endif

#ifndef __ISynchronizeMutex_FWD_DEFINED__
#define __ISynchronizeMutex_FWD_DEFINED__
typedef struct ISynchronizeMutex ISynchronizeMutex;
#endif

#ifndef __ICancelMethodCalls_FWD_DEFINED__
#define __ICancelMethodCalls_FWD_DEFINED__
typedef struct ICancelMethodCalls ICancelMethodCalls;
#endif

#ifndef __IAsyncManager_FWD_DEFINED__
#define __IAsyncManager_FWD_DEFINED__
typedef struct IAsyncManager IAsyncManager;
#endif

#ifndef __ICallFactory_FWD_DEFINED__
#define __ICallFactory_FWD_DEFINED__
typedef struct ICallFactory ICallFactory;
#endif

#ifndef __IRpcHelper_FWD_DEFINED__
#define __IRpcHelper_FWD_DEFINED__
typedef struct IRpcHelper IRpcHelper;
#endif

#ifndef __IReleaseMarshalBuffers_FWD_DEFINED__
#define __IReleaseMarshalBuffers_FWD_DEFINED__
typedef struct IReleaseMarshalBuffers IReleaseMarshalBuffers;
#endif

#ifndef __IWaitMultiple_FWD_DEFINED__
#define __IWaitMultiple_FWD_DEFINED__
typedef struct IWaitMultiple IWaitMultiple;
#endif

#ifndef __IUrlMon_FWD_DEFINED__
#define __IUrlMon_FWD_DEFINED__
typedef struct IUrlMon IUrlMon;
#endif

#ifndef __IForegroundTransfer_FWD_DEFINED__
#define __IForegroundTransfer_FWD_DEFINED__
typedef struct IForegroundTransfer IForegroundTransfer;
#endif

#ifndef __IAddrTrackingControl_FWD_DEFINED__
#define __IAddrTrackingControl_FWD_DEFINED__
typedef struct IAddrTrackingControl IAddrTrackingControl;
#endif

#ifndef __IAddrExclusionControl_FWD_DEFINED__
#define __IAddrExclusionControl_FWD_DEFINED__
typedef struct IAddrExclusionControl IAddrExclusionControl;
#endif

#ifndef __IPipeByte_FWD_DEFINED__
#define __IPipeByte_FWD_DEFINED__
typedef struct IPipeByte IPipeByte;
#endif

#ifndef __AsyncIPipeByte_FWD_DEFINED__
#define __AsyncIPipeByte_FWD_DEFINED__
typedef struct AsyncIPipeByte AsyncIPipeByte;
#endif

#ifndef __IPipeLong_FWD_DEFINED__
#define __IPipeLong_FWD_DEFINED__
typedef struct IPipeLong IPipeLong;
#endif

#ifndef __AsyncIPipeLong_FWD_DEFINED__
#define __AsyncIPipeLong_FWD_DEFINED__
typedef struct AsyncIPipeLong AsyncIPipeLong;
#endif

#ifndef __IPipeDouble_FWD_DEFINED__
#define __IPipeDouble_FWD_DEFINED__
typedef struct IPipeDouble IPipeDouble;
#endif

#ifndef __AsyncIPipeDouble_FWD_DEFINED__
#define __AsyncIPipeDouble_FWD_DEFINED__
typedef struct AsyncIPipeDouble AsyncIPipeDouble;
#endif

#ifndef __IThumbnailExtractor_FWD_DEFINED__
#define __IThumbnailExtractor_FWD_DEFINED__
typedef struct IThumbnailExtractor IThumbnailExtractor;
#endif

#ifndef __IDummyHICONIncluder_FWD_DEFINED__
#define __IDummyHICONIncluder_FWD_DEFINED__
typedef struct IDummyHICONIncluder IDummyHICONIncluder;
#endif

#ifndef __IEnumContextProps_FWD_DEFINED__
#define __IEnumContextProps_FWD_DEFINED__
typedef struct IEnumContextProps IEnumContextProps;
#endif

#ifndef __IContext_FWD_DEFINED__
#define __IContext_FWD_DEFINED__
typedef struct IContext IContext;
#endif

#ifndef __IObjContext_FWD_DEFINED__
#define __IObjContext_FWD_DEFINED__
typedef struct IObjContext IObjContext;
#endif

#ifndef __IProcessLock_FWD_DEFINED__
#define __IProcessLock_FWD_DEFINED__
typedef struct IProcessLock IProcessLock;
#endif

#ifndef __ISurrogateService_FWD_DEFINED__
#define __ISurrogateService_FWD_DEFINED__
typedef struct ISurrogateService ISurrogateService;
#endif

#ifndef __IComThreadingInfo_FWD_DEFINED__
#define __IComThreadingInfo_FWD_DEFINED__
typedef struct IComThreadingInfo IComThreadingInfo;
#endif

#ifndef __IProcessInitControl_FWD_DEFINED__
#define __IProcessInitControl_FWD_DEFINED__
typedef struct IProcessInitControl IProcessInitControl;
#endif

#ifndef __IInitializeSpy_FWD_DEFINED__
#define __IInitializeSpy_FWD_DEFINED__
typedef struct IInitializeSpy IInitializeSpy;
#endif

#include "unknwn.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __MIDL_user_allocate_free_DEFINED__
#define __MIDL_user_allocate_free_DEFINED__
  void *__RPC_API MIDL_user_allocate(size_t);
  void __RPC_API MIDL_user_free(void *);
#endif

  typedef struct _COSERVERINFO {
    DWORD dwReserved1;
    LPWSTR pwszName;
    COAUTHINFO *pAuthInfo;
    DWORD dwReserved2;
  } COSERVERINFO;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0000_v0_0_s_ifspec;

#ifndef __IMarshal_INTERFACE_DEFINED__
#define __IMarshal_INTERFACE_DEFINED__
  typedef IMarshal *LPMARSHAL;

  EXTERN_C const IID IID_IMarshal;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMarshal : public IUnknown {
  public:
    virtual HRESULT WINAPI GetUnmarshalClass(REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,CLSID *pCid) = 0;
    virtual HRESULT WINAPI GetMarshalSizeMax(REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,DWORD *pSize) = 0;
    virtual HRESULT WINAPI MarshalInterface(IStream *pStm,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags) = 0;
    virtual HRESULT WINAPI UnmarshalInterface(IStream *pStm,REFIID riid,void **ppv) = 0;
    virtual HRESULT WINAPI ReleaseMarshalData(IStream *pStm) = 0;
    virtual HRESULT WINAPI DisconnectObject(DWORD dwReserved) = 0;
  };
#else
  typedef struct IMarshalVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMarshal *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMarshal *This);
      ULONG (WINAPI *Release)(IMarshal *This);
      HRESULT (WINAPI *GetUnmarshalClass)(IMarshal *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,CLSID *pCid);
      HRESULT (WINAPI *GetMarshalSizeMax)(IMarshal *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,DWORD *pSize);
      HRESULT (WINAPI *MarshalInterface)(IMarshal *This,IStream *pStm,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags);
      HRESULT (WINAPI *UnmarshalInterface)(IMarshal *This,IStream *pStm,REFIID riid,void **ppv);
      HRESULT (WINAPI *ReleaseMarshalData)(IMarshal *This,IStream *pStm);
      HRESULT (WINAPI *DisconnectObject)(IMarshal *This,DWORD dwReserved);
    END_INTERFACE
  } IMarshalVtbl;
  struct IMarshal {
    CONST_VTBL struct IMarshalVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMarshal_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMarshal_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMarshal_Release(This) (This)->lpVtbl->Release(This)
#define IMarshal_GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid) (This)->lpVtbl->GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)
#define IMarshal_GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize) (This)->lpVtbl->GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)
#define IMarshal_MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags) (This)->lpVtbl->MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)
#define IMarshal_UnmarshalInterface(This,pStm,riid,ppv) (This)->lpVtbl->UnmarshalInterface(This,pStm,riid,ppv)
#define IMarshal_ReleaseMarshalData(This,pStm) (This)->lpVtbl->ReleaseMarshalData(This,pStm)
#define IMarshal_DisconnectObject(This,dwReserved) (This)->lpVtbl->DisconnectObject(This,dwReserved)
#endif
#endif
  HRESULT WINAPI IMarshal_GetUnmarshalClass_Proxy(IMarshal *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,CLSID *pCid);
  void __RPC_STUB IMarshal_GetUnmarshalClass_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMarshal_GetMarshalSizeMax_Proxy(IMarshal *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,DWORD *pSize);
  void __RPC_STUB IMarshal_GetMarshalSizeMax_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMarshal_MarshalInterface_Proxy(IMarshal *This,IStream *pStm,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags);
  void __RPC_STUB IMarshal_MarshalInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMarshal_UnmarshalInterface_Proxy(IMarshal *This,IStream *pStm,REFIID riid,void **ppv);
  void __RPC_STUB IMarshal_UnmarshalInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMarshal_ReleaseMarshalData_Proxy(IMarshal *This,IStream *pStm);
  void __RPC_STUB IMarshal_ReleaseMarshalData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMarshal_DisconnectObject_Proxy(IMarshal *This,DWORD dwReserved);
  void __RPC_STUB IMarshal_DisconnectObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IMarshal2_INTERFACE_DEFINED__
#define __IMarshal2_INTERFACE_DEFINED__
  typedef IMarshal2 *LPMARSHAL2;

  EXTERN_C const IID IID_IMarshal2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMarshal2 : public IMarshal {
  };
#else
  typedef struct IMarshal2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMarshal2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMarshal2 *This);
      ULONG (WINAPI *Release)(IMarshal2 *This);
      HRESULT (WINAPI *GetUnmarshalClass)(IMarshal2 *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,CLSID *pCid);
      HRESULT (WINAPI *GetMarshalSizeMax)(IMarshal2 *This,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags,DWORD *pSize);
      HRESULT (WINAPI *MarshalInterface)(IMarshal2 *This,IStream *pStm,REFIID riid,void *pv,DWORD dwDestContext,void *pvDestContext,DWORD mshlflags);
      HRESULT (WINAPI *UnmarshalInterface)(IMarshal2 *This,IStream *pStm,REFIID riid,void **ppv);
      HRESULT (WINAPI *ReleaseMarshalData)(IMarshal2 *This,IStream *pStm);
      HRESULT (WINAPI *DisconnectObject)(IMarshal2 *This,DWORD dwReserved);
    END_INTERFACE
  } IMarshal2Vtbl;
  struct IMarshal2 {
    CONST_VTBL struct IMarshal2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMarshal2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMarshal2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMarshal2_Release(This) (This)->lpVtbl->Release(This)
#define IMarshal2_GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid) (This)->lpVtbl->GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)
#define IMarshal2_GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize) (This)->lpVtbl->GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)
#define IMarshal2_MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags) (This)->lpVtbl->MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)
#define IMarshal2_UnmarshalInterface(This,pStm,riid,ppv) (This)->lpVtbl->UnmarshalInterface(This,pStm,riid,ppv)
#define IMarshal2_ReleaseMarshalData(This,pStm) (This)->lpVtbl->ReleaseMarshalData(This,pStm)
#define IMarshal2_DisconnectObject(This,dwReserved) (This)->lpVtbl->DisconnectObject(This,dwReserved)
#endif
#endif
#endif

#ifndef __IMalloc_INTERFACE_DEFINED__
#define __IMalloc_INTERFACE_DEFINED__
  typedef IMalloc *LPMALLOC;

  EXTERN_C const IID IID_IMalloc;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMalloc : public IUnknown {
  public:
    virtual void *WINAPI Alloc(SIZE_T cb) = 0;
    virtual void *WINAPI Realloc(void *pv,SIZE_T cb) = 0;
    virtual void WINAPI Free(void *pv) = 0;
    virtual SIZE_T WINAPI GetSize(void *pv) = 0;
    virtual int WINAPI DidAlloc(void *pv) = 0;
    virtual void WINAPI HeapMinimize(void) = 0;
  };
#else
  typedef struct IMallocVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMalloc *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMalloc *This);
      ULONG (WINAPI *Release)(IMalloc *This);
      void *(WINAPI *Alloc)(IMalloc *This,SIZE_T cb);
      void *(WINAPI *Realloc)(IMalloc *This,void *pv,SIZE_T cb);
      void (WINAPI *Free)(IMalloc *This,void *pv);
      SIZE_T (WINAPI *GetSize)(IMalloc *This,void *pv);
      int (WINAPI *DidAlloc)(IMalloc *This,void *pv);
      void (WINAPI *HeapMinimize)(IMalloc *This);
    END_INTERFACE
  } IMallocVtbl;
  struct IMalloc {
    CONST_VTBL struct IMallocVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMalloc_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMalloc_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMalloc_Release(This) (This)->lpVtbl->Release(This)
#define IMalloc_Alloc(This,cb) (This)->lpVtbl->Alloc(This,cb)
#define IMalloc_Realloc(This,pv,cb) (This)->lpVtbl->Realloc(This,pv,cb)
#define IMalloc_Free(This,pv) (This)->lpVtbl->Free(This,pv)
#define IMalloc_GetSize(This,pv) (This)->lpVtbl->GetSize(This,pv)
#define IMalloc_DidAlloc(This,pv) (This)->lpVtbl->DidAlloc(This,pv)
#define IMalloc_HeapMinimize(This) (This)->lpVtbl->HeapMinimize(This)
#endif
#endif
  void *WINAPI IMalloc_Alloc_Proxy(IMalloc *This,SIZE_T cb);
  void __RPC_STUB IMalloc_Alloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMalloc_Realloc_Proxy(IMalloc *This,void *pv,SIZE_T cb);
  void __RPC_STUB IMalloc_Realloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IMalloc_Free_Proxy(IMalloc *This,void *pv);
  void __RPC_STUB IMalloc_Free_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  SIZE_T WINAPI IMalloc_GetSize_Proxy(IMalloc *This,void *pv);
  void __RPC_STUB IMalloc_GetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  int WINAPI IMalloc_DidAlloc_Proxy(IMalloc *This,void *pv);
  void __RPC_STUB IMalloc_DidAlloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IMalloc_HeapMinimize_Proxy(IMalloc *This);
  void __RPC_STUB IMalloc_HeapMinimize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IMallocSpy_INTERFACE_DEFINED__
#define __IMallocSpy_INTERFACE_DEFINED__
  typedef IMallocSpy *LPMALLOCSPY;

  EXTERN_C const IID IID_IMallocSpy;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMallocSpy : public IUnknown {
  public:
    virtual SIZE_T WINAPI PreAlloc(SIZE_T cbRequest) = 0;
    virtual void *WINAPI PostAlloc(void *pActual) = 0;
    virtual void *WINAPI PreFree(void *pRequest,WINBOOL fSpyed) = 0;
    virtual void WINAPI PostFree(WINBOOL fSpyed) = 0;
    virtual SIZE_T WINAPI PreRealloc(void *pRequest,SIZE_T cbRequest,void **ppNewRequest,WINBOOL fSpyed) = 0;
    virtual void *WINAPI PostRealloc(void *pActual,WINBOOL fSpyed) = 0;
    virtual void *WINAPI PreGetSize(void *pRequest,WINBOOL fSpyed) = 0;
    virtual SIZE_T WINAPI PostGetSize(SIZE_T cbActual,WINBOOL fSpyed) = 0;
    virtual void *WINAPI PreDidAlloc(void *pRequest,WINBOOL fSpyed) = 0;
    virtual int WINAPI PostDidAlloc(void *pRequest,WINBOOL fSpyed,int fActual) = 0;
    virtual void WINAPI PreHeapMinimize(void) = 0;
    virtual void WINAPI PostHeapMinimize(void) = 0;
  };
#else
  typedef struct IMallocSpyVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMallocSpy *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMallocSpy *This);
      ULONG (WINAPI *Release)(IMallocSpy *This);
      SIZE_T (WINAPI *PreAlloc)(IMallocSpy *This,SIZE_T cbRequest);
      void *(WINAPI *PostAlloc)(IMallocSpy *This,void *pActual);
      void *(WINAPI *PreFree)(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
      void (WINAPI *PostFree)(IMallocSpy *This,WINBOOL fSpyed);
      SIZE_T (WINAPI *PreRealloc)(IMallocSpy *This,void *pRequest,SIZE_T cbRequest,void **ppNewRequest,WINBOOL fSpyed);
      void *(WINAPI *PostRealloc)(IMallocSpy *This,void *pActual,WINBOOL fSpyed);
      void *(WINAPI *PreGetSize)(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
      SIZE_T (WINAPI *PostGetSize)(IMallocSpy *This,SIZE_T cbActual,WINBOOL fSpyed);
      void *(WINAPI *PreDidAlloc)(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
      int (WINAPI *PostDidAlloc)(IMallocSpy *This,void *pRequest,WINBOOL fSpyed,int fActual);
      void (WINAPI *PreHeapMinimize)(IMallocSpy *This);
      void (WINAPI *PostHeapMinimize)(IMallocSpy *This);
    END_INTERFACE
  } IMallocSpyVtbl;
  struct IMallocSpy {
    CONST_VTBL struct IMallocSpyVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMallocSpy_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMallocSpy_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMallocSpy_Release(This) (This)->lpVtbl->Release(This)
#define IMallocSpy_PreAlloc(This,cbRequest) (This)->lpVtbl->PreAlloc(This,cbRequest)
#define IMallocSpy_PostAlloc(This,pActual) (This)->lpVtbl->PostAlloc(This,pActual)
#define IMallocSpy_PreFree(This,pRequest,fSpyed) (This)->lpVtbl->PreFree(This,pRequest,fSpyed)
#define IMallocSpy_PostFree(This,fSpyed) (This)->lpVtbl->PostFree(This,fSpyed)
#define IMallocSpy_PreRealloc(This,pRequest,cbRequest,ppNewRequest,fSpyed) (This)->lpVtbl->PreRealloc(This,pRequest,cbRequest,ppNewRequest,fSpyed)
#define IMallocSpy_PostRealloc(This,pActual,fSpyed) (This)->lpVtbl->PostRealloc(This,pActual,fSpyed)
#define IMallocSpy_PreGetSize(This,pRequest,fSpyed) (This)->lpVtbl->PreGetSize(This,pRequest,fSpyed)
#define IMallocSpy_PostGetSize(This,cbActual,fSpyed) (This)->lpVtbl->PostGetSize(This,cbActual,fSpyed)
#define IMallocSpy_PreDidAlloc(This,pRequest,fSpyed) (This)->lpVtbl->PreDidAlloc(This,pRequest,fSpyed)
#define IMallocSpy_PostDidAlloc(This,pRequest,fSpyed,fActual) (This)->lpVtbl->PostDidAlloc(This,pRequest,fSpyed,fActual)
#define IMallocSpy_PreHeapMinimize(This) (This)->lpVtbl->PreHeapMinimize(This)
#define IMallocSpy_PostHeapMinimize(This) (This)->lpVtbl->PostHeapMinimize(This)
#endif
#endif
  SIZE_T WINAPI IMallocSpy_PreAlloc_Proxy(IMallocSpy *This,SIZE_T cbRequest);
  void __RPC_STUB IMallocSpy_PreAlloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMallocSpy_PostAlloc_Proxy(IMallocSpy *This,void *pActual);
  void __RPC_STUB IMallocSpy_PostAlloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMallocSpy_PreFree_Proxy(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PreFree_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IMallocSpy_PostFree_Proxy(IMallocSpy *This,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PostFree_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  SIZE_T WINAPI IMallocSpy_PreRealloc_Proxy(IMallocSpy *This,void *pRequest,SIZE_T cbRequest,void **ppNewRequest,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PreRealloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMallocSpy_PostRealloc_Proxy(IMallocSpy *This,void *pActual,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PostRealloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMallocSpy_PreGetSize_Proxy(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PreGetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  SIZE_T WINAPI IMallocSpy_PostGetSize_Proxy(IMallocSpy *This,SIZE_T cbActual,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PostGetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void *WINAPI IMallocSpy_PreDidAlloc_Proxy(IMallocSpy *This,void *pRequest,WINBOOL fSpyed);
  void __RPC_STUB IMallocSpy_PreDidAlloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  int WINAPI IMallocSpy_PostDidAlloc_Proxy(IMallocSpy *This,void *pRequest,WINBOOL fSpyed,int fActual);
  void __RPC_STUB IMallocSpy_PostDidAlloc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IMallocSpy_PreHeapMinimize_Proxy(IMallocSpy *This);
  void __RPC_STUB IMallocSpy_PreHeapMinimize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IMallocSpy_PostHeapMinimize_Proxy(IMallocSpy *This);
  void __RPC_STUB IMallocSpy_PostHeapMinimize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IStdMarshalInfo_INTERFACE_DEFINED__
#define __IStdMarshalInfo_INTERFACE_DEFINED__
  typedef IStdMarshalInfo *LPSTDMARSHALINFO;

  EXTERN_C const IID IID_IStdMarshalInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IStdMarshalInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetClassForHandler(DWORD dwDestContext,void *pvDestContext,CLSID *pClsid) = 0;
  };
#else
  typedef struct IStdMarshalInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IStdMarshalInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IStdMarshalInfo *This);
      ULONG (WINAPI *Release)(IStdMarshalInfo *This);
      HRESULT (WINAPI *GetClassForHandler)(IStdMarshalInfo *This,DWORD dwDestContext,void *pvDestContext,CLSID *pClsid);
    END_INTERFACE
  } IStdMarshalInfoVtbl;
  struct IStdMarshalInfo {
    CONST_VTBL struct IStdMarshalInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IStdMarshalInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IStdMarshalInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IStdMarshalInfo_Release(This) (This)->lpVtbl->Release(This)
#define IStdMarshalInfo_GetClassForHandler(This,dwDestContext,pvDestContext,pClsid) (This)->lpVtbl->GetClassForHandler(This,dwDestContext,pvDestContext,pClsid)
#endif
#endif
  HRESULT WINAPI IStdMarshalInfo_GetClassForHandler_Proxy(IStdMarshalInfo *This,DWORD dwDestContext,void *pvDestContext,CLSID *pClsid);
  void __RPC_STUB IStdMarshalInfo_GetClassForHandler_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IExternalConnection_INTERFACE_DEFINED__
#define __IExternalConnection_INTERFACE_DEFINED__
  typedef IExternalConnection *LPEXTERNALCONNECTION;

  typedef enum tagEXTCONN {
    EXTCONN_STRONG = 0x1,EXTCONN_WEAK = 0x2,EXTCONN_CALLABLE = 0x4
  } EXTCONN;

  EXTERN_C const IID IID_IExternalConnection;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IExternalConnection : public IUnknown {
  public:
    virtual DWORD WINAPI AddConnection(DWORD extconn,DWORD reserved) = 0;
    virtual DWORD WINAPI ReleaseConnection(DWORD extconn,DWORD reserved,WINBOOL fLastReleaseCloses) = 0;
  };
#else
  typedef struct IExternalConnectionVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IExternalConnection *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IExternalConnection *This);
      ULONG (WINAPI *Release)(IExternalConnection *This);
      DWORD (WINAPI *AddConnection)(IExternalConnection *This,DWORD extconn,DWORD reserved);
      DWORD (WINAPI *ReleaseConnection)(IExternalConnection *This,DWORD extconn,DWORD reserved,WINBOOL fLastReleaseCloses);
    END_INTERFACE
  } IExternalConnectionVtbl;
  struct IExternalConnection {
    CONST_VTBL struct IExternalConnectionVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IExternalConnection_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IExternalConnection_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IExternalConnection_Release(This) (This)->lpVtbl->Release(This)
#define IExternalConnection_AddConnection(This,extconn,reserved) (This)->lpVtbl->AddConnection(This,extconn,reserved)
#define IExternalConnection_ReleaseConnection(This,extconn,reserved,fLastReleaseCloses) (This)->lpVtbl->ReleaseConnection(This,extconn,reserved,fLastReleaseCloses)
#endif
#endif
  DWORD WINAPI IExternalConnection_AddConnection_Proxy(IExternalConnection *This,DWORD extconn,DWORD reserved);
  void __RPC_STUB IExternalConnection_AddConnection_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  DWORD WINAPI IExternalConnection_ReleaseConnection_Proxy(IExternalConnection *This,DWORD extconn,DWORD reserved,WINBOOL fLastReleaseCloses);
  void __RPC_STUB IExternalConnection_ReleaseConnection_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  typedef IMultiQI *LPMULTIQI;

  typedef struct tagMULTI_QI {
    const IID *pIID;
    IUnknown *pItf;
    HRESULT hr;
  } MULTI_QI;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0015_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0015_v0_0_s_ifspec;
#ifndef __IMultiQI_INTERFACE_DEFINED__
#define __IMultiQI_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IMultiQI;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMultiQI : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryMultipleInterfaces(ULONG cMQIs,MULTI_QI *pMQIs) = 0;
  };
#else
  typedef struct IMultiQIVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMultiQI *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMultiQI *This);
      ULONG (WINAPI *Release)(IMultiQI *This);
      HRESULT (WINAPI *QueryMultipleInterfaces)(IMultiQI *This,ULONG cMQIs,MULTI_QI *pMQIs);
    END_INTERFACE
  } IMultiQIVtbl;
  struct IMultiQI {
    CONST_VTBL struct IMultiQIVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMultiQI_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMultiQI_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMultiQI_Release(This) (This)->lpVtbl->Release(This)
#define IMultiQI_QueryMultipleInterfaces(This,cMQIs,pMQIs) (This)->lpVtbl->QueryMultipleInterfaces(This,cMQIs,pMQIs)
#endif
#endif
  HRESULT WINAPI IMultiQI_QueryMultipleInterfaces_Proxy(IMultiQI *This,ULONG cMQIs,MULTI_QI *pMQIs);
  void __RPC_STUB IMultiQI_QueryMultipleInterfaces_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIMultiQI_INTERFACE_DEFINED__
#define __AsyncIMultiQI_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIMultiQI;
#if defined(__cplusplus) && !defined(CINTERFACE)

  struct AsyncIMultiQI : public IUnknown {
  public:
    virtual HRESULT WINAPI Begin_QueryMultipleInterfaces(ULONG cMQIs,MULTI_QI *pMQIs) = 0;
    virtual HRESULT WINAPI Finish_QueryMultipleInterfaces(MULTI_QI *pMQIs) = 0;
  };
#else
  typedef struct AsyncIMultiQIVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIMultiQI *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIMultiQI *This);
      ULONG (WINAPI *Release)(AsyncIMultiQI *This);
      HRESULT (WINAPI *Begin_QueryMultipleInterfaces)(AsyncIMultiQI *This,ULONG cMQIs,MULTI_QI *pMQIs);
      HRESULT (WINAPI *Finish_QueryMultipleInterfaces)(AsyncIMultiQI *This,MULTI_QI *pMQIs);
    END_INTERFACE
  } AsyncIMultiQIVtbl;
  struct AsyncIMultiQI {
    CONST_VTBL struct AsyncIMultiQIVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIMultiQI_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIMultiQI_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIMultiQI_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIMultiQI_Begin_QueryMultipleInterfaces(This,cMQIs,pMQIs) (This)->lpVtbl->Begin_QueryMultipleInterfaces(This,cMQIs,pMQIs)
#define AsyncIMultiQI_Finish_QueryMultipleInterfaces(This,pMQIs) (This)->lpVtbl->Finish_QueryMultipleInterfaces(This,pMQIs)
#endif
#endif
  HRESULT WINAPI AsyncIMultiQI_Begin_QueryMultipleInterfaces_Proxy(AsyncIMultiQI *This,ULONG cMQIs,MULTI_QI *pMQIs);
  void __RPC_STUB AsyncIMultiQI_Begin_QueryMultipleInterfaces_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIMultiQI_Finish_QueryMultipleInterfaces_Proxy(AsyncIMultiQI *This,MULTI_QI *pMQIs);
  void __RPC_STUB AsyncIMultiQI_Finish_QueryMultipleInterfaces_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IInternalUnknown_INTERFACE_DEFINED__
#define __IInternalUnknown_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IInternalUnknown;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternalUnknown : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryInternalInterface(REFIID riid,void **ppv) = 0;
  };
#else
  typedef struct IInternalUnknownVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternalUnknown *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternalUnknown *This);
      ULONG (WINAPI *Release)(IInternalUnknown *This);
      HRESULT (WINAPI *QueryInternalInterface)(IInternalUnknown *This,REFIID riid,void **ppv);
    END_INTERFACE
  } IInternalUnknownVtbl;
  struct IInternalUnknown {
    CONST_VTBL struct IInternalUnknownVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternalUnknown_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternalUnknown_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternalUnknown_Release(This) (This)->lpVtbl->Release(This)
#define IInternalUnknown_QueryInternalInterface(This,riid,ppv) (This)->lpVtbl->QueryInternalInterface(This,riid,ppv)
#endif
#endif
  HRESULT WINAPI IInternalUnknown_QueryInternalInterface_Proxy(IInternalUnknown *This,REFIID riid,void **ppv);
  void __RPC_STUB IInternalUnknown_QueryInternalInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumUnknown_INTERFACE_DEFINED__
#define __IEnumUnknown_INTERFACE_DEFINED__
  typedef IEnumUnknown *LPENUMUNKNOWN;

  EXTERN_C const IID IID_IEnumUnknown;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumUnknown : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,IUnknown **rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumUnknown **ppenum) = 0;
  };
#else
  typedef struct IEnumUnknownVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumUnknown *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumUnknown *This);
      ULONG (WINAPI *Release)(IEnumUnknown *This);
      HRESULT (WINAPI *Next)(IEnumUnknown *This,ULONG celt,IUnknown **rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumUnknown *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumUnknown *This);
      HRESULT (WINAPI *Clone)(IEnumUnknown *This,IEnumUnknown **ppenum);
    END_INTERFACE
  } IEnumUnknownVtbl;
  struct IEnumUnknown {
    CONST_VTBL struct IEnumUnknownVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumUnknown_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumUnknown_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumUnknown_Release(This) (This)->lpVtbl->Release(This)
#define IEnumUnknown_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumUnknown_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumUnknown_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumUnknown_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumUnknown_RemoteNext_Proxy(IEnumUnknown *This,ULONG celt,IUnknown **rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumUnknown_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumUnknown_Skip_Proxy(IEnumUnknown *This,ULONG celt);
  void __RPC_STUB IEnumUnknown_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumUnknown_Reset_Proxy(IEnumUnknown *This);
  void __RPC_STUB IEnumUnknown_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumUnknown_Clone_Proxy(IEnumUnknown *This,IEnumUnknown **ppenum);
  void __RPC_STUB IEnumUnknown_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IBindCtx_INTERFACE_DEFINED__
#define __IBindCtx_INTERFACE_DEFINED__
  typedef IBindCtx *LPBC;
  typedef IBindCtx *LPBINDCTX;

  typedef struct tagBIND_OPTS {
    DWORD cbStruct;
    DWORD grfFlags;
    DWORD grfMode;
    DWORD dwTickCountDeadline;
  } BIND_OPTS;

  typedef struct tagBIND_OPTS *LPBIND_OPTS;

#if defined(__cplusplus)
  typedef struct tagBIND_OPTS2 : tagBIND_OPTS {
    DWORD dwTrackFlags;
    DWORD dwClassContext;
    LCID locale;
    COSERVERINFO *pServerInfo;
  } BIND_OPTS2,*LPBIND_OPTS2;
#else
  typedef struct tagBIND_OPTS2 {
    DWORD cbStruct;
    DWORD grfFlags;
    DWORD grfMode;
    DWORD dwTickCountDeadline;
    DWORD dwTrackFlags;
    DWORD dwClassContext;
    LCID locale;
    COSERVERINFO *pServerInfo;
  } BIND_OPTS2;

  typedef struct tagBIND_OPTS2 *LPBIND_OPTS2;
#endif
  typedef enum tagBIND_FLAGS {
    BIND_MAYBOTHERUSER = 1,BIND_JUSTTESTEXISTENCE = 2
  } BIND_FLAGS;

  EXTERN_C const IID IID_IBindCtx;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBindCtx : public IUnknown {
  public:
    virtual HRESULT WINAPI RegisterObjectBound(IUnknown *punk) = 0;
    virtual HRESULT WINAPI RevokeObjectBound(IUnknown *punk) = 0;
    virtual HRESULT WINAPI ReleaseBoundObjects(void) = 0;
    virtual HRESULT WINAPI SetBindOptions(BIND_OPTS *pbindopts) = 0;
    virtual HRESULT WINAPI GetBindOptions(BIND_OPTS *pbindopts) = 0;
    virtual HRESULT WINAPI GetRunningObjectTable(IRunningObjectTable **pprot) = 0;
    virtual HRESULT WINAPI RegisterObjectParam(LPOLESTR pszKey,IUnknown *punk) = 0;
    virtual HRESULT WINAPI GetObjectParam(LPOLESTR pszKey,IUnknown **ppunk) = 0;
    virtual HRESULT WINAPI EnumObjectParam(IEnumString **ppenum) = 0;
    virtual HRESULT WINAPI RevokeObjectParam(LPOLESTR pszKey) = 0;
  };
#else
  typedef struct IBindCtxVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBindCtx *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBindCtx *This);
      ULONG (WINAPI *Release)(IBindCtx *This);
      HRESULT (WINAPI *RegisterObjectBound)(IBindCtx *This,IUnknown *punk);
      HRESULT (WINAPI *RevokeObjectBound)(IBindCtx *This,IUnknown *punk);
      HRESULT (WINAPI *ReleaseBoundObjects)(IBindCtx *This);
      HRESULT (WINAPI *SetBindOptions)(IBindCtx *This,BIND_OPTS *pbindopts);
      HRESULT (WINAPI *GetBindOptions)(IBindCtx *This,BIND_OPTS *pbindopts);
      HRESULT (WINAPI *GetRunningObjectTable)(IBindCtx *This,IRunningObjectTable **pprot);
      HRESULT (WINAPI *RegisterObjectParam)(IBindCtx *This,LPOLESTR pszKey,IUnknown *punk);
      HRESULT (WINAPI *GetObjectParam)(IBindCtx *This,LPOLESTR pszKey,IUnknown **ppunk);
      HRESULT (WINAPI *EnumObjectParam)(IBindCtx *This,IEnumString **ppenum);
      HRESULT (WINAPI *RevokeObjectParam)(IBindCtx *This,LPOLESTR pszKey);
    END_INTERFACE
  } IBindCtxVtbl;
  struct IBindCtx {
    CONST_VTBL struct IBindCtxVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBindCtx_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBindCtx_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBindCtx_Release(This) (This)->lpVtbl->Release(This)
#define IBindCtx_RegisterObjectBound(This,punk) (This)->lpVtbl->RegisterObjectBound(This,punk)
#define IBindCtx_RevokeObjectBound(This,punk) (This)->lpVtbl->RevokeObjectBound(This,punk)
#define IBindCtx_ReleaseBoundObjects(This) (This)->lpVtbl->ReleaseBoundObjects(This)
#define IBindCtx_SetBindOptions(This,pbindopts) (This)->lpVtbl->SetBindOptions(This,pbindopts)
#define IBindCtx_GetBindOptions(This,pbindopts) (This)->lpVtbl->GetBindOptions(This,pbindopts)
#define IBindCtx_GetRunningObjectTable(This,pprot) (This)->lpVtbl->GetRunningObjectTable(This,pprot)
#define IBindCtx_RegisterObjectParam(This,pszKey,punk) (This)->lpVtbl->RegisterObjectParam(This,pszKey,punk)
#define IBindCtx_GetObjectParam(This,pszKey,ppunk) (This)->lpVtbl->GetObjectParam(This,pszKey,ppunk)
#define IBindCtx_EnumObjectParam(This,ppenum) (This)->lpVtbl->EnumObjectParam(This,ppenum)
#define IBindCtx_RevokeObjectParam(This,pszKey) (This)->lpVtbl->RevokeObjectParam(This,pszKey)
#endif
#endif
  HRESULT WINAPI IBindCtx_RegisterObjectBound_Proxy(IBindCtx *This,IUnknown *punk);
  void __RPC_STUB IBindCtx_RegisterObjectBound_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_RevokeObjectBound_Proxy(IBindCtx *This,IUnknown *punk);
  void __RPC_STUB IBindCtx_RevokeObjectBound_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_ReleaseBoundObjects_Proxy(IBindCtx *This);
  void __RPC_STUB IBindCtx_ReleaseBoundObjects_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_RemoteSetBindOptions_Proxy(IBindCtx *This,BIND_OPTS2 *pbindopts);
  void __RPC_STUB IBindCtx_RemoteSetBindOptions_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_RemoteGetBindOptions_Proxy(IBindCtx *This,BIND_OPTS2 *pbindopts);
  void __RPC_STUB IBindCtx_RemoteGetBindOptions_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_GetRunningObjectTable_Proxy(IBindCtx *This,IRunningObjectTable **pprot);
  void __RPC_STUB IBindCtx_GetRunningObjectTable_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_RegisterObjectParam_Proxy(IBindCtx *This,LPOLESTR pszKey,IUnknown *punk);
  void __RPC_STUB IBindCtx_RegisterObjectParam_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_GetObjectParam_Proxy(IBindCtx *This,LPOLESTR pszKey,IUnknown **ppunk);
  void __RPC_STUB IBindCtx_GetObjectParam_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_EnumObjectParam_Proxy(IBindCtx *This,IEnumString **ppenum);
  void __RPC_STUB IBindCtx_EnumObjectParam_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindCtx_RevokeObjectParam_Proxy(IBindCtx *This,LPOLESTR pszKey);
  void __RPC_STUB IBindCtx_RevokeObjectParam_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumMoniker_INTERFACE_DEFINED__
#define __IEnumMoniker_INTERFACE_DEFINED__
  typedef IEnumMoniker *LPENUMMONIKER;

  EXTERN_C const IID IID_IEnumMoniker;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumMoniker : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,IMoniker **rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumMoniker **ppenum) = 0;
  };
#else
  typedef struct IEnumMonikerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumMoniker *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumMoniker *This);
      ULONG (WINAPI *Release)(IEnumMoniker *This);
      HRESULT (WINAPI *Next)(IEnumMoniker *This,ULONG celt,IMoniker **rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumMoniker *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumMoniker *This);
      HRESULT (WINAPI *Clone)(IEnumMoniker *This,IEnumMoniker **ppenum);
    END_INTERFACE
  } IEnumMonikerVtbl;
  struct IEnumMoniker {
    CONST_VTBL struct IEnumMonikerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumMoniker_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumMoniker_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumMoniker_Release(This) (This)->lpVtbl->Release(This)
#define IEnumMoniker_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumMoniker_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumMoniker_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumMoniker_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumMoniker_RemoteNext_Proxy(IEnumMoniker *This,ULONG celt,IMoniker **rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumMoniker_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumMoniker_Skip_Proxy(IEnumMoniker *This,ULONG celt);
  void __RPC_STUB IEnumMoniker_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumMoniker_Reset_Proxy(IEnumMoniker *This);
  void __RPC_STUB IEnumMoniker_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumMoniker_Clone_Proxy(IEnumMoniker *This,IEnumMoniker **ppenum);
  void __RPC_STUB IEnumMoniker_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRunnableObject_INTERFACE_DEFINED__
#define __IRunnableObject_INTERFACE_DEFINED__
  typedef IRunnableObject *LPRUNNABLEOBJECT;

  EXTERN_C const IID IID_IRunnableObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRunnableObject : public IUnknown {
  public:
    virtual HRESULT WINAPI GetRunningClass(LPCLSID lpClsid) = 0;
    virtual HRESULT WINAPI Run(LPBINDCTX pbc) = 0;
    virtual WINBOOL WINAPI IsRunning(void) = 0;
    virtual HRESULT WINAPI LockRunning(WINBOOL fLock,WINBOOL fLastUnlockCloses) = 0;
    virtual HRESULT WINAPI SetContainedObject(WINBOOL fContained) = 0;
  };
#else
  typedef struct IRunnableObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRunnableObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRunnableObject *This);
      ULONG (WINAPI *Release)(IRunnableObject *This);
      HRESULT (WINAPI *GetRunningClass)(IRunnableObject *This,LPCLSID lpClsid);
      HRESULT (WINAPI *Run)(IRunnableObject *This,LPBINDCTX pbc);
      WINBOOL (WINAPI *IsRunning)(IRunnableObject *This);
      HRESULT (WINAPI *LockRunning)(IRunnableObject *This,WINBOOL fLock,WINBOOL fLastUnlockCloses);
      HRESULT (WINAPI *SetContainedObject)(IRunnableObject *This,WINBOOL fContained);
    END_INTERFACE
  } IRunnableObjectVtbl;
  struct IRunnableObject {
    CONST_VTBL struct IRunnableObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRunnableObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRunnableObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRunnableObject_Release(This) (This)->lpVtbl->Release(This)
#define IRunnableObject_GetRunningClass(This,lpClsid) (This)->lpVtbl->GetRunningClass(This,lpClsid)
#define IRunnableObject_Run(This,pbc) (This)->lpVtbl->Run(This,pbc)
#define IRunnableObject_IsRunning(This) (This)->lpVtbl->IsRunning(This)
#define IRunnableObject_LockRunning(This,fLock,fLastUnlockCloses) (This)->lpVtbl->LockRunning(This,fLock,fLastUnlockCloses)
#define IRunnableObject_SetContainedObject(This,fContained) (This)->lpVtbl->SetContainedObject(This,fContained)
#endif
#endif
  HRESULT WINAPI IRunnableObject_GetRunningClass_Proxy(IRunnableObject *This,LPCLSID lpClsid);
  void __RPC_STUB IRunnableObject_GetRunningClass_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunnableObject_Run_Proxy(IRunnableObject *This,LPBINDCTX pbc);
  void __RPC_STUB IRunnableObject_Run_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunnableObject_RemoteIsRunning_Proxy(IRunnableObject *This);
  void __RPC_STUB IRunnableObject_RemoteIsRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunnableObject_LockRunning_Proxy(IRunnableObject *This,WINBOOL fLock,WINBOOL fLastUnlockCloses);
  void __RPC_STUB IRunnableObject_LockRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunnableObject_SetContainedObject_Proxy(IRunnableObject *This,WINBOOL fContained);
  void __RPC_STUB IRunnableObject_SetContainedObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRunningObjectTable_INTERFACE_DEFINED__
#define __IRunningObjectTable_INTERFACE_DEFINED__
  typedef IRunningObjectTable *LPRUNNINGOBJECTTABLE;

  EXTERN_C const IID IID_IRunningObjectTable;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRunningObjectTable : public IUnknown {
  public:
    virtual HRESULT WINAPI Register(DWORD grfFlags,IUnknown *punkObject,IMoniker *pmkObjectName,DWORD *pdwRegister) = 0;
    virtual HRESULT WINAPI Revoke(DWORD dwRegister) = 0;
    virtual HRESULT WINAPI IsRunning(IMoniker *pmkObjectName) = 0;
    virtual HRESULT WINAPI GetObject(IMoniker *pmkObjectName,IUnknown **ppunkObject) = 0;
    virtual HRESULT WINAPI NoteChangeTime(DWORD dwRegister,FILETIME *pfiletime) = 0;
    virtual HRESULT WINAPI GetTimeOfLastChange(IMoniker *pmkObjectName,FILETIME *pfiletime) = 0;
    virtual HRESULT WINAPI EnumRunning(IEnumMoniker **ppenumMoniker) = 0;
  };
#else
  typedef struct IRunningObjectTableVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRunningObjectTable *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRunningObjectTable *This);
      ULONG (WINAPI *Release)(IRunningObjectTable *This);
      HRESULT (WINAPI *Register)(IRunningObjectTable *This,DWORD grfFlags,IUnknown *punkObject,IMoniker *pmkObjectName,DWORD *pdwRegister);
      HRESULT (WINAPI *Revoke)(IRunningObjectTable *This,DWORD dwRegister);
      HRESULT (WINAPI *IsRunning)(IRunningObjectTable *This,IMoniker *pmkObjectName);
      HRESULT (WINAPI *GetObject)(IRunningObjectTable *This,IMoniker *pmkObjectName,IUnknown **ppunkObject);
      HRESULT (WINAPI *NoteChangeTime)(IRunningObjectTable *This,DWORD dwRegister,FILETIME *pfiletime);
      HRESULT (WINAPI *GetTimeOfLastChange)(IRunningObjectTable *This,IMoniker *pmkObjectName,FILETIME *pfiletime);
      HRESULT (WINAPI *EnumRunning)(IRunningObjectTable *This,IEnumMoniker **ppenumMoniker);
    END_INTERFACE
  } IRunningObjectTableVtbl;
  struct IRunningObjectTable {
    CONST_VTBL struct IRunningObjectTableVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRunningObjectTable_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRunningObjectTable_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRunningObjectTable_Release(This) (This)->lpVtbl->Release(This)
#define IRunningObjectTable_Register(This,grfFlags,punkObject,pmkObjectName,pdwRegister) (This)->lpVtbl->Register(This,grfFlags,punkObject,pmkObjectName,pdwRegister)
#define IRunningObjectTable_Revoke(This,dwRegister) (This)->lpVtbl->Revoke(This,dwRegister)
#define IRunningObjectTable_IsRunning(This,pmkObjectName) (This)->lpVtbl->IsRunning(This,pmkObjectName)
#define IRunningObjectTable_GetObject(This,pmkObjectName,ppunkObject) (This)->lpVtbl->GetObject(This,pmkObjectName,ppunkObject)
#define IRunningObjectTable_NoteChangeTime(This,dwRegister,pfiletime) (This)->lpVtbl->NoteChangeTime(This,dwRegister,pfiletime)
#define IRunningObjectTable_GetTimeOfLastChange(This,pmkObjectName,pfiletime) (This)->lpVtbl->GetTimeOfLastChange(This,pmkObjectName,pfiletime)
#define IRunningObjectTable_EnumRunning(This,ppenumMoniker) (This)->lpVtbl->EnumRunning(This,ppenumMoniker)
#endif
#endif
  HRESULT WINAPI IRunningObjectTable_Register_Proxy(IRunningObjectTable *This,DWORD grfFlags,IUnknown *punkObject,IMoniker *pmkObjectName,DWORD *pdwRegister);
  void __RPC_STUB IRunningObjectTable_Register_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_Revoke_Proxy(IRunningObjectTable *This,DWORD dwRegister);
  void __RPC_STUB IRunningObjectTable_Revoke_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_IsRunning_Proxy(IRunningObjectTable *This,IMoniker *pmkObjectName);
  void __RPC_STUB IRunningObjectTable_IsRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_GetObject_Proxy(IRunningObjectTable *This,IMoniker *pmkObjectName,IUnknown **ppunkObject);
  void __RPC_STUB IRunningObjectTable_GetObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_NoteChangeTime_Proxy(IRunningObjectTable *This,DWORD dwRegister,FILETIME *pfiletime);
  void __RPC_STUB IRunningObjectTable_NoteChangeTime_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_GetTimeOfLastChange_Proxy(IRunningObjectTable *This,IMoniker *pmkObjectName,FILETIME *pfiletime);
  void __RPC_STUB IRunningObjectTable_GetTimeOfLastChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRunningObjectTable_EnumRunning_Proxy(IRunningObjectTable *This,IEnumMoniker **ppenumMoniker);
  void __RPC_STUB IRunningObjectTable_EnumRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPersist_INTERFACE_DEFINED__
#define __IPersist_INTERFACE_DEFINED__
  typedef IPersist *LPPERSIST;

  EXTERN_C const IID IID_IPersist;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPersist : public IUnknown {
  public:
    virtual HRESULT WINAPI GetClassID(CLSID *pClassID) = 0;
  };
#else
  typedef struct IPersistVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPersist *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPersist *This);
      ULONG (WINAPI *Release)(IPersist *This);
      HRESULT (WINAPI *GetClassID)(IPersist *This,CLSID *pClassID);
    END_INTERFACE
  } IPersistVtbl;
  struct IPersist {
    CONST_VTBL struct IPersistVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPersist_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPersist_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPersist_Release(This) (This)->lpVtbl->Release(This)
#define IPersist_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#endif
#endif
  HRESULT WINAPI IPersist_GetClassID_Proxy(IPersist *This,CLSID *pClassID);
  void __RPC_STUB IPersist_GetClassID_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPersistStream_INTERFACE_DEFINED__
#define __IPersistStream_INTERFACE_DEFINED__
  typedef IPersistStream *LPPERSISTSTREAM;

  EXTERN_C const IID IID_IPersistStream;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPersistStream : public IPersist {
  public:
    virtual HRESULT WINAPI IsDirty(void) = 0;
    virtual HRESULT WINAPI Load(IStream *pStm) = 0;
    virtual HRESULT WINAPI Save(IStream *pStm,WINBOOL fClearDirty) = 0;
    virtual HRESULT WINAPI GetSizeMax(ULARGE_INTEGER *pcbSize) = 0;
  };
#else
  typedef struct IPersistStreamVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPersistStream *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPersistStream *This);
      ULONG (WINAPI *Release)(IPersistStream *This);
      HRESULT (WINAPI *GetClassID)(IPersistStream *This,CLSID *pClassID);
      HRESULT (WINAPI *IsDirty)(IPersistStream *This);
      HRESULT (WINAPI *Load)(IPersistStream *This,IStream *pStm);
      HRESULT (WINAPI *Save)(IPersistStream *This,IStream *pStm,WINBOOL fClearDirty);
      HRESULT (WINAPI *GetSizeMax)(IPersistStream *This,ULARGE_INTEGER *pcbSize);
    END_INTERFACE
  } IPersistStreamVtbl;
  struct IPersistStream {
    CONST_VTBL struct IPersistStreamVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPersistStream_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPersistStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPersistStream_Release(This) (This)->lpVtbl->Release(This)
#define IPersistStream_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#define IPersistStream_IsDirty(This) (This)->lpVtbl->IsDirty(This)
#define IPersistStream_Load(This,pStm) (This)->lpVtbl->Load(This,pStm)
#define IPersistStream_Save(This,pStm,fClearDirty) (This)->lpVtbl->Save(This,pStm,fClearDirty)
#define IPersistStream_GetSizeMax(This,pcbSize) (This)->lpVtbl->GetSizeMax(This,pcbSize)
#endif
#endif
  HRESULT WINAPI IPersistStream_IsDirty_Proxy(IPersistStream *This);
  void __RPC_STUB IPersistStream_IsDirty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStream_Load_Proxy(IPersistStream *This,IStream *pStm);
  void __RPC_STUB IPersistStream_Load_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStream_Save_Proxy(IPersistStream *This,IStream *pStm,WINBOOL fClearDirty);
  void __RPC_STUB IPersistStream_Save_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStream_GetSizeMax_Proxy(IPersistStream *This,ULARGE_INTEGER *pcbSize);
  void __RPC_STUB IPersistStream_GetSizeMax_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IMoniker_INTERFACE_DEFINED__
#define __IMoniker_INTERFACE_DEFINED__
  typedef IMoniker *LPMONIKER;

  typedef enum tagMKSYS {
    MKSYS_NONE = 0,MKSYS_GENERICCOMPOSITE = 1,MKSYS_FILEMONIKER = 2,MKSYS_ANTIMONIKER = 3,MKSYS_ITEMMONIKER = 4,MKSYS_POINTERMONIKER = 5,
    MKSYS_CLASSMONIKER = 7,MKSYS_OBJREFMONIKER = 8,MKSYS_SESSIONMONIKER = 9
  } MKSYS;

  typedef enum tagMKREDUCE {
    MKRREDUCE_ONE = 3 << 16,MKRREDUCE_TOUSER = 2 << 16,MKRREDUCE_THROUGHUSER = 1 << 16,MKRREDUCE_ALL = 0
  } MKRREDUCE;

  EXTERN_C const IID IID_IMoniker;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMoniker : public IPersistStream {
  public:
    virtual HRESULT WINAPI BindToObject(IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riidResult,void **ppvResult) = 0;
    virtual HRESULT WINAPI BindToStorage(IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riid,void **ppvObj) = 0;
    virtual HRESULT WINAPI Reduce(IBindCtx *pbc,DWORD dwReduceHowFar,IMoniker **ppmkToLeft,IMoniker **ppmkReduced) = 0;
    virtual HRESULT WINAPI ComposeWith(IMoniker *pmkRight,WINBOOL fOnlyIfNotGeneric,IMoniker **ppmkComposite) = 0;
    virtual HRESULT WINAPI Enum(WINBOOL fForward,IEnumMoniker **ppenumMoniker) = 0;
    virtual HRESULT WINAPI IsEqual(IMoniker *pmkOtherMoniker) = 0;
    virtual HRESULT WINAPI Hash(DWORD *pdwHash) = 0;
    virtual HRESULT WINAPI IsRunning(IBindCtx *pbc,IMoniker *pmkToLeft,IMoniker *pmkNewlyRunning) = 0;
    virtual HRESULT WINAPI GetTimeOfLastChange(IBindCtx *pbc,IMoniker *pmkToLeft,FILETIME *pFileTime) = 0;
    virtual HRESULT WINAPI Inverse(IMoniker **ppmk) = 0;
    virtual HRESULT WINAPI CommonPrefixWith(IMoniker *pmkOther,IMoniker **ppmkPrefix) = 0;
    virtual HRESULT WINAPI RelativePathTo(IMoniker *pmkOther,IMoniker **ppmkRelPath) = 0;
    virtual HRESULT WINAPI GetDisplayName(IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR *ppszDisplayName) = 0;
    virtual HRESULT WINAPI ParseDisplayName(IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut) = 0;
    virtual HRESULT WINAPI IsSystemMoniker(DWORD *pdwMksys) = 0;
  };
#else
  typedef struct IMonikerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMoniker *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMoniker *This);
      ULONG (WINAPI *Release)(IMoniker *This);
      HRESULT (WINAPI *GetClassID)(IMoniker *This,CLSID *pClassID);
      HRESULT (WINAPI *IsDirty)(IMoniker *This);
      HRESULT (WINAPI *Load)(IMoniker *This,IStream *pStm);
      HRESULT (WINAPI *Save)(IMoniker *This,IStream *pStm,WINBOOL fClearDirty);
      HRESULT (WINAPI *GetSizeMax)(IMoniker *This,ULARGE_INTEGER *pcbSize);
      HRESULT (WINAPI *BindToObject)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riidResult,void **ppvResult);
      HRESULT (WINAPI *BindToStorage)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riid,void **ppvObj);
      HRESULT (WINAPI *Reduce)(IMoniker *This,IBindCtx *pbc,DWORD dwReduceHowFar,IMoniker **ppmkToLeft,IMoniker **ppmkReduced);
      HRESULT (WINAPI *ComposeWith)(IMoniker *This,IMoniker *pmkRight,WINBOOL fOnlyIfNotGeneric,IMoniker **ppmkComposite);
      HRESULT (WINAPI *Enum)(IMoniker *This,WINBOOL fForward,IEnumMoniker **ppenumMoniker);
      HRESULT (WINAPI *IsEqual)(IMoniker *This,IMoniker *pmkOtherMoniker);
      HRESULT (WINAPI *Hash)(IMoniker *This,DWORD *pdwHash);
      HRESULT (WINAPI *IsRunning)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,IMoniker *pmkNewlyRunning);
      HRESULT (WINAPI *GetTimeOfLastChange)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,FILETIME *pFileTime);
      HRESULT (WINAPI *Inverse)(IMoniker *This,IMoniker **ppmk);
      HRESULT (WINAPI *CommonPrefixWith)(IMoniker *This,IMoniker *pmkOther,IMoniker **ppmkPrefix);
      HRESULT (WINAPI *RelativePathTo)(IMoniker *This,IMoniker *pmkOther,IMoniker **ppmkRelPath);
      HRESULT (WINAPI *GetDisplayName)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR *ppszDisplayName);
      HRESULT (WINAPI *ParseDisplayName)(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
      HRESULT (WINAPI *IsSystemMoniker)(IMoniker *This,DWORD *pdwMksys);
    END_INTERFACE
  } IMonikerVtbl;
  struct IMoniker {
    CONST_VTBL struct IMonikerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMoniker_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMoniker_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMoniker_Release(This) (This)->lpVtbl->Release(This)
#define IMoniker_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#define IMoniker_IsDirty(This) (This)->lpVtbl->IsDirty(This)
#define IMoniker_Load(This,pStm) (This)->lpVtbl->Load(This,pStm)
#define IMoniker_Save(This,pStm,fClearDirty) (This)->lpVtbl->Save(This,pStm,fClearDirty)
#define IMoniker_GetSizeMax(This,pcbSize) (This)->lpVtbl->GetSizeMax(This,pcbSize)
#define IMoniker_BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult) (This)->lpVtbl->BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult)
#define IMoniker_BindToStorage(This,pbc,pmkToLeft,riid,ppvObj) (This)->lpVtbl->BindToStorage(This,pbc,pmkToLeft,riid,ppvObj)
#define IMoniker_Reduce(This,pbc,dwReduceHowFar,ppmkToLeft,ppmkReduced) (This)->lpVtbl->Reduce(This,pbc,dwReduceHowFar,ppmkToLeft,ppmkReduced)
#define IMoniker_ComposeWith(This,pmkRight,fOnlyIfNotGeneric,ppmkComposite) (This)->lpVtbl->ComposeWith(This,pmkRight,fOnlyIfNotGeneric,ppmkComposite)
#define IMoniker_Enum(This,fForward,ppenumMoniker) (This)->lpVtbl->Enum(This,fForward,ppenumMoniker)
#define IMoniker_IsEqual(This,pmkOtherMoniker) (This)->lpVtbl->IsEqual(This,pmkOtherMoniker)
#define IMoniker_Hash(This,pdwHash) (This)->lpVtbl->Hash(This,pdwHash)
#define IMoniker_IsRunning(This,pbc,pmkToLeft,pmkNewlyRunning) (This)->lpVtbl->IsRunning(This,pbc,pmkToLeft,pmkNewlyRunning)
#define IMoniker_GetTimeOfLastChange(This,pbc,pmkToLeft,pFileTime) (This)->lpVtbl->GetTimeOfLastChange(This,pbc,pmkToLeft,pFileTime)
#define IMoniker_Inverse(This,ppmk) (This)->lpVtbl->Inverse(This,ppmk)
#define IMoniker_CommonPrefixWith(This,pmkOther,ppmkPrefix) (This)->lpVtbl->CommonPrefixWith(This,pmkOther,ppmkPrefix)
#define IMoniker_RelativePathTo(This,pmkOther,ppmkRelPath) (This)->lpVtbl->RelativePathTo(This,pmkOther,ppmkRelPath)
#define IMoniker_GetDisplayName(This,pbc,pmkToLeft,ppszDisplayName) (This)->lpVtbl->GetDisplayName(This,pbc,pmkToLeft,ppszDisplayName)
#define IMoniker_ParseDisplayName(This,pbc,pmkToLeft,pszDisplayName,pchEaten,ppmkOut) (This)->lpVtbl->ParseDisplayName(This,pbc,pmkToLeft,pszDisplayName,pchEaten,ppmkOut)
#define IMoniker_IsSystemMoniker(This,pdwMksys) (This)->lpVtbl->IsSystemMoniker(This,pdwMksys)
#endif
#endif
  HRESULT WINAPI IMoniker_RemoteBindToObject_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riidResult,IUnknown **ppvResult);
  void __RPC_STUB IMoniker_RemoteBindToObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_RemoteBindToStorage_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riid,IUnknown **ppvObj);
  void __RPC_STUB IMoniker_RemoteBindToStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_Reduce_Proxy(IMoniker *This,IBindCtx *pbc,DWORD dwReduceHowFar,IMoniker **ppmkToLeft,IMoniker **ppmkReduced);
  void __RPC_STUB IMoniker_Reduce_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_ComposeWith_Proxy(IMoniker *This,IMoniker *pmkRight,WINBOOL fOnlyIfNotGeneric,IMoniker **ppmkComposite);
  void __RPC_STUB IMoniker_ComposeWith_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_Enum_Proxy(IMoniker *This,WINBOOL fForward,IEnumMoniker **ppenumMoniker);
  void __RPC_STUB IMoniker_Enum_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_IsEqual_Proxy(IMoniker *This,IMoniker *pmkOtherMoniker);
  void __RPC_STUB IMoniker_IsEqual_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_Hash_Proxy(IMoniker *This,DWORD *pdwHash);
  void __RPC_STUB IMoniker_Hash_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_IsRunning_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,IMoniker *pmkNewlyRunning);
  void __RPC_STUB IMoniker_IsRunning_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_GetTimeOfLastChange_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,FILETIME *pFileTime);
  void __RPC_STUB IMoniker_GetTimeOfLastChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_Inverse_Proxy(IMoniker *This,IMoniker **ppmk);
  void __RPC_STUB IMoniker_Inverse_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_CommonPrefixWith_Proxy(IMoniker *This,IMoniker *pmkOther,IMoniker **ppmkPrefix);
  void __RPC_STUB IMoniker_CommonPrefixWith_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_RelativePathTo_Proxy(IMoniker *This,IMoniker *pmkOther,IMoniker **ppmkRelPath);
  void __RPC_STUB IMoniker_RelativePathTo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_GetDisplayName_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR *ppszDisplayName);
  void __RPC_STUB IMoniker_GetDisplayName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_ParseDisplayName_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,LPOLESTR pszDisplayName,ULONG *pchEaten,IMoniker **ppmkOut);
  void __RPC_STUB IMoniker_ParseDisplayName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IMoniker_IsSystemMoniker_Proxy(IMoniker *This,DWORD *pdwMksys);
  void __RPC_STUB IMoniker_IsSystemMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IROTData_INTERFACE_DEFINED__
#define __IROTData_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IROTData;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IROTData : public IUnknown {
  public:
    virtual HRESULT WINAPI GetComparisonData(byte *pbData,ULONG cbMax,ULONG *pcbData) = 0;
  };
#else
  typedef struct IROTDataVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IROTData *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IROTData *This);
      ULONG (WINAPI *Release)(IROTData *This);
      HRESULT (WINAPI *GetComparisonData)(IROTData *This,byte *pbData,ULONG cbMax,ULONG *pcbData);
    END_INTERFACE
  } IROTDataVtbl;
  struct IROTData {
    CONST_VTBL struct IROTDataVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IROTData_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IROTData_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IROTData_Release(This) (This)->lpVtbl->Release(This)
#define IROTData_GetComparisonData(This,pbData,cbMax,pcbData) (This)->lpVtbl->GetComparisonData(This,pbData,cbMax,pcbData)
#endif
#endif
  HRESULT WINAPI IROTData_GetComparisonData_Proxy(IROTData *This,byte *pbData,ULONG cbMax,ULONG *pcbData);
  void __RPC_STUB IROTData_GetComparisonData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumString_INTERFACE_DEFINED__
#define __IEnumString_INTERFACE_DEFINED__
  typedef IEnumString *LPENUMSTRING;

  EXTERN_C const IID IID_IEnumString;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumString : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,LPOLESTR *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumString **ppenum) = 0;
  };
#else
  typedef struct IEnumStringVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumString *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumString *This);
      ULONG (WINAPI *Release)(IEnumString *This);
      HRESULT (WINAPI *Next)(IEnumString *This,ULONG celt,LPOLESTR *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumString *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumString *This);
      HRESULT (WINAPI *Clone)(IEnumString *This,IEnumString **ppenum);
    END_INTERFACE
  } IEnumStringVtbl;
  struct IEnumString {
    CONST_VTBL struct IEnumStringVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumString_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumString_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumString_Release(This) (This)->lpVtbl->Release(This)
#define IEnumString_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumString_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumString_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumString_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumString_RemoteNext_Proxy(IEnumString *This,ULONG celt,LPOLESTR *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumString_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumString_Skip_Proxy(IEnumString *This,ULONG celt);
  void __RPC_STUB IEnumString_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumString_Reset_Proxy(IEnumString *This);
  void __RPC_STUB IEnumString_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumString_Clone_Proxy(IEnumString *This,IEnumString **ppenum);
  void __RPC_STUB IEnumString_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISequentialStream_INTERFACE_DEFINED__
#define __ISequentialStream_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISequentialStream;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISequentialStream : public IUnknown {
  public:
    virtual HRESULT WINAPI Read(void *pv,ULONG cb,ULONG *pcbRead) = 0;
    virtual HRESULT WINAPI Write(const void *pv,ULONG cb,ULONG *pcbWritten) = 0;
  };
#else
  typedef struct ISequentialStreamVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISequentialStream *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISequentialStream *This);
      ULONG (WINAPI *Release)(ISequentialStream *This);
      HRESULT (WINAPI *Read)(ISequentialStream *This,void *pv,ULONG cb,ULONG *pcbRead);
      HRESULT (WINAPI *Write)(ISequentialStream *This,const void *pv,ULONG cb,ULONG *pcbWritten);
    END_INTERFACE
  } ISequentialStreamVtbl;
  struct ISequentialStream {
    CONST_VTBL struct ISequentialStreamVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISequentialStream_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISequentialStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISequentialStream_Release(This) (This)->lpVtbl->Release(This)
#define ISequentialStream_Read(This,pv,cb,pcbRead) (This)->lpVtbl->Read(This,pv,cb,pcbRead)
#define ISequentialStream_Write(This,pv,cb,pcbWritten) (This)->lpVtbl->Write(This,pv,cb,pcbWritten)
#endif
#endif
  HRESULT WINAPI ISequentialStream_RemoteRead_Proxy(ISequentialStream *This,byte *pv,ULONG cb,ULONG *pcbRead);
  void __RPC_STUB ISequentialStream_RemoteRead_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISequentialStream_RemoteWrite_Proxy(ISequentialStream *This,const byte *pv,ULONG cb,ULONG *pcbWritten);
  void __RPC_STUB ISequentialStream_RemoteWrite_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IStream_INTERFACE_DEFINED__
#define __IStream_INTERFACE_DEFINED__
  typedef IStream *LPSTREAM;

  typedef struct tagSTATSTG {
    LPOLESTR pwcsName;
    DWORD type;
    ULARGE_INTEGER cbSize;
    FILETIME mtime;
    FILETIME ctime;
    FILETIME atime;
    DWORD grfMode;
    DWORD grfLocksSupported;
    CLSID clsid;
    DWORD grfStateBits;
    DWORD reserved;
  } STATSTG;

  typedef enum tagSTGTY {
    STGTY_STORAGE = 1,STGTY_STREAM = 2,STGTY_LOCKBYTES = 3,STGTY_PROPERTY = 4
  } STGTY;

  typedef enum tagSTREAM_SEEK {
    STREAM_SEEK_SET = 0,STREAM_SEEK_CUR = 1,STREAM_SEEK_END = 2
  } STREAM_SEEK;

  typedef enum tagLOCKTYPE {
    LOCK_WRITE = 1,LOCK_EXCLUSIVE = 2,LOCK_ONLYONCE = 4
  } LOCKTYPE;

  EXTERN_C const IID IID_IStream;

#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IStream : public ISequentialStream {
  public:
    virtual HRESULT WINAPI Seek(LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition) = 0;
    virtual HRESULT WINAPI SetSize(ULARGE_INTEGER libNewSize) = 0;
    virtual HRESULT WINAPI CopyTo(IStream *pstm,ULARGE_INTEGER cb,ULARGE_INTEGER *pcbRead,ULARGE_INTEGER *pcbWritten) = 0;
    virtual HRESULT WINAPI Commit(DWORD grfCommitFlags) = 0;
    virtual HRESULT WINAPI Revert(void) = 0;
    virtual HRESULT WINAPI LockRegion(ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType) = 0;
    virtual HRESULT WINAPI UnlockRegion(ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType) = 0;
    virtual HRESULT WINAPI Stat(STATSTG *pstatstg,DWORD grfStatFlag) = 0;
    virtual HRESULT WINAPI Clone(IStream **ppstm) = 0;
  };
#else
  typedef struct IStreamVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IStream *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IStream *This);
      ULONG (WINAPI *Release)(IStream *This);
      HRESULT (WINAPI *Read)(IStream *This,void *pv,ULONG cb,ULONG *pcbRead);
      HRESULT (WINAPI *Write)(IStream *This,const void *pv,ULONG cb,ULONG *pcbWritten);
      HRESULT (WINAPI *Seek)(IStream *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
      HRESULT (WINAPI *SetSize)(IStream *This,ULARGE_INTEGER libNewSize);
      HRESULT (WINAPI *CopyTo)(IStream *This,IStream *pstm,ULARGE_INTEGER cb,ULARGE_INTEGER *pcbRead,ULARGE_INTEGER *pcbWritten);
      HRESULT (WINAPI *Commit)(IStream *This,DWORD grfCommitFlags);
      HRESULT (WINAPI *Revert)(IStream *This);
      HRESULT (WINAPI *LockRegion)(IStream *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
      HRESULT (WINAPI *UnlockRegion)(IStream *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
      HRESULT (WINAPI *Stat)(IStream *This,STATSTG *pstatstg,DWORD grfStatFlag);
      HRESULT (WINAPI *Clone)(IStream *This,IStream **ppstm);
    END_INTERFACE
  } IStreamVtbl;
  struct IStream {
    CONST_VTBL struct IStreamVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IStream_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IStream_Release(This) (This)->lpVtbl->Release(This)
#define IStream_Read(This,pv,cb,pcbRead) (This)->lpVtbl->Read(This,pv,cb,pcbRead)
#define IStream_Write(This,pv,cb,pcbWritten) (This)->lpVtbl->Write(This,pv,cb,pcbWritten)
#define IStream_Seek(This,dlibMove,dwOrigin,plibNewPosition) (This)->lpVtbl->Seek(This,dlibMove,dwOrigin,plibNewPosition)
#define IStream_SetSize(This,libNewSize) (This)->lpVtbl->SetSize(This,libNewSize)
#define IStream_CopyTo(This,pstm,cb,pcbRead,pcbWritten) (This)->lpVtbl->CopyTo(This,pstm,cb,pcbRead,pcbWritten)
#define IStream_Commit(This,grfCommitFlags) (This)->lpVtbl->Commit(This,grfCommitFlags)
#define IStream_Revert(This) (This)->lpVtbl->Revert(This)
#define IStream_LockRegion(This,libOffset,cb,dwLockType) (This)->lpVtbl->LockRegion(This,libOffset,cb,dwLockType)
#define IStream_UnlockRegion(This,libOffset,cb,dwLockType) (This)->lpVtbl->UnlockRegion(This,libOffset,cb,dwLockType)
#define IStream_Stat(This,pstatstg,grfStatFlag) (This)->lpVtbl->Stat(This,pstatstg,grfStatFlag)
#define IStream_Clone(This,ppstm) (This)->lpVtbl->Clone(This,ppstm)
#endif
#endif
  HRESULT WINAPI IStream_RemoteSeek_Proxy(IStream *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
  void __RPC_STUB IStream_RemoteSeek_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_SetSize_Proxy(IStream *This,ULARGE_INTEGER libNewSize);
  void __RPC_STUB IStream_SetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_RemoteCopyTo_Proxy(IStream *This,IStream *pstm,ULARGE_INTEGER cb,ULARGE_INTEGER *pcbRead,ULARGE_INTEGER *pcbWritten);
  void __RPC_STUB IStream_RemoteCopyTo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_Commit_Proxy(IStream *This,DWORD grfCommitFlags);
  void __RPC_STUB IStream_Commit_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_Revert_Proxy(IStream *This);
  void __RPC_STUB IStream_Revert_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_LockRegion_Proxy(IStream *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
  void __RPC_STUB IStream_LockRegion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_UnlockRegion_Proxy(IStream *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
  void __RPC_STUB IStream_UnlockRegion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_Stat_Proxy(IStream *This,STATSTG *pstatstg,DWORD grfStatFlag);
  void __RPC_STUB IStream_Stat_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStream_Clone_Proxy(IStream *This,IStream **ppstm);
  void __RPC_STUB IStream_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumSTATSTG_INTERFACE_DEFINED__
#define __IEnumSTATSTG_INTERFACE_DEFINED__
  typedef IEnumSTATSTG *LPENUMSTATSTG;

  EXTERN_C const IID IID_IEnumSTATSTG;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumSTATSTG : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,STATSTG *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumSTATSTG **ppenum) = 0;
  };
#else
  typedef struct IEnumSTATSTGVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumSTATSTG *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumSTATSTG *This);
      ULONG (WINAPI *Release)(IEnumSTATSTG *This);
      HRESULT (WINAPI *Next)(IEnumSTATSTG *This,ULONG celt,STATSTG *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumSTATSTG *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumSTATSTG *This);
      HRESULT (WINAPI *Clone)(IEnumSTATSTG *This,IEnumSTATSTG **ppenum);
    END_INTERFACE
  } IEnumSTATSTGVtbl;
  struct IEnumSTATSTG {
    CONST_VTBL struct IEnumSTATSTGVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumSTATSTG_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATSTG_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumSTATSTG_Release(This) (This)->lpVtbl->Release(This)
#define IEnumSTATSTG_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATSTG_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATSTG_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumSTATSTG_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumSTATSTG_RemoteNext_Proxy(IEnumSTATSTG *This,ULONG celt,STATSTG *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumSTATSTG_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATSTG_Skip_Proxy(IEnumSTATSTG *This,ULONG celt);
  void __RPC_STUB IEnumSTATSTG_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATSTG_Reset_Proxy(IEnumSTATSTG *This);
  void __RPC_STUB IEnumSTATSTG_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATSTG_Clone_Proxy(IEnumSTATSTG *This,IEnumSTATSTG **ppenum);
  void __RPC_STUB IEnumSTATSTG_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IStorage_INTERFACE_DEFINED__
#define __IStorage_INTERFACE_DEFINED__
  typedef IStorage *LPSTORAGE;

  typedef struct tagRemSNB {
    unsigned long ulCntStr;
    unsigned long ulCntChar;
    OLECHAR rgString[1 ];
  } RemSNB;

  typedef RemSNB *wireSNB;
  typedef OLECHAR **SNB;

  EXTERN_C const IID IID_IStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateStream(const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStream **ppstm) = 0;
    virtual HRESULT WINAPI OpenStream(const OLECHAR *pwcsName,void *reserved1,DWORD grfMode,DWORD reserved2,IStream **ppstm) = 0;
    virtual HRESULT WINAPI CreateStorage(const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStorage **ppstg) = 0;
    virtual HRESULT WINAPI OpenStorage(const OLECHAR *pwcsName,IStorage *pstgPriority,DWORD grfMode,SNB snbExclude,DWORD reserved,IStorage **ppstg) = 0;
    virtual HRESULT WINAPI CopyTo(DWORD ciidExclude,const IID *rgiidExclude,SNB snbExclude,IStorage *pstgDest) = 0;
    virtual HRESULT WINAPI MoveElementTo(const OLECHAR *pwcsName,IStorage *pstgDest,const OLECHAR *pwcsNewName,DWORD grfFlags) = 0;
    virtual HRESULT WINAPI Commit(DWORD grfCommitFlags) = 0;
    virtual HRESULT WINAPI Revert(void) = 0;
    virtual HRESULT WINAPI EnumElements(DWORD reserved1,void *reserved2,DWORD reserved3,IEnumSTATSTG **ppenum) = 0;
    virtual HRESULT WINAPI DestroyElement(const OLECHAR *pwcsName) = 0;
    virtual HRESULT WINAPI RenameElement(const OLECHAR *pwcsOldName,const OLECHAR *pwcsNewName) = 0;
    virtual HRESULT WINAPI SetElementTimes(const OLECHAR *pwcsName,const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime) = 0;
    virtual HRESULT WINAPI SetClass(REFCLSID clsid) = 0;
    virtual HRESULT WINAPI SetStateBits(DWORD grfStateBits,DWORD grfMask) = 0;
    virtual HRESULT WINAPI Stat(STATSTG *pstatstg,DWORD grfStatFlag) = 0;
  };
#else
  typedef struct IStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IStorage *This);
      ULONG (WINAPI *Release)(IStorage *This);
      HRESULT (WINAPI *CreateStream)(IStorage *This,const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStream **ppstm);
      HRESULT (WINAPI *OpenStream)(IStorage *This,const OLECHAR *pwcsName,void *reserved1,DWORD grfMode,DWORD reserved2,IStream **ppstm);
      HRESULT (WINAPI *CreateStorage)(IStorage *This,const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStorage **ppstg);
      HRESULT (WINAPI *OpenStorage)(IStorage *This,const OLECHAR *pwcsName,IStorage *pstgPriority,DWORD grfMode,SNB snbExclude,DWORD reserved,IStorage **ppstg);
      HRESULT (WINAPI *CopyTo)(IStorage *This,DWORD ciidExclude,const IID *rgiidExclude,SNB snbExclude,IStorage *pstgDest);
      HRESULT (WINAPI *MoveElementTo)(IStorage *This,const OLECHAR *pwcsName,IStorage *pstgDest,const OLECHAR *pwcsNewName,DWORD grfFlags);
      HRESULT (WINAPI *Commit)(IStorage *This,DWORD grfCommitFlags);
      HRESULT (WINAPI *Revert)(IStorage *This);
      HRESULT (WINAPI *EnumElements)(IStorage *This,DWORD reserved1,void *reserved2,DWORD reserved3,IEnumSTATSTG **ppenum);
      HRESULT (WINAPI *DestroyElement)(IStorage *This,const OLECHAR *pwcsName);
      HRESULT (WINAPI *RenameElement)(IStorage *This,const OLECHAR *pwcsOldName,const OLECHAR *pwcsNewName);
      HRESULT (WINAPI *SetElementTimes)(IStorage *This,const OLECHAR *pwcsName,const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime);
      HRESULT (WINAPI *SetClass)(IStorage *This,REFCLSID clsid);
      HRESULT (WINAPI *SetStateBits)(IStorage *This,DWORD grfStateBits,DWORD grfMask);
      HRESULT (WINAPI *Stat)(IStorage *This,STATSTG *pstatstg,DWORD grfStatFlag);
    END_INTERFACE
  } IStorageVtbl;
  struct IStorage {
    CONST_VTBL struct IStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IStorage_Release(This) (This)->lpVtbl->Release(This)
#define IStorage_CreateStream(This,pwcsName,grfMode,reserved1,reserved2,ppstm) (This)->lpVtbl->CreateStream(This,pwcsName,grfMode,reserved1,reserved2,ppstm)
#define IStorage_OpenStream(This,pwcsName,reserved1,grfMode,reserved2,ppstm) (This)->lpVtbl->OpenStream(This,pwcsName,reserved1,grfMode,reserved2,ppstm)
#define IStorage_CreateStorage(This,pwcsName,grfMode,reserved1,reserved2,ppstg) (This)->lpVtbl->CreateStorage(This,pwcsName,grfMode,reserved1,reserved2,ppstg)
#define IStorage_OpenStorage(This,pwcsName,pstgPriority,grfMode,snbExclude,reserved,ppstg) (This)->lpVtbl->OpenStorage(This,pwcsName,pstgPriority,grfMode,snbExclude,reserved,ppstg)
#define IStorage_CopyTo(This,ciidExclude,rgiidExclude,snbExclude,pstgDest) (This)->lpVtbl->CopyTo(This,ciidExclude,rgiidExclude,snbExclude,pstgDest)
#define IStorage_MoveElementTo(This,pwcsName,pstgDest,pwcsNewName,grfFlags) (This)->lpVtbl->MoveElementTo(This,pwcsName,pstgDest,pwcsNewName,grfFlags)
#define IStorage_Commit(This,grfCommitFlags) (This)->lpVtbl->Commit(This,grfCommitFlags)
#define IStorage_Revert(This) (This)->lpVtbl->Revert(This)
#define IStorage_EnumElements(This,reserved1,reserved2,reserved3,ppenum) (This)->lpVtbl->EnumElements(This,reserved1,reserved2,reserved3,ppenum)
#define IStorage_DestroyElement(This,pwcsName) (This)->lpVtbl->DestroyElement(This,pwcsName)
#define IStorage_RenameElement(This,pwcsOldName,pwcsNewName) (This)->lpVtbl->RenameElement(This,pwcsOldName,pwcsNewName)
#define IStorage_SetElementTimes(This,pwcsName,pctime,patime,pmtime) (This)->lpVtbl->SetElementTimes(This,pwcsName,pctime,patime,pmtime)
#define IStorage_SetClass(This,clsid) (This)->lpVtbl->SetClass(This,clsid)
#define IStorage_SetStateBits(This,grfStateBits,grfMask) (This)->lpVtbl->SetStateBits(This,grfStateBits,grfMask)
#define IStorage_Stat(This,pstatstg,grfStatFlag) (This)->lpVtbl->Stat(This,pstatstg,grfStatFlag)
#endif
#endif
  HRESULT WINAPI IStorage_CreateStream_Proxy(IStorage *This,const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStream **ppstm);
  void __RPC_STUB IStorage_CreateStream_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_RemoteOpenStream_Proxy(IStorage *This,const OLECHAR *pwcsName,unsigned long cbReserved1,byte *reserved1,DWORD grfMode,DWORD reserved2,IStream **ppstm);
  void __RPC_STUB IStorage_RemoteOpenStream_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_CreateStorage_Proxy(IStorage *This,const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved1,DWORD reserved2,IStorage **ppstg);
  void __RPC_STUB IStorage_CreateStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_OpenStorage_Proxy(IStorage *This,const OLECHAR *pwcsName,IStorage *pstgPriority,DWORD grfMode,SNB snbExclude,DWORD reserved,IStorage **ppstg);
  void __RPC_STUB IStorage_OpenStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_CopyTo_Proxy(IStorage *This,DWORD ciidExclude,const IID *rgiidExclude,SNB snbExclude,IStorage *pstgDest);
  void __RPC_STUB IStorage_CopyTo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_MoveElementTo_Proxy(IStorage *This,const OLECHAR *pwcsName,IStorage *pstgDest,const OLECHAR *pwcsNewName,DWORD grfFlags);
  void __RPC_STUB IStorage_MoveElementTo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_Commit_Proxy(IStorage *This,DWORD grfCommitFlags);
  void __RPC_STUB IStorage_Commit_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_Revert_Proxy(IStorage *This);
  void __RPC_STUB IStorage_Revert_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_RemoteEnumElements_Proxy(IStorage *This,DWORD reserved1,unsigned long cbReserved2,byte *reserved2,DWORD reserved3,IEnumSTATSTG **ppenum);
  void __RPC_STUB IStorage_RemoteEnumElements_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_DestroyElement_Proxy(IStorage *This,const OLECHAR *pwcsName);
  void __RPC_STUB IStorage_DestroyElement_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_RenameElement_Proxy(IStorage *This,const OLECHAR *pwcsOldName,const OLECHAR *pwcsNewName);
  void __RPC_STUB IStorage_RenameElement_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_SetElementTimes_Proxy(IStorage *This,const OLECHAR *pwcsName,const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime);
  void __RPC_STUB IStorage_SetElementTimes_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_SetClass_Proxy(IStorage *This,REFCLSID clsid);
  void __RPC_STUB IStorage_SetClass_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_SetStateBits_Proxy(IStorage *This,DWORD grfStateBits,DWORD grfMask);
  void __RPC_STUB IStorage_SetStateBits_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IStorage_Stat_Proxy(IStorage *This,STATSTG *pstatstg,DWORD grfStatFlag);
  void __RPC_STUB IStorage_Stat_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPersistFile_INTERFACE_DEFINED__
#define __IPersistFile_INTERFACE_DEFINED__
  typedef IPersistFile *LPPERSISTFILE;

  EXTERN_C const IID IID_IPersistFile;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPersistFile : public IPersist {
  public:
    virtual HRESULT WINAPI IsDirty(void) = 0;
    virtual HRESULT WINAPI Load(LPCOLESTR pszFileName,DWORD dwMode) = 0;
    virtual HRESULT WINAPI Save(LPCOLESTR pszFileName,WINBOOL fRemember) = 0;
    virtual HRESULT WINAPI SaveCompleted(LPCOLESTR pszFileName) = 0;
    virtual HRESULT WINAPI GetCurFile(LPOLESTR *ppszFileName) = 0;
  };
#else
  typedef struct IPersistFileVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPersistFile *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPersistFile *This);
      ULONG (WINAPI *Release)(IPersistFile *This);
      HRESULT (WINAPI *GetClassID)(IPersistFile *This,CLSID *pClassID);
      HRESULT (WINAPI *IsDirty)(IPersistFile *This);
      HRESULT (WINAPI *Load)(IPersistFile *This,LPCOLESTR pszFileName,DWORD dwMode);
      HRESULT (WINAPI *Save)(IPersistFile *This,LPCOLESTR pszFileName,WINBOOL fRemember);
      HRESULT (WINAPI *SaveCompleted)(IPersistFile *This,LPCOLESTR pszFileName);
      HRESULT (WINAPI *GetCurFile)(IPersistFile *This,LPOLESTR *ppszFileName);
    END_INTERFACE
  } IPersistFileVtbl;
  struct IPersistFile {
    CONST_VTBL struct IPersistFileVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPersistFile_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPersistFile_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPersistFile_Release(This) (This)->lpVtbl->Release(This)
#define IPersistFile_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#define IPersistFile_IsDirty(This) (This)->lpVtbl->IsDirty(This)
#define IPersistFile_Load(This,pszFileName,dwMode) (This)->lpVtbl->Load(This,pszFileName,dwMode)
#define IPersistFile_Save(This,pszFileName,fRemember) (This)->lpVtbl->Save(This,pszFileName,fRemember)
#define IPersistFile_SaveCompleted(This,pszFileName) (This)->lpVtbl->SaveCompleted(This,pszFileName)
#define IPersistFile_GetCurFile(This,ppszFileName) (This)->lpVtbl->GetCurFile(This,ppszFileName)
#endif
#endif
  HRESULT WINAPI IPersistFile_IsDirty_Proxy(IPersistFile *This);
  void __RPC_STUB IPersistFile_IsDirty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistFile_Load_Proxy(IPersistFile *This,LPCOLESTR pszFileName,DWORD dwMode);
  void __RPC_STUB IPersistFile_Load_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistFile_Save_Proxy(IPersistFile *This,LPCOLESTR pszFileName,WINBOOL fRemember);
  void __RPC_STUB IPersistFile_Save_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistFile_SaveCompleted_Proxy(IPersistFile *This,LPCOLESTR pszFileName);
  void __RPC_STUB IPersistFile_SaveCompleted_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistFile_GetCurFile_Proxy(IPersistFile *This,LPOLESTR *ppszFileName);
  void __RPC_STUB IPersistFile_GetCurFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPersistStorage_INTERFACE_DEFINED__
#define __IPersistStorage_INTERFACE_DEFINED__
  typedef IPersistStorage *LPPERSISTSTORAGE;

  EXTERN_C const IID IID_IPersistStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPersistStorage : public IPersist {
  public:
    virtual HRESULT WINAPI IsDirty(void) = 0;
    virtual HRESULT WINAPI InitNew(IStorage *pStg) = 0;
    virtual HRESULT WINAPI Load(IStorage *pStg) = 0;
    virtual HRESULT WINAPI Save(IStorage *pStgSave,WINBOOL fSameAsLoad) = 0;
    virtual HRESULT WINAPI SaveCompleted(IStorage *pStgNew) = 0;
    virtual HRESULT WINAPI HandsOffStorage(void) = 0;
  };
#else
  typedef struct IPersistStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPersistStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPersistStorage *This);
      ULONG (WINAPI *Release)(IPersistStorage *This);
      HRESULT (WINAPI *GetClassID)(IPersistStorage *This,CLSID *pClassID);
      HRESULT (WINAPI *IsDirty)(IPersistStorage *This);
      HRESULT (WINAPI *InitNew)(IPersistStorage *This,IStorage *pStg);
      HRESULT (WINAPI *Load)(IPersistStorage *This,IStorage *pStg);
      HRESULT (WINAPI *Save)(IPersistStorage *This,IStorage *pStgSave,WINBOOL fSameAsLoad);
      HRESULT (WINAPI *SaveCompleted)(IPersistStorage *This,IStorage *pStgNew);
      HRESULT (WINAPI *HandsOffStorage)(IPersistStorage *This);
    END_INTERFACE
  } IPersistStorageVtbl;
  struct IPersistStorage {
    CONST_VTBL struct IPersistStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPersistStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPersistStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPersistStorage_Release(This) (This)->lpVtbl->Release(This)
#define IPersistStorage_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#define IPersistStorage_IsDirty(This) (This)->lpVtbl->IsDirty(This)
#define IPersistStorage_InitNew(This,pStg) (This)->lpVtbl->InitNew(This,pStg)
#define IPersistStorage_Load(This,pStg) (This)->lpVtbl->Load(This,pStg)
#define IPersistStorage_Save(This,pStgSave,fSameAsLoad) (This)->lpVtbl->Save(This,pStgSave,fSameAsLoad)
#define IPersistStorage_SaveCompleted(This,pStgNew) (This)->lpVtbl->SaveCompleted(This,pStgNew)
#define IPersistStorage_HandsOffStorage(This) (This)->lpVtbl->HandsOffStorage(This)
#endif
#endif
  HRESULT WINAPI IPersistStorage_IsDirty_Proxy(IPersistStorage *This);
  void __RPC_STUB IPersistStorage_IsDirty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStorage_InitNew_Proxy(IPersistStorage *This,IStorage *pStg);
  void __RPC_STUB IPersistStorage_InitNew_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStorage_Load_Proxy(IPersistStorage *This,IStorage *pStg);
  void __RPC_STUB IPersistStorage_Load_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStorage_Save_Proxy(IPersistStorage *This,IStorage *pStgSave,WINBOOL fSameAsLoad);
  void __RPC_STUB IPersistStorage_Save_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStorage_SaveCompleted_Proxy(IPersistStorage *This,IStorage *pStgNew);
  void __RPC_STUB IPersistStorage_SaveCompleted_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistStorage_HandsOffStorage_Proxy(IPersistStorage *This);
  void __RPC_STUB IPersistStorage_HandsOffStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ILockBytes_INTERFACE_DEFINED__
#define __ILockBytes_INTERFACE_DEFINED__
  typedef ILockBytes *LPLOCKBYTES;

  EXTERN_C const IID IID_ILockBytes;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ILockBytes : public IUnknown {
  public:
    virtual HRESULT WINAPI ReadAt(ULARGE_INTEGER ulOffset,void *pv,ULONG cb,ULONG *pcbRead) = 0;
    virtual HRESULT WINAPI WriteAt(ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten) = 0;
    virtual HRESULT WINAPI Flush(void) = 0;
    virtual HRESULT WINAPI SetSize(ULARGE_INTEGER cb) = 0;
    virtual HRESULT WINAPI LockRegion(ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType) = 0;
    virtual HRESULT WINAPI UnlockRegion(ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType) = 0;
    virtual HRESULT WINAPI Stat(STATSTG *pstatstg,DWORD grfStatFlag) = 0;
  };
#else
  typedef struct ILockBytesVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ILockBytes *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ILockBytes *This);
      ULONG (WINAPI *Release)(ILockBytes *This);
      HRESULT (WINAPI *ReadAt)(ILockBytes *This,ULARGE_INTEGER ulOffset,void *pv,ULONG cb,ULONG *pcbRead);
      HRESULT (WINAPI *WriteAt)(ILockBytes *This,ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten);
      HRESULT (WINAPI *Flush)(ILockBytes *This);
      HRESULT (WINAPI *SetSize)(ILockBytes *This,ULARGE_INTEGER cb);
      HRESULT (WINAPI *LockRegion)(ILockBytes *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
      HRESULT (WINAPI *UnlockRegion)(ILockBytes *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
      HRESULT (WINAPI *Stat)(ILockBytes *This,STATSTG *pstatstg,DWORD grfStatFlag);
    END_INTERFACE
  } ILockBytesVtbl;
  struct ILockBytes {
    CONST_VTBL struct ILockBytesVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ILockBytes_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ILockBytes_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ILockBytes_Release(This) (This)->lpVtbl->Release(This)
#define ILockBytes_ReadAt(This,ulOffset,pv,cb,pcbRead) (This)->lpVtbl->ReadAt(This,ulOffset,pv,cb,pcbRead)
#define ILockBytes_WriteAt(This,ulOffset,pv,cb,pcbWritten) (This)->lpVtbl->WriteAt(This,ulOffset,pv,cb,pcbWritten)
#define ILockBytes_Flush(This) (This)->lpVtbl->Flush(This)
#define ILockBytes_SetSize(This,cb) (This)->lpVtbl->SetSize(This,cb)
#define ILockBytes_LockRegion(This,libOffset,cb,dwLockType) (This)->lpVtbl->LockRegion(This,libOffset,cb,dwLockType)
#define ILockBytes_UnlockRegion(This,libOffset,cb,dwLockType) (This)->lpVtbl->UnlockRegion(This,libOffset,cb,dwLockType)
#define ILockBytes_Stat(This,pstatstg,grfStatFlag) (This)->lpVtbl->Stat(This,pstatstg,grfStatFlag)
#endif
#endif
  HRESULT WINAPI ILockBytes_RemoteReadAt_Proxy(ILockBytes *This,ULARGE_INTEGER ulOffset,byte *pv,ULONG cb,ULONG *pcbRead);
  void __RPC_STUB ILockBytes_RemoteReadAt_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_RemoteWriteAt_Proxy(ILockBytes *This,ULARGE_INTEGER ulOffset,const byte *pv,ULONG cb,ULONG *pcbWritten);
  void __RPC_STUB ILockBytes_RemoteWriteAt_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_Flush_Proxy(ILockBytes *This);
  void __RPC_STUB ILockBytes_Flush_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_SetSize_Proxy(ILockBytes *This,ULARGE_INTEGER cb);
  void __RPC_STUB ILockBytes_SetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_LockRegion_Proxy(ILockBytes *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
  void __RPC_STUB ILockBytes_LockRegion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_UnlockRegion_Proxy(ILockBytes *This,ULARGE_INTEGER libOffset,ULARGE_INTEGER cb,DWORD dwLockType);
  void __RPC_STUB ILockBytes_UnlockRegion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILockBytes_Stat_Proxy(ILockBytes *This,STATSTG *pstatstg,DWORD grfStatFlag);
  void __RPC_STUB ILockBytes_Stat_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumFORMATETC_INTERFACE_DEFINED__
#define __IEnumFORMATETC_INTERFACE_DEFINED__
  typedef IEnumFORMATETC *LPENUMFORMATETC;

  typedef struct tagDVTARGETDEVICE {
    DWORD tdSize;
    WORD tdDriverNameOffset;
    WORD tdDeviceNameOffset;
    WORD tdPortNameOffset;
    WORD tdExtDevmodeOffset;
    BYTE tdData[1 ];
  } DVTARGETDEVICE;

  typedef CLIPFORMAT *LPCLIPFORMAT;

  typedef struct tagFORMATETC {
    CLIPFORMAT cfFormat;
    DVTARGETDEVICE *ptd;
    DWORD dwAspect;
    LONG lindex;
    DWORD tymed;
  } FORMATETC;

  typedef struct tagFORMATETC *LPFORMATETC;

  EXTERN_C const IID IID_IEnumFORMATETC;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumFORMATETC : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,FORMATETC *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumFORMATETC **ppenum) = 0;
  };
#else
  typedef struct IEnumFORMATETCVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumFORMATETC *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumFORMATETC *This);
      ULONG (WINAPI *Release)(IEnumFORMATETC *This);
      HRESULT (WINAPI *Next)(IEnumFORMATETC *This,ULONG celt,FORMATETC *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumFORMATETC *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumFORMATETC *This);
      HRESULT (WINAPI *Clone)(IEnumFORMATETC *This,IEnumFORMATETC **ppenum);
    END_INTERFACE
  } IEnumFORMATETCVtbl;
  struct IEnumFORMATETC {
    CONST_VTBL struct IEnumFORMATETCVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumFORMATETC_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumFORMATETC_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumFORMATETC_Release(This) (This)->lpVtbl->Release(This)
#define IEnumFORMATETC_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumFORMATETC_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumFORMATETC_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumFORMATETC_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumFORMATETC_RemoteNext_Proxy(IEnumFORMATETC *This,ULONG celt,FORMATETC *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumFORMATETC_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumFORMATETC_Skip_Proxy(IEnumFORMATETC *This,ULONG celt);
  void __RPC_STUB IEnumFORMATETC_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumFORMATETC_Reset_Proxy(IEnumFORMATETC *This);
  void __RPC_STUB IEnumFORMATETC_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumFORMATETC_Clone_Proxy(IEnumFORMATETC *This,IEnumFORMATETC **ppenum);
  void __RPC_STUB IEnumFORMATETC_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumSTATDATA_INTERFACE_DEFINED__
#define __IEnumSTATDATA_INTERFACE_DEFINED__
  typedef IEnumSTATDATA *LPENUMSTATDATA;

  typedef enum tagADVF {
    ADVF_NODATA = 1,ADVF_PRIMEFIRST = 2,ADVF_ONLYONCE = 4,ADVF_DATAONSTOP = 64,ADVFCACHE_NOHANDLER = 8,ADVFCACHE_FORCEBUILTIN = 16,
    ADVFCACHE_ONSAVE = 32
  } ADVF;

  typedef struct tagSTATDATA {
    FORMATETC formatetc;
    DWORD advf;
    IAdviseSink *pAdvSink;
    DWORD dwConnection;
  } STATDATA;

  typedef STATDATA *LPSTATDATA;

  EXTERN_C const IID IID_IEnumSTATDATA;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumSTATDATA : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,STATDATA *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumSTATDATA **ppenum) = 0;
  };
#else
  typedef struct IEnumSTATDATAVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumSTATDATA *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumSTATDATA *This);
      ULONG (WINAPI *Release)(IEnumSTATDATA *This);
      HRESULT (WINAPI *Next)(IEnumSTATDATA *This,ULONG celt,STATDATA *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumSTATDATA *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumSTATDATA *This);
      HRESULT (WINAPI *Clone)(IEnumSTATDATA *This,IEnumSTATDATA **ppenum);
    END_INTERFACE
  } IEnumSTATDATAVtbl;
  struct IEnumSTATDATA {
    CONST_VTBL struct IEnumSTATDATAVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumSTATDATA_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATDATA_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumSTATDATA_Release(This) (This)->lpVtbl->Release(This)
#define IEnumSTATDATA_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATDATA_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATDATA_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumSTATDATA_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumSTATDATA_RemoteNext_Proxy(IEnumSTATDATA *This,ULONG celt,STATDATA *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumSTATDATA_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATDATA_Skip_Proxy(IEnumSTATDATA *This,ULONG celt);
  void __RPC_STUB IEnumSTATDATA_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATDATA_Reset_Proxy(IEnumSTATDATA *This);
  void __RPC_STUB IEnumSTATDATA_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATDATA_Clone_Proxy(IEnumSTATDATA *This,IEnumSTATDATA **ppenum);
  void __RPC_STUB IEnumSTATDATA_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRootStorage_INTERFACE_DEFINED__
#define __IRootStorage_INTERFACE_DEFINED__
  typedef IRootStorage *LPROOTSTORAGE;

  EXTERN_C const IID IID_IRootStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRootStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI SwitchToFile(LPOLESTR pszFile) = 0;
  };
#else
  typedef struct IRootStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRootStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRootStorage *This);
      ULONG (WINAPI *Release)(IRootStorage *This);
      HRESULT (WINAPI *SwitchToFile)(IRootStorage *This,LPOLESTR pszFile);
    END_INTERFACE
  } IRootStorageVtbl;
  struct IRootStorage {
    CONST_VTBL struct IRootStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRootStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRootStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRootStorage_Release(This) (This)->lpVtbl->Release(This)
#define IRootStorage_SwitchToFile(This,pszFile) (This)->lpVtbl->SwitchToFile(This,pszFile)
#endif
#endif
  HRESULT WINAPI IRootStorage_SwitchToFile_Proxy(IRootStorage *This,LPOLESTR pszFile);
  void __RPC_STUB IRootStorage_SwitchToFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAdviseSink_INTERFACE_DEFINED__
#define __IAdviseSink_INTERFACE_DEFINED__
  typedef IAdviseSink *LPADVISESINK;

  typedef enum tagTYMED {
    TYMED_HGLOBAL = 1,TYMED_FILE = 2,TYMED_ISTREAM = 4,TYMED_ISTORAGE = 8,
    TYMED_GDI = 16,TYMED_MFPICT = 32,TYMED_ENHMF = 64,TYMED_NULL = 0
  } TYMED;

  typedef struct tagRemSTGMEDIUM {
    DWORD tymed;
    DWORD dwHandleType;
    unsigned long pData;
    unsigned long pUnkForRelease;
    unsigned long cbData;
    byte data[1 ];
  } RemSTGMEDIUM;

#ifdef NONAMELESSUNION
  typedef struct tagSTGMEDIUM {
    DWORD tymed;
    union {
      HBITMAP hBitmap;
      HMETAFILEPICT hMetaFilePict;
      HENHMETAFILE hEnhMetaFile;
      HGLOBAL hGlobal;
      LPOLESTR lpszFileName;
      IStream *pstm;
      IStorage *pstg;
    } u;
    IUnknown *pUnkForRelease;
  } uSTGMEDIUM;
#else
  typedef struct tagSTGMEDIUM {
    DWORD tymed;
    __MINGW_EXTENSION union {
      HBITMAP hBitmap;
      HMETAFILEPICT hMetaFilePict;
      HENHMETAFILE hEnhMetaFile;
      HGLOBAL hGlobal;
      LPOLESTR lpszFileName;
      IStream *pstm;
      IStorage *pstg;
    };
    IUnknown *pUnkForRelease;
  } uSTGMEDIUM;
#endif
  typedef struct _GDI_OBJECT {
    DWORD ObjectType;
    union __MIDL_IAdviseSink_0002 {
      wireHBITMAP hBitmap;
      wireHPALETTE hPalette;
      wireHGLOBAL hGeneric;
    } u;
  } GDI_OBJECT;

  typedef struct _userSTGMEDIUM {
    struct _STGMEDIUM_UNION {
      DWORD tymed;
      union __MIDL_IAdviseSink_0003 {
	wireHMETAFILEPICT hMetaFilePict;
	wireHENHMETAFILE hHEnhMetaFile;
	GDI_OBJECT *hGdiHandle;
	wireHGLOBAL hGlobal;
	LPOLESTR lpszFileName;
	BYTE_BLOB *pstm;
	BYTE_BLOB *pstg;
      } u;
    } _unnamed;
    IUnknown *pUnkForRelease;
  } userSTGMEDIUM;

  typedef userSTGMEDIUM *wireSTGMEDIUM;
  typedef uSTGMEDIUM STGMEDIUM;
  typedef userSTGMEDIUM *wireASYNC_STGMEDIUM;
  typedef STGMEDIUM ASYNC_STGMEDIUM;
  typedef STGMEDIUM *LPSTGMEDIUM;

  typedef struct _userFLAG_STGMEDIUM {
    long ContextFlags;
    long fPassOwnership;
    userSTGMEDIUM Stgmed;
  } userFLAG_STGMEDIUM;

  typedef userFLAG_STGMEDIUM *wireFLAG_STGMEDIUM;

  typedef struct _FLAG_STGMEDIUM {
    long ContextFlags;
    long fPassOwnership;
    STGMEDIUM Stgmed;
  } FLAG_STGMEDIUM;

  EXTERN_C const IID IID_IAdviseSink;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAdviseSink : public IUnknown {
  public:
    virtual void WINAPI OnDataChange(FORMATETC *pFormatetc,STGMEDIUM *pStgmed) = 0;
    virtual void WINAPI OnViewChange(DWORD dwAspect,LONG lindex) = 0;
    virtual void WINAPI OnRename(IMoniker *pmk) = 0;
    virtual void WINAPI OnSave(void) = 0;
    virtual void WINAPI OnClose(void) = 0;
  };
#else
  typedef struct IAdviseSinkVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAdviseSink *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAdviseSink *This);
      ULONG (WINAPI *Release)(IAdviseSink *This);
      void (WINAPI *OnDataChange)(IAdviseSink *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
      void (WINAPI *OnViewChange)(IAdviseSink *This,DWORD dwAspect,LONG lindex);
      void (WINAPI *OnRename)(IAdviseSink *This,IMoniker *pmk);
      void (WINAPI *OnSave)(IAdviseSink *This);
      void (WINAPI *OnClose)(IAdviseSink *This);
    END_INTERFACE
  } IAdviseSinkVtbl;
  struct IAdviseSink {
    CONST_VTBL struct IAdviseSinkVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAdviseSink_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAdviseSink_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAdviseSink_Release(This) (This)->lpVtbl->Release(This)
#define IAdviseSink_OnDataChange(This,pFormatetc,pStgmed) (This)->lpVtbl->OnDataChange(This,pFormatetc,pStgmed)
#define IAdviseSink_OnViewChange(This,dwAspect,lindex) (This)->lpVtbl->OnViewChange(This,dwAspect,lindex)
#define IAdviseSink_OnRename(This,pmk) (This)->lpVtbl->OnRename(This,pmk)
#define IAdviseSink_OnSave(This) (This)->lpVtbl->OnSave(This)
#define IAdviseSink_OnClose(This) (This)->lpVtbl->OnClose(This)
#endif
#endif
  HRESULT WINAPI IAdviseSink_RemoteOnDataChange_Proxy(IAdviseSink *This,FORMATETC *pFormatetc,ASYNC_STGMEDIUM *pStgmed);
  void __RPC_STUB IAdviseSink_RemoteOnDataChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAdviseSink_RemoteOnViewChange_Proxy(IAdviseSink *This,DWORD dwAspect,LONG lindex);
  void __RPC_STUB IAdviseSink_RemoteOnViewChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAdviseSink_RemoteOnRename_Proxy(IAdviseSink *This,IMoniker *pmk);
  void __RPC_STUB IAdviseSink_RemoteOnRename_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAdviseSink_RemoteOnSave_Proxy(IAdviseSink *This);
  void __RPC_STUB IAdviseSink_RemoteOnSave_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAdviseSink_RemoteOnClose_Proxy(IAdviseSink *This);
  void __RPC_STUB IAdviseSink_RemoteOnClose_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIAdviseSink_INTERFACE_DEFINED__
#define __AsyncIAdviseSink_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIAdviseSink;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIAdviseSink : public IUnknown {
  public:
    virtual void WINAPI Begin_OnDataChange(FORMATETC *pFormatetc,STGMEDIUM *pStgmed) = 0;
    virtual void WINAPI Finish_OnDataChange(void) = 0;
    virtual void WINAPI Begin_OnViewChange(DWORD dwAspect,LONG lindex) = 0;
    virtual void WINAPI Finish_OnViewChange(void) = 0;
    virtual void WINAPI Begin_OnRename(IMoniker *pmk) = 0;
    virtual void WINAPI Finish_OnRename(void) = 0;
    virtual void WINAPI Begin_OnSave(void) = 0;
    virtual void WINAPI Finish_OnSave(void) = 0;
    virtual void WINAPI Begin_OnClose(void) = 0;
    virtual void WINAPI Finish_OnClose(void) = 0;
  };
#else
  typedef struct AsyncIAdviseSinkVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIAdviseSink *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIAdviseSink *This);
      ULONG (WINAPI *Release)(AsyncIAdviseSink *This);
      void (WINAPI *Begin_OnDataChange)(AsyncIAdviseSink *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
      void (WINAPI *Finish_OnDataChange)(AsyncIAdviseSink *This);
      void (WINAPI *Begin_OnViewChange)(AsyncIAdviseSink *This,DWORD dwAspect,LONG lindex);
      void (WINAPI *Finish_OnViewChange)(AsyncIAdviseSink *This);
      void (WINAPI *Begin_OnRename)(AsyncIAdviseSink *This,IMoniker *pmk);
      void (WINAPI *Finish_OnRename)(AsyncIAdviseSink *This);
      void (WINAPI *Begin_OnSave)(AsyncIAdviseSink *This);
      void (WINAPI *Finish_OnSave)(AsyncIAdviseSink *This);
      void (WINAPI *Begin_OnClose)(AsyncIAdviseSink *This);
      void (WINAPI *Finish_OnClose)(AsyncIAdviseSink *This);
    END_INTERFACE
  } AsyncIAdviseSinkVtbl;
  struct AsyncIAdviseSink {
    CONST_VTBL struct AsyncIAdviseSinkVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIAdviseSink_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIAdviseSink_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIAdviseSink_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIAdviseSink_Begin_OnDataChange(This,pFormatetc,pStgmed) (This)->lpVtbl->Begin_OnDataChange(This,pFormatetc,pStgmed)
#define AsyncIAdviseSink_Finish_OnDataChange(This) (This)->lpVtbl->Finish_OnDataChange(This)
#define AsyncIAdviseSink_Begin_OnViewChange(This,dwAspect,lindex) (This)->lpVtbl->Begin_OnViewChange(This,dwAspect,lindex)
#define AsyncIAdviseSink_Finish_OnViewChange(This) (This)->lpVtbl->Finish_OnViewChange(This)
#define AsyncIAdviseSink_Begin_OnRename(This,pmk) (This)->lpVtbl->Begin_OnRename(This,pmk)
#define AsyncIAdviseSink_Finish_OnRename(This) (This)->lpVtbl->Finish_OnRename(This)
#define AsyncIAdviseSink_Begin_OnSave(This) (This)->lpVtbl->Begin_OnSave(This)
#define AsyncIAdviseSink_Finish_OnSave(This) (This)->lpVtbl->Finish_OnSave(This)
#define AsyncIAdviseSink_Begin_OnClose(This) (This)->lpVtbl->Begin_OnClose(This)
#define AsyncIAdviseSink_Finish_OnClose(This) (This)->lpVtbl->Finish_OnClose(This)
#endif
#endif
  HRESULT WINAPI AsyncIAdviseSink_Begin_RemoteOnDataChange_Proxy(AsyncIAdviseSink *This,FORMATETC *pFormatetc,ASYNC_STGMEDIUM *pStgmed);
  void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnDataChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Finish_RemoteOnDataChange_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnDataChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Begin_RemoteOnViewChange_Proxy(AsyncIAdviseSink *This,DWORD dwAspect,LONG lindex);
  void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnViewChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Finish_RemoteOnViewChange_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnViewChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Begin_RemoteOnRename_Proxy(AsyncIAdviseSink *This,IMoniker *pmk);
  void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnRename_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Finish_RemoteOnRename_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnRename_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Begin_RemoteOnSave_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnSave_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Finish_RemoteOnSave_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnSave_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Begin_RemoteOnClose_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnClose_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink_Finish_RemoteOnClose_Proxy(AsyncIAdviseSink *This);
  void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnClose_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAdviseSink2_INTERFACE_DEFINED__
#define __IAdviseSink2_INTERFACE_DEFINED__
  typedef IAdviseSink2 *LPADVISESINK2;

  EXTERN_C const IID IID_IAdviseSink2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAdviseSink2 : public IAdviseSink {
  public:
    virtual void WINAPI OnLinkSrcChange(IMoniker *pmk) = 0;
  };
#else
  typedef struct IAdviseSink2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAdviseSink2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAdviseSink2 *This);
      ULONG (WINAPI *Release)(IAdviseSink2 *This);
      void (WINAPI *OnDataChange)(IAdviseSink2 *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
      void (WINAPI *OnViewChange)(IAdviseSink2 *This,DWORD dwAspect,LONG lindex);
      void (WINAPI *OnRename)(IAdviseSink2 *This,IMoniker *pmk);
      void (WINAPI *OnSave)(IAdviseSink2 *This);
      void (WINAPI *OnClose)(IAdviseSink2 *This);
      void (WINAPI *OnLinkSrcChange)(IAdviseSink2 *This,IMoniker *pmk);
    END_INTERFACE
  } IAdviseSink2Vtbl;
  struct IAdviseSink2 {
    CONST_VTBL struct IAdviseSink2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAdviseSink2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAdviseSink2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAdviseSink2_Release(This) (This)->lpVtbl->Release(This)
#define IAdviseSink2_OnDataChange(This,pFormatetc,pStgmed) (This)->lpVtbl->OnDataChange(This,pFormatetc,pStgmed)
#define IAdviseSink2_OnViewChange(This,dwAspect,lindex) (This)->lpVtbl->OnViewChange(This,dwAspect,lindex)
#define IAdviseSink2_OnRename(This,pmk) (This)->lpVtbl->OnRename(This,pmk)
#define IAdviseSink2_OnSave(This) (This)->lpVtbl->OnSave(This)
#define IAdviseSink2_OnClose(This) (This)->lpVtbl->OnClose(This)
#define IAdviseSink2_OnLinkSrcChange(This,pmk) (This)->lpVtbl->OnLinkSrcChange(This,pmk)
#endif
#endif
  HRESULT WINAPI IAdviseSink2_RemoteOnLinkSrcChange_Proxy(IAdviseSink2 *This,IMoniker *pmk);
  void __RPC_STUB IAdviseSink2_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIAdviseSink2_INTERFACE_DEFINED__
#define __AsyncIAdviseSink2_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIAdviseSink2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIAdviseSink2 : public AsyncIAdviseSink {
  public:
    virtual void WINAPI Begin_OnLinkSrcChange(IMoniker *pmk) = 0;
    virtual void WINAPI Finish_OnLinkSrcChange(void) = 0;
  };
#else
  typedef struct AsyncIAdviseSink2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIAdviseSink2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIAdviseSink2 *This);
      ULONG (WINAPI *Release)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnDataChange)(AsyncIAdviseSink2 *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
      void (WINAPI *Finish_OnDataChange)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnViewChange)(AsyncIAdviseSink2 *This,DWORD dwAspect,LONG lindex);
      void (WINAPI *Finish_OnViewChange)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnRename)(AsyncIAdviseSink2 *This,IMoniker *pmk);
      void (WINAPI *Finish_OnRename)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnSave)(AsyncIAdviseSink2 *This);
      void (WINAPI *Finish_OnSave)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnClose)(AsyncIAdviseSink2 *This);
      void (WINAPI *Finish_OnClose)(AsyncIAdviseSink2 *This);
      void (WINAPI *Begin_OnLinkSrcChange)(AsyncIAdviseSink2 *This,IMoniker *pmk);
      void (WINAPI *Finish_OnLinkSrcChange)(AsyncIAdviseSink2 *This);
    END_INTERFACE
  } AsyncIAdviseSink2Vtbl;
  struct AsyncIAdviseSink2 {
    CONST_VTBL struct AsyncIAdviseSink2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIAdviseSink2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIAdviseSink2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIAdviseSink2_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIAdviseSink2_Begin_OnDataChange(This,pFormatetc,pStgmed) (This)->lpVtbl->Begin_OnDataChange(This,pFormatetc,pStgmed)
#define AsyncIAdviseSink2_Finish_OnDataChange(This) (This)->lpVtbl->Finish_OnDataChange(This)
#define AsyncIAdviseSink2_Begin_OnViewChange(This,dwAspect,lindex) (This)->lpVtbl->Begin_OnViewChange(This,dwAspect,lindex)
#define AsyncIAdviseSink2_Finish_OnViewChange(This) (This)->lpVtbl->Finish_OnViewChange(This)
#define AsyncIAdviseSink2_Begin_OnRename(This,pmk) (This)->lpVtbl->Begin_OnRename(This,pmk)
#define AsyncIAdviseSink2_Finish_OnRename(This) (This)->lpVtbl->Finish_OnRename(This)
#define AsyncIAdviseSink2_Begin_OnSave(This) (This)->lpVtbl->Begin_OnSave(This)
#define AsyncIAdviseSink2_Finish_OnSave(This) (This)->lpVtbl->Finish_OnSave(This)
#define AsyncIAdviseSink2_Begin_OnClose(This) (This)->lpVtbl->Begin_OnClose(This)
#define AsyncIAdviseSink2_Finish_OnClose(This) (This)->lpVtbl->Finish_OnClose(This)
#define AsyncIAdviseSink2_Begin_OnLinkSrcChange(This,pmk) (This)->lpVtbl->Begin_OnLinkSrcChange(This,pmk)
#define AsyncIAdviseSink2_Finish_OnLinkSrcChange(This) (This)->lpVtbl->Finish_OnLinkSrcChange(This)
#endif
#endif
  HRESULT WINAPI AsyncIAdviseSink2_Begin_RemoteOnLinkSrcChange_Proxy(AsyncIAdviseSink2 *This,IMoniker *pmk);
  void __RPC_STUB AsyncIAdviseSink2_Begin_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIAdviseSink2_Finish_RemoteOnLinkSrcChange_Proxy(AsyncIAdviseSink2 *This);
  void __RPC_STUB AsyncIAdviseSink2_Finish_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDataObject_INTERFACE_DEFINED__
#define __IDataObject_INTERFACE_DEFINED__
  typedef IDataObject *LPDATAOBJECT;

  typedef enum tagDATADIR {
    DATADIR_GET = 1,DATADIR_SET = 2
  } DATADIR;

  EXTERN_C const IID IID_IDataObject;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDataObject : public IUnknown {
  public:
    virtual HRESULT WINAPI GetData(FORMATETC *pformatetcIn,STGMEDIUM *pmedium) = 0;
    virtual HRESULT WINAPI GetDataHere(FORMATETC *pformatetc,STGMEDIUM *pmedium) = 0;
    virtual HRESULT WINAPI QueryGetData(FORMATETC *pformatetc) = 0;
    virtual HRESULT WINAPI GetCanonicalFormatEtc(FORMATETC *pformatectIn,FORMATETC *pformatetcOut) = 0;
    virtual HRESULT WINAPI SetData(FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease) = 0;
    virtual HRESULT WINAPI EnumFormatEtc(DWORD dwDirection,IEnumFORMATETC **ppenumFormatEtc) = 0;
    virtual HRESULT WINAPI DAdvise(FORMATETC *pformatetc,DWORD advf,IAdviseSink *pAdvSink,DWORD *pdwConnection) = 0;
    virtual HRESULT WINAPI DUnadvise(DWORD dwConnection) = 0;
    virtual HRESULT WINAPI EnumDAdvise(IEnumSTATDATA **ppenumAdvise) = 0;
  };
#else
  typedef struct IDataObjectVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDataObject *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDataObject *This);
      ULONG (WINAPI *Release)(IDataObject *This);
      HRESULT (WINAPI *GetData)(IDataObject *This,FORMATETC *pformatetcIn,STGMEDIUM *pmedium);
      HRESULT (WINAPI *GetDataHere)(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pmedium);
      HRESULT (WINAPI *QueryGetData)(IDataObject *This,FORMATETC *pformatetc);
      HRESULT (WINAPI *GetCanonicalFormatEtc)(IDataObject *This,FORMATETC *pformatectIn,FORMATETC *pformatetcOut);
      HRESULT (WINAPI *SetData)(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease);
      HRESULT (WINAPI *EnumFormatEtc)(IDataObject *This,DWORD dwDirection,IEnumFORMATETC **ppenumFormatEtc);
      HRESULT (WINAPI *DAdvise)(IDataObject *This,FORMATETC *pformatetc,DWORD advf,IAdviseSink *pAdvSink,DWORD *pdwConnection);
      HRESULT (WINAPI *DUnadvise)(IDataObject *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumDAdvise)(IDataObject *This,IEnumSTATDATA **ppenumAdvise);
    END_INTERFACE
  } IDataObjectVtbl;
  struct IDataObject {
    CONST_VTBL struct IDataObjectVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDataObject_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDataObject_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDataObject_Release(This) (This)->lpVtbl->Release(This)
#define IDataObject_GetData(This,pformatetcIn,pmedium) (This)->lpVtbl->GetData(This,pformatetcIn,pmedium)
#define IDataObject_GetDataHere(This,pformatetc,pmedium) (This)->lpVtbl->GetDataHere(This,pformatetc,pmedium)
#define IDataObject_QueryGetData(This,pformatetc) (This)->lpVtbl->QueryGetData(This,pformatetc)
#define IDataObject_GetCanonicalFormatEtc(This,pformatectIn,pformatetcOut) (This)->lpVtbl->GetCanonicalFormatEtc(This,pformatectIn,pformatetcOut)
#define IDataObject_SetData(This,pformatetc,pmedium,fRelease) (This)->lpVtbl->SetData(This,pformatetc,pmedium,fRelease)
#define IDataObject_EnumFormatEtc(This,dwDirection,ppenumFormatEtc) (This)->lpVtbl->EnumFormatEtc(This,dwDirection,ppenumFormatEtc)
#define IDataObject_DAdvise(This,pformatetc,advf,pAdvSink,pdwConnection) (This)->lpVtbl->DAdvise(This,pformatetc,advf,pAdvSink,pdwConnection)
#define IDataObject_DUnadvise(This,dwConnection) (This)->lpVtbl->DUnadvise(This,dwConnection)
#define IDataObject_EnumDAdvise(This,ppenumAdvise) (This)->lpVtbl->EnumDAdvise(This,ppenumAdvise)
#endif
#endif
  HRESULT WINAPI IDataObject_RemoteGetData_Proxy(IDataObject *This,FORMATETC *pformatetcIn,STGMEDIUM *pRemoteMedium);
  void __RPC_STUB IDataObject_RemoteGetData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_RemoteGetDataHere_Proxy(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pRemoteMedium);
  void __RPC_STUB IDataObject_RemoteGetDataHere_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_QueryGetData_Proxy(IDataObject *This,FORMATETC *pformatetc);
  void __RPC_STUB IDataObject_QueryGetData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_GetCanonicalFormatEtc_Proxy(IDataObject *This,FORMATETC *pformatectIn,FORMATETC *pformatetcOut);
  void __RPC_STUB IDataObject_GetCanonicalFormatEtc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_RemoteSetData_Proxy(IDataObject *This,FORMATETC *pformatetc,FLAG_STGMEDIUM *pmedium,WINBOOL fRelease);
  void __RPC_STUB IDataObject_RemoteSetData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_EnumFormatEtc_Proxy(IDataObject *This,DWORD dwDirection,IEnumFORMATETC **ppenumFormatEtc);
  void __RPC_STUB IDataObject_EnumFormatEtc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_DAdvise_Proxy(IDataObject *This,FORMATETC *pformatetc,DWORD advf,IAdviseSink *pAdvSink,DWORD *pdwConnection);
  void __RPC_STUB IDataObject_DAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_DUnadvise_Proxy(IDataObject *This,DWORD dwConnection);
  void __RPC_STUB IDataObject_DUnadvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataObject_EnumDAdvise_Proxy(IDataObject *This,IEnumSTATDATA **ppenumAdvise);
  void __RPC_STUB IDataObject_EnumDAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDataAdviseHolder_INTERFACE_DEFINED__
#define __IDataAdviseHolder_INTERFACE_DEFINED__
  typedef IDataAdviseHolder *LPDATAADVISEHOLDER;
  EXTERN_C const IID IID_IDataAdviseHolder;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDataAdviseHolder : public IUnknown {
  public:
    virtual HRESULT WINAPI Advise(IDataObject *pDataObject,FORMATETC *pFetc,DWORD advf,IAdviseSink *pAdvise,DWORD *pdwConnection) = 0;
    virtual HRESULT WINAPI Unadvise(DWORD dwConnection) = 0;
    virtual HRESULT WINAPI EnumAdvise(IEnumSTATDATA **ppenumAdvise) = 0;
    virtual HRESULT WINAPI SendOnDataChange(IDataObject *pDataObject,DWORD dwReserved,DWORD advf) = 0;
  };
#else
  typedef struct IDataAdviseHolderVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDataAdviseHolder *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDataAdviseHolder *This);
      ULONG (WINAPI *Release)(IDataAdviseHolder *This);
      HRESULT (WINAPI *Advise)(IDataAdviseHolder *This,IDataObject *pDataObject,FORMATETC *pFetc,DWORD advf,IAdviseSink *pAdvise,DWORD *pdwConnection);
      HRESULT (WINAPI *Unadvise)(IDataAdviseHolder *This,DWORD dwConnection);
      HRESULT (WINAPI *EnumAdvise)(IDataAdviseHolder *This,IEnumSTATDATA **ppenumAdvise);
      HRESULT (WINAPI *SendOnDataChange)(IDataAdviseHolder *This,IDataObject *pDataObject,DWORD dwReserved,DWORD advf);
    END_INTERFACE
  } IDataAdviseHolderVtbl;
  struct IDataAdviseHolder {
    CONST_VTBL struct IDataAdviseHolderVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDataAdviseHolder_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDataAdviseHolder_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDataAdviseHolder_Release(This) (This)->lpVtbl->Release(This)
#define IDataAdviseHolder_Advise(This,pDataObject,pFetc,advf,pAdvise,pdwConnection) (This)->lpVtbl->Advise(This,pDataObject,pFetc,advf,pAdvise,pdwConnection)
#define IDataAdviseHolder_Unadvise(This,dwConnection) (This)->lpVtbl->Unadvise(This,dwConnection)
#define IDataAdviseHolder_EnumAdvise(This,ppenumAdvise) (This)->lpVtbl->EnumAdvise(This,ppenumAdvise)
#define IDataAdviseHolder_SendOnDataChange(This,pDataObject,dwReserved,advf) (This)->lpVtbl->SendOnDataChange(This,pDataObject,dwReserved,advf)
#endif
#endif
  HRESULT WINAPI IDataAdviseHolder_Advise_Proxy(IDataAdviseHolder *This,IDataObject *pDataObject,FORMATETC *pFetc,DWORD advf,IAdviseSink *pAdvise,DWORD *pdwConnection);
  void __RPC_STUB IDataAdviseHolder_Advise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataAdviseHolder_Unadvise_Proxy(IDataAdviseHolder *This,DWORD dwConnection);
  void __RPC_STUB IDataAdviseHolder_Unadvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataAdviseHolder_EnumAdvise_Proxy(IDataAdviseHolder *This,IEnumSTATDATA **ppenumAdvise);
  void __RPC_STUB IDataAdviseHolder_EnumAdvise_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataAdviseHolder_SendOnDataChange_Proxy(IDataAdviseHolder *This,IDataObject *pDataObject,DWORD dwReserved,DWORD advf);
  void __RPC_STUB IDataAdviseHolder_SendOnDataChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IMessageFilter_INTERFACE_DEFINED__
#define __IMessageFilter_INTERFACE_DEFINED__
  typedef IMessageFilter *LPMESSAGEFILTER;

  typedef enum tagCALLTYPE {
    CALLTYPE_TOPLEVEL = 1,CALLTYPE_NESTED = 2,CALLTYPE_ASYNC = 3,CALLTYPE_TOPLEVEL_CALLPENDING = 4,CALLTYPE_ASYNC_CALLPENDING = 5
  } CALLTYPE;

  typedef enum tagSERVERCALL {
    SERVERCALL_ISHANDLED = 0,SERVERCALL_REJECTED = 1,SERVERCALL_RETRYLATER = 2
  } SERVERCALL;

  typedef enum tagPENDINGTYPE {
    PENDINGTYPE_TOPLEVEL = 1,PENDINGTYPE_NESTED = 2
  } PENDINGTYPE;

  typedef enum tagPENDINGMSG {
    PENDINGMSG_CANCELCALL = 0,PENDINGMSG_WAITNOPROCESS = 1,PENDINGMSG_WAITDEFPROCESS = 2
  } PENDINGMSG;

  typedef struct tagINTERFACEINFO {
    IUnknown *pUnk;
    IID iid;
    WORD wMethod;
  } INTERFACEINFO;

  typedef struct tagINTERFACEINFO *LPINTERFACEINFO;

  EXTERN_C const IID IID_IMessageFilter;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMessageFilter : public IUnknown {
  public:
    virtual DWORD WINAPI HandleInComingCall(DWORD dwCallType,HTASK htaskCaller,DWORD dwTickCount,LPINTERFACEINFO lpInterfaceInfo) = 0;
    virtual DWORD WINAPI RetryRejectedCall(HTASK htaskCallee,DWORD dwTickCount,DWORD dwRejectType) = 0;
    virtual DWORD WINAPI MessagePending(HTASK htaskCallee,DWORD dwTickCount,DWORD dwPendingType) = 0;
  };
#else
  typedef struct IMessageFilterVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMessageFilter *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMessageFilter *This);
      ULONG (WINAPI *Release)(IMessageFilter *This);
      DWORD (WINAPI *HandleInComingCall)(IMessageFilter *This,DWORD dwCallType,HTASK htaskCaller,DWORD dwTickCount,LPINTERFACEINFO lpInterfaceInfo);
      DWORD (WINAPI *RetryRejectedCall)(IMessageFilter *This,HTASK htaskCallee,DWORD dwTickCount,DWORD dwRejectType);
      DWORD (WINAPI *MessagePending)(IMessageFilter *This,HTASK htaskCallee,DWORD dwTickCount,DWORD dwPendingType);
    END_INTERFACE
  } IMessageFilterVtbl;
  struct IMessageFilter {
    CONST_VTBL struct IMessageFilterVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMessageFilter_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMessageFilter_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMessageFilter_Release(This) (This)->lpVtbl->Release(This)
#define IMessageFilter_HandleInComingCall(This,dwCallType,htaskCaller,dwTickCount,lpInterfaceInfo) (This)->lpVtbl->HandleInComingCall(This,dwCallType,htaskCaller,dwTickCount,lpInterfaceInfo)
#define IMessageFilter_RetryRejectedCall(This,htaskCallee,dwTickCount,dwRejectType) (This)->lpVtbl->RetryRejectedCall(This,htaskCallee,dwTickCount,dwRejectType)
#define IMessageFilter_MessagePending(This,htaskCallee,dwTickCount,dwPendingType) (This)->lpVtbl->MessagePending(This,htaskCallee,dwTickCount,dwPendingType)
#endif
#endif
  DWORD WINAPI IMessageFilter_HandleInComingCall_Proxy(IMessageFilter *This,DWORD dwCallType,HTASK htaskCaller,DWORD dwTickCount,LPINTERFACEINFO lpInterfaceInfo);
  void __RPC_STUB IMessageFilter_HandleInComingCall_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  DWORD WINAPI IMessageFilter_RetryRejectedCall_Proxy(IMessageFilter *This,HTASK htaskCallee,DWORD dwTickCount,DWORD dwRejectType);
  void __RPC_STUB IMessageFilter_RetryRejectedCall_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  DWORD WINAPI IMessageFilter_MessagePending_Proxy(IMessageFilter *This,HTASK htaskCallee,DWORD dwTickCount,DWORD dwPendingType);
  void __RPC_STUB IMessageFilter_MessagePending_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcChannelBuffer_INTERFACE_DEFINED__
#define __IRpcChannelBuffer_INTERFACE_DEFINED__
  typedef unsigned long RPCOLEDATAREP;

  typedef struct tagRPCOLEMESSAGE {
    void *reserved1;
    RPCOLEDATAREP dataRepresentation;
    void *Buffer;
    ULONG cbBuffer;
    ULONG iMethod;
    void *reserved2[5 ];
    ULONG rpcFlags;
  } RPCOLEMESSAGE;

  typedef RPCOLEMESSAGE *PRPCOLEMESSAGE;

  EXTERN_C const IID IID_IRpcChannelBuffer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcChannelBuffer : public IUnknown {
  public:
    virtual HRESULT WINAPI GetBuffer(RPCOLEMESSAGE *pMessage,REFIID riid) = 0;
    virtual HRESULT WINAPI SendReceive(RPCOLEMESSAGE *pMessage,ULONG *pStatus) = 0;
    virtual HRESULT WINAPI FreeBuffer(RPCOLEMESSAGE *pMessage) = 0;
    virtual HRESULT WINAPI GetDestCtx(DWORD *pdwDestContext,void **ppvDestContext) = 0;
    virtual HRESULT WINAPI IsConnected(void) = 0;
  };
#else
  typedef struct IRpcChannelBufferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcChannelBuffer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcChannelBuffer *This);
      ULONG (WINAPI *Release)(IRpcChannelBuffer *This);
      HRESULT (WINAPI *GetBuffer)(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,REFIID riid);
      HRESULT (WINAPI *SendReceive)(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,ULONG *pStatus);
      HRESULT (WINAPI *FreeBuffer)(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage);
      HRESULT (WINAPI *GetDestCtx)(IRpcChannelBuffer *This,DWORD *pdwDestContext,void **ppvDestContext);
      HRESULT (WINAPI *IsConnected)(IRpcChannelBuffer *This);
    END_INTERFACE
  } IRpcChannelBufferVtbl;
  struct IRpcChannelBuffer {
    CONST_VTBL struct IRpcChannelBufferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcChannelBuffer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcChannelBuffer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcChannelBuffer_Release(This) (This)->lpVtbl->Release(This)
#define IRpcChannelBuffer_GetBuffer(This,pMessage,riid) (This)->lpVtbl->GetBuffer(This,pMessage,riid)
#define IRpcChannelBuffer_SendReceive(This,pMessage,pStatus) (This)->lpVtbl->SendReceive(This,pMessage,pStatus)
#define IRpcChannelBuffer_FreeBuffer(This,pMessage) (This)->lpVtbl->FreeBuffer(This,pMessage)
#define IRpcChannelBuffer_GetDestCtx(This,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtx(This,pdwDestContext,ppvDestContext)
#define IRpcChannelBuffer_IsConnected(This) (This)->lpVtbl->IsConnected(This)
#endif
#endif
  HRESULT WINAPI IRpcChannelBuffer_GetBuffer_Proxy(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,REFIID riid);
  void __RPC_STUB IRpcChannelBuffer_GetBuffer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer_SendReceive_Proxy(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,ULONG *pStatus);
  void __RPC_STUB IRpcChannelBuffer_SendReceive_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer_FreeBuffer_Proxy(IRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage);
  void __RPC_STUB IRpcChannelBuffer_FreeBuffer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer_GetDestCtx_Proxy(IRpcChannelBuffer *This,DWORD *pdwDestContext,void **ppvDestContext);
  void __RPC_STUB IRpcChannelBuffer_GetDestCtx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer_IsConnected_Proxy(IRpcChannelBuffer *This);
  void __RPC_STUB IRpcChannelBuffer_IsConnected_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcChannelBuffer2_INTERFACE_DEFINED__
#define __IRpcChannelBuffer2_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcChannelBuffer2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcChannelBuffer2 : public IRpcChannelBuffer {
  public:
    virtual HRESULT WINAPI GetProtocolVersion(DWORD *pdwVersion) = 0;
  };
#else
  typedef struct IRpcChannelBuffer2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcChannelBuffer2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcChannelBuffer2 *This);
      ULONG (WINAPI *Release)(IRpcChannelBuffer2 *This);
      HRESULT (WINAPI *GetBuffer)(IRpcChannelBuffer2 *This,RPCOLEMESSAGE *pMessage,REFIID riid);
      HRESULT (WINAPI *SendReceive)(IRpcChannelBuffer2 *This,RPCOLEMESSAGE *pMessage,ULONG *pStatus);
      HRESULT (WINAPI *FreeBuffer)(IRpcChannelBuffer2 *This,RPCOLEMESSAGE *pMessage);
      HRESULT (WINAPI *GetDestCtx)(IRpcChannelBuffer2 *This,DWORD *pdwDestContext,void **ppvDestContext);
      HRESULT (WINAPI *IsConnected)(IRpcChannelBuffer2 *This);
      HRESULT (WINAPI *GetProtocolVersion)(IRpcChannelBuffer2 *This,DWORD *pdwVersion);
    END_INTERFACE
  } IRpcChannelBuffer2Vtbl;
  struct IRpcChannelBuffer2 {
    CONST_VTBL struct IRpcChannelBuffer2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcChannelBuffer2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcChannelBuffer2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcChannelBuffer2_Release(This) (This)->lpVtbl->Release(This)
#define IRpcChannelBuffer2_GetBuffer(This,pMessage,riid) (This)->lpVtbl->GetBuffer(This,pMessage,riid)
#define IRpcChannelBuffer2_SendReceive(This,pMessage,pStatus) (This)->lpVtbl->SendReceive(This,pMessage,pStatus)
#define IRpcChannelBuffer2_FreeBuffer(This,pMessage) (This)->lpVtbl->FreeBuffer(This,pMessage)
#define IRpcChannelBuffer2_GetDestCtx(This,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtx(This,pdwDestContext,ppvDestContext)
#define IRpcChannelBuffer2_IsConnected(This) (This)->lpVtbl->IsConnected(This)
#define IRpcChannelBuffer2_GetProtocolVersion(This,pdwVersion) (This)->lpVtbl->GetProtocolVersion(This,pdwVersion)
#endif
#endif
  HRESULT WINAPI IRpcChannelBuffer2_GetProtocolVersion_Proxy(IRpcChannelBuffer2 *This,DWORD *pdwVersion);
  void __RPC_STUB IRpcChannelBuffer2_GetProtocolVersion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAsyncRpcChannelBuffer_INTERFACE_DEFINED__
#define __IAsyncRpcChannelBuffer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IAsyncRpcChannelBuffer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAsyncRpcChannelBuffer : public IRpcChannelBuffer2 {
  public:
    virtual HRESULT WINAPI Send(RPCOLEMESSAGE *pMsg,ISynchronize *pSync,ULONG *pulStatus) = 0;
    virtual HRESULT WINAPI Receive(RPCOLEMESSAGE *pMsg,ULONG *pulStatus) = 0;
    virtual HRESULT WINAPI GetDestCtxEx(RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext) = 0;
  };
#else
  typedef struct IAsyncRpcChannelBufferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAsyncRpcChannelBuffer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAsyncRpcChannelBuffer *This);
      ULONG (WINAPI *Release)(IAsyncRpcChannelBuffer *This);
      HRESULT (WINAPI *GetBuffer)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,REFIID riid);
      HRESULT (WINAPI *SendReceive)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage,ULONG *pStatus);
      HRESULT (WINAPI *FreeBuffer)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMessage);
      HRESULT (WINAPI *GetDestCtx)(IAsyncRpcChannelBuffer *This,DWORD *pdwDestContext,void **ppvDestContext);
      HRESULT (WINAPI *IsConnected)(IAsyncRpcChannelBuffer *This);
      HRESULT (WINAPI *GetProtocolVersion)(IAsyncRpcChannelBuffer *This,DWORD *pdwVersion);
      HRESULT (WINAPI *Send)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,ISynchronize *pSync,ULONG *pulStatus);
      HRESULT (WINAPI *Receive)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,ULONG *pulStatus);
      HRESULT (WINAPI *GetDestCtxEx)(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext);
    END_INTERFACE
  } IAsyncRpcChannelBufferVtbl;
  struct IAsyncRpcChannelBuffer {
    CONST_VTBL struct IAsyncRpcChannelBufferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAsyncRpcChannelBuffer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAsyncRpcChannelBuffer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAsyncRpcChannelBuffer_Release(This) (This)->lpVtbl->Release(This)
#define IAsyncRpcChannelBuffer_GetBuffer(This,pMessage,riid) (This)->lpVtbl->GetBuffer(This,pMessage,riid)
#define IAsyncRpcChannelBuffer_SendReceive(This,pMessage,pStatus) (This)->lpVtbl->SendReceive(This,pMessage,pStatus)
#define IAsyncRpcChannelBuffer_FreeBuffer(This,pMessage) (This)->lpVtbl->FreeBuffer(This,pMessage)
#define IAsyncRpcChannelBuffer_GetDestCtx(This,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtx(This,pdwDestContext,ppvDestContext)
#define IAsyncRpcChannelBuffer_IsConnected(This) (This)->lpVtbl->IsConnected(This)
#define IAsyncRpcChannelBuffer_GetProtocolVersion(This,pdwVersion) (This)->lpVtbl->GetProtocolVersion(This,pdwVersion)
#define IAsyncRpcChannelBuffer_Send(This,pMsg,pSync,pulStatus) (This)->lpVtbl->Send(This,pMsg,pSync,pulStatus)
#define IAsyncRpcChannelBuffer_Receive(This,pMsg,pulStatus) (This)->lpVtbl->Receive(This,pMsg,pulStatus)
#define IAsyncRpcChannelBuffer_GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)
#endif
#endif
  HRESULT WINAPI IAsyncRpcChannelBuffer_Send_Proxy(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,ISynchronize *pSync,ULONG *pulStatus);
  void __RPC_STUB IAsyncRpcChannelBuffer_Send_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAsyncRpcChannelBuffer_Receive_Proxy(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,ULONG *pulStatus);
  void __RPC_STUB IAsyncRpcChannelBuffer_Receive_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAsyncRpcChannelBuffer_GetDestCtxEx_Proxy(IAsyncRpcChannelBuffer *This,RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext);
  void __RPC_STUB IAsyncRpcChannelBuffer_GetDestCtxEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcChannelBuffer3_INTERFACE_DEFINED__
#define __IRpcChannelBuffer3_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcChannelBuffer3;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcChannelBuffer3 : public IRpcChannelBuffer2 {
  public:
    virtual HRESULT WINAPI Send(RPCOLEMESSAGE *pMsg,ULONG *pulStatus) = 0;
    virtual HRESULT WINAPI Receive(RPCOLEMESSAGE *pMsg,ULONG ulSize,ULONG *pulStatus) = 0;
    virtual HRESULT WINAPI Cancel(RPCOLEMESSAGE *pMsg) = 0;
    virtual HRESULT WINAPI GetCallContext(RPCOLEMESSAGE *pMsg,REFIID riid,void **pInterface) = 0;
    virtual HRESULT WINAPI GetDestCtxEx(RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext) = 0;
    virtual HRESULT WINAPI GetState(RPCOLEMESSAGE *pMsg,DWORD *pState) = 0;
    virtual HRESULT WINAPI RegisterAsync(RPCOLEMESSAGE *pMsg,IAsyncManager *pAsyncMgr) = 0;
  };
#else
  typedef struct IRpcChannelBuffer3Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcChannelBuffer3 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcChannelBuffer3 *This);
      ULONG (WINAPI *Release)(IRpcChannelBuffer3 *This);
      HRESULT (WINAPI *GetBuffer)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMessage,REFIID riid);
      HRESULT (WINAPI *SendReceive)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMessage,ULONG *pStatus);
      HRESULT (WINAPI *FreeBuffer)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMessage);
      HRESULT (WINAPI *GetDestCtx)(IRpcChannelBuffer3 *This,DWORD *pdwDestContext,void **ppvDestContext);
      HRESULT (WINAPI *IsConnected)(IRpcChannelBuffer3 *This);
      HRESULT (WINAPI *GetProtocolVersion)(IRpcChannelBuffer3 *This,DWORD *pdwVersion);
      HRESULT (WINAPI *Send)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,ULONG *pulStatus);
      HRESULT (WINAPI *Receive)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,ULONG ulSize,ULONG *pulStatus);
      HRESULT (WINAPI *Cancel)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg);
      HRESULT (WINAPI *GetCallContext)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,REFIID riid,void **pInterface);
      HRESULT (WINAPI *GetDestCtxEx)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext);
      HRESULT (WINAPI *GetState)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,DWORD *pState);
      HRESULT (WINAPI *RegisterAsync)(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,IAsyncManager *pAsyncMgr);
    END_INTERFACE
  } IRpcChannelBuffer3Vtbl;
  struct IRpcChannelBuffer3 {
    CONST_VTBL struct IRpcChannelBuffer3Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcChannelBuffer3_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcChannelBuffer3_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcChannelBuffer3_Release(This) (This)->lpVtbl->Release(This)
#define IRpcChannelBuffer3_GetBuffer(This,pMessage,riid) (This)->lpVtbl->GetBuffer(This,pMessage,riid)
#define IRpcChannelBuffer3_SendReceive(This,pMessage,pStatus) (This)->lpVtbl->SendReceive(This,pMessage,pStatus)
#define IRpcChannelBuffer3_FreeBuffer(This,pMessage) (This)->lpVtbl->FreeBuffer(This,pMessage)
#define IRpcChannelBuffer3_GetDestCtx(This,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtx(This,pdwDestContext,ppvDestContext)
#define IRpcChannelBuffer3_IsConnected(This) (This)->lpVtbl->IsConnected(This)
#define IRpcChannelBuffer3_GetProtocolVersion(This,pdwVersion) (This)->lpVtbl->GetProtocolVersion(This,pdwVersion)
#define IRpcChannelBuffer3_Send(This,pMsg,pulStatus) (This)->lpVtbl->Send(This,pMsg,pulStatus)
#define IRpcChannelBuffer3_Receive(This,pMsg,ulSize,pulStatus) (This)->lpVtbl->Receive(This,pMsg,ulSize,pulStatus)
#define IRpcChannelBuffer3_Cancel(This,pMsg) (This)->lpVtbl->Cancel(This,pMsg)
#define IRpcChannelBuffer3_GetCallContext(This,pMsg,riid,pInterface) (This)->lpVtbl->GetCallContext(This,pMsg,riid,pInterface)
#define IRpcChannelBuffer3_GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext) (This)->lpVtbl->GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)
#define IRpcChannelBuffer3_GetState(This,pMsg,pState) (This)->lpVtbl->GetState(This,pMsg,pState)
#define IRpcChannelBuffer3_RegisterAsync(This,pMsg,pAsyncMgr) (This)->lpVtbl->RegisterAsync(This,pMsg,pAsyncMgr)
#endif
#endif
  HRESULT WINAPI IRpcChannelBuffer3_Send_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,ULONG *pulStatus);
  void __RPC_STUB IRpcChannelBuffer3_Send_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_Receive_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,ULONG ulSize,ULONG *pulStatus);
  void __RPC_STUB IRpcChannelBuffer3_Receive_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_Cancel_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg);
  void __RPC_STUB IRpcChannelBuffer3_Cancel_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_GetCallContext_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,REFIID riid,void **pInterface);
  void __RPC_STUB IRpcChannelBuffer3_GetCallContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_GetDestCtxEx_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,DWORD *pdwDestContext,void **ppvDestContext);
  void __RPC_STUB IRpcChannelBuffer3_GetDestCtxEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_GetState_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,DWORD *pState);
  void __RPC_STUB IRpcChannelBuffer3_GetState_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcChannelBuffer3_RegisterAsync_Proxy(IRpcChannelBuffer3 *This,RPCOLEMESSAGE *pMsg,IAsyncManager *pAsyncMgr);
  void __RPC_STUB IRpcChannelBuffer3_RegisterAsync_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcSyntaxNegotiate_INTERFACE_DEFINED__
#define __IRpcSyntaxNegotiate_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcSyntaxNegotiate;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcSyntaxNegotiate : public IUnknown {
  public:
    virtual HRESULT WINAPI NegotiateSyntax(RPCOLEMESSAGE *pMsg) = 0;
  };
#else
  typedef struct IRpcSyntaxNegotiateVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcSyntaxNegotiate *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcSyntaxNegotiate *This);
      ULONG (WINAPI *Release)(IRpcSyntaxNegotiate *This);
      HRESULT (WINAPI *NegotiateSyntax)(IRpcSyntaxNegotiate *This,RPCOLEMESSAGE *pMsg);
    END_INTERFACE
  } IRpcSyntaxNegotiateVtbl;
  struct IRpcSyntaxNegotiate {
    CONST_VTBL struct IRpcSyntaxNegotiateVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcSyntaxNegotiate_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcSyntaxNegotiate_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcSyntaxNegotiate_Release(This) (This)->lpVtbl->Release(This)
#define IRpcSyntaxNegotiate_NegotiateSyntax(This,pMsg) (This)->lpVtbl->NegotiateSyntax(This,pMsg)
#endif
#endif
  HRESULT WINAPI IRpcSyntaxNegotiate_NegotiateSyntax_Proxy(IRpcSyntaxNegotiate *This,RPCOLEMESSAGE *pMsg);
  void __RPC_STUB IRpcSyntaxNegotiate_NegotiateSyntax_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcProxyBuffer_INTERFACE_DEFINED__
#define __IRpcProxyBuffer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcProxyBuffer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcProxyBuffer : public IUnknown {
  public:
    virtual HRESULT WINAPI Connect(IRpcChannelBuffer *pRpcChannelBuffer) = 0;
    virtual void WINAPI Disconnect(void) = 0;
  };
#else
  typedef struct IRpcProxyBufferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcProxyBuffer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcProxyBuffer *This);
      ULONG (WINAPI *Release)(IRpcProxyBuffer *This);
      HRESULT (WINAPI *Connect)(IRpcProxyBuffer *This,IRpcChannelBuffer *pRpcChannelBuffer);
      void (WINAPI *Disconnect)(IRpcProxyBuffer *This);
    END_INTERFACE
  } IRpcProxyBufferVtbl;
  struct IRpcProxyBuffer {
    CONST_VTBL struct IRpcProxyBufferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcProxyBuffer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcProxyBuffer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcProxyBuffer_Release(This) (This)->lpVtbl->Release(This)
#define IRpcProxyBuffer_Connect(This,pRpcChannelBuffer) (This)->lpVtbl->Connect(This,pRpcChannelBuffer)
#define IRpcProxyBuffer_Disconnect(This) (This)->lpVtbl->Disconnect(This)
#endif
#endif
  HRESULT WINAPI IRpcProxyBuffer_Connect_Proxy(IRpcProxyBuffer *This,IRpcChannelBuffer *pRpcChannelBuffer);
  void __RPC_STUB IRpcProxyBuffer_Connect_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IRpcProxyBuffer_Disconnect_Proxy(IRpcProxyBuffer *This);
  void __RPC_STUB IRpcProxyBuffer_Disconnect_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcStubBuffer_INTERFACE_DEFINED__
#define __IRpcStubBuffer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcStubBuffer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcStubBuffer : public IUnknown {
  public:
    virtual HRESULT WINAPI Connect(IUnknown *pUnkServer) = 0;
    virtual void WINAPI Disconnect(void) = 0;
    virtual HRESULT WINAPI Invoke(RPCOLEMESSAGE *_prpcmsg,IRpcChannelBuffer *_pRpcChannelBuffer) = 0;
    virtual IRpcStubBuffer *WINAPI IsIIDSupported(REFIID riid) = 0;
    virtual ULONG WINAPI CountRefs(void) = 0;
    virtual HRESULT WINAPI DebugServerQueryInterface(void **ppv) = 0;
    virtual void WINAPI DebugServerRelease(void *pv) = 0;
  };
#else
  typedef struct IRpcStubBufferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcStubBuffer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcStubBuffer *This);
      ULONG (WINAPI *Release)(IRpcStubBuffer *This);
      HRESULT (WINAPI *Connect)(IRpcStubBuffer *This,IUnknown *pUnkServer);
      void (WINAPI *Disconnect)(IRpcStubBuffer *This);
      HRESULT (WINAPI *Invoke)(IRpcStubBuffer *This,RPCOLEMESSAGE *_prpcmsg,IRpcChannelBuffer *_pRpcChannelBuffer);
      IRpcStubBuffer *(WINAPI *IsIIDSupported)(IRpcStubBuffer *This,REFIID riid);
      ULONG (WINAPI *CountRefs)(IRpcStubBuffer *This);
      HRESULT (WINAPI *DebugServerQueryInterface)(IRpcStubBuffer *This,void **ppv);
      void (WINAPI *DebugServerRelease)(IRpcStubBuffer *This,void *pv);
    END_INTERFACE
  } IRpcStubBufferVtbl;
  struct IRpcStubBuffer {
    CONST_VTBL struct IRpcStubBufferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcStubBuffer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcStubBuffer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcStubBuffer_Release(This) (This)->lpVtbl->Release(This)
#define IRpcStubBuffer_Connect(This,pUnkServer) (This)->lpVtbl->Connect(This,pUnkServer)
#define IRpcStubBuffer_Disconnect(This) (This)->lpVtbl->Disconnect(This)
#define IRpcStubBuffer_Invoke(This,_prpcmsg,_pRpcChannelBuffer) (This)->lpVtbl->Invoke(This,_prpcmsg,_pRpcChannelBuffer)
#define IRpcStubBuffer_IsIIDSupported(This,riid) (This)->lpVtbl->IsIIDSupported(This,riid)
#define IRpcStubBuffer_CountRefs(This) (This)->lpVtbl->CountRefs(This)
#define IRpcStubBuffer_DebugServerQueryInterface(This,ppv) (This)->lpVtbl->DebugServerQueryInterface(This,ppv)
#define IRpcStubBuffer_DebugServerRelease(This,pv) (This)->lpVtbl->DebugServerRelease(This,pv)
#endif
#endif
  HRESULT WINAPI IRpcStubBuffer_Connect_Proxy(IRpcStubBuffer *This,IUnknown *pUnkServer);
  void __RPC_STUB IRpcStubBuffer_Connect_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IRpcStubBuffer_Disconnect_Proxy(IRpcStubBuffer *This);
  void __RPC_STUB IRpcStubBuffer_Disconnect_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcStubBuffer_Invoke_Proxy(IRpcStubBuffer *This,RPCOLEMESSAGE *_prpcmsg,IRpcChannelBuffer *_pRpcChannelBuffer);
  void __RPC_STUB IRpcStubBuffer_Invoke_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  IRpcStubBuffer *WINAPI IRpcStubBuffer_IsIIDSupported_Proxy(IRpcStubBuffer *This,REFIID riid);
  void __RPC_STUB IRpcStubBuffer_IsIIDSupported_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IRpcStubBuffer_CountRefs_Proxy(IRpcStubBuffer *This);
  void __RPC_STUB IRpcStubBuffer_CountRefs_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcStubBuffer_DebugServerQueryInterface_Proxy(IRpcStubBuffer *This,void **ppv);
  void __RPC_STUB IRpcStubBuffer_DebugServerQueryInterface_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IRpcStubBuffer_DebugServerRelease_Proxy(IRpcStubBuffer *This,void *pv);
  void __RPC_STUB IRpcStubBuffer_DebugServerRelease_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPSFactoryBuffer_INTERFACE_DEFINED__
#define __IPSFactoryBuffer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IPSFactoryBuffer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPSFactoryBuffer : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateProxy(IUnknown *pUnkOuter,REFIID riid,IRpcProxyBuffer **ppProxy,void **ppv) = 0;
    virtual HRESULT WINAPI CreateStub(REFIID riid,IUnknown *pUnkServer,IRpcStubBuffer **ppStub) = 0;
  };
#else
  typedef struct IPSFactoryBufferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPSFactoryBuffer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPSFactoryBuffer *This);
      ULONG (WINAPI *Release)(IPSFactoryBuffer *This);
      HRESULT (WINAPI *CreateProxy)(IPSFactoryBuffer *This,IUnknown *pUnkOuter,REFIID riid,IRpcProxyBuffer **ppProxy,void **ppv);
      HRESULT (WINAPI *CreateStub)(IPSFactoryBuffer *This,REFIID riid,IUnknown *pUnkServer,IRpcStubBuffer **ppStub);
    END_INTERFACE
  } IPSFactoryBufferVtbl;
  struct IPSFactoryBuffer {
    CONST_VTBL struct IPSFactoryBufferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPSFactoryBuffer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPSFactoryBuffer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPSFactoryBuffer_Release(This) (This)->lpVtbl->Release(This)
#define IPSFactoryBuffer_CreateProxy(This,pUnkOuter,riid,ppProxy,ppv) (This)->lpVtbl->CreateProxy(This,pUnkOuter,riid,ppProxy,ppv)
#define IPSFactoryBuffer_CreateStub(This,riid,pUnkServer,ppStub) (This)->lpVtbl->CreateStub(This,riid,pUnkServer,ppStub)
#endif
#endif
  HRESULT WINAPI IPSFactoryBuffer_CreateProxy_Proxy(IPSFactoryBuffer *This,IUnknown *pUnkOuter,REFIID riid,IRpcProxyBuffer **ppProxy,void **ppv);
  void __RPC_STUB IPSFactoryBuffer_CreateProxy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPSFactoryBuffer_CreateStub_Proxy(IPSFactoryBuffer *This,REFIID riid,IUnknown *pUnkServer,IRpcStubBuffer **ppStub);
  void __RPC_STUB IPSFactoryBuffer_CreateStub_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  typedef struct SChannelHookCallInfo {
    IID iid;
    DWORD cbSize;
    GUID uCausality;
    DWORD dwServerPid;
    DWORD iMethod;
    void *pObject;
  } SChannelHookCallInfo;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0050_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0050_v0_0_s_ifspec;

#ifndef __IChannelHook_INTERFACE_DEFINED__
#define __IChannelHook_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IChannelHook;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IChannelHook : public IUnknown {
  public:
    virtual void WINAPI ClientGetSize(REFGUID uExtent,REFIID riid,ULONG *pDataSize) = 0;
    virtual void WINAPI ClientFillBuffer(REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer) = 0;
    virtual void WINAPI ClientNotify(REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep,HRESULT hrFault) = 0;
    virtual void WINAPI ServerNotify(REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep) = 0;
    virtual void WINAPI ServerGetSize(REFGUID uExtent,REFIID riid,HRESULT hrFault,ULONG *pDataSize) = 0;
    virtual void WINAPI ServerFillBuffer(REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer,HRESULT hrFault) = 0;
  };
#else
  typedef struct IChannelHookVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IChannelHook *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IChannelHook *This);
      ULONG (WINAPI *Release)(IChannelHook *This);
      void (WINAPI *ClientGetSize)(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize);
      void (WINAPI *ClientFillBuffer)(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer);
      void (WINAPI *ClientNotify)(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep,HRESULT hrFault);
      void (WINAPI *ServerNotify)(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep);
      void (WINAPI *ServerGetSize)(IChannelHook *This,REFGUID uExtent,REFIID riid,HRESULT hrFault,ULONG *pDataSize);
      void (WINAPI *ServerFillBuffer)(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer,HRESULT hrFault);
    END_INTERFACE
  } IChannelHookVtbl;
  struct IChannelHook {
    CONST_VTBL struct IChannelHookVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IChannelHook_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IChannelHook_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IChannelHook_Release(This) (This)->lpVtbl->Release(This)
#define IChannelHook_ClientGetSize(This,uExtent,riid,pDataSize) (This)->lpVtbl->ClientGetSize(This,uExtent,riid,pDataSize)
#define IChannelHook_ClientFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer) (This)->lpVtbl->ClientFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer)
#define IChannelHook_ClientNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep,hrFault) (This)->lpVtbl->ClientNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep,hrFault)
#define IChannelHook_ServerNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep) (This)->lpVtbl->ServerNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep)
#define IChannelHook_ServerGetSize(This,uExtent,riid,hrFault,pDataSize) (This)->lpVtbl->ServerGetSize(This,uExtent,riid,hrFault,pDataSize)
#define IChannelHook_ServerFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer,hrFault) (This)->lpVtbl->ServerFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer,hrFault)
#endif
#endif
  void WINAPI IChannelHook_ClientGetSize_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize);
  void __RPC_STUB IChannelHook_ClientGetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IChannelHook_ClientFillBuffer_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer);
  void __RPC_STUB IChannelHook_ClientFillBuffer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IChannelHook_ClientNotify_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep,HRESULT hrFault);
  void __RPC_STUB IChannelHook_ClientNotify_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IChannelHook_ServerNotify_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG cbDataSize,void *pDataBuffer,DWORD lDataRep);
  void __RPC_STUB IChannelHook_ServerNotify_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IChannelHook_ServerGetSize_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,HRESULT hrFault,ULONG *pDataSize);
  void __RPC_STUB IChannelHook_ServerGetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IChannelHook_ServerFillBuffer_Proxy(IChannelHook *This,REFGUID uExtent,REFIID riid,ULONG *pDataSize,void *pDataBuffer,HRESULT hrFault);
  void __RPC_STUB IChannelHook_ServerFillBuffer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  extern const FMTID FMTID_SummaryInformation;
  extern const FMTID FMTID_DocSummaryInformation;
  extern const FMTID FMTID_UserDefinedProperties;
  extern const FMTID FMTID_DiscardableInformation;
  extern const FMTID FMTID_ImageSummaryInformation;
  extern const FMTID FMTID_AudioSummaryInformation;
  extern const FMTID FMTID_VideoSummaryInformation;
  extern const FMTID FMTID_MediaFileSummaryInformation;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0051_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0051_v0_0_s_ifspec;
#ifndef __IClientSecurity_INTERFACE_DEFINED__
#define __IClientSecurity_INTERFACE_DEFINED__
  typedef struct tagSOLE_AUTHENTICATION_SERVICE {
    DWORD dwAuthnSvc;
    DWORD dwAuthzSvc;
    OLECHAR *pPrincipalName;
    HRESULT hr;
  } SOLE_AUTHENTICATION_SERVICE;

  typedef SOLE_AUTHENTICATION_SERVICE *PSOLE_AUTHENTICATION_SERVICE;

  typedef enum tagEOLE_AUTHENTICATION_CAPABILITIES {
    EOAC_NONE = 0,EOAC_MUTUAL_AUTH = 0x1,EOAC_STATIC_CLOAKING = 0x20,EOAC_DYNAMIC_CLOAKING = 0x40,EOAC_ANY_AUTHORITY = 0x80,EOAC_MAKE_FULLSIC = 0x100,
    EOAC_DEFAULT = 0x800,EOAC_SECURE_REFS = 0x2,EOAC_ACCESS_CONTROL = 0x4,EOAC_APPID = 0x8,EOAC_DYNAMIC = 0x10,EOAC_REQUIRE_FULLSIC = 0x200,
    EOAC_AUTO_IMPERSONATE = 0x400,EOAC_NO_CUSTOM_MARSHAL = 0x2000,EOAC_DISABLE_AAA = 0x1000
  } EOLE_AUTHENTICATION_CAPABILITIES;

#define COLE_DEFAULT_PRINCIPAL ((OLECHAR *)-1)
#define COLE_DEFAULT_AUTHINFO ((void *)-1)

  typedef struct tagSOLE_AUTHENTICATION_INFO {
    DWORD dwAuthnSvc;
    DWORD dwAuthzSvc;
    void *pAuthInfo;
  } SOLE_AUTHENTICATION_INFO;

  typedef struct tagSOLE_AUTHENTICATION_INFO *PSOLE_AUTHENTICATION_INFO;

  typedef struct tagSOLE_AUTHENTICATION_LIST {
    DWORD cAuthInfo;
    SOLE_AUTHENTICATION_INFO *aAuthInfo;
  } SOLE_AUTHENTICATION_LIST;

  typedef struct tagSOLE_AUTHENTICATION_LIST *PSOLE_AUTHENTICATION_LIST;

  EXTERN_C const IID IID_IClientSecurity;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IClientSecurity : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryBlanket(IUnknown *pProxy,DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pAuthInfo,DWORD *pCapabilites) = 0;
    virtual HRESULT WINAPI SetBlanket(IUnknown *pProxy,DWORD dwAuthnSvc,DWORD dwAuthzSvc,OLECHAR *pServerPrincName,DWORD dwAuthnLevel,DWORD dwImpLevel,void *pAuthInfo,DWORD dwCapabilities) = 0;
    virtual HRESULT WINAPI CopyProxy(IUnknown *pProxy,IUnknown **ppCopy) = 0;
  };
#else
  typedef struct IClientSecurityVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IClientSecurity *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IClientSecurity *This);
      ULONG (WINAPI *Release)(IClientSecurity *This);
      HRESULT (WINAPI *QueryBlanket)(IClientSecurity *This,IUnknown *pProxy,DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pAuthInfo,DWORD *pCapabilites);
      HRESULT (WINAPI *SetBlanket)(IClientSecurity *This,IUnknown *pProxy,DWORD dwAuthnSvc,DWORD dwAuthzSvc,OLECHAR *pServerPrincName,DWORD dwAuthnLevel,DWORD dwImpLevel,void *pAuthInfo,DWORD dwCapabilities);
      HRESULT (WINAPI *CopyProxy)(IClientSecurity *This,IUnknown *pProxy,IUnknown **ppCopy);
    END_INTERFACE
  } IClientSecurityVtbl;
  struct IClientSecurity {
    CONST_VTBL struct IClientSecurityVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IClientSecurity_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IClientSecurity_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IClientSecurity_Release(This) (This)->lpVtbl->Release(This)
#define IClientSecurity_QueryBlanket(This,pProxy,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pAuthInfo,pCapabilites) (This)->lpVtbl->QueryBlanket(This,pProxy,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pAuthInfo,pCapabilites)
#define IClientSecurity_SetBlanket(This,pProxy,dwAuthnSvc,dwAuthzSvc,pServerPrincName,dwAuthnLevel,dwImpLevel,pAuthInfo,dwCapabilities) (This)->lpVtbl->SetBlanket(This,pProxy,dwAuthnSvc,dwAuthzSvc,pServerPrincName,dwAuthnLevel,dwImpLevel,pAuthInfo,dwCapabilities)
#define IClientSecurity_CopyProxy(This,pProxy,ppCopy) (This)->lpVtbl->CopyProxy(This,pProxy,ppCopy)
#endif
#endif
  HRESULT WINAPI IClientSecurity_QueryBlanket_Proxy(IClientSecurity *This,IUnknown *pProxy,DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pAuthInfo,DWORD *pCapabilites);
  void __RPC_STUB IClientSecurity_QueryBlanket_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IClientSecurity_SetBlanket_Proxy(IClientSecurity *This,IUnknown *pProxy,DWORD dwAuthnSvc,DWORD dwAuthzSvc,OLECHAR *pServerPrincName,DWORD dwAuthnLevel,DWORD dwImpLevel,void *pAuthInfo,DWORD dwCapabilities);
  void __RPC_STUB IClientSecurity_SetBlanket_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IClientSecurity_CopyProxy_Proxy(IClientSecurity *This,IUnknown *pProxy,IUnknown **ppCopy);
  void __RPC_STUB IClientSecurity_CopyProxy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IServerSecurity_INTERFACE_DEFINED__
#define __IServerSecurity_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IServerSecurity;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IServerSecurity : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryBlanket(DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pPrivs,DWORD *pCapabilities) = 0;
    virtual HRESULT WINAPI ImpersonateClient(void) = 0;
    virtual HRESULT WINAPI RevertToSelf(void) = 0;
    virtual WINBOOL WINAPI IsImpersonating(void) = 0;
  };
#else
  typedef struct IServerSecurityVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IServerSecurity *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IServerSecurity *This);
      ULONG (WINAPI *Release)(IServerSecurity *This);
      HRESULT (WINAPI *QueryBlanket)(IServerSecurity *This,DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pPrivs,DWORD *pCapabilities);
      HRESULT (WINAPI *ImpersonateClient)(IServerSecurity *This);
      HRESULT (WINAPI *RevertToSelf)(IServerSecurity *This);
      WINBOOL (WINAPI *IsImpersonating)(IServerSecurity *This);
    END_INTERFACE
  } IServerSecurityVtbl;
  struct IServerSecurity {
    CONST_VTBL struct IServerSecurityVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IServerSecurity_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IServerSecurity_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IServerSecurity_Release(This) (This)->lpVtbl->Release(This)
#define IServerSecurity_QueryBlanket(This,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pPrivs,pCapabilities) (This)->lpVtbl->QueryBlanket(This,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pPrivs,pCapabilities)
#define IServerSecurity_ImpersonateClient(This) (This)->lpVtbl->ImpersonateClient(This)
#define IServerSecurity_RevertToSelf(This) (This)->lpVtbl->RevertToSelf(This)
#define IServerSecurity_IsImpersonating(This) (This)->lpVtbl->IsImpersonating(This)
#endif
#endif
  HRESULT WINAPI IServerSecurity_QueryBlanket_Proxy(IServerSecurity *This,DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,void **pPrivs,DWORD *pCapabilities);
  void __RPC_STUB IServerSecurity_QueryBlanket_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IServerSecurity_ImpersonateClient_Proxy(IServerSecurity *This);
  void __RPC_STUB IServerSecurity_ImpersonateClient_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IServerSecurity_RevertToSelf_Proxy(IServerSecurity *This);
  void __RPC_STUB IServerSecurity_RevertToSelf_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  WINBOOL WINAPI IServerSecurity_IsImpersonating_Proxy(IServerSecurity *This);
  void __RPC_STUB IServerSecurity_IsImpersonating_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IClassActivator_INTERFACE_DEFINED__
#define __IClassActivator_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IClassActivator;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IClassActivator : public IUnknown {
  public:
    virtual HRESULT WINAPI GetClassObject(REFCLSID rclsid,DWORD dwClassContext,LCID locale,REFIID riid,void **ppv) = 0;
  };
#else
  typedef struct IClassActivatorVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IClassActivator *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IClassActivator *This);
      ULONG (WINAPI *Release)(IClassActivator *This);
      HRESULT (WINAPI *GetClassObject)(IClassActivator *This,REFCLSID rclsid,DWORD dwClassContext,LCID locale,REFIID riid,void **ppv);
    END_INTERFACE
  } IClassActivatorVtbl;
  struct IClassActivator {
    CONST_VTBL struct IClassActivatorVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IClassActivator_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IClassActivator_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IClassActivator_Release(This) (This)->lpVtbl->Release(This)
#define IClassActivator_GetClassObject(This,rclsid,dwClassContext,locale,riid,ppv) (This)->lpVtbl->GetClassObject(This,rclsid,dwClassContext,locale,riid,ppv)
#endif
#endif
  HRESULT WINAPI IClassActivator_GetClassObject_Proxy(IClassActivator *This,REFCLSID rclsid,DWORD dwClassContext,LCID locale,REFIID riid,void **ppv);
  void __RPC_STUB IClassActivator_GetClassObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcOptions_INTERFACE_DEFINED__
#define __IRpcOptions_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcOptions;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcOptions : public IUnknown {
  public:
    virtual HRESULT WINAPI Set(IUnknown *pPrx,DWORD dwProperty,ULONG_PTR dwValue) = 0;
    virtual HRESULT WINAPI Query(IUnknown *pPrx,DWORD dwProperty,ULONG_PTR *pdwValue) = 0;
  };
#else
  typedef struct IRpcOptionsVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcOptions *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcOptions *This);
      ULONG (WINAPI *Release)(IRpcOptions *This);
      HRESULT (WINAPI *Set)(IRpcOptions *This,IUnknown *pPrx,DWORD dwProperty,ULONG_PTR dwValue);
      HRESULT (WINAPI *Query)(IRpcOptions *This,IUnknown *pPrx,DWORD dwProperty,ULONG_PTR *pdwValue);
    END_INTERFACE
  } IRpcOptionsVtbl;
  struct IRpcOptions {
    CONST_VTBL struct IRpcOptionsVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcOptions_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcOptions_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcOptions_Release(This) (This)->lpVtbl->Release(This)
#define IRpcOptions_Set(This,pPrx,dwProperty,dwValue) (This)->lpVtbl->Set(This,pPrx,dwProperty,dwValue)
#define IRpcOptions_Query(This,pPrx,dwProperty,pdwValue) (This)->lpVtbl->Query(This,pPrx,dwProperty,pdwValue)
#endif
#endif
  HRESULT WINAPI IRpcOptions_Set_Proxy(IRpcOptions *This,IUnknown *pPrx,DWORD dwProperty,ULONG_PTR dwValue);
  void __RPC_STUB IRpcOptions_Set_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcOptions_Query_Proxy(IRpcOptions *This,IUnknown *pPrx,DWORD dwProperty,ULONG_PTR *pdwValue);
  void __RPC_STUB IRpcOptions_Query_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  enum __MIDL___MIDL_itf_objidl_0055_0001 {
    COMBND_RPCTIMEOUT = 0x1,COMBND_SERVER_LOCALITY = 0x2
  };

  enum __MIDL___MIDL_itf_objidl_0055_0002 {
    SERVER_LOCALITY_PROCESS_LOCAL = 0,SERVER_LOCALITY_MACHINE_LOCAL = 1,SERVER_LOCALITY_REMOTE = 2
  };
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0055_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0055_v0_0_s_ifspec;

#ifndef __IFillLockBytes_INTERFACE_DEFINED__
#define __IFillLockBytes_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IFillLockBytes;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IFillLockBytes : public IUnknown {
  public:
    virtual HRESULT WINAPI FillAppend(const void *pv,ULONG cb,ULONG *pcbWritten) = 0;
    virtual HRESULT WINAPI FillAt(ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten) = 0;
    virtual HRESULT WINAPI SetFillSize(ULARGE_INTEGER ulSize) = 0;
    virtual HRESULT WINAPI Terminate(WINBOOL bCanceled) = 0;
  };
#else
  typedef struct IFillLockBytesVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IFillLockBytes *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IFillLockBytes *This);
      ULONG (WINAPI *Release)(IFillLockBytes *This);
      HRESULT (WINAPI *FillAppend)(IFillLockBytes *This,const void *pv,ULONG cb,ULONG *pcbWritten);
      HRESULT (WINAPI *FillAt)(IFillLockBytes *This,ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten);
      HRESULT (WINAPI *SetFillSize)(IFillLockBytes *This,ULARGE_INTEGER ulSize);
      HRESULT (WINAPI *Terminate)(IFillLockBytes *This,WINBOOL bCanceled);
    END_INTERFACE
  } IFillLockBytesVtbl;
  struct IFillLockBytes {
    CONST_VTBL struct IFillLockBytesVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IFillLockBytes_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IFillLockBytes_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IFillLockBytes_Release(This) (This)->lpVtbl->Release(This)
#define IFillLockBytes_FillAppend(This,pv,cb,pcbWritten) (This)->lpVtbl->FillAppend(This,pv,cb,pcbWritten)
#define IFillLockBytes_FillAt(This,ulOffset,pv,cb,pcbWritten) (This)->lpVtbl->FillAt(This,ulOffset,pv,cb,pcbWritten)
#define IFillLockBytes_SetFillSize(This,ulSize) (This)->lpVtbl->SetFillSize(This,ulSize)
#define IFillLockBytes_Terminate(This,bCanceled) (This)->lpVtbl->Terminate(This,bCanceled)
#endif
#endif
  HRESULT WINAPI IFillLockBytes_RemoteFillAppend_Proxy(IFillLockBytes *This,const byte *pv,ULONG cb,ULONG *pcbWritten);
  void __RPC_STUB IFillLockBytes_RemoteFillAppend_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IFillLockBytes_RemoteFillAt_Proxy(IFillLockBytes *This,ULARGE_INTEGER ulOffset,const byte *pv,ULONG cb,ULONG *pcbWritten);
  void __RPC_STUB IFillLockBytes_RemoteFillAt_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IFillLockBytes_SetFillSize_Proxy(IFillLockBytes *This,ULARGE_INTEGER ulSize);
  void __RPC_STUB IFillLockBytes_SetFillSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IFillLockBytes_Terminate_Proxy(IFillLockBytes *This,WINBOOL bCanceled);
  void __RPC_STUB IFillLockBytes_Terminate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IProgressNotify_INTERFACE_DEFINED__
#define __IProgressNotify_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IProgressNotify;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IProgressNotify : public IUnknown {
  public:
    virtual HRESULT WINAPI OnProgress(DWORD dwProgressCurrent,DWORD dwProgressMaximum,WINBOOL fAccurate,WINBOOL fOwner) = 0;
  };
#else
  typedef struct IProgressNotifyVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IProgressNotify *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IProgressNotify *This);
      ULONG (WINAPI *Release)(IProgressNotify *This);
      HRESULT (WINAPI *OnProgress)(IProgressNotify *This,DWORD dwProgressCurrent,DWORD dwProgressMaximum,WINBOOL fAccurate,WINBOOL fOwner);
    END_INTERFACE
  } IProgressNotifyVtbl;
  struct IProgressNotify {
    CONST_VTBL struct IProgressNotifyVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IProgressNotify_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IProgressNotify_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IProgressNotify_Release(This) (This)->lpVtbl->Release(This)
#define IProgressNotify_OnProgress(This,dwProgressCurrent,dwProgressMaximum,fAccurate,fOwner) (This)->lpVtbl->OnProgress(This,dwProgressCurrent,dwProgressMaximum,fAccurate,fOwner)
#endif
#endif
  HRESULT WINAPI IProgressNotify_OnProgress_Proxy(IProgressNotify *This,DWORD dwProgressCurrent,DWORD dwProgressMaximum,WINBOOL fAccurate,WINBOOL fOwner);
  void __RPC_STUB IProgressNotify_OnProgress_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ILayoutStorage_INTERFACE_DEFINED__
#define __ILayoutStorage_INTERFACE_DEFINED__
  typedef struct tagStorageLayout {
    DWORD LayoutType;
    OLECHAR *pwcsElementName;
    LARGE_INTEGER cOffset;
    LARGE_INTEGER cBytes;
  } StorageLayout;

  EXTERN_C const IID IID_ILayoutStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ILayoutStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI LayoutScript(StorageLayout *pStorageLayout,DWORD nEntries,DWORD glfInterleavedFlag) = 0;
    virtual HRESULT WINAPI BeginMonitor(void) = 0;
    virtual HRESULT WINAPI EndMonitor(void) = 0;
    virtual HRESULT WINAPI ReLayoutDocfile(OLECHAR *pwcsNewDfName) = 0;
    virtual HRESULT WINAPI ReLayoutDocfileOnILockBytes(ILockBytes *pILockBytes) = 0;
  };
#else
  typedef struct ILayoutStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ILayoutStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ILayoutStorage *This);
      ULONG (WINAPI *Release)(ILayoutStorage *This);
      HRESULT (WINAPI *LayoutScript)(ILayoutStorage *This,StorageLayout *pStorageLayout,DWORD nEntries,DWORD glfInterleavedFlag);
      HRESULT (WINAPI *BeginMonitor)(ILayoutStorage *This);
      HRESULT (WINAPI *EndMonitor)(ILayoutStorage *This);
      HRESULT (WINAPI *ReLayoutDocfile)(ILayoutStorage *This,OLECHAR *pwcsNewDfName);
      HRESULT (WINAPI *ReLayoutDocfileOnILockBytes)(ILayoutStorage *This,ILockBytes *pILockBytes);
    END_INTERFACE
  } ILayoutStorageVtbl;
  struct ILayoutStorage {
    CONST_VTBL struct ILayoutStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ILayoutStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ILayoutStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ILayoutStorage_Release(This) (This)->lpVtbl->Release(This)
#define ILayoutStorage_LayoutScript(This,pStorageLayout,nEntries,glfInterleavedFlag) (This)->lpVtbl->LayoutScript(This,pStorageLayout,nEntries,glfInterleavedFlag)
#define ILayoutStorage_BeginMonitor(This) (This)->lpVtbl->BeginMonitor(This)
#define ILayoutStorage_EndMonitor(This) (This)->lpVtbl->EndMonitor(This)
#define ILayoutStorage_ReLayoutDocfile(This,pwcsNewDfName) (This)->lpVtbl->ReLayoutDocfile(This,pwcsNewDfName)
#define ILayoutStorage_ReLayoutDocfileOnILockBytes(This,pILockBytes) (This)->lpVtbl->ReLayoutDocfileOnILockBytes(This,pILockBytes)
#endif
#endif
  HRESULT WINAPI ILayoutStorage_LayoutScript_Proxy(ILayoutStorage *This,StorageLayout *pStorageLayout,DWORD nEntries,DWORD glfInterleavedFlag);
  void __RPC_STUB ILayoutStorage_LayoutScript_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILayoutStorage_BeginMonitor_Proxy(ILayoutStorage *This);
  void __RPC_STUB ILayoutStorage_BeginMonitor_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILayoutStorage_EndMonitor_Proxy(ILayoutStorage *This);
  void __RPC_STUB ILayoutStorage_EndMonitor_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILayoutStorage_ReLayoutDocfile_Proxy(ILayoutStorage *This,OLECHAR *pwcsNewDfName);
  void __RPC_STUB ILayoutStorage_ReLayoutDocfile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ILayoutStorage_ReLayoutDocfileOnILockBytes_Proxy(ILayoutStorage *This,ILockBytes *pILockBytes);
  void __RPC_STUB ILayoutStorage_ReLayoutDocfileOnILockBytes_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IBlockingLock_INTERFACE_DEFINED__
#define __IBlockingLock_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IBlockingLock;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBlockingLock : public IUnknown {
  public:
    virtual HRESULT WINAPI Lock(DWORD dwTimeout) = 0;
    virtual HRESULT WINAPI Unlock(void) = 0;
  };
#else
  typedef struct IBlockingLockVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBlockingLock *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBlockingLock *This);
      ULONG (WINAPI *Release)(IBlockingLock *This);
      HRESULT (WINAPI *Lock)(IBlockingLock *This,DWORD dwTimeout);
      HRESULT (WINAPI *Unlock)(IBlockingLock *This);
    END_INTERFACE
  } IBlockingLockVtbl;
  struct IBlockingLock {
    CONST_VTBL struct IBlockingLockVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBlockingLock_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBlockingLock_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBlockingLock_Release(This) (This)->lpVtbl->Release(This)
#define IBlockingLock_Lock(This,dwTimeout) (This)->lpVtbl->Lock(This,dwTimeout)
#define IBlockingLock_Unlock(This) (This)->lpVtbl->Unlock(This)
#endif
#endif
  HRESULT WINAPI IBlockingLock_Lock_Proxy(IBlockingLock *This,DWORD dwTimeout);
  void __RPC_STUB IBlockingLock_Lock_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBlockingLock_Unlock_Proxy(IBlockingLock *This);
  void __RPC_STUB IBlockingLock_Unlock_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITimeAndNoticeControl_INTERFACE_DEFINED__
#define __ITimeAndNoticeControl_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ITimeAndNoticeControl;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITimeAndNoticeControl : public IUnknown {
  public:
    virtual HRESULT WINAPI SuppressChanges(DWORD res1,DWORD res2) = 0;
  };
#else
  typedef struct ITimeAndNoticeControlVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITimeAndNoticeControl *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITimeAndNoticeControl *This);
      ULONG (WINAPI *Release)(ITimeAndNoticeControl *This);
      HRESULT (WINAPI *SuppressChanges)(ITimeAndNoticeControl *This,DWORD res1,DWORD res2);
    END_INTERFACE
  } ITimeAndNoticeControlVtbl;
  struct ITimeAndNoticeControl {
    CONST_VTBL struct ITimeAndNoticeControlVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITimeAndNoticeControl_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITimeAndNoticeControl_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITimeAndNoticeControl_Release(This) (This)->lpVtbl->Release(This)
#define ITimeAndNoticeControl_SuppressChanges(This,res1,res2) (This)->lpVtbl->SuppressChanges(This,res1,res2)
#endif
#endif
  HRESULT WINAPI ITimeAndNoticeControl_SuppressChanges_Proxy(ITimeAndNoticeControl *This,DWORD res1,DWORD res2);
  void __RPC_STUB ITimeAndNoticeControl_SuppressChanges_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IOplockStorage_INTERFACE_DEFINED__
#define __IOplockStorage_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IOplockStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IOplockStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateStorageEx(LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen) = 0;
    virtual HRESULT WINAPI OpenStorageEx(LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen) = 0;
  };
#else
  typedef struct IOplockStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IOplockStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IOplockStorage *This);
      ULONG (WINAPI *Release)(IOplockStorage *This);
      HRESULT (WINAPI *CreateStorageEx)(IOplockStorage *This,LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen);
      HRESULT (WINAPI *OpenStorageEx)(IOplockStorage *This,LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen);
    END_INTERFACE
  } IOplockStorageVtbl;
  struct IOplockStorage {
    CONST_VTBL struct IOplockStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IOplockStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IOplockStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IOplockStorage_Release(This) (This)->lpVtbl->Release(This)
#define IOplockStorage_CreateStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen) (This)->lpVtbl->CreateStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)
#define IOplockStorage_OpenStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen) (This)->lpVtbl->OpenStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)
#endif
#endif
  HRESULT WINAPI IOplockStorage_CreateStorageEx_Proxy(IOplockStorage *This,LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen);
  void __RPC_STUB IOplockStorage_CreateStorageEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IOplockStorage_OpenStorageEx_Proxy(IOplockStorage *This,LPCWSTR pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,REFIID riid,void **ppstgOpen);
  void __RPC_STUB IOplockStorage_OpenStorageEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISurrogate_INTERFACE_DEFINED__
#define __ISurrogate_INTERFACE_DEFINED__
  typedef ISurrogate *LPSURROGATE;

  EXTERN_C const IID IID_ISurrogate;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISurrogate : public IUnknown {
  public:
    virtual HRESULT WINAPI LoadDllServer(REFCLSID Clsid) = 0;
    virtual HRESULT WINAPI FreeSurrogate(void) = 0;
  };
#else
  typedef struct ISurrogateVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISurrogate *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISurrogate *This);
      ULONG (WINAPI *Release)(ISurrogate *This);
      HRESULT (WINAPI *LoadDllServer)(ISurrogate *This,REFCLSID Clsid);
      HRESULT (WINAPI *FreeSurrogate)(ISurrogate *This);
    END_INTERFACE
  } ISurrogateVtbl;
  struct ISurrogate {
    CONST_VTBL struct ISurrogateVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISurrogate_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISurrogate_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISurrogate_Release(This) (This)->lpVtbl->Release(This)
#define ISurrogate_LoadDllServer(This,Clsid) (This)->lpVtbl->LoadDllServer(This,Clsid)
#define ISurrogate_FreeSurrogate(This) (This)->lpVtbl->FreeSurrogate(This)
#endif
#endif
  HRESULT WINAPI ISurrogate_LoadDllServer_Proxy(ISurrogate *This,REFCLSID Clsid);
  void __RPC_STUB ISurrogate_LoadDllServer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISurrogate_FreeSurrogate_Proxy(ISurrogate *This);
  void __RPC_STUB ISurrogate_FreeSurrogate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IGlobalInterfaceTable_INTERFACE_DEFINED__
#define __IGlobalInterfaceTable_INTERFACE_DEFINED__
  typedef IGlobalInterfaceTable *LPGLOBALINTERFACETABLE;

  EXTERN_C const IID IID_IGlobalInterfaceTable;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IGlobalInterfaceTable : public IUnknown {
  public:
    virtual HRESULT WINAPI RegisterInterfaceInGlobal(IUnknown *pUnk,REFIID riid,DWORD *pdwCookie) = 0;
    virtual HRESULT WINAPI RevokeInterfaceFromGlobal(DWORD dwCookie) = 0;
    virtual HRESULT WINAPI GetInterfaceFromGlobal(DWORD dwCookie,REFIID riid,void **ppv) = 0;
  };
#else
  typedef struct IGlobalInterfaceTableVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IGlobalInterfaceTable *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IGlobalInterfaceTable *This);
      ULONG (WINAPI *Release)(IGlobalInterfaceTable *This);
      HRESULT (WINAPI *RegisterInterfaceInGlobal)(IGlobalInterfaceTable *This,IUnknown *pUnk,REFIID riid,DWORD *pdwCookie);
      HRESULT (WINAPI *RevokeInterfaceFromGlobal)(IGlobalInterfaceTable *This,DWORD dwCookie);
      HRESULT (WINAPI *GetInterfaceFromGlobal)(IGlobalInterfaceTable *This,DWORD dwCookie,REFIID riid,void **ppv);
    END_INTERFACE
  } IGlobalInterfaceTableVtbl;
  struct IGlobalInterfaceTable {
    CONST_VTBL struct IGlobalInterfaceTableVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IGlobalInterfaceTable_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IGlobalInterfaceTable_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IGlobalInterfaceTable_Release(This) (This)->lpVtbl->Release(This)
#define IGlobalInterfaceTable_RegisterInterfaceInGlobal(This,pUnk,riid,pdwCookie) (This)->lpVtbl->RegisterInterfaceInGlobal(This,pUnk,riid,pdwCookie)
#define IGlobalInterfaceTable_RevokeInterfaceFromGlobal(This,dwCookie) (This)->lpVtbl->RevokeInterfaceFromGlobal(This,dwCookie)
#define IGlobalInterfaceTable_GetInterfaceFromGlobal(This,dwCookie,riid,ppv) (This)->lpVtbl->GetInterfaceFromGlobal(This,dwCookie,riid,ppv)
#endif
#endif
  HRESULT WINAPI IGlobalInterfaceTable_RegisterInterfaceInGlobal_Proxy(IGlobalInterfaceTable *This,IUnknown *pUnk,REFIID riid,DWORD *pdwCookie);
  void __RPC_STUB IGlobalInterfaceTable_RegisterInterfaceInGlobal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IGlobalInterfaceTable_RevokeInterfaceFromGlobal_Proxy(IGlobalInterfaceTable *This,DWORD dwCookie);
  void __RPC_STUB IGlobalInterfaceTable_RevokeInterfaceFromGlobal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IGlobalInterfaceTable_GetInterfaceFromGlobal_Proxy(IGlobalInterfaceTable *This,DWORD dwCookie,REFIID riid,void **ppv);
  void __RPC_STUB IGlobalInterfaceTable_GetInterfaceFromGlobal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDirectWriterLock_INTERFACE_DEFINED__
#define __IDirectWriterLock_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IDirectWriterLock;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDirectWriterLock : public IUnknown {
  public:
    virtual HRESULT WINAPI WaitForWriteAccess(DWORD dwTimeout) = 0;
    virtual HRESULT WINAPI ReleaseWriteAccess(void) = 0;
    virtual HRESULT WINAPI HaveWriteAccess(void) = 0;
  };
#else
  typedef struct IDirectWriterLockVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDirectWriterLock *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDirectWriterLock *This);
      ULONG (WINAPI *Release)(IDirectWriterLock *This);
      HRESULT (WINAPI *WaitForWriteAccess)(IDirectWriterLock *This,DWORD dwTimeout);
      HRESULT (WINAPI *ReleaseWriteAccess)(IDirectWriterLock *This);
      HRESULT (WINAPI *HaveWriteAccess)(IDirectWriterLock *This);
    END_INTERFACE
  } IDirectWriterLockVtbl;
  struct IDirectWriterLock {
    CONST_VTBL struct IDirectWriterLockVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDirectWriterLock_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDirectWriterLock_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDirectWriterLock_Release(This) (This)->lpVtbl->Release(This)
#define IDirectWriterLock_WaitForWriteAccess(This,dwTimeout) (This)->lpVtbl->WaitForWriteAccess(This,dwTimeout)
#define IDirectWriterLock_ReleaseWriteAccess(This) (This)->lpVtbl->ReleaseWriteAccess(This)
#define IDirectWriterLock_HaveWriteAccess(This) (This)->lpVtbl->HaveWriteAccess(This)
#endif
#endif
  HRESULT WINAPI IDirectWriterLock_WaitForWriteAccess_Proxy(IDirectWriterLock *This,DWORD dwTimeout);
  void __RPC_STUB IDirectWriterLock_WaitForWriteAccess_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDirectWriterLock_ReleaseWriteAccess_Proxy(IDirectWriterLock *This);
  void __RPC_STUB IDirectWriterLock_ReleaseWriteAccess_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDirectWriterLock_HaveWriteAccess_Proxy(IDirectWriterLock *This);
  void __RPC_STUB IDirectWriterLock_HaveWriteAccess_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISynchronize_INTERFACE_DEFINED__
#define __ISynchronize_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISynchronize;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISynchronize : public IUnknown {
  public:
    virtual HRESULT WINAPI Wait(DWORD dwFlags,DWORD dwMilliseconds) = 0;
    virtual HRESULT WINAPI Signal(void) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
  };
#else
  typedef struct ISynchronizeVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISynchronize *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISynchronize *This);
      ULONG (WINAPI *Release)(ISynchronize *This);
      HRESULT (WINAPI *Wait)(ISynchronize *This,DWORD dwFlags,DWORD dwMilliseconds);
      HRESULT (WINAPI *Signal)(ISynchronize *This);
      HRESULT (WINAPI *Reset)(ISynchronize *This);
    END_INTERFACE
  } ISynchronizeVtbl;
  struct ISynchronize {
    CONST_VTBL struct ISynchronizeVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISynchronize_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISynchronize_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISynchronize_Release(This) (This)->lpVtbl->Release(This)
#define ISynchronize_Wait(This,dwFlags,dwMilliseconds) (This)->lpVtbl->Wait(This,dwFlags,dwMilliseconds)
#define ISynchronize_Signal(This) (This)->lpVtbl->Signal(This)
#define ISynchronize_Reset(This) (This)->lpVtbl->Reset(This)
#endif
#endif
  HRESULT WINAPI ISynchronize_Wait_Proxy(ISynchronize *This,DWORD dwFlags,DWORD dwMilliseconds);
  void __RPC_STUB ISynchronize_Wait_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISynchronize_Signal_Proxy(ISynchronize *This);
  void __RPC_STUB ISynchronize_Signal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISynchronize_Reset_Proxy(ISynchronize *This);
  void __RPC_STUB ISynchronize_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISynchronizeHandle_INTERFACE_DEFINED__
#define __ISynchronizeHandle_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISynchronizeHandle;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISynchronizeHandle : public IUnknown {
  public:
    virtual HRESULT WINAPI GetHandle(HANDLE *ph) = 0;
  };
#else
  typedef struct ISynchronizeHandleVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISynchronizeHandle *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISynchronizeHandle *This);
      ULONG (WINAPI *Release)(ISynchronizeHandle *This);
      HRESULT (WINAPI *GetHandle)(ISynchronizeHandle *This,HANDLE *ph);
    END_INTERFACE
  } ISynchronizeHandleVtbl;
  struct ISynchronizeHandle {
    CONST_VTBL struct ISynchronizeHandleVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISynchronizeHandle_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISynchronizeHandle_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISynchronizeHandle_Release(This) (This)->lpVtbl->Release(This)
#define ISynchronizeHandle_GetHandle(This,ph) (This)->lpVtbl->GetHandle(This,ph)
#endif
#endif
  HRESULT WINAPI ISynchronizeHandle_GetHandle_Proxy(ISynchronizeHandle *This,HANDLE *ph);
  void __RPC_STUB ISynchronizeHandle_GetHandle_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISynchronizeEvent_INTERFACE_DEFINED__
#define __ISynchronizeEvent_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISynchronizeEvent;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISynchronizeEvent : public ISynchronizeHandle {
  public:
    virtual HRESULT WINAPI SetEventHandle(HANDLE *ph) = 0;
  };
#else
  typedef struct ISynchronizeEventVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISynchronizeEvent *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISynchronizeEvent *This);
      ULONG (WINAPI *Release)(ISynchronizeEvent *This);
      HRESULT (WINAPI *GetHandle)(ISynchronizeEvent *This,HANDLE *ph);
      HRESULT (WINAPI *SetEventHandle)(ISynchronizeEvent *This,HANDLE *ph);
    END_INTERFACE
  } ISynchronizeEventVtbl;
  struct ISynchronizeEvent {
    CONST_VTBL struct ISynchronizeEventVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISynchronizeEvent_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISynchronizeEvent_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISynchronizeEvent_Release(This) (This)->lpVtbl->Release(This)
#define ISynchronizeEvent_GetHandle(This,ph) (This)->lpVtbl->GetHandle(This,ph)
#define ISynchronizeEvent_SetEventHandle(This,ph) (This)->lpVtbl->SetEventHandle(This,ph)
#endif
#endif
  HRESULT WINAPI ISynchronizeEvent_SetEventHandle_Proxy(ISynchronizeEvent *This,HANDLE *ph);
  void __RPC_STUB ISynchronizeEvent_SetEventHandle_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISynchronizeContainer_INTERFACE_DEFINED__
#define __ISynchronizeContainer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISynchronizeContainer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISynchronizeContainer : public IUnknown {
  public:
    virtual HRESULT WINAPI AddSynchronize(ISynchronize *pSync) = 0;
    virtual HRESULT WINAPI WaitMultiple(DWORD dwFlags,DWORD dwTimeOut,ISynchronize **ppSync) = 0;
  };
#else
  typedef struct ISynchronizeContainerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISynchronizeContainer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISynchronizeContainer *This);
      ULONG (WINAPI *Release)(ISynchronizeContainer *This);
      HRESULT (WINAPI *AddSynchronize)(ISynchronizeContainer *This,ISynchronize *pSync);
      HRESULT (WINAPI *WaitMultiple)(ISynchronizeContainer *This,DWORD dwFlags,DWORD dwTimeOut,ISynchronize **ppSync);
    END_INTERFACE
  } ISynchronizeContainerVtbl;
  struct ISynchronizeContainer {
    CONST_VTBL struct ISynchronizeContainerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISynchronizeContainer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISynchronizeContainer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISynchronizeContainer_Release(This) (This)->lpVtbl->Release(This)
#define ISynchronizeContainer_AddSynchronize(This,pSync) (This)->lpVtbl->AddSynchronize(This,pSync)
#define ISynchronizeContainer_WaitMultiple(This,dwFlags,dwTimeOut,ppSync) (This)->lpVtbl->WaitMultiple(This,dwFlags,dwTimeOut,ppSync)
#endif
#endif
  HRESULT WINAPI ISynchronizeContainer_AddSynchronize_Proxy(ISynchronizeContainer *This,ISynchronize *pSync);
  void __RPC_STUB ISynchronizeContainer_AddSynchronize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISynchronizeContainer_WaitMultiple_Proxy(ISynchronizeContainer *This,DWORD dwFlags,DWORD dwTimeOut,ISynchronize **ppSync);
  void __RPC_STUB ISynchronizeContainer_WaitMultiple_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISynchronizeMutex_INTERFACE_DEFINED__
#define __ISynchronizeMutex_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISynchronizeMutex;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISynchronizeMutex : public ISynchronize {
  public:
    virtual HRESULT WINAPI ReleaseMutex(void) = 0;
  };
#else
  typedef struct ISynchronizeMutexVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISynchronizeMutex *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISynchronizeMutex *This);
      ULONG (WINAPI *Release)(ISynchronizeMutex *This);
      HRESULT (WINAPI *Wait)(ISynchronizeMutex *This,DWORD dwFlags,DWORD dwMilliseconds);
      HRESULT (WINAPI *Signal)(ISynchronizeMutex *This);
      HRESULT (WINAPI *Reset)(ISynchronizeMutex *This);
      HRESULT (WINAPI *ReleaseMutex)(ISynchronizeMutex *This);
    END_INTERFACE
  } ISynchronizeMutexVtbl;
  struct ISynchronizeMutex {
    CONST_VTBL struct ISynchronizeMutexVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISynchronizeMutex_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISynchronizeMutex_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISynchronizeMutex_Release(This) (This)->lpVtbl->Release(This)
#define ISynchronizeMutex_Wait(This,dwFlags,dwMilliseconds) (This)->lpVtbl->Wait(This,dwFlags,dwMilliseconds)
#define ISynchronizeMutex_Signal(This) (This)->lpVtbl->Signal(This)
#define ISynchronizeMutex_Reset(This) (This)->lpVtbl->Reset(This)
#define ISynchronizeMutex_ReleaseMutex(This) (This)->lpVtbl->ReleaseMutex(This)
#endif
#endif
  HRESULT WINAPI ISynchronizeMutex_ReleaseMutex_Proxy(ISynchronizeMutex *This);
  void __RPC_STUB ISynchronizeMutex_ReleaseMutex_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICancelMethodCalls_INTERFACE_DEFINED__
#define __ICancelMethodCalls_INTERFACE_DEFINED__
  typedef ICancelMethodCalls *LPCANCELMETHODCALLS;

  EXTERN_C const IID IID_ICancelMethodCalls;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICancelMethodCalls : public IUnknown {
  public:
    virtual HRESULT WINAPI Cancel(ULONG ulSeconds) = 0;
    virtual HRESULT WINAPI TestCancel(void) = 0;
  };
#else
  typedef struct ICancelMethodCallsVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICancelMethodCalls *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICancelMethodCalls *This);
      ULONG (WINAPI *Release)(ICancelMethodCalls *This);
      HRESULT (WINAPI *Cancel)(ICancelMethodCalls *This,ULONG ulSeconds);
      HRESULT (WINAPI *TestCancel)(ICancelMethodCalls *This);
    END_INTERFACE
  } ICancelMethodCallsVtbl;
  struct ICancelMethodCalls {
    CONST_VTBL struct ICancelMethodCallsVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICancelMethodCalls_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICancelMethodCalls_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICancelMethodCalls_Release(This) (This)->lpVtbl->Release(This)
#define ICancelMethodCalls_Cancel(This,ulSeconds) (This)->lpVtbl->Cancel(This,ulSeconds)
#define ICancelMethodCalls_TestCancel(This) (This)->lpVtbl->TestCancel(This)
#endif
#endif
  HRESULT WINAPI ICancelMethodCalls_Cancel_Proxy(ICancelMethodCalls *This,ULONG ulSeconds);
  void __RPC_STUB ICancelMethodCalls_Cancel_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICancelMethodCalls_TestCancel_Proxy(ICancelMethodCalls *This);
  void __RPC_STUB ICancelMethodCalls_TestCancel_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAsyncManager_INTERFACE_DEFINED__
#define __IAsyncManager_INTERFACE_DEFINED__
  typedef enum tagDCOM_CALL_STATE {
    DCOM_NONE = 0,DCOM_CALL_COMPLETE = 0x1,DCOM_CALL_CANCELED = 0x2
  } DCOM_CALL_STATE;

  EXTERN_C const IID IID_IAsyncManager;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAsyncManager : public IUnknown {
  public:
    virtual HRESULT WINAPI CompleteCall(HRESULT Result) = 0;
    virtual HRESULT WINAPI GetCallContext(REFIID riid,void **pInterface) = 0;
    virtual HRESULT WINAPI GetState(ULONG *pulStateFlags) = 0;
  };
#else
  typedef struct IAsyncManagerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAsyncManager *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAsyncManager *This);
      ULONG (WINAPI *Release)(IAsyncManager *This);
      HRESULT (WINAPI *CompleteCall)(IAsyncManager *This,HRESULT Result);
      HRESULT (WINAPI *GetCallContext)(IAsyncManager *This,REFIID riid,void **pInterface);
      HRESULT (WINAPI *GetState)(IAsyncManager *This,ULONG *pulStateFlags);
    END_INTERFACE
  } IAsyncManagerVtbl;
  struct IAsyncManager {
    CONST_VTBL struct IAsyncManagerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAsyncManager_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAsyncManager_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAsyncManager_Release(This) (This)->lpVtbl->Release(This)
#define IAsyncManager_CompleteCall(This,Result) (This)->lpVtbl->CompleteCall(This,Result)
#define IAsyncManager_GetCallContext(This,riid,pInterface) (This)->lpVtbl->GetCallContext(This,riid,pInterface)
#define IAsyncManager_GetState(This,pulStateFlags) (This)->lpVtbl->GetState(This,pulStateFlags)
#endif
#endif
  HRESULT WINAPI IAsyncManager_CompleteCall_Proxy(IAsyncManager *This,HRESULT Result);
  void __RPC_STUB IAsyncManager_CompleteCall_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAsyncManager_GetCallContext_Proxy(IAsyncManager *This,REFIID riid,void **pInterface);
  void __RPC_STUB IAsyncManager_GetCallContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAsyncManager_GetState_Proxy(IAsyncManager *This,ULONG *pulStateFlags);
  void __RPC_STUB IAsyncManager_GetState_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICallFactory_INTERFACE_DEFINED__
#define __ICallFactory_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ICallFactory;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICallFactory : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateCall(REFIID riid,IUnknown *pCtrlUnk,REFIID riid2,IUnknown **ppv) = 0;
  };
#else
  typedef struct ICallFactoryVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICallFactory *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICallFactory *This);
      ULONG (WINAPI *Release)(ICallFactory *This);
      HRESULT (WINAPI *CreateCall)(ICallFactory *This,REFIID riid,IUnknown *pCtrlUnk,REFIID riid2,IUnknown **ppv);
    END_INTERFACE
  } ICallFactoryVtbl;
  struct ICallFactory {
    CONST_VTBL struct ICallFactoryVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICallFactory_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICallFactory_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICallFactory_Release(This) (This)->lpVtbl->Release(This)
#define ICallFactory_CreateCall(This,riid,pCtrlUnk,riid2,ppv) (This)->lpVtbl->CreateCall(This,riid,pCtrlUnk,riid2,ppv)
#endif
#endif
  HRESULT WINAPI ICallFactory_CreateCall_Proxy(ICallFactory *This,REFIID riid,IUnknown *pCtrlUnk,REFIID riid2,IUnknown **ppv);
  void __RPC_STUB ICallFactory_CreateCall_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRpcHelper_INTERFACE_DEFINED__
#define __IRpcHelper_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IRpcHelper;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRpcHelper : public IUnknown {
  public:
    virtual HRESULT WINAPI GetDCOMProtocolVersion(DWORD *pComVersion) = 0;
    virtual HRESULT WINAPI GetIIDFromOBJREF(void *pObjRef,IID **piid) = 0;
  };
#else
  typedef struct IRpcHelperVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRpcHelper *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRpcHelper *This);
      ULONG (WINAPI *Release)(IRpcHelper *This);
      HRESULT (WINAPI *GetDCOMProtocolVersion)(IRpcHelper *This,DWORD *pComVersion);
      HRESULT (WINAPI *GetIIDFromOBJREF)(IRpcHelper *This,void *pObjRef,IID **piid);
    END_INTERFACE
  } IRpcHelperVtbl;
  struct IRpcHelper {
    CONST_VTBL struct IRpcHelperVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRpcHelper_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRpcHelper_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRpcHelper_Release(This) (This)->lpVtbl->Release(This)
#define IRpcHelper_GetDCOMProtocolVersion(This,pComVersion) (This)->lpVtbl->GetDCOMProtocolVersion(This,pComVersion)
#define IRpcHelper_GetIIDFromOBJREF(This,pObjRef,piid) (This)->lpVtbl->GetIIDFromOBJREF(This,pObjRef,piid)
#endif
#endif
  HRESULT WINAPI IRpcHelper_GetDCOMProtocolVersion_Proxy(IRpcHelper *This,DWORD *pComVersion);
  void __RPC_STUB IRpcHelper_GetDCOMProtocolVersion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRpcHelper_GetIIDFromOBJREF_Proxy(IRpcHelper *This,void *pObjRef,IID **piid);
  void __RPC_STUB IRpcHelper_GetIIDFromOBJREF_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IReleaseMarshalBuffers_INTERFACE_DEFINED__
#define __IReleaseMarshalBuffers_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IReleaseMarshalBuffers;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IReleaseMarshalBuffers : public IUnknown {
  public:
    virtual HRESULT WINAPI ReleaseMarshalBuffer(RPCOLEMESSAGE *pMsg,DWORD dwFlags,IUnknown *pChnl) = 0;
  };
#else
  typedef struct IReleaseMarshalBuffersVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IReleaseMarshalBuffers *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IReleaseMarshalBuffers *This);
      ULONG (WINAPI *Release)(IReleaseMarshalBuffers *This);
      HRESULT (WINAPI *ReleaseMarshalBuffer)(IReleaseMarshalBuffers *This,RPCOLEMESSAGE *pMsg,DWORD dwFlags,IUnknown *pChnl);
    END_INTERFACE
  } IReleaseMarshalBuffersVtbl;
  struct IReleaseMarshalBuffers {
    CONST_VTBL struct IReleaseMarshalBuffersVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IReleaseMarshalBuffers_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IReleaseMarshalBuffers_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IReleaseMarshalBuffers_Release(This) (This)->lpVtbl->Release(This)
#define IReleaseMarshalBuffers_ReleaseMarshalBuffer(This,pMsg,dwFlags,pChnl) (This)->lpVtbl->ReleaseMarshalBuffer(This,pMsg,dwFlags,pChnl)
#endif
#endif
  HRESULT WINAPI IReleaseMarshalBuffers_ReleaseMarshalBuffer_Proxy(IReleaseMarshalBuffers *This,RPCOLEMESSAGE *pMsg,DWORD dwFlags,IUnknown *pChnl);
  void __RPC_STUB IReleaseMarshalBuffers_ReleaseMarshalBuffer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IWaitMultiple_INTERFACE_DEFINED__
#define __IWaitMultiple_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IWaitMultiple;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWaitMultiple : public IUnknown {
  public:
    virtual HRESULT WINAPI WaitMultiple(DWORD timeout,ISynchronize **pSync) = 0;
    virtual HRESULT WINAPI AddSynchronize(ISynchronize *pSync) = 0;
  };
#else
  typedef struct IWaitMultipleVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWaitMultiple *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWaitMultiple *This);
      ULONG (WINAPI *Release)(IWaitMultiple *This);
      HRESULT (WINAPI *WaitMultiple)(IWaitMultiple *This,DWORD timeout,ISynchronize **pSync);
      HRESULT (WINAPI *AddSynchronize)(IWaitMultiple *This,ISynchronize *pSync);
    END_INTERFACE
  } IWaitMultipleVtbl;
  struct IWaitMultiple {
    CONST_VTBL struct IWaitMultipleVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWaitMultiple_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWaitMultiple_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWaitMultiple_Release(This) (This)->lpVtbl->Release(This)
#define IWaitMultiple_WaitMultiple(This,timeout,pSync) (This)->lpVtbl->WaitMultiple(This,timeout,pSync)
#define IWaitMultiple_AddSynchronize(This,pSync) (This)->lpVtbl->AddSynchronize(This,pSync)
#endif
#endif
  HRESULT WINAPI IWaitMultiple_WaitMultiple_Proxy(IWaitMultiple *This,DWORD timeout,ISynchronize **pSync);
  void __RPC_STUB IWaitMultiple_WaitMultiple_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IWaitMultiple_AddSynchronize_Proxy(IWaitMultiple *This,ISynchronize *pSync);
  void __RPC_STUB IWaitMultiple_AddSynchronize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IUrlMon_INTERFACE_DEFINED__
#define __IUrlMon_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IUrlMon;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IUrlMon : public IUnknown {
  public:
    virtual HRESULT WINAPI AsyncGetClassBits(REFCLSID rclsid,LPCWSTR pszTYPE,LPCWSTR pszExt,DWORD dwFileVersionMS,DWORD dwFileVersionLS,LPCWSTR pszCodeBase,IBindCtx *pbc,DWORD dwClassContext,REFIID riid,DWORD flags) = 0;
  };
#else
  typedef struct IUrlMonVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IUrlMon *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IUrlMon *This);
      ULONG (WINAPI *Release)(IUrlMon *This);
      HRESULT (WINAPI *AsyncGetClassBits)(IUrlMon *This,REFCLSID rclsid,LPCWSTR pszTYPE,LPCWSTR pszExt,DWORD dwFileVersionMS,DWORD dwFileVersionLS,LPCWSTR pszCodeBase,IBindCtx *pbc,DWORD dwClassContext,REFIID riid,DWORD flags);
    END_INTERFACE
  } IUrlMonVtbl;
  struct IUrlMon {
    CONST_VTBL struct IUrlMonVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IUrlMon_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IUrlMon_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IUrlMon_Release(This) (This)->lpVtbl->Release(This)
#define IUrlMon_AsyncGetClassBits(This,rclsid,pszTYPE,pszExt,dwFileVersionMS,dwFileVersionLS,pszCodeBase,pbc,dwClassContext,riid,flags) (This)->lpVtbl->AsyncGetClassBits(This,rclsid,pszTYPE,pszExt,dwFileVersionMS,dwFileVersionLS,pszCodeBase,pbc,dwClassContext,riid,flags)
#endif
#endif
  HRESULT WINAPI IUrlMon_AsyncGetClassBits_Proxy(IUrlMon *This,REFCLSID rclsid,LPCWSTR pszTYPE,LPCWSTR pszExt,DWORD dwFileVersionMS,DWORD dwFileVersionLS,LPCWSTR pszCodeBase,IBindCtx *pbc,DWORD dwClassContext,REFIID riid,DWORD flags);
  void __RPC_STUB IUrlMon_AsyncGetClassBits_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IForegroundTransfer_INTERFACE_DEFINED__
#define __IForegroundTransfer_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IForegroundTransfer;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IForegroundTransfer : public IUnknown {
  public:
    virtual HRESULT WINAPI AllowForegroundTransfer(void *lpvReserved) = 0;
  };
#else
  typedef struct IForegroundTransferVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IForegroundTransfer *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IForegroundTransfer *This);
      ULONG (WINAPI *Release)(IForegroundTransfer *This);
      HRESULT (WINAPI *AllowForegroundTransfer)(IForegroundTransfer *This,void *lpvReserved);
    END_INTERFACE
  } IForegroundTransferVtbl;
  struct IForegroundTransfer {
    CONST_VTBL struct IForegroundTransferVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IForegroundTransfer_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IForegroundTransfer_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IForegroundTransfer_Release(This) (This)->lpVtbl->Release(This)
#define IForegroundTransfer_AllowForegroundTransfer(This,lpvReserved) (This)->lpVtbl->AllowForegroundTransfer(This,lpvReserved)
#endif
#endif
  HRESULT WINAPI IForegroundTransfer_AllowForegroundTransfer_Proxy(IForegroundTransfer *This,void *lpvReserved);
  void __RPC_STUB IForegroundTransfer_AllowForegroundTransfer_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAddrTrackingControl_INTERFACE_DEFINED__
#define __IAddrTrackingControl_INTERFACE_DEFINED__
  typedef IAddrTrackingControl *LPADDRTRACKINGCONTROL;

  EXTERN_C const IID IID_IAddrTrackingControl;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAddrTrackingControl : public IUnknown {
  public:
    virtual HRESULT WINAPI EnableCOMDynamicAddrTracking(void) = 0;
    virtual HRESULT WINAPI DisableCOMDynamicAddrTracking(void) = 0;
  };
#else
  typedef struct IAddrTrackingControlVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAddrTrackingControl *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAddrTrackingControl *This);
      ULONG (WINAPI *Release)(IAddrTrackingControl *This);
      HRESULT (WINAPI *EnableCOMDynamicAddrTracking)(IAddrTrackingControl *This);
      HRESULT (WINAPI *DisableCOMDynamicAddrTracking)(IAddrTrackingControl *This);
    END_INTERFACE
  } IAddrTrackingControlVtbl;
  struct IAddrTrackingControl {
    CONST_VTBL struct IAddrTrackingControlVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAddrTrackingControl_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAddrTrackingControl_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAddrTrackingControl_Release(This) (This)->lpVtbl->Release(This)
#define IAddrTrackingControl_EnableCOMDynamicAddrTracking(This) (This)->lpVtbl->EnableCOMDynamicAddrTracking(This)
#define IAddrTrackingControl_DisableCOMDynamicAddrTracking(This) (This)->lpVtbl->DisableCOMDynamicAddrTracking(This)
#endif
#endif
  HRESULT WINAPI IAddrTrackingControl_EnableCOMDynamicAddrTracking_Proxy(IAddrTrackingControl *This);
  void __RPC_STUB IAddrTrackingControl_EnableCOMDynamicAddrTracking_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAddrTrackingControl_DisableCOMDynamicAddrTracking_Proxy(IAddrTrackingControl *This);
  void __RPC_STUB IAddrTrackingControl_DisableCOMDynamicAddrTracking_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IAddrExclusionControl_INTERFACE_DEFINED__
#define __IAddrExclusionControl_INTERFACE_DEFINED__
  typedef IAddrExclusionControl *LPADDREXCLUSIONCONTROL;

  EXTERN_C const IID IID_IAddrExclusionControl;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAddrExclusionControl : public IUnknown {
  public:
    virtual HRESULT WINAPI GetCurrentAddrExclusionList(REFIID riid,void **ppEnumerator) = 0;
    virtual HRESULT WINAPI UpdateAddrExclusionList(IUnknown *pEnumerator) = 0;
  };
#else
  typedef struct IAddrExclusionControlVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAddrExclusionControl *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAddrExclusionControl *This);
      ULONG (WINAPI *Release)(IAddrExclusionControl *This);
      HRESULT (WINAPI *GetCurrentAddrExclusionList)(IAddrExclusionControl *This,REFIID riid,void **ppEnumerator);
      HRESULT (WINAPI *UpdateAddrExclusionList)(IAddrExclusionControl *This,IUnknown *pEnumerator);
    END_INTERFACE
  } IAddrExclusionControlVtbl;
  struct IAddrExclusionControl {
    CONST_VTBL struct IAddrExclusionControlVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAddrExclusionControl_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAddrExclusionControl_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAddrExclusionControl_Release(This) (This)->lpVtbl->Release(This)
#define IAddrExclusionControl_GetCurrentAddrExclusionList(This,riid,ppEnumerator) (This)->lpVtbl->GetCurrentAddrExclusionList(This,riid,ppEnumerator)
#define IAddrExclusionControl_UpdateAddrExclusionList(This,pEnumerator) (This)->lpVtbl->UpdateAddrExclusionList(This,pEnumerator)
#endif
#endif
  HRESULT WINAPI IAddrExclusionControl_GetCurrentAddrExclusionList_Proxy(IAddrExclusionControl *This,REFIID riid,void **ppEnumerator);
  void __RPC_STUB IAddrExclusionControl_GetCurrentAddrExclusionList_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IAddrExclusionControl_UpdateAddrExclusionList_Proxy(IAddrExclusionControl *This,IUnknown *pEnumerator);
  void __RPC_STUB IAddrExclusionControl_UpdateAddrExclusionList_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPipeByte_INTERFACE_DEFINED__
#define __IPipeByte_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IPipeByte;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPipeByte : public IUnknown {
  public:
    virtual HRESULT WINAPI Pull(BYTE *buf,ULONG cRequest,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Push(BYTE *buf,ULONG cSent) = 0;
  };
#else
  typedef struct IPipeByteVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPipeByte *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPipeByte *This);
      ULONG (WINAPI *Release)(IPipeByte *This);
      HRESULT (WINAPI *Pull)(IPipeByte *This,BYTE *buf,ULONG cRequest,ULONG *pcReturned);
      HRESULT (WINAPI *Push)(IPipeByte *This,BYTE *buf,ULONG cSent);
    END_INTERFACE
  } IPipeByteVtbl;
  struct IPipeByte {
    CONST_VTBL struct IPipeByteVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPipeByte_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPipeByte_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPipeByte_Release(This) (This)->lpVtbl->Release(This)
#define IPipeByte_Pull(This,buf,cRequest,pcReturned) (This)->lpVtbl->Pull(This,buf,cRequest,pcReturned)
#define IPipeByte_Push(This,buf,cSent) (This)->lpVtbl->Push(This,buf,cSent)
#endif
#endif
  HRESULT WINAPI IPipeByte_Pull_Proxy(IPipeByte *This,BYTE *buf,ULONG cRequest,ULONG *pcReturned);
  void __RPC_STUB IPipeByte_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPipeByte_Push_Proxy(IPipeByte *This,BYTE *buf,ULONG cSent);
  void __RPC_STUB IPipeByte_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIPipeByte_INTERFACE_DEFINED__
#define __AsyncIPipeByte_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIPipeByte;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIPipeByte : public IUnknown {
  public:
    virtual HRESULT WINAPI Begin_Pull(ULONG cRequest) = 0;
    virtual HRESULT WINAPI Finish_Pull(BYTE *buf,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Begin_Push(BYTE *buf,ULONG cSent) = 0;
    virtual HRESULT WINAPI Finish_Push(void) = 0;
  };
#else
  typedef struct AsyncIPipeByteVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIPipeByte *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIPipeByte *This);
      ULONG (WINAPI *Release)(AsyncIPipeByte *This);
      HRESULT (WINAPI *Begin_Pull)(AsyncIPipeByte *This,ULONG cRequest);
      HRESULT (WINAPI *Finish_Pull)(AsyncIPipeByte *This,BYTE *buf,ULONG *pcReturned);
      HRESULT (WINAPI *Begin_Push)(AsyncIPipeByte *This,BYTE *buf,ULONG cSent);
      HRESULT (WINAPI *Finish_Push)(AsyncIPipeByte *This);
    END_INTERFACE
  } AsyncIPipeByteVtbl;
  struct AsyncIPipeByte {
    CONST_VTBL struct AsyncIPipeByteVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIPipeByte_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIPipeByte_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIPipeByte_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIPipeByte_Begin_Pull(This,cRequest) (This)->lpVtbl->Begin_Pull(This,cRequest)
#define AsyncIPipeByte_Finish_Pull(This,buf,pcReturned) (This)->lpVtbl->Finish_Pull(This,buf,pcReturned)
#define AsyncIPipeByte_Begin_Push(This,buf,cSent) (This)->lpVtbl->Begin_Push(This,buf,cSent)
#define AsyncIPipeByte_Finish_Push(This) (This)->lpVtbl->Finish_Push(This)
#endif
#endif
  HRESULT WINAPI AsyncIPipeByte_Begin_Pull_Proxy(AsyncIPipeByte *This,ULONG cRequest);
  void __RPC_STUB AsyncIPipeByte_Begin_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeByte_Finish_Pull_Proxy(AsyncIPipeByte *This,BYTE *buf,ULONG *pcReturned);
  void __RPC_STUB AsyncIPipeByte_Finish_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeByte_Begin_Push_Proxy(AsyncIPipeByte *This,BYTE *buf,ULONG cSent);
  void __RPC_STUB AsyncIPipeByte_Begin_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeByte_Finish_Push_Proxy(AsyncIPipeByte *This);
  void __RPC_STUB AsyncIPipeByte_Finish_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPipeLong_INTERFACE_DEFINED__
#define __IPipeLong_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IPipeLong;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPipeLong : public IUnknown {
  public:
    virtual HRESULT WINAPI Pull(LONG *buf,ULONG cRequest,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Push(LONG *buf,ULONG cSent) = 0;
  };
#else
  typedef struct IPipeLongVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPipeLong *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPipeLong *This);
      ULONG (WINAPI *Release)(IPipeLong *This);
      HRESULT (WINAPI *Pull)(IPipeLong *This,LONG *buf,ULONG cRequest,ULONG *pcReturned);
      HRESULT (WINAPI *Push)(IPipeLong *This,LONG *buf,ULONG cSent);
    END_INTERFACE
  } IPipeLongVtbl;
  struct IPipeLong {
    CONST_VTBL struct IPipeLongVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPipeLong_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPipeLong_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPipeLong_Release(This) (This)->lpVtbl->Release(This)
#define IPipeLong_Pull(This,buf,cRequest,pcReturned) (This)->lpVtbl->Pull(This,buf,cRequest,pcReturned)
#define IPipeLong_Push(This,buf,cSent) (This)->lpVtbl->Push(This,buf,cSent)
#endif
#endif
  HRESULT WINAPI IPipeLong_Pull_Proxy(IPipeLong *This,LONG *buf,ULONG cRequest,ULONG *pcReturned);
  void __RPC_STUB IPipeLong_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPipeLong_Push_Proxy(IPipeLong *This,LONG *buf,ULONG cSent);
  void __RPC_STUB IPipeLong_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIPipeLong_INTERFACE_DEFINED__
#define __AsyncIPipeLong_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIPipeLong;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIPipeLong : public IUnknown {
  public:
    virtual HRESULT WINAPI Begin_Pull(ULONG cRequest) = 0;
    virtual HRESULT WINAPI Finish_Pull(LONG *buf,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Begin_Push(LONG *buf,ULONG cSent) = 0;
    virtual HRESULT WINAPI Finish_Push(void) = 0;
  };
#else
  typedef struct AsyncIPipeLongVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIPipeLong *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIPipeLong *This);
      ULONG (WINAPI *Release)(AsyncIPipeLong *This);
      HRESULT (WINAPI *Begin_Pull)(AsyncIPipeLong *This,ULONG cRequest);
      HRESULT (WINAPI *Finish_Pull)(AsyncIPipeLong *This,LONG *buf,ULONG *pcReturned);
      HRESULT (WINAPI *Begin_Push)(AsyncIPipeLong *This,LONG *buf,ULONG cSent);
      HRESULT (WINAPI *Finish_Push)(AsyncIPipeLong *This);
    END_INTERFACE
  } AsyncIPipeLongVtbl;
  struct AsyncIPipeLong {
    CONST_VTBL struct AsyncIPipeLongVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIPipeLong_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIPipeLong_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIPipeLong_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIPipeLong_Begin_Pull(This,cRequest) (This)->lpVtbl->Begin_Pull(This,cRequest)
#define AsyncIPipeLong_Finish_Pull(This,buf,pcReturned) (This)->lpVtbl->Finish_Pull(This,buf,pcReturned)
#define AsyncIPipeLong_Begin_Push(This,buf,cSent) (This)->lpVtbl->Begin_Push(This,buf,cSent)
#define AsyncIPipeLong_Finish_Push(This) (This)->lpVtbl->Finish_Push(This)
#endif
#endif
  HRESULT WINAPI AsyncIPipeLong_Begin_Pull_Proxy(AsyncIPipeLong *This,ULONG cRequest);
  void __RPC_STUB AsyncIPipeLong_Begin_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeLong_Finish_Pull_Proxy(AsyncIPipeLong *This,LONG *buf,ULONG *pcReturned);
  void __RPC_STUB AsyncIPipeLong_Finish_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeLong_Begin_Push_Proxy(AsyncIPipeLong *This,LONG *buf,ULONG cSent);
  void __RPC_STUB AsyncIPipeLong_Begin_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeLong_Finish_Push_Proxy(AsyncIPipeLong *This);
  void __RPC_STUB AsyncIPipeLong_Finish_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPipeDouble_INTERFACE_DEFINED__
#define __IPipeDouble_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IPipeDouble;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPipeDouble : public IUnknown {
  public:
    virtual HRESULT WINAPI Pull(DOUBLE *buf,ULONG cRequest,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Push(DOUBLE *buf,ULONG cSent) = 0;
  };
#else
  typedef struct IPipeDoubleVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPipeDouble *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPipeDouble *This);
      ULONG (WINAPI *Release)(IPipeDouble *This);
      HRESULT (WINAPI *Pull)(IPipeDouble *This,DOUBLE *buf,ULONG cRequest,ULONG *pcReturned);
      HRESULT (WINAPI *Push)(IPipeDouble *This,DOUBLE *buf,ULONG cSent);
    END_INTERFACE
  } IPipeDoubleVtbl;
  struct IPipeDouble {
    CONST_VTBL struct IPipeDoubleVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPipeDouble_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPipeDouble_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPipeDouble_Release(This) (This)->lpVtbl->Release(This)
#define IPipeDouble_Pull(This,buf,cRequest,pcReturned) (This)->lpVtbl->Pull(This,buf,cRequest,pcReturned)
#define IPipeDouble_Push(This,buf,cSent) (This)->lpVtbl->Push(This,buf,cSent)
#endif
#endif
  HRESULT WINAPI IPipeDouble_Pull_Proxy(IPipeDouble *This,DOUBLE *buf,ULONG cRequest,ULONG *pcReturned);
  void __RPC_STUB IPipeDouble_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPipeDouble_Push_Proxy(IPipeDouble *This,DOUBLE *buf,ULONG cSent);
  void __RPC_STUB IPipeDouble_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __AsyncIPipeDouble_INTERFACE_DEFINED__
#define __AsyncIPipeDouble_INTERFACE_DEFINED__
  EXTERN_C const IID IID_AsyncIPipeDouble;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct AsyncIPipeDouble : public IUnknown {
  public:
    virtual HRESULT WINAPI Begin_Pull(ULONG cRequest) = 0;
    virtual HRESULT WINAPI Finish_Pull(DOUBLE *buf,ULONG *pcReturned) = 0;
    virtual HRESULT WINAPI Begin_Push(DOUBLE *buf,ULONG cSent) = 0;
    virtual HRESULT WINAPI Finish_Push(void) = 0;
  };
#else
  typedef struct AsyncIPipeDoubleVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(AsyncIPipeDouble *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(AsyncIPipeDouble *This);
      ULONG (WINAPI *Release)(AsyncIPipeDouble *This);
      HRESULT (WINAPI *Begin_Pull)(AsyncIPipeDouble *This,ULONG cRequest);
      HRESULT (WINAPI *Finish_Pull)(AsyncIPipeDouble *This,DOUBLE *buf,ULONG *pcReturned);
      HRESULT (WINAPI *Begin_Push)(AsyncIPipeDouble *This,DOUBLE *buf,ULONG cSent);
      HRESULT (WINAPI *Finish_Push)(AsyncIPipeDouble *This);
    END_INTERFACE
  } AsyncIPipeDoubleVtbl;
  struct AsyncIPipeDouble {
    CONST_VTBL struct AsyncIPipeDoubleVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define AsyncIPipeDouble_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define AsyncIPipeDouble_AddRef(This) (This)->lpVtbl->AddRef(This)
#define AsyncIPipeDouble_Release(This) (This)->lpVtbl->Release(This)
#define AsyncIPipeDouble_Begin_Pull(This,cRequest) (This)->lpVtbl->Begin_Pull(This,cRequest)
#define AsyncIPipeDouble_Finish_Pull(This,buf,pcReturned) (This)->lpVtbl->Finish_Pull(This,buf,pcReturned)
#define AsyncIPipeDouble_Begin_Push(This,buf,cSent) (This)->lpVtbl->Begin_Push(This,buf,cSent)
#define AsyncIPipeDouble_Finish_Push(This) (This)->lpVtbl->Finish_Push(This)
#endif
#endif
  HRESULT WINAPI AsyncIPipeDouble_Begin_Pull_Proxy(AsyncIPipeDouble *This,ULONG cRequest);
  void __RPC_STUB AsyncIPipeDouble_Begin_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeDouble_Finish_Pull_Proxy(AsyncIPipeDouble *This,DOUBLE *buf,ULONG *pcReturned);
  void __RPC_STUB AsyncIPipeDouble_Finish_Pull_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeDouble_Begin_Push_Proxy(AsyncIPipeDouble *This,DOUBLE *buf,ULONG cSent);
  void __RPC_STUB AsyncIPipeDouble_Begin_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI AsyncIPipeDouble_Finish_Push_Proxy(AsyncIPipeDouble *This);
  void __RPC_STUB AsyncIPipeDouble_Finish_Push_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IThumbnailExtractor_INTERFACE_DEFINED__
#define __IThumbnailExtractor_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IThumbnailExtractor;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IThumbnailExtractor : public IUnknown {
  public:
    virtual HRESULT WINAPI ExtractThumbnail(IStorage *pStg,ULONG ulLength,ULONG ulHeight,ULONG *pulOutputLength,ULONG *pulOutputHeight,HBITMAP *phOutputBitmap) = 0;
    virtual HRESULT WINAPI OnFileUpdated(IStorage *pStg) = 0;
  };
#else
  typedef struct IThumbnailExtractorVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IThumbnailExtractor *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IThumbnailExtractor *This);
      ULONG (WINAPI *Release)(IThumbnailExtractor *This);
      HRESULT (WINAPI *ExtractThumbnail)(IThumbnailExtractor *This,IStorage *pStg,ULONG ulLength,ULONG ulHeight,ULONG *pulOutputLength,ULONG *pulOutputHeight,HBITMAP *phOutputBitmap);
      HRESULT (WINAPI *OnFileUpdated)(IThumbnailExtractor *This,IStorage *pStg);
    END_INTERFACE
  } IThumbnailExtractorVtbl;
  struct IThumbnailExtractor {
    CONST_VTBL struct IThumbnailExtractorVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IThumbnailExtractor_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IThumbnailExtractor_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IThumbnailExtractor_Release(This) (This)->lpVtbl->Release(This)
#define IThumbnailExtractor_ExtractThumbnail(This,pStg,ulLength,ulHeight,pulOutputLength,pulOutputHeight,phOutputBitmap) (This)->lpVtbl->ExtractThumbnail(This,pStg,ulLength,ulHeight,pulOutputLength,pulOutputHeight,phOutputBitmap)
#define IThumbnailExtractor_OnFileUpdated(This,pStg) (This)->lpVtbl->OnFileUpdated(This,pStg)
#endif
#endif
  HRESULT WINAPI IThumbnailExtractor_ExtractThumbnail_Proxy(IThumbnailExtractor *This,IStorage *pStg,ULONG ulLength,ULONG ulHeight,ULONG *pulOutputLength,ULONG *pulOutputHeight,HBITMAP *phOutputBitmap);
  void __RPC_STUB IThumbnailExtractor_ExtractThumbnail_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IThumbnailExtractor_OnFileUpdated_Proxy(IThumbnailExtractor *This,IStorage *pStg);
  void __RPC_STUB IThumbnailExtractor_OnFileUpdated_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDummyHICONIncluder_INTERFACE_DEFINED__
#define __IDummyHICONIncluder_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IDummyHICONIncluder;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDummyHICONIncluder : public IUnknown {
  public:
    virtual HRESULT WINAPI Dummy(HICON h1,HDC h2) = 0;
  };
#else
  typedef struct IDummyHICONIncluderVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDummyHICONIncluder *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDummyHICONIncluder *This);
      ULONG (WINAPI *Release)(IDummyHICONIncluder *This);
      HRESULT (WINAPI *Dummy)(IDummyHICONIncluder *This,HICON h1,HDC h2);
    END_INTERFACE
  } IDummyHICONIncluderVtbl;
  struct IDummyHICONIncluder {
    CONST_VTBL struct IDummyHICONIncluderVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDummyHICONIncluder_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDummyHICONIncluder_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDummyHICONIncluder_Release(This) (This)->lpVtbl->Release(This)
#define IDummyHICONIncluder_Dummy(This,h1,h2) (This)->lpVtbl->Dummy(This,h1,h2)
#endif
#endif
  HRESULT WINAPI IDummyHICONIncluder_Dummy_Proxy(IDummyHICONIncluder *This,HICON h1,HDC h2);
  void __RPC_STUB IDummyHICONIncluder_Dummy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#if defined USE_COM_CONTEXT_DEF || defined BUILDTYPE_COMSVCS
  typedef DWORD CPFLAGS;

  typedef struct tagContextProperty {
    GUID policyId;
    CPFLAGS flags;
    IUnknown *pUnk;
  } ContextProperty;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0084_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0084_v0_0_s_ifspec;

#ifndef __IEnumContextProps_INTERFACE_DEFINED__
#define __IEnumContextProps_INTERFACE_DEFINED__
  typedef IEnumContextProps *LPENUMCONTEXTPROPS;

  EXTERN_C const IID IID_IEnumContextProps;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumContextProps : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,ContextProperty *pContextProperties,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumContextProps **ppEnumContextProps) = 0;
    virtual HRESULT WINAPI Count(ULONG *pcelt) = 0;
  };
#else
  typedef struct IEnumContextPropsVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumContextProps *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumContextProps *This);
      ULONG (WINAPI *Release)(IEnumContextProps *This);
      HRESULT (WINAPI *Next)(IEnumContextProps *This,ULONG celt,ContextProperty *pContextProperties,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumContextProps *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumContextProps *This);
      HRESULT (WINAPI *Clone)(IEnumContextProps *This,IEnumContextProps **ppEnumContextProps);
      HRESULT (WINAPI *Count)(IEnumContextProps *This,ULONG *pcelt);
    END_INTERFACE
  } IEnumContextPropsVtbl;
  struct IEnumContextProps {
    CONST_VTBL struct IEnumContextPropsVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumContextProps_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumContextProps_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumContextProps_Release(This) (This)->lpVtbl->Release(This)
#define IEnumContextProps_Next(This,celt,pContextProperties,pceltFetched) (This)->lpVtbl->Next(This,celt,pContextProperties,pceltFetched)
#define IEnumContextProps_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumContextProps_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumContextProps_Clone(This,ppEnumContextProps) (This)->lpVtbl->Clone(This,ppEnumContextProps)
#define IEnumContextProps_Count(This,pcelt) (This)->lpVtbl->Count(This,pcelt)
#endif
#endif
  HRESULT WINAPI IEnumContextProps_Next_Proxy(IEnumContextProps *This,ULONG celt,ContextProperty *pContextProperties,ULONG *pceltFetched);
  void __RPC_STUB IEnumContextProps_Next_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumContextProps_Skip_Proxy(IEnumContextProps *This,ULONG celt);
  void __RPC_STUB IEnumContextProps_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumContextProps_Reset_Proxy(IEnumContextProps *This);
  void __RPC_STUB IEnumContextProps_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumContextProps_Clone_Proxy(IEnumContextProps *This,IEnumContextProps **ppEnumContextProps);
  void __RPC_STUB IEnumContextProps_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumContextProps_Count_Proxy(IEnumContextProps *This,ULONG *pcelt);
  void __RPC_STUB IEnumContextProps_Count_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IContext_INTERFACE_DEFINED__
#define __IContext_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IContext;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IContext : public IUnknown {
  public:
    virtual HRESULT WINAPI SetProperty(REFGUID rpolicyId,CPFLAGS flags,IUnknown *pUnk) = 0;
    virtual HRESULT WINAPI RemoveProperty(REFGUID rPolicyId) = 0;
    virtual HRESULT WINAPI GetProperty(REFGUID rGuid,CPFLAGS *pFlags,IUnknown **ppUnk) = 0;
    virtual HRESULT WINAPI EnumContextProps(IEnumContextProps **ppEnumContextProps) = 0;
  };
#else
  typedef struct IContextVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IContext *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IContext *This);
      ULONG (WINAPI *Release)(IContext *This);
      HRESULT (WINAPI *SetProperty)(IContext *This,REFGUID rpolicyId,CPFLAGS flags,IUnknown *pUnk);
      HRESULT (WINAPI *RemoveProperty)(IContext *This,REFGUID rPolicyId);
      HRESULT (WINAPI *GetProperty)(IContext *This,REFGUID rGuid,CPFLAGS *pFlags,IUnknown **ppUnk);
      HRESULT (WINAPI *EnumContextProps)(IContext *This,IEnumContextProps **ppEnumContextProps);
    END_INTERFACE
  } IContextVtbl;
  struct IContext {
    CONST_VTBL struct IContextVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IContext_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IContext_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IContext_Release(This) (This)->lpVtbl->Release(This)
#define IContext_SetProperty(This,rpolicyId,flags,pUnk) (This)->lpVtbl->SetProperty(This,rpolicyId,flags,pUnk)
#define IContext_RemoveProperty(This,rPolicyId) (This)->lpVtbl->RemoveProperty(This,rPolicyId)
#define IContext_GetProperty(This,rGuid,pFlags,ppUnk) (This)->lpVtbl->GetProperty(This,rGuid,pFlags,ppUnk)
#define IContext_EnumContextProps(This,ppEnumContextProps) (This)->lpVtbl->EnumContextProps(This,ppEnumContextProps)
#endif
#endif
  HRESULT WINAPI IContext_SetProperty_Proxy(IContext *This,REFGUID rpolicyId,CPFLAGS flags,IUnknown *pUnk);
  void __RPC_STUB IContext_SetProperty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IContext_RemoveProperty_Proxy(IContext *This,REFGUID rPolicyId);
  void __RPC_STUB IContext_RemoveProperty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IContext_GetProperty_Proxy(IContext *This,REFGUID rGuid,CPFLAGS *pFlags,IUnknown **ppUnk);
  void __RPC_STUB IContext_GetProperty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IContext_EnumContextProps_Proxy(IContext *This,IEnumContextProps **ppEnumContextProps);
  void __RPC_STUB IContext_EnumContextProps_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef BUILDTYPE_COMSVCS
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0086_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0086_v0_0_s_ifspec;

#ifndef __IObjContext_INTERFACE_DEFINED__
#define __IObjContext_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IObjContext;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IObjContext : public IContext {
  public:
    virtual void WINAPI Reserved1(void) = 0;
    virtual void WINAPI Reserved2(void) = 0;
    virtual void WINAPI Reserved3(void) = 0;
    virtual void WINAPI Reserved4(void) = 0;
    virtual void WINAPI Reserved5(void) = 0;
    virtual void WINAPI Reserved6(void) = 0;
    virtual void WINAPI Reserved7(void) = 0;
  };
#else
  typedef struct IObjContextVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IObjContext *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IObjContext *This);
      ULONG (WINAPI *Release)(IObjContext *This);
      HRESULT (WINAPI *SetProperty)(IObjContext *This,REFGUID rpolicyId,CPFLAGS flags,IUnknown *pUnk);
      HRESULT (WINAPI *RemoveProperty)(IObjContext *This,REFGUID rPolicyId);
      HRESULT (WINAPI *GetProperty)(IObjContext *This,REFGUID rGuid,CPFLAGS *pFlags,IUnknown **ppUnk);
      HRESULT (WINAPI *EnumContextProps)(IObjContext *This,IEnumContextProps **ppEnumContextProps);
      void (WINAPI *Reserved1)(IObjContext *This);
      void (WINAPI *Reserved2)(IObjContext *This);
      void (WINAPI *Reserved3)(IObjContext *This);
      void (WINAPI *Reserved4)(IObjContext *This);
      void (WINAPI *Reserved5)(IObjContext *This);
      void (WINAPI *Reserved6)(IObjContext *This);
      void (WINAPI *Reserved7)(IObjContext *This);
    END_INTERFACE
  } IObjContextVtbl;
  struct IObjContext {
    CONST_VTBL struct IObjContextVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IObjContext_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IObjContext_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IObjContext_Release(This) (This)->lpVtbl->Release(This)
#define IObjContext_SetProperty(This,rpolicyId,flags,pUnk) (This)->lpVtbl->SetProperty(This,rpolicyId,flags,pUnk)
#define IObjContext_RemoveProperty(This,rPolicyId) (This)->lpVtbl->RemoveProperty(This,rPolicyId)
#define IObjContext_GetProperty(This,rGuid,pFlags,ppUnk) (This)->lpVtbl->GetProperty(This,rGuid,pFlags,ppUnk)
#define IObjContext_EnumContextProps(This,ppEnumContextProps) (This)->lpVtbl->EnumContextProps(This,ppEnumContextProps)
#define IObjContext_Reserved1(This) (This)->lpVtbl->Reserved1(This)
#define IObjContext_Reserved2(This) (This)->lpVtbl->Reserved2(This)
#define IObjContext_Reserved3(This) (This)->lpVtbl->Reserved3(This)
#define IObjContext_Reserved4(This) (This)->lpVtbl->Reserved4(This)
#define IObjContext_Reserved5(This) (This)->lpVtbl->Reserved5(This)
#define IObjContext_Reserved6(This) (This)->lpVtbl->Reserved6(This)
#define IObjContext_Reserved7(This) (This)->lpVtbl->Reserved7(This)
#endif
#endif
  void WINAPI IObjContext_Reserved1_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved1_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved2_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved2_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved3_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved3_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved4_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved4_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved5_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved5_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved6_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved6_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  void WINAPI IObjContext_Reserved7_Proxy(IObjContext *This);
  void __RPC_STUB IObjContext_Reserved7_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif
#endif
  typedef enum tagApplicationType {
    ServerApplication = 0,LibraryApplication = ServerApplication + 1
  } ApplicationType;

  typedef enum tagShutdownType {
    IdleShutdown = 0,ForcedShutdown = IdleShutdown + 1
  } ShutdownType;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0087_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0087_v0_0_s_ifspec;

#ifndef __IProcessLock_INTERFACE_DEFINED__
#define __IProcessLock_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IProcessLock;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IProcessLock : public IUnknown {
  public:
    virtual ULONG WINAPI AddRefOnProcess(void) = 0;
    virtual ULONG WINAPI ReleaseRefOnProcess(void) = 0;
  };
#else
  typedef struct IProcessLockVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IProcessLock *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IProcessLock *This);
      ULONG (WINAPI *Release)(IProcessLock *This);
      ULONG (WINAPI *AddRefOnProcess)(IProcessLock *This);
      ULONG (WINAPI *ReleaseRefOnProcess)(IProcessLock *This);
    END_INTERFACE
  } IProcessLockVtbl;
  struct IProcessLock {
    CONST_VTBL struct IProcessLockVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IProcessLock_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IProcessLock_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IProcessLock_Release(This) (This)->lpVtbl->Release(This)
#define IProcessLock_AddRefOnProcess(This) (This)->lpVtbl->AddRefOnProcess(This)
#define IProcessLock_ReleaseRefOnProcess(This) (This)->lpVtbl->ReleaseRefOnProcess(This)
#endif
#endif
  ULONG WINAPI IProcessLock_AddRefOnProcess_Proxy(IProcessLock *This);
  void __RPC_STUB IProcessLock_AddRefOnProcess_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  ULONG WINAPI IProcessLock_ReleaseRefOnProcess_Proxy(IProcessLock *This);
  void __RPC_STUB IProcessLock_ReleaseRefOnProcess_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISurrogateService_INTERFACE_DEFINED__
#define __ISurrogateService_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISurrogateService;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISurrogateService : public IUnknown {
  public:
    virtual HRESULT WINAPI Init(REFGUID rguidProcessID,IProcessLock *pProcessLock,WINBOOL *pfApplicationAware) = 0;
    virtual HRESULT WINAPI ApplicationLaunch(REFGUID rguidApplID,ApplicationType appType) = 0;
    virtual HRESULT WINAPI ApplicationFree(REFGUID rguidApplID) = 0;
    virtual HRESULT WINAPI CatalogRefresh(ULONG ulReserved) = 0;
    virtual HRESULT WINAPI ProcessShutdown(ShutdownType shutdownType) = 0;
  };
#else
  typedef struct ISurrogateServiceVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISurrogateService *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISurrogateService *This);
      ULONG (WINAPI *Release)(ISurrogateService *This);
      HRESULT (WINAPI *Init)(ISurrogateService *This,REFGUID rguidProcessID,IProcessLock *pProcessLock,WINBOOL *pfApplicationAware);
      HRESULT (WINAPI *ApplicationLaunch)(ISurrogateService *This,REFGUID rguidApplID,ApplicationType appType);
      HRESULT (WINAPI *ApplicationFree)(ISurrogateService *This,REFGUID rguidApplID);
      HRESULT (WINAPI *CatalogRefresh)(ISurrogateService *This,ULONG ulReserved);
      HRESULT (WINAPI *ProcessShutdown)(ISurrogateService *This,ShutdownType shutdownType);
    END_INTERFACE
  } ISurrogateServiceVtbl;
  struct ISurrogateService {
    CONST_VTBL struct ISurrogateServiceVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISurrogateService_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISurrogateService_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISurrogateService_Release(This) (This)->lpVtbl->Release(This)
#define ISurrogateService_Init(This,rguidProcessID,pProcessLock,pfApplicationAware) (This)->lpVtbl->Init(This,rguidProcessID,pProcessLock,pfApplicationAware)
#define ISurrogateService_ApplicationLaunch(This,rguidApplID,appType) (This)->lpVtbl->ApplicationLaunch(This,rguidApplID,appType)
#define ISurrogateService_ApplicationFree(This,rguidApplID) (This)->lpVtbl->ApplicationFree(This,rguidApplID)
#define ISurrogateService_CatalogRefresh(This,ulReserved) (This)->lpVtbl->CatalogRefresh(This,ulReserved)
#define ISurrogateService_ProcessShutdown(This,shutdownType) (This)->lpVtbl->ProcessShutdown(This,shutdownType)
#endif
#endif
  HRESULT WINAPI ISurrogateService_Init_Proxy(ISurrogateService *This,REFGUID rguidProcessID,IProcessLock *pProcessLock,WINBOOL *pfApplicationAware);
  void __RPC_STUB ISurrogateService_Init_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISurrogateService_ApplicationLaunch_Proxy(ISurrogateService *This,REFGUID rguidApplID,ApplicationType appType);
  void __RPC_STUB ISurrogateService_ApplicationLaunch_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISurrogateService_ApplicationFree_Proxy(ISurrogateService *This,REFGUID rguidApplID);
  void __RPC_STUB ISurrogateService_ApplicationFree_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISurrogateService_CatalogRefresh_Proxy(ISurrogateService *This,ULONG ulReserved);
  void __RPC_STUB ISurrogateService_CatalogRefresh_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISurrogateService_ProcessShutdown_Proxy(ISurrogateService *This,ShutdownType shutdownType);
  void __RPC_STUB ISurrogateService_ProcessShutdown_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  typedef enum _APTTYPE {
    APTTYPE_CURRENT = -1,APTTYPE_STA = 0,APTTYPE_MTA = 1,APTTYPE_NA = 2,APTTYPE_MAINSTA = 3
  } APTTYPE;

  typedef enum _THDTYPE {
    THDTYPE_BLOCKMESSAGES = 0,THDTYPE_PROCESSMESSAGES = 1
  } THDTYPE;

  typedef DWORD APARTMENTID;

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0089_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0089_v0_0_s_ifspec;

#ifndef __IComThreadingInfo_INTERFACE_DEFINED__
#define __IComThreadingInfo_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IComThreadingInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IComThreadingInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetCurrentApartmentType(APTTYPE *pAptType) = 0;
    virtual HRESULT WINAPI GetCurrentThreadType(THDTYPE *pThreadType) = 0;
    virtual HRESULT WINAPI GetCurrentLogicalThreadId(GUID *pguidLogicalThreadId) = 0;
    virtual HRESULT WINAPI SetCurrentLogicalThreadId(REFGUID rguid) = 0;
  };
#else
  typedef struct IComThreadingInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IComThreadingInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IComThreadingInfo *This);
      ULONG (WINAPI *Release)(IComThreadingInfo *This);
      HRESULT (WINAPI *GetCurrentApartmentType)(IComThreadingInfo *This,APTTYPE *pAptType);
      HRESULT (WINAPI *GetCurrentThreadType)(IComThreadingInfo *This,THDTYPE *pThreadType);
      HRESULT (WINAPI *GetCurrentLogicalThreadId)(IComThreadingInfo *This,GUID *pguidLogicalThreadId);
      HRESULT (WINAPI *SetCurrentLogicalThreadId)(IComThreadingInfo *This,REFGUID rguid);
    END_INTERFACE
  } IComThreadingInfoVtbl;
  struct IComThreadingInfo {
    CONST_VTBL struct IComThreadingInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IComThreadingInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IComThreadingInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IComThreadingInfo_Release(This) (This)->lpVtbl->Release(This)
#define IComThreadingInfo_GetCurrentApartmentType(This,pAptType) (This)->lpVtbl->GetCurrentApartmentType(This,pAptType)
#define IComThreadingInfo_GetCurrentThreadType(This,pThreadType) (This)->lpVtbl->GetCurrentThreadType(This,pThreadType)
#define IComThreadingInfo_GetCurrentLogicalThreadId(This,pguidLogicalThreadId) (This)->lpVtbl->GetCurrentLogicalThreadId(This,pguidLogicalThreadId)
#define IComThreadingInfo_SetCurrentLogicalThreadId(This,rguid) (This)->lpVtbl->SetCurrentLogicalThreadId(This,rguid)
#endif
#endif
  HRESULT WINAPI IComThreadingInfo_GetCurrentApartmentType_Proxy(IComThreadingInfo *This,APTTYPE *pAptType);
  void __RPC_STUB IComThreadingInfo_GetCurrentApartmentType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IComThreadingInfo_GetCurrentThreadType_Proxy(IComThreadingInfo *This,THDTYPE *pThreadType);
  void __RPC_STUB IComThreadingInfo_GetCurrentThreadType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IComThreadingInfo_GetCurrentLogicalThreadId_Proxy(IComThreadingInfo *This,GUID *pguidLogicalThreadId);
  void __RPC_STUB IComThreadingInfo_GetCurrentLogicalThreadId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IComThreadingInfo_SetCurrentLogicalThreadId_Proxy(IComThreadingInfo *This,REFGUID rguid);
  void __RPC_STUB IComThreadingInfo_SetCurrentLogicalThreadId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IProcessInitControl_INTERFACE_DEFINED__
#define __IProcessInitControl_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IProcessInitControl;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IProcessInitControl : public IUnknown {
  public:
    virtual HRESULT WINAPI ResetInitializerTimeout(DWORD dwSecondsRemaining) = 0;
  };
#else
  typedef struct IProcessInitControlVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IProcessInitControl *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IProcessInitControl *This);
      ULONG (WINAPI *Release)(IProcessInitControl *This);
      HRESULT (WINAPI *ResetInitializerTimeout)(IProcessInitControl *This,DWORD dwSecondsRemaining);
    END_INTERFACE
  } IProcessInitControlVtbl;
  struct IProcessInitControl {
    CONST_VTBL struct IProcessInitControlVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IProcessInitControl_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IProcessInitControl_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IProcessInitControl_Release(This) (This)->lpVtbl->Release(This)
#define IProcessInitControl_ResetInitializerTimeout(This,dwSecondsRemaining) (This)->lpVtbl->ResetInitializerTimeout(This,dwSecondsRemaining)
#endif
#endif
  HRESULT WINAPI IProcessInitControl_ResetInitializerTimeout_Proxy(IProcessInitControl *This,DWORD dwSecondsRemaining);
  void __RPC_STUB IProcessInitControl_ResetInitializerTimeout_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0091_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0091_v0_0_s_ifspec;

#ifndef __IInitializeSpy_INTERFACE_DEFINED__
#define __IInitializeSpy_INTERFACE_DEFINED__
  typedef IInitializeSpy *LPINITIALIZESPY;

  EXTERN_C const IID IID_IInitializeSpy;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInitializeSpy : public IUnknown {
  public:
    virtual HRESULT WINAPI PreInitialize(DWORD dwCoInit,DWORD dwCurThreadAptRefs) = 0;
    virtual HRESULT WINAPI PostInitialize(HRESULT hrCoInit,DWORD dwCoInit,DWORD dwNewThreadAptRefs) = 0;
    virtual HRESULT WINAPI PreUninitialize(DWORD dwCurThreadAptRefs) = 0;
    virtual HRESULT WINAPI PostUninitialize(DWORD dwNewThreadAptRefs) = 0;
  };
#else
  typedef struct IInitializeSpyVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInitializeSpy *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInitializeSpy *This);
      ULONG (WINAPI *Release)(IInitializeSpy *This);
      HRESULT (WINAPI *PreInitialize)(IInitializeSpy *This,DWORD dwCoInit,DWORD dwCurThreadAptRefs);
      HRESULT (WINAPI *PostInitialize)(IInitializeSpy *This,HRESULT hrCoInit,DWORD dwCoInit,DWORD dwNewThreadAptRefs);
      HRESULT (WINAPI *PreUninitialize)(IInitializeSpy *This,DWORD dwCurThreadAptRefs);
      HRESULT (WINAPI *PostUninitialize)(IInitializeSpy *This,DWORD dwNewThreadAptRefs);
    END_INTERFACE
  } IInitializeSpyVtbl;
  struct IInitializeSpy {
    CONST_VTBL struct IInitializeSpyVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInitializeSpy_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInitializeSpy_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInitializeSpy_Release(This) (This)->lpVtbl->Release(This)
#define IInitializeSpy_PreInitialize(This,dwCoInit,dwCurThreadAptRefs) (This)->lpVtbl->PreInitialize(This,dwCoInit,dwCurThreadAptRefs)
#define IInitializeSpy_PostInitialize(This,hrCoInit,dwCoInit,dwNewThreadAptRefs) (This)->lpVtbl->PostInitialize(This,hrCoInit,dwCoInit,dwNewThreadAptRefs)
#define IInitializeSpy_PreUninitialize(This,dwCurThreadAptRefs) (This)->lpVtbl->PreUninitialize(This,dwCurThreadAptRefs)
#define IInitializeSpy_PostUninitialize(This,dwNewThreadAptRefs) (This)->lpVtbl->PostUninitialize(This,dwNewThreadAptRefs)
#endif
#endif
  HRESULT WINAPI IInitializeSpy_PreInitialize_Proxy(IInitializeSpy *This,DWORD dwCoInit,DWORD dwCurThreadAptRefs);
  void __RPC_STUB IInitializeSpy_PreInitialize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInitializeSpy_PostInitialize_Proxy(IInitializeSpy *This,HRESULT hrCoInit,DWORD dwCoInit,DWORD dwNewThreadAptRefs);
  void __RPC_STUB IInitializeSpy_PostInitialize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInitializeSpy_PreUninitialize_Proxy(IInitializeSpy *This,DWORD dwCurThreadAptRefs);
  void __RPC_STUB IInitializeSpy_PreUninitialize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInitializeSpy_PostUninitialize_Proxy(IInitializeSpy *This,DWORD dwNewThreadAptRefs);
  void __RPC_STUB IInitializeSpy_PostUninitialize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_objidl_0092_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_objidl_0092_v0_0_s_ifspec;

  unsigned long __RPC_API ASYNC_STGMEDIUM_UserSize(unsigned long *,unsigned long,ASYNC_STGMEDIUM *);
  unsigned char *__RPC_API ASYNC_STGMEDIUM_UserMarshal(unsigned long *,unsigned char *,ASYNC_STGMEDIUM *);
  unsigned char *__RPC_API ASYNC_STGMEDIUM_UserUnmarshal(unsigned long *,unsigned char *,ASYNC_STGMEDIUM *);
  void __RPC_API ASYNC_STGMEDIUM_UserFree(unsigned long *,ASYNC_STGMEDIUM *);
  unsigned long __RPC_API CLIPFORMAT_UserSize(unsigned long *,unsigned long,CLIPFORMAT *);
  unsigned char *__RPC_API CLIPFORMAT_UserMarshal(unsigned long *,unsigned char *,CLIPFORMAT *);
  unsigned char *__RPC_API CLIPFORMAT_UserUnmarshal(unsigned long *,unsigned char *,CLIPFORMAT *);
  void __RPC_API CLIPFORMAT_UserFree(unsigned long *,CLIPFORMAT *);
  unsigned long __RPC_API FLAG_STGMEDIUM_UserSize(unsigned long *,unsigned long,FLAG_STGMEDIUM *);
  unsigned char *__RPC_API FLAG_STGMEDIUM_UserMarshal(unsigned long *,unsigned char *,FLAG_STGMEDIUM *);
  unsigned char *__RPC_API FLAG_STGMEDIUM_UserUnmarshal(unsigned long *,unsigned char *,FLAG_STGMEDIUM *);
  void __RPC_API FLAG_STGMEDIUM_UserFree(unsigned long *,FLAG_STGMEDIUM *);
  unsigned long __RPC_API HBITMAP_UserSize(unsigned long *,unsigned long,HBITMAP *);
  unsigned char *__RPC_API HBITMAP_UserMarshal(unsigned long *,unsigned char *,HBITMAP *);
  unsigned char *__RPC_API HBITMAP_UserUnmarshal(unsigned long *,unsigned char *,HBITMAP *);
  void __RPC_API HBITMAP_UserFree(unsigned long *,HBITMAP *);
  unsigned long __RPC_API HDC_UserSize(unsigned long *,unsigned long,HDC *);
  unsigned char *__RPC_API HDC_UserMarshal(unsigned long *,unsigned char *,HDC *);
  unsigned char *__RPC_API HDC_UserUnmarshal(unsigned long *,unsigned char *,HDC *);
  void __RPC_API HDC_UserFree(unsigned long *,HDC *);
  unsigned long __RPC_API HICON_UserSize(unsigned long *,unsigned long,HICON *);
  unsigned char *__RPC_API HICON_UserMarshal(unsigned long *,unsigned char *,HICON *);
  unsigned char *__RPC_API HICON_UserUnmarshal(unsigned long *,unsigned char *,HICON *);
  void __RPC_API HICON_UserFree(unsigned long *,HICON *);
  unsigned long __RPC_API SNB_UserSize(unsigned long *,unsigned long,SNB *);
  unsigned char *__RPC_API SNB_UserMarshal(unsigned long *,unsigned char *,SNB *);
  unsigned char *__RPC_API SNB_UserUnmarshal(unsigned long *,unsigned char *,SNB *);
  void __RPC_API SNB_UserFree(unsigned long *,SNB *);
  unsigned long __RPC_API STGMEDIUM_UserSize(unsigned long *,unsigned long,STGMEDIUM *);
  unsigned char *__RPC_API STGMEDIUM_UserMarshal(unsigned long *,unsigned char *,STGMEDIUM *);
  unsigned char *__RPC_API STGMEDIUM_UserUnmarshal(unsigned long *,unsigned char *,STGMEDIUM *);
  void __RPC_API STGMEDIUM_UserFree(unsigned long *,STGMEDIUM *);

  HRESULT WINAPI IEnumUnknown_Next_Proxy(IEnumUnknown *This,ULONG celt,IUnknown **rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumUnknown_Next_Stub(IEnumUnknown *This,ULONG celt,IUnknown **rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IBindCtx_SetBindOptions_Proxy(IBindCtx *This,BIND_OPTS *pbindopts);
  HRESULT WINAPI IBindCtx_SetBindOptions_Stub(IBindCtx *This,BIND_OPTS2 *pbindopts);
  HRESULT WINAPI IBindCtx_GetBindOptions_Proxy(IBindCtx *This,BIND_OPTS *pbindopts);
  HRESULT WINAPI IBindCtx_GetBindOptions_Stub(IBindCtx *This,BIND_OPTS2 *pbindopts);
  HRESULT WINAPI IEnumMoniker_Next_Proxy(IEnumMoniker *This,ULONG celt,IMoniker **rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumMoniker_Next_Stub(IEnumMoniker *This,ULONG celt,IMoniker **rgelt,ULONG *pceltFetched);
  WINBOOL WINAPI IRunnableObject_IsRunning_Proxy(IRunnableObject *This);
  HRESULT WINAPI IRunnableObject_IsRunning_Stub(IRunnableObject *This);
  HRESULT WINAPI IMoniker_BindToObject_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riidResult,void **ppvResult);
  HRESULT WINAPI IMoniker_BindToObject_Stub(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riidResult,IUnknown **ppvResult);
  HRESULT WINAPI IMoniker_BindToStorage_Proxy(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riid,void **ppvObj);
  HRESULT WINAPI IMoniker_BindToStorage_Stub(IMoniker *This,IBindCtx *pbc,IMoniker *pmkToLeft,REFIID riid,IUnknown **ppvObj);
  HRESULT WINAPI IEnumString_Next_Proxy(IEnumString *This,ULONG celt,LPOLESTR *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumString_Next_Stub(IEnumString *This,ULONG celt,LPOLESTR *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI ISequentialStream_Read_Proxy(ISequentialStream *This,void *pv,ULONG cb,ULONG *pcbRead);
  HRESULT WINAPI ISequentialStream_Read_Stub(ISequentialStream *This,byte *pv,ULONG cb,ULONG *pcbRead);
  HRESULT WINAPI ISequentialStream_Write_Proxy(ISequentialStream *This,const void *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI ISequentialStream_Write_Stub(ISequentialStream *This,const byte *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI IStream_Seek_Proxy(IStream *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
  HRESULT WINAPI IStream_Seek_Stub(IStream *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
  HRESULT WINAPI IStream_CopyTo_Proxy(IStream *This,IStream *pstm,ULARGE_INTEGER cb,ULARGE_INTEGER *pcbRead,ULARGE_INTEGER *pcbWritten);
  HRESULT WINAPI IStream_CopyTo_Stub(IStream *This,IStream *pstm,ULARGE_INTEGER cb,ULARGE_INTEGER *pcbRead,ULARGE_INTEGER *pcbWritten);
  HRESULT WINAPI IEnumSTATSTG_Next_Proxy(IEnumSTATSTG *This,ULONG celt,STATSTG *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATSTG_Next_Stub(IEnumSTATSTG *This,ULONG celt,STATSTG *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IStorage_OpenStream_Proxy(IStorage *This,const OLECHAR *pwcsName,void *reserved1,DWORD grfMode,DWORD reserved2,IStream **ppstm);
  HRESULT WINAPI IStorage_OpenStream_Stub(IStorage *This,const OLECHAR *pwcsName,unsigned long cbReserved1,byte *reserved1,DWORD grfMode,DWORD reserved2,IStream **ppstm);
  HRESULT WINAPI IStorage_EnumElements_Proxy(IStorage *This,DWORD reserved1,void *reserved2,DWORD reserved3,IEnumSTATSTG **ppenum);
  HRESULT WINAPI IStorage_EnumElements_Stub(IStorage *This,DWORD reserved1,unsigned long cbReserved2,byte *reserved2,DWORD reserved3,IEnumSTATSTG **ppenum);
  HRESULT WINAPI ILockBytes_ReadAt_Proxy(ILockBytes *This,ULARGE_INTEGER ulOffset,void *pv,ULONG cb,ULONG *pcbRead);
  HRESULT WINAPI ILockBytes_ReadAt_Stub(ILockBytes *This,ULARGE_INTEGER ulOffset,byte *pv,ULONG cb,ULONG *pcbRead);
  HRESULT WINAPI ILockBytes_WriteAt_Proxy(ILockBytes *This,ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI ILockBytes_WriteAt_Stub(ILockBytes *This,ULARGE_INTEGER ulOffset,const byte *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI IEnumFORMATETC_Next_Proxy(IEnumFORMATETC *This,ULONG celt,FORMATETC *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumFORMATETC_Next_Stub(IEnumFORMATETC *This,ULONG celt,FORMATETC *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATDATA_Next_Proxy(IEnumSTATDATA *This,ULONG celt,STATDATA *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATDATA_Next_Stub(IEnumSTATDATA *This,ULONG celt,STATDATA *rgelt,ULONG *pceltFetched);
  void WINAPI IAdviseSink_OnDataChange_Proxy(IAdviseSink *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
  HRESULT WINAPI IAdviseSink_OnDataChange_Stub(IAdviseSink *This,FORMATETC *pFormatetc,ASYNC_STGMEDIUM *pStgmed);
  void WINAPI IAdviseSink_OnViewChange_Proxy(IAdviseSink *This,DWORD dwAspect,LONG lindex);
  HRESULT WINAPI IAdviseSink_OnViewChange_Stub(IAdviseSink *This,DWORD dwAspect,LONG lindex);
  void WINAPI IAdviseSink_OnRename_Proxy(IAdviseSink *This,IMoniker *pmk);
  HRESULT WINAPI IAdviseSink_OnRename_Stub(IAdviseSink *This,IMoniker *pmk);
  void WINAPI IAdviseSink_OnSave_Proxy(IAdviseSink *This);
  HRESULT WINAPI IAdviseSink_OnSave_Stub(IAdviseSink *This);
  void WINAPI IAdviseSink_OnClose_Proxy(IAdviseSink *This);
  HRESULT WINAPI IAdviseSink_OnClose_Stub(IAdviseSink *This);
  void WINAPI IAdviseSink2_OnLinkSrcChange_Proxy(IAdviseSink2 *This,IMoniker *pmk);
  HRESULT WINAPI IAdviseSink2_OnLinkSrcChange_Stub(IAdviseSink2 *This,IMoniker *pmk);
  HRESULT WINAPI IDataObject_GetData_Proxy(IDataObject *This,FORMATETC *pformatetcIn,STGMEDIUM *pmedium);
  HRESULT WINAPI IDataObject_GetData_Stub(IDataObject *This,FORMATETC *pformatetcIn,STGMEDIUM *pRemoteMedium);
  HRESULT WINAPI IDataObject_GetDataHere_Proxy(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pmedium);
  HRESULT WINAPI IDataObject_GetDataHere_Stub(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pRemoteMedium);
  HRESULT WINAPI IDataObject_SetData_Proxy(IDataObject *This,FORMATETC *pformatetc,STGMEDIUM *pmedium,WINBOOL fRelease);
  HRESULT WINAPI IDataObject_SetData_Stub(IDataObject *This,FORMATETC *pformatetc,FLAG_STGMEDIUM *pmedium,WINBOOL fRelease);
  HRESULT WINAPI IFillLockBytes_FillAppend_Proxy(IFillLockBytes *This,const void *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI IFillLockBytes_FillAppend_Stub(IFillLockBytes *This,const byte *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI IFillLockBytes_FillAt_Proxy(IFillLockBytes *This,ULARGE_INTEGER ulOffset,const void *pv,ULONG cb,ULONG *pcbWritten);
  HRESULT WINAPI IFillLockBytes_FillAt_Stub(IFillLockBytes *This,ULARGE_INTEGER ulOffset,const byte *pv,ULONG cb,ULONG *pcbWritten);
  void WINAPI AsyncIAdviseSink_Begin_OnDataChange_Proxy(AsyncIAdviseSink *This,FORMATETC *pFormatetc,STGMEDIUM *pStgmed);
  HRESULT WINAPI AsyncIAdviseSink_Begin_OnDataChange_Stub(AsyncIAdviseSink *This,FORMATETC *pFormatetc,ASYNC_STGMEDIUM *pStgmed);
  void WINAPI AsyncIAdviseSink_Finish_OnDataChange_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Finish_OnDataChange_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Begin_OnViewChange_Proxy(AsyncIAdviseSink *This,DWORD dwAspect,LONG lindex);
  HRESULT WINAPI AsyncIAdviseSink_Begin_OnViewChange_Stub(AsyncIAdviseSink *This,DWORD dwAspect,LONG lindex);
  void WINAPI AsyncIAdviseSink_Finish_OnViewChange_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Finish_OnViewChange_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Begin_OnRename_Proxy(AsyncIAdviseSink *This,IMoniker *pmk);
  HRESULT WINAPI AsyncIAdviseSink_Begin_OnRename_Stub(AsyncIAdviseSink *This,IMoniker *pmk);
  void WINAPI AsyncIAdviseSink_Finish_OnRename_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Finish_OnRename_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Begin_OnSave_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Begin_OnSave_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Finish_OnSave_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Finish_OnSave_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Begin_OnClose_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Begin_OnClose_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink_Finish_OnClose_Proxy(AsyncIAdviseSink *This);
  HRESULT WINAPI AsyncIAdviseSink_Finish_OnClose_Stub(AsyncIAdviseSink *This);
  void WINAPI AsyncIAdviseSink2_Begin_OnLinkSrcChange_Proxy(AsyncIAdviseSink2 *This,IMoniker *pmk);
  HRESULT WINAPI AsyncIAdviseSink2_Begin_OnLinkSrcChange_Stub(AsyncIAdviseSink2 *This,IMoniker *pmk);
  void WINAPI AsyncIAdviseSink2_Finish_OnLinkSrcChange_Proxy(AsyncIAdviseSink2 *This);
  HRESULT WINAPI AsyncIAdviseSink2_Finish_OnLinkSrcChange_Stub(AsyncIAdviseSink2 *This);

#ifdef __cplusplus
}
#endif
#endif
