/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#include <rpc.h>
#include <rpcndr.h>

#ifndef _OBJBASE_H_
#define _OBJBASE_H_

#include <pshpack8.h>

#define WINOLEAPI EXTERN_C DECLSPEC_IMPORT HRESULT WINAPI
#define WINOLEAPI_(type) EXTERN_C DECLSPEC_IMPORT type WINAPI

#if defined(__cplusplus) && !defined(CINTERFACE)

#ifndef __OBJC__
#define interface struct
#endif

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

#if !defined(BEGIN_INTERFACE)
#define BEGIN_INTERFACE
#define END_INTERFACE
#endif
#else

#ifndef __OBJC__
#define interface struct
#endif

#define STDMETHOD(method) HRESULT (WINAPI *method)
#define STDMETHOD_(type,method) type (WINAPI *method)
#define STDMETHODV(method) HRESULT (STDMETHODVCALLTYPE *method)
#define STDMETHODV_(type,method) type (STDMETHODVCALLTYPE *method)

#if !defined(BEGIN_INTERFACE)
#define BEGIN_INTERFACE
#define END_INTERFACE
#endif

#define PURE
#define THIS_ INTERFACE *This,
#define THIS INTERFACE *This
#ifdef CONST_VTABLE
#undef CONST_VTBL
#define CONST_VTBL const
#define DECLARE_INTERFACE(iface) typedef struct iface { const struct iface##Vtbl *lpVtbl; } iface; typedef const struct iface##Vtbl iface##Vtbl; const struct iface##Vtbl
#else
#undef CONST_VTBL
#define CONST_VTBL
#define DECLARE_INTERFACE(iface) typedef struct iface { struct iface##Vtbl *lpVtbl; } iface; typedef struct iface##Vtbl iface##Vtbl; struct iface##Vtbl
#endif
#define DECLARE_INTERFACE_(iface,baseiface) DECLARE_INTERFACE(iface)
#endif

#ifndef FARSTRUCT
#define FARSTRUCT
#endif

#ifndef HUGEP
#define HUGEP
#endif

#include <stdlib.h>

#define LISet32(li,v) ((li).HighPart = ((LONG) (v)) < 0 ? -1 : 0,(li).LowPart = (v))
#define ULISet32(li,v) ((li).HighPart = 0,(li).LowPart = (v))
#define CLSCTX_INPROC (CLSCTX_INPROC_SERVER|CLSCTX_INPROC_HANDLER)

#define CLSCTX_ALL (CLSCTX_INPROC_SERVER| CLSCTX_INPROC_HANDLER| CLSCTX_LOCAL_SERVER| CLSCTX_REMOTE_SERVER)
#define CLSCTX_SERVER (CLSCTX_INPROC_SERVER|CLSCTX_LOCAL_SERVER|CLSCTX_REMOTE_SERVER)

typedef enum tagREGCLS {
  REGCLS_SINGLEUSE = 0,REGCLS_MULTIPLEUSE = 1,REGCLS_MULTI_SEPARATE = 2,REGCLS_SUSPENDED = 4,REGCLS_SURROGATE = 8
} REGCLS;

#define MARSHALINTERFACE_MIN 500

#define CWCSTORAGENAME 32

#define STGM_DIRECT 0x00000000L
#define STGM_TRANSACTED 0x00010000L
#define STGM_SIMPLE 0x08000000L

#define STGM_READ 0x00000000L
#define STGM_WRITE 0x00000001L
#define STGM_READWRITE 0x00000002L

#define STGM_SHARE_DENY_NONE 0x00000040L
#define STGM_SHARE_DENY_READ 0x00000030L
#define STGM_SHARE_DENY_WRITE 0x00000020L
#define STGM_SHARE_EXCLUSIVE 0x00000010L

#define STGM_PRIORITY 0x00040000L
#define STGM_DELETEONRELEASE 0x04000000L
#define STGM_NOSCRATCH 0x00100000L
#define STGM_CREATE 0x00001000L
#define STGM_CONVERT 0x00020000L
#define STGM_FAILIFTHERE 0x00000000L
#define STGM_NOSNAPSHOT 0x00200000L
#define STGM_DIRECT_SWMR 0x00400000L

#define ASYNC_MODE_COMPATIBILITY 0x00000001L
#define ASYNC_MODE_DEFAULT 0x00000000L

#define STGTY_REPEAT 0x00000100L
#define STG_TOEND 0xFFFFFFFFL

#define STG_LAYOUT_SEQUENTIAL 0x00000000L
#define STG_LAYOUT_INTERLEAVED 0x00000001L

#define STGFMT_STORAGE 0
#define STGFMT_NATIVE 1
#define STGFMT_FILE 3
#define STGFMT_ANY 4
#define STGFMT_DOCFILE 5

#define STGFMT_DOCUMENT 0

#ifndef __IRpcStubBuffer_FWD_DEFINED__
#define __IRpcStubBuffer_FWD_DEFINED__
typedef struct IRpcStubBuffer IRpcStubBuffer;
#endif
#ifndef __IRpcChannelBuffer_FWD_DEFINED__
#define __IRpcChannelBuffer_FWD_DEFINED__
typedef struct IRpcChannelBuffer IRpcChannelBuffer;
#endif

#include <wtypes.h>
#include <unknwn.h>
#include <objidl.h>

#include <guiddef.h>

#ifndef INITGUID
#include <cguid.h>
#endif

typedef enum tagCOINIT {
  COINIT_APARTMENTTHREADED = 0x2,
  COINIT_MULTITHREADED = 0x3,
  COINIT_DISABLE_OLE1DDE = 0x4,
  COINIT_SPEED_OVER_MEMORY = 0x8
} COINIT;

WINOLEAPI_(DWORD) CoBuildVersion(VOID);
WINOLEAPI CoInitialize(LPVOID pvReserved);
WINOLEAPI_(void) CoUninitialize(void);
WINOLEAPI CoGetMalloc(DWORD dwMemContext,LPMALLOC *ppMalloc);
WINOLEAPI_(DWORD) CoGetCurrentProcess(void);
WINOLEAPI CoRegisterMallocSpy(LPMALLOCSPY pMallocSpy);
WINOLEAPI CoRevokeMallocSpy(void);
WINOLEAPI CoCreateStandardMalloc(DWORD memctx,IMalloc **ppMalloc);
WINOLEAPI CoInitializeEx(LPVOID pvReserved,DWORD dwCoInit);
WINOLEAPI CoGetCallerTID(LPDWORD lpdwTID);
WINOLEAPI CoRegisterInitializeSpy(LPINITIALIZESPY pSpy,ULARGE_INTEGER *puliCookie);
WINOLEAPI CoRevokeInitializeSpy(ULARGE_INTEGER uliCookie);
WINOLEAPI CoGetContextToken(ULONG_PTR *pToken);

typedef enum tagCOMSD {
  SD_LAUNCHPERMISSIONS = 0,SD_ACCESSPERMISSIONS = 1,SD_LAUNCHRESTRICTIONS = 2,SD_ACCESSRESTRICTIONS = 3
} COMSD;

WINOLEAPI CoGetSystemSecurityPermissions(COMSD comSDType,PSECURITY_DESCRIPTOR *ppSD);

typedef struct tagSOleTlsDataPublic {
  void *pvReserved0[2];
  DWORD dwReserved0[3];
  void *pvReserved1[1];
  DWORD dwReserved1[3];
  void *pvReserved2[4];
  DWORD dwReserved2[1];
  void *pCurrentCtx;
} SOleTlsDataPublic;

WINOLEAPI CoGetObjectContext(REFIID riid,LPVOID *ppv);
WINOLEAPI CoGetClassObject(REFCLSID rclsid,DWORD dwClsContext,LPVOID pvReserved,REFIID riid,LPVOID *ppv);
WINOLEAPI CoRegisterClassObject(REFCLSID rclsid,LPUNKNOWN pUnk,DWORD dwClsContext,DWORD flags,LPDWORD lpdwRegister);
WINOLEAPI CoRevokeClassObject(DWORD dwRegister);
WINOLEAPI CoResumeClassObjects(void);
WINOLEAPI CoSuspendClassObjects(void);
WINOLEAPI_(ULONG) CoAddRefServerProcess(void);
WINOLEAPI_(ULONG) CoReleaseServerProcess(void);
WINOLEAPI CoGetPSClsid(REFIID riid,CLSID *pClsid);
WINOLEAPI CoRegisterPSClsid(REFIID riid,REFCLSID rclsid);
WINOLEAPI CoRegisterSurrogate(LPSURROGATE pSurrogate);
WINOLEAPI CoGetMarshalSizeMax(ULONG *pulSize,REFIID riid,LPUNKNOWN pUnk,DWORD dwDestContext,LPVOID pvDestContext,DWORD mshlflags);
WINOLEAPI CoMarshalInterface(LPSTREAM pStm,REFIID riid,LPUNKNOWN pUnk,DWORD dwDestContext,LPVOID pvDestContext,DWORD mshlflags);
WINOLEAPI CoUnmarshalInterface(LPSTREAM pStm,REFIID riid,LPVOID *ppv);
WINOLEAPI CoMarshalHresult(LPSTREAM pstm,HRESULT hresult);
WINOLEAPI CoUnmarshalHresult(LPSTREAM pstm,HRESULT *phresult);
WINOLEAPI CoReleaseMarshalData(LPSTREAM pStm);
WINOLEAPI CoDisconnectObject(LPUNKNOWN pUnk,DWORD dwReserved);
WINOLEAPI CoLockObjectExternal(LPUNKNOWN pUnk,WINBOOL fLock,WINBOOL fLastUnlockReleases);
WINOLEAPI CoGetStandardMarshal(REFIID riid,LPUNKNOWN pUnk,DWORD dwDestContext,LPVOID pvDestContext,DWORD mshlflags,LPMARSHAL *ppMarshal);
WINOLEAPI CoGetStdMarshalEx(LPUNKNOWN pUnkOuter,DWORD smexflags,LPUNKNOWN *ppUnkInner);

typedef enum tagSTDMSHLFLAGS {
  SMEXF_SERVER = 0x01,SMEXF_HANDLER = 0x02
} STDMSHLFLAGS;

WINOLEAPI_(WINBOOL) CoIsHandlerConnected(LPUNKNOWN pUnk);
WINOLEAPI CoMarshalInterThreadInterfaceInStream(REFIID riid,LPUNKNOWN pUnk,LPSTREAM *ppStm);
WINOLEAPI CoGetInterfaceAndReleaseStream(LPSTREAM pStm,REFIID iid,LPVOID *ppv);
WINOLEAPI CoCreateFreeThreadedMarshaler(LPUNKNOWN punkOuter,LPUNKNOWN *ppunkMarshal);
WINOLEAPI_(HINSTANCE) CoLoadLibrary(LPOLESTR lpszLibName,WINBOOL bAutoFree);
WINOLEAPI_(void) CoFreeLibrary(HINSTANCE hInst);
WINOLEAPI_(void) CoFreeAllLibraries(void);
WINOLEAPI_(void) CoFreeUnusedLibraries(void);
WINOLEAPI_(void) CoFreeUnusedLibrariesEx(DWORD dwUnloadDelay,DWORD dwReserved);
WINOLEAPI CoInitializeSecurity(PSECURITY_DESCRIPTOR pSecDesc,LONG cAuthSvc,SOLE_AUTHENTICATION_SERVICE *asAuthSvc,void *pReserved1,DWORD dwAuthnLevel,DWORD dwImpLevel,void *pAuthList,DWORD dwCapabilities,void *pReserved3);
WINOLEAPI CoGetCallContext(REFIID riid,void **ppInterface);
WINOLEAPI CoQueryProxyBlanket(IUnknown *pProxy,DWORD *pwAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,RPC_AUTH_IDENTITY_HANDLE *pAuthInfo,DWORD *pCapabilites);
WINOLEAPI CoSetProxyBlanket(IUnknown *pProxy,DWORD dwAuthnSvc,DWORD dwAuthzSvc,OLECHAR *pServerPrincName,DWORD dwAuthnLevel,DWORD dwImpLevel,RPC_AUTH_IDENTITY_HANDLE pAuthInfo,DWORD dwCapabilities);
WINOLEAPI CoCopyProxy(IUnknown *pProxy,IUnknown **ppCopy);
WINOLEAPI CoQueryClientBlanket(DWORD *pAuthnSvc,DWORD *pAuthzSvc,OLECHAR **pServerPrincName,DWORD *pAuthnLevel,DWORD *pImpLevel,RPC_AUTHZ_HANDLE *pPrivs,DWORD *pCapabilities);
WINOLEAPI CoImpersonateClient();
WINOLEAPI CoRevertToSelf();
WINOLEAPI CoQueryAuthenticationServices(DWORD *pcAuthSvc,SOLE_AUTHENTICATION_SERVICE **asAuthSvc);
WINOLEAPI CoSwitchCallContext(IUnknown *pNewObject,IUnknown **ppOldObject);

#define COM_RIGHTS_EXECUTE 1
#define COM_RIGHTS_EXECUTE_LOCAL 2
#define COM_RIGHTS_EXECUTE_REMOTE 4
#define COM_RIGHTS_ACTIVATE_LOCAL 8
#define COM_RIGHTS_ACTIVATE_REMOTE 16

WINOLEAPI CoCreateInstance(REFCLSID rclsid,LPUNKNOWN pUnkOuter,DWORD dwClsContext,REFIID riid,LPVOID *ppv);
WINOLEAPI CoGetInstanceFromFile(COSERVERINFO *pServerInfo,CLSID *pClsid,IUnknown *punkOuter,DWORD dwClsCtx,DWORD grfMode,OLECHAR *pwszName,DWORD dwCount,MULTI_QI *pResults);
WINOLEAPI CoGetInstanceFromIStorage(COSERVERINFO *pServerInfo,CLSID *pClsid,IUnknown *punkOuter,DWORD dwClsCtx,struct IStorage *pstg,DWORD dwCount,MULTI_QI *pResults);
WINOLEAPI CoCreateInstanceEx(REFCLSID Clsid,IUnknown *punkOuter,DWORD dwClsCtx,COSERVERINFO *pServerInfo,DWORD dwCount,MULTI_QI *pResults);
WINOLEAPI CoGetCancelObject(DWORD dwThreadId,REFIID iid,void **ppUnk);
WINOLEAPI CoSetCancelObject(IUnknown *pUnk);
WINOLEAPI CoCancelCall(DWORD dwThreadId,ULONG ulTimeout);
WINOLEAPI CoTestCancel();
WINOLEAPI CoEnableCallCancellation(LPVOID pReserved);
WINOLEAPI CoDisableCallCancellation(LPVOID pReserved);
WINOLEAPI CoAllowSetForegroundWindow(IUnknown *pUnk,LPVOID lpvReserved);
WINOLEAPI DcomChannelSetHResult(LPVOID pvReserved,ULONG *pulReserved,HRESULT appsHR);
WINOLEAPI StringFromCLSID(REFCLSID rclsid,LPOLESTR *lplpsz);
WINOLEAPI CLSIDFromString(LPOLESTR lpsz,LPCLSID pclsid);
WINOLEAPI StringFromIID(REFIID rclsid,LPOLESTR *lplpsz);
WINOLEAPI IIDFromString(LPOLESTR lpsz,LPIID lpiid);
WINOLEAPI_(WINBOOL) CoIsOle1Class(REFCLSID rclsid);
WINOLEAPI ProgIDFromCLSID (REFCLSID clsid,LPOLESTR *lplpszProgID);
WINOLEAPI CLSIDFromProgID (LPCOLESTR lpszProgID,LPCLSID lpclsid);
WINOLEAPI CLSIDFromProgIDEx (LPCOLESTR lpszProgID,LPCLSID lpclsid);
WINOLEAPI_(int) StringFromGUID2(REFGUID rguid,LPOLESTR lpsz,int cchMax);
WINOLEAPI CoCreateGuid(GUID *pguid);
WINOLEAPI_(WINBOOL) CoFileTimeToDosDateTime(FILETIME *lpFileTime,LPWORD lpDosDate,LPWORD lpDosTime);
WINOLEAPI_(WINBOOL) CoDosDateTimeToFileTime(WORD nDosDate,WORD nDosTime,FILETIME *lpFileTime);
WINOLEAPI CoFileTimeNow(FILETIME *lpFileTime);
WINOLEAPI CoRegisterMessageFilter(LPMESSAGEFILTER lpMessageFilter,LPMESSAGEFILTER *lplpMessageFilter);
WINOLEAPI CoRegisterChannelHook(REFGUID ExtensionUuid,IChannelHook *pChannelHook);
WINOLEAPI CoWaitForMultipleHandles (DWORD dwFlags,DWORD dwTimeout,ULONG cHandles,LPHANDLE pHandles,LPDWORD lpdwindex);

typedef enum tagCOWAIT_FLAGS {
  COWAIT_WAITALL = 1,COWAIT_ALERTABLE = 2,COWAIT_INPUTAVAILABLE = 4
} COWAIT_FLAGS;

WINOLEAPI CoInvalidateRemoteMachineBindings(LPOLESTR pszMachineName);
WINOLEAPI CoGetTreatAsClass(REFCLSID clsidOld,LPCLSID pClsidNew);
WINOLEAPI CoTreatAsClass(REFCLSID clsidOld,REFCLSID clsidNew);

typedef HRESULT (WINAPI *LPFNGETCLASSOBJECT)(REFCLSID,REFIID,LPVOID *);
typedef HRESULT (WINAPI *LPFNCANUNLOADNOW)(void);

STDAPI DllGetClassObject(REFCLSID rclsid,REFIID riid,LPVOID *ppv);
STDAPI DllCanUnloadNow(void);
WINOLEAPI_(LPVOID) CoTaskMemAlloc(SIZE_T cb);
WINOLEAPI_(LPVOID) CoTaskMemRealloc(LPVOID pv,SIZE_T cb);
WINOLEAPI_(void) CoTaskMemFree(LPVOID pv);
WINOLEAPI CreateDataAdviseHolder(LPDATAADVISEHOLDER *ppDAHolder);
WINOLEAPI CreateDataCache(LPUNKNOWN pUnkOuter,REFCLSID rclsid,REFIID iid,LPVOID *ppv);
WINOLEAPI StgCreateDocfile(const OLECHAR *pwcsName,DWORD grfMode,DWORD reserved,IStorage **ppstgOpen);
WINOLEAPI StgCreateDocfileOnILockBytes(ILockBytes *plkbyt,DWORD grfMode,DWORD reserved,IStorage **ppstgOpen);
WINOLEAPI StgOpenStorage(const OLECHAR *pwcsName,IStorage *pstgPriority,DWORD grfMode,SNB snbExclude,DWORD reserved,IStorage **ppstgOpen);
WINOLEAPI StgOpenStorageOnILockBytes(ILockBytes *plkbyt,IStorage *pstgPriority,DWORD grfMode,SNB snbExclude,DWORD reserved,IStorage **ppstgOpen);
WINOLEAPI StgIsStorageFile(const OLECHAR *pwcsName);
WINOLEAPI StgIsStorageILockBytes(ILockBytes *plkbyt);
WINOLEAPI StgSetTimes(OLECHAR const *lpszName,FILETIME const *pctime,FILETIME const *patime,FILETIME const *pmtime);
WINOLEAPI StgOpenAsyncDocfileOnIFillLockBytes(IFillLockBytes *pflb,DWORD grfMode,DWORD asyncFlags,IStorage **ppstgOpen);
WINOLEAPI StgGetIFillLockBytesOnILockBytes(ILockBytes *pilb,IFillLockBytes **ppflb);
WINOLEAPI StgGetIFillLockBytesOnFile(OLECHAR const *pwcsName,IFillLockBytes **ppflb);
WINOLEAPI StgOpenLayoutDocfile(OLECHAR const *pwcsDfName,DWORD grfMode,DWORD reserved,IStorage **ppstgOpen);

#define STGOPTIONS_VERSION 2

typedef struct tagSTGOPTIONS {
  USHORT usVersion;
  USHORT reserved;
  ULONG ulSectorSize;
  const WCHAR *pwcsTemplateFile;
} STGOPTIONS;

WINOLEAPI StgCreateStorageEx (const WCHAR *pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,STGOPTIONS *pStgOptions,void *reserved,REFIID riid,void **ppObjectOpen);
WINOLEAPI StgOpenStorageEx (const WCHAR *pwcsName,DWORD grfMode,DWORD stgfmt,DWORD grfAttrs,STGOPTIONS *pStgOptions,void *reserved,REFIID riid,void **ppObjectOpen);
WINOLEAPI BindMoniker(LPMONIKER pmk,DWORD grfOpt,REFIID iidResult,LPVOID *ppvResult);
WINOLEAPI CoInstall(IBindCtx *pbc,DWORD dwFlags,uCLSSPEC *pClassSpec,QUERYCONTEXT *pQuery,LPWSTR pszCodeBase);
WINOLEAPI CoGetObject(LPCWSTR pszName,BIND_OPTS *pBindOptions,REFIID riid,void **ppv);
WINOLEAPI MkParseDisplayName(LPBC pbc,LPCOLESTR szUserName,ULONG *pchEaten,LPMONIKER *ppmk);
WINOLEAPI MonikerRelativePathTo(LPMONIKER pmkSrc,LPMONIKER pmkDest,LPMONIKER *ppmkRelPath,WINBOOL dwReserved);
WINOLEAPI MonikerCommonPrefixWith(LPMONIKER pmkThis,LPMONIKER pmkOther,LPMONIKER *ppmkCommon);
WINOLEAPI CreateBindCtx(DWORD reserved,LPBC *ppbc);
WINOLEAPI CreateGenericComposite(LPMONIKER pmkFirst,LPMONIKER pmkRest,LPMONIKER *ppmkComposite);
WINOLEAPI GetClassFile (LPCOLESTR szFilename,CLSID *pclsid);
WINOLEAPI CreateClassMoniker(REFCLSID rclsid,LPMONIKER *ppmk);
WINOLEAPI CreateFileMoniker(LPCOLESTR lpszPathName,LPMONIKER *ppmk);
WINOLEAPI CreateItemMoniker(LPCOLESTR lpszDelim,LPCOLESTR lpszItem,LPMONIKER *ppmk);
WINOLEAPI CreateAntiMoniker(LPMONIKER *ppmk);
WINOLEAPI CreatePointerMoniker(LPUNKNOWN punk,LPMONIKER *ppmk);
WINOLEAPI CreateObjrefMoniker(LPUNKNOWN punk,LPMONIKER *ppmk);
WINOLEAPI GetRunningObjectTable(DWORD reserved,LPRUNNINGOBJECTTABLE *pprot);

#include <urlmon.h>
#include <propidl.h>

WINOLEAPI CreateStdProgressIndicator(HWND hwndParent,LPCOLESTR pszTitle,IBindStatusCallback *pIbscCaller,IBindStatusCallback **ppIbsc);

#ifndef RC_INVOKED
#include <poppack.h>
#endif
#endif
