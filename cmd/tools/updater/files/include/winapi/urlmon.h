/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
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

#ifndef __urlmon_h__
#define __urlmon_h__

#ifndef __IPersistMoniker_FWD_DEFINED__
#define __IPersistMoniker_FWD_DEFINED__
typedef struct IPersistMoniker IPersistMoniker;
#endif

#ifndef __IMonikerProp_FWD_DEFINED__
#define __IMonikerProp_FWD_DEFINED__
typedef struct IMonikerProp IMonikerProp;
#endif

#ifndef __IBindProtocol_FWD_DEFINED__
#define __IBindProtocol_FWD_DEFINED__
typedef struct IBindProtocol IBindProtocol;
#endif

#ifndef __IBinding_FWD_DEFINED__
#define __IBinding_FWD_DEFINED__
typedef struct IBinding IBinding;
#endif

#ifndef __IBindStatusCallback_FWD_DEFINED__
#define __IBindStatusCallback_FWD_DEFINED__
typedef struct IBindStatusCallback IBindStatusCallback;
#endif

#ifndef __IAuthenticate_FWD_DEFINED__
#define __IAuthenticate_FWD_DEFINED__
typedef struct IAuthenticate IAuthenticate;
#endif

#ifndef __IHttpNegotiate_FWD_DEFINED__
#define __IHttpNegotiate_FWD_DEFINED__
typedef struct IHttpNegotiate IHttpNegotiate;
#endif

#ifndef __IHttpNegotiate2_FWD_DEFINED__
#define __IHttpNegotiate2_FWD_DEFINED__
typedef struct IHttpNegotiate2 IHttpNegotiate2;
#endif

#ifndef __IWinInetFileStream_FWD_DEFINED__
#define __IWinInetFileStream_FWD_DEFINED__
typedef struct IWinInetFileStream IWinInetFileStream;
#endif

#ifndef __IWindowForBindingUI_FWD_DEFINED__
#define __IWindowForBindingUI_FWD_DEFINED__
typedef struct IWindowForBindingUI IWindowForBindingUI;
#endif

#ifndef __ICodeInstall_FWD_DEFINED__
#define __ICodeInstall_FWD_DEFINED__
typedef struct ICodeInstall ICodeInstall;
#endif

#ifndef __IWinInetInfo_FWD_DEFINED__
#define __IWinInetInfo_FWD_DEFINED__
typedef struct IWinInetInfo IWinInetInfo;
#endif

#ifndef __IHttpSecurity_FWD_DEFINED__
#define __IHttpSecurity_FWD_DEFINED__
typedef struct IHttpSecurity IHttpSecurity;
#endif

#ifndef __IWinInetHttpInfo_FWD_DEFINED__
#define __IWinInetHttpInfo_FWD_DEFINED__
typedef struct IWinInetHttpInfo IWinInetHttpInfo;
#endif

#ifndef __IWinInetCacheHints_FWD_DEFINED__
#define __IWinInetCacheHints_FWD_DEFINED__
typedef struct IWinInetCacheHints IWinInetCacheHints;
#endif

#ifndef __IBindHost_FWD_DEFINED__
#define __IBindHost_FWD_DEFINED__
typedef struct IBindHost IBindHost;
#endif

#ifndef __IInternet_FWD_DEFINED__
#define __IInternet_FWD_DEFINED__
typedef struct IInternet IInternet;
#endif

#ifndef __IInternetBindInfo_FWD_DEFINED__
#define __IInternetBindInfo_FWD_DEFINED__
typedef struct IInternetBindInfo IInternetBindInfo;
#endif

#ifndef __IInternetProtocolRoot_FWD_DEFINED__
#define __IInternetProtocolRoot_FWD_DEFINED__
typedef struct IInternetProtocolRoot IInternetProtocolRoot;
#endif

#ifndef __IInternetProtocol_FWD_DEFINED__
#define __IInternetProtocol_FWD_DEFINED__
typedef struct IInternetProtocol IInternetProtocol;
#endif

#ifndef __IInternetProtocolSink_FWD_DEFINED__
#define __IInternetProtocolSink_FWD_DEFINED__
typedef struct IInternetProtocolSink IInternetProtocolSink;
#endif

#ifndef __IInternetProtocolSinkStackable_FWD_DEFINED__
#define __IInternetProtocolSinkStackable_FWD_DEFINED__
typedef struct IInternetProtocolSinkStackable IInternetProtocolSinkStackable;
#endif

#ifndef __IInternetSession_FWD_DEFINED__
#define __IInternetSession_FWD_DEFINED__
typedef struct IInternetSession IInternetSession;
#endif

#ifndef __IInternetThreadSwitch_FWD_DEFINED__
#define __IInternetThreadSwitch_FWD_DEFINED__
typedef struct IInternetThreadSwitch IInternetThreadSwitch;
#endif

#ifndef __IInternetPriority_FWD_DEFINED__
#define __IInternetPriority_FWD_DEFINED__
typedef struct IInternetPriority IInternetPriority;
#endif

#ifndef __IInternetProtocolInfo_FWD_DEFINED__
#define __IInternetProtocolInfo_FWD_DEFINED__
typedef struct IInternetProtocolInfo IInternetProtocolInfo;
#endif

#ifndef __IInternetSecurityMgrSite_FWD_DEFINED__
#define __IInternetSecurityMgrSite_FWD_DEFINED__
typedef struct IInternetSecurityMgrSite IInternetSecurityMgrSite;
#endif

#ifndef __IInternetSecurityManager_FWD_DEFINED__
#define __IInternetSecurityManager_FWD_DEFINED__
typedef struct IInternetSecurityManager IInternetSecurityManager;
#endif

#ifndef __IInternetSecurityManagerEx_FWD_DEFINED__
#define __IInternetSecurityManagerEx_FWD_DEFINED__
typedef struct IInternetSecurityManagerEx IInternetSecurityManagerEx;
#endif

#ifndef __IZoneIdentifier_FWD_DEFINED__
#define __IZoneIdentifier_FWD_DEFINED__
typedef struct IZoneIdentifier IZoneIdentifier;
#endif

#ifndef __IInternetHostSecurityManager_FWD_DEFINED__
#define __IInternetHostSecurityManager_FWD_DEFINED__
typedef struct IInternetHostSecurityManager IInternetHostSecurityManager;
#endif

#ifndef __IInternetZoneManager_FWD_DEFINED__
#define __IInternetZoneManager_FWD_DEFINED__
typedef struct IInternetZoneManager IInternetZoneManager;
#endif

#ifndef __IInternetZoneManagerEx_FWD_DEFINED__
#define __IInternetZoneManagerEx_FWD_DEFINED__
typedef struct IInternetZoneManagerEx IInternetZoneManagerEx;
#endif

#ifndef __ISoftDistExt_FWD_DEFINED__
#define __ISoftDistExt_FWD_DEFINED__
typedef struct ISoftDistExt ISoftDistExt;
#endif

#ifndef __ICatalogFileInfo_FWD_DEFINED__
#define __ICatalogFileInfo_FWD_DEFINED__
typedef struct ICatalogFileInfo ICatalogFileInfo;
#endif

#ifndef __IDataFilter_FWD_DEFINED__
#define __IDataFilter_FWD_DEFINED__
typedef struct IDataFilter IDataFilter;
#endif

#ifndef __IEncodingFilterFactory_FWD_DEFINED__
#define __IEncodingFilterFactory_FWD_DEFINED__
typedef struct IEncodingFilterFactory IEncodingFilterFactory;
#endif

#ifndef __IWrappedProtocol_FWD_DEFINED__
#define __IWrappedProtocol_FWD_DEFINED__
typedef struct IWrappedProtocol IWrappedProtocol;
#endif

#include "objidl.h"
#include "oleidl.h"
#include "servprov.h"
#include "msxml2.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __MIDL_user_allocate_free_DEFINED__
#define __MIDL_user_allocate_free_DEFINED__
  void *__RPC_API MIDL_user_allocate(size_t);
  void __RPC_API MIDL_user_free(void *);
#endif

  EXTERN_C const IID CLSID_SBS_StdURLMoniker;
  EXTERN_C const IID CLSID_SBS_HttpProtocol;
  EXTERN_C const IID CLSID_SBS_FtpProtocol;
  EXTERN_C const IID CLSID_SBS_GopherProtocol;
  EXTERN_C const IID CLSID_SBS_HttpSProtocol;
  EXTERN_C const IID CLSID_SBS_FileProtocol;
  EXTERN_C const IID CLSID_SBS_MkProtocol;
  EXTERN_C const IID CLSID_SBS_UrlMkBindCtx;
  EXTERN_C const IID CLSID_SBS_SoftDistExt;
  EXTERN_C const IID CLSID_SBS_StdEncodingFilterFac;
  EXTERN_C const IID CLSID_SBS_DeCompMimeFilter;
  EXTERN_C const IID CLSID_SBS_CdlProtocol;
  EXTERN_C const IID CLSID_SBS_ClassInstallFilter;
  EXTERN_C const IID CLSID_SBS_InternetSecurityManager;
  EXTERN_C const IID CLSID_SBS_InternetZoneManager;

#define BINDF_DONTUSECACHE BINDF_GETNEWESTVERSION
#define BINDF_DONTPUTINCACHE BINDF_NOWRITECACHE
#define BINDF_NOCOPYDATA BINDF_PULLDATA
#define INVALID_P_ROOT_SECURITY_ID ((BYTE*)-1)
#define PI_DOCFILECLSIDLOOKUP PI_CLSIDLOOKUP
  EXTERN_C const IID IID_IAsyncMoniker;
  EXTERN_C const IID CLSID_StdURLMoniker;
  EXTERN_C const IID CLSID_HttpProtocol;
  EXTERN_C const IID CLSID_FtpProtocol;
  EXTERN_C const IID CLSID_GopherProtocol;
  EXTERN_C const IID CLSID_HttpSProtocol;
  EXTERN_C const IID CLSID_FileProtocol;
  EXTERN_C const IID CLSID_MkProtocol;
  EXTERN_C const IID CLSID_StdURLProtocol;
  EXTERN_C const IID CLSID_UrlMkBindCtx;
  EXTERN_C const IID CLSID_StdEncodingFilterFac;
  EXTERN_C const IID CLSID_DeCompMimeFilter;
  EXTERN_C const IID CLSID_CdlProtocol;
  EXTERN_C const IID CLSID_ClassInstallFilter;
  EXTERN_C const IID IID_IAsyncBindCtx;

#define SZ_URLCONTEXT OLESTR("URL Context")
#define SZ_ASYNC_CALLEE OLESTR("AsyncCallee")
#define MKSYS_URLMONIKER 6
#define URL_MK_LEGACY 0
#define URL_MK_UNIFORM 1
#define URL_MK_NO_CANONICALIZE 2

  STDAPI CreateURLMoniker(LPMONIKER pMkCtx,LPCWSTR szURL,LPMONIKER *ppmk);
  STDAPI CreateURLMonikerEx(LPMONIKER pMkCtx,LPCWSTR szURL,LPMONIKER *ppmk,DWORD dwFlags);
  STDAPI GetClassURL(LPCWSTR szURL,CLSID *pClsID);
  STDAPI CreateAsyncBindCtx(DWORD reserved,IBindStatusCallback *pBSCb,IEnumFORMATETC *pEFetc,IBindCtx **ppBC);
  STDAPI CreateAsyncBindCtxEx(IBindCtx *pbc,DWORD dwOptions,IBindStatusCallback *pBSCb,IEnumFORMATETC *pEnum,IBindCtx **ppBC,DWORD reserved);
  STDAPI MkParseDisplayNameEx(IBindCtx *pbc,LPCWSTR szDisplayName,ULONG *pchEaten,LPMONIKER *ppmk);
  STDAPI RegisterBindStatusCallback(LPBC pBC,IBindStatusCallback *pBSCb,IBindStatusCallback **ppBSCBPrev,DWORD dwReserved);
  STDAPI RevokeBindStatusCallback(LPBC pBC,IBindStatusCallback *pBSCb);
  STDAPI GetClassFileOrMime(LPBC pBC,LPCWSTR szFilename,LPVOID pBuffer,DWORD cbSize,LPCWSTR szMime,DWORD dwReserved,CLSID *pclsid);
  STDAPI IsValidURL(LPBC pBC,LPCWSTR szURL,DWORD dwReserved);
  STDAPI CoGetClassObjectFromURL(REFCLSID rCLASSID,LPCWSTR szCODE,DWORD dwFileVersionMS,DWORD dwFileVersionLS,LPCWSTR szTYPE,LPBINDCTX pBindCtx,DWORD dwClsContext,LPVOID pvReserved,REFIID riid,LPVOID *ppv);
  STDAPI FaultInIEFeature(HWND hWnd,uCLSSPEC *pClassSpec,QUERYCONTEXT *pQuery,DWORD dwFlags);
  STDAPI GetComponentIDFromCLSSPEC(uCLSSPEC *pClassspec,LPSTR *ppszComponentID);

#define FIEF_FLAG_FORCE_JITUI 0x1

#define FIEF_FLAG_PEEK 0x2
#define FIEF_FLAG_SKIP_INSTALLED_VERSION_CHECK 0x4

  STDAPI IsAsyncMoniker(IMoniker *pmk);
  STDAPI CreateURLBinding(LPCWSTR lpszUrl,IBindCtx *pbc,IBinding **ppBdg);
  STDAPI RegisterMediaTypes(UINT ctypes,const LPCSTR *rgszTypes,CLIPFORMAT *rgcfTypes);
  STDAPI FindMediaType(LPCSTR rgszTypes,CLIPFORMAT *rgcfTypes);
  STDAPI CreateFormatEnumerator(UINT cfmtetc,FORMATETC *rgfmtetc,IEnumFORMATETC **ppenumfmtetc);
  STDAPI RegisterFormatEnumerator(LPBC pBC,IEnumFORMATETC *pEFetc,DWORD reserved);
  STDAPI RevokeFormatEnumerator(LPBC pBC,IEnumFORMATETC *pEFetc);
  STDAPI RegisterMediaTypeClass(LPBC pBC,UINT ctypes,const LPCSTR *rgszTypes,CLSID *rgclsID,DWORD reserved);
  STDAPI FindMediaTypeClass(LPBC pBC,LPCSTR szType,CLSID *pclsID,DWORD reserved);
  STDAPI UrlMkSetSessionOption(DWORD dwOption,LPVOID pBuffer,DWORD dwBufferLength,DWORD dwReserved);
  STDAPI UrlMkGetSessionOption(DWORD dwOption,LPVOID pBuffer,DWORD dwBufferLength,DWORD *pdwBufferLength,DWORD dwReserved);
  STDAPI FindMimeFromData(LPBC pBC,LPCWSTR pwzUrl,LPVOID pBuffer,DWORD cbSize,LPCWSTR pwzMimeProposed,DWORD dwMimeFlags,LPWSTR *ppwzMimeOut,DWORD dwReserved);
#define FMFD_DEFAULT 0x00000000
#define FMFD_URLASFILENAME 0x00000001
#define FMFD_ENABLEMIMESNIFFING 0x00000002
#define FMFD_IGNOREMIMETEXTPLAIN 0x00000004
  STDAPI ObtainUserAgentString(DWORD dwOption,LPSTR pszUAOut,DWORD *cbSize);
  STDAPI CompareSecurityIds(BYTE *pbSecurityId1,DWORD dwLen1,BYTE *pbSecurityId2,DWORD dwLen2,DWORD dwReserved);
  STDAPI CompatFlagsFromClsid(CLSID *pclsid,LPDWORD pdwCompatFlags,LPDWORD pdwMiscStatusFlags);

#define URLMON_OPTION_USERAGENT 0x10000001
#define URLMON_OPTION_USERAGENT_REFRESH 0x10000002
#define URLMON_OPTION_URL_ENCODING 0x10000004
#define URLMON_OPTION_USE_BINDSTRINGCREDS 0x10000008

#define CF_NULL 0
#define CFSTR_MIME_NULL NULL
#define CFSTR_MIME_TEXT (TEXT("text/plain"))
#define CFSTR_MIME_RICHTEXT (TEXT("text/richtext"))
#define CFSTR_MIME_X_BITMAP (TEXT("image/x-xbitmap"))
#define CFSTR_MIME_POSTSCRIPT (TEXT("application/postscript"))
#define CFSTR_MIME_AIFF (TEXT("audio/aiff"))
#define CFSTR_MIME_BASICAUDIO (TEXT("audio/basic"))
#define CFSTR_MIME_WAV (TEXT("audio/wav"))
#define CFSTR_MIME_X_WAV (TEXT("audio/x-wav"))
#define CFSTR_MIME_GIF (TEXT("image/gif"))
#define CFSTR_MIME_PJPEG (TEXT("image/pjpeg"))
#define CFSTR_MIME_JPEG (TEXT("image/jpeg"))
#define CFSTR_MIME_TIFF (TEXT("image/tiff"))
#define CFSTR_MIME_X_PNG (TEXT("image/x-png"))
#define CFSTR_MIME_BMP (TEXT("image/bmp"))
#define CFSTR_MIME_X_ART (TEXT("image/x-jg"))
#define CFSTR_MIME_X_EMF (TEXT("image/x-emf"))
#define CFSTR_MIME_X_WMF (TEXT("image/x-wmf"))
#define CFSTR_MIME_AVI (TEXT("video/avi"))
#define CFSTR_MIME_MPEG (TEXT("video/mpeg"))
#define CFSTR_MIME_FRACTALS (TEXT("application/fractals"))
#define CFSTR_MIME_RAWDATA (TEXT("application/octet-stream"))
#define CFSTR_MIME_RAWDATASTRM (TEXT("application/octet-stream"))
#define CFSTR_MIME_PDF (TEXT("application/pdf"))
#define CFSTR_MIME_HTA (TEXT("application/hta"))
#define CFSTR_MIME_X_AIFF (TEXT("audio/x-aiff"))
#define CFSTR_MIME_X_REALAUDIO (TEXT("audio/x-pn-realaudio"))
#define CFSTR_MIME_XBM (TEXT("image/xbm"))
#define CFSTR_MIME_QUICKTIME (TEXT("video/quicktime"))
#define CFSTR_MIME_X_MSVIDEO (TEXT("video/x-msvideo"))
#define CFSTR_MIME_X_SGI_MOVIE (TEXT("video/x-sgi-movie"))
#define CFSTR_MIME_HTML (TEXT("text/html"))
#define CFSTR_MIME_XML (TEXT("text/xml"))

#define MK_S_ASYNCHRONOUS _HRESULT_TYPEDEF_(0x000401E8L)
#ifndef S_ASYNCHRONOUS
#define S_ASYNCHRONOUS MK_S_ASYNCHRONOUS
#endif

#ifndef E_PENDING
#define E_PENDING _HRESULT_TYPEDEF_(0x8000000AL)
#endif

#define INET_E_INVALID_URL _HRESULT_TYPEDEF_(0x800C0002L)
#define INET_E_NO_SESSION _HRESULT_TYPEDEF_(0x800C0003L)
#define INET_E_CANNOT_CONNECT _HRESULT_TYPEDEF_(0x800C0004L)
#define INET_E_RESOURCE_NOT_FOUND _HRESULT_TYPEDEF_(0x800C0005L)
#define INET_E_OBJECT_NOT_FOUND _HRESULT_TYPEDEF_(0x800C0006L)
#define INET_E_DATA_NOT_AVAILABLE _HRESULT_TYPEDEF_(0x800C0007L)
#define INET_E_DOWNLOAD_FAILURE _HRESULT_TYPEDEF_(0x800C0008L)
#define INET_E_AUTHENTICATION_REQUIRED _HRESULT_TYPEDEF_(0x800C0009L)
#define INET_E_NO_VALID_MEDIA _HRESULT_TYPEDEF_(0x800C000AL)
#define INET_E_CONNECTION_TIMEOUT _HRESULT_TYPEDEF_(0x800C000BL)
#define INET_E_INVALID_REQUEST _HRESULT_TYPEDEF_(0x800C000CL)
#define INET_E_UNKNOWN_PROTOCOL _HRESULT_TYPEDEF_(0x800C000DL)
#define INET_E_SECURITY_PROBLEM _HRESULT_TYPEDEF_(0x800C000EL)
#define INET_E_CANNOT_LOAD_DATA _HRESULT_TYPEDEF_(0x800C000FL)
#define INET_E_CANNOT_INSTANTIATE_OBJECT _HRESULT_TYPEDEF_(0x800C0010L)
#define INET_E_REDIRECT_FAILED _HRESULT_TYPEDEF_(0x800C0014L)
#define INET_E_REDIRECT_TO_DIR _HRESULT_TYPEDEF_(0x800C0015L)
#define INET_E_CANNOT_LOCK_REQUEST _HRESULT_TYPEDEF_(0x800C0016L)
#define INET_E_USE_EXTEND_BINDING _HRESULT_TYPEDEF_(0x800C0017L)
#define INET_E_TERMINATED_BIND _HRESULT_TYPEDEF_(0x800C0018L)
#define INET_E_ERROR_FIRST _HRESULT_TYPEDEF_(0x800C0002L)
#define INET_E_CODE_DOWNLOAD_DECLINED _HRESULT_TYPEDEF_(0x800C0100L)
#define INET_E_RESULT_DISPATCHED _HRESULT_TYPEDEF_(0x800C0200L)
#define INET_E_CANNOT_REPLACE_SFP_FILE _HRESULT_TYPEDEF_(0x800C0300L)
#define INET_E_CODE_INSTALL_SUPPRESSED _HRESULT_TYPEDEF_(0x800C0400L)
#define INET_E_ERROR_LAST INET_E_CANNOT_REPLACE_SFP_FILE

#ifndef _LPPERSISTMONIKER_DEFINED
#define _LPPERSISTMONIKER_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0000_v0_0_s_ifspec;
#ifndef __IPersistMoniker_INTERFACE_DEFINED__
#define __IPersistMoniker_INTERFACE_DEFINED__
  typedef IPersistMoniker *LPPERSISTMONIKER;

  EXTERN_C const IID IID_IPersistMoniker;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPersistMoniker : public IUnknown {
  public:
    virtual HRESULT WINAPI GetClassID(CLSID *pClassID) = 0;
    virtual HRESULT WINAPI IsDirty(void) = 0;
    virtual HRESULT WINAPI Load(WINBOOL fFullyAvailable,IMoniker *pimkName,LPBC pibc,DWORD grfMode) = 0;
    virtual HRESULT WINAPI Save(IMoniker *pimkName,LPBC pbc,WINBOOL fRemember) = 0;
    virtual HRESULT WINAPI SaveCompleted(IMoniker *pimkName,LPBC pibc) = 0;
    virtual HRESULT WINAPI GetCurMoniker(IMoniker **ppimkName) = 0;
  };
#else
  typedef struct IPersistMonikerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPersistMoniker *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPersistMoniker *This);
      ULONG (WINAPI *Release)(IPersistMoniker *This);
      HRESULT (WINAPI *GetClassID)(IPersistMoniker *This,CLSID *pClassID);
      HRESULT (WINAPI *IsDirty)(IPersistMoniker *This);
      HRESULT (WINAPI *Load)(IPersistMoniker *This,WINBOOL fFullyAvailable,IMoniker *pimkName,LPBC pibc,DWORD grfMode);
      HRESULT (WINAPI *Save)(IPersistMoniker *This,IMoniker *pimkName,LPBC pbc,WINBOOL fRemember);
      HRESULT (WINAPI *SaveCompleted)(IPersistMoniker *This,IMoniker *pimkName,LPBC pibc);
      HRESULT (WINAPI *GetCurMoniker)(IPersistMoniker *This,IMoniker **ppimkName);
    END_INTERFACE
  } IPersistMonikerVtbl;
  struct IPersistMoniker {
    CONST_VTBL struct IPersistMonikerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPersistMoniker_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPersistMoniker_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPersistMoniker_Release(This) (This)->lpVtbl->Release(This)
#define IPersistMoniker_GetClassID(This,pClassID) (This)->lpVtbl->GetClassID(This,pClassID)
#define IPersistMoniker_IsDirty(This) (This)->lpVtbl->IsDirty(This)
#define IPersistMoniker_Load(This,fFullyAvailable,pimkName,pibc,grfMode) (This)->lpVtbl->Load(This,fFullyAvailable,pimkName,pibc,grfMode)
#define IPersistMoniker_Save(This,pimkName,pbc,fRemember) (This)->lpVtbl->Save(This,pimkName,pbc,fRemember)
#define IPersistMoniker_SaveCompleted(This,pimkName,pibc) (This)->lpVtbl->SaveCompleted(This,pimkName,pibc)
#define IPersistMoniker_GetCurMoniker(This,ppimkName) (This)->lpVtbl->GetCurMoniker(This,ppimkName)
#endif
#endif
  HRESULT WINAPI IPersistMoniker_GetClassID_Proxy(IPersistMoniker *This,CLSID *pClassID);
  void __RPC_STUB IPersistMoniker_GetClassID_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistMoniker_IsDirty_Proxy(IPersistMoniker *This);
  void __RPC_STUB IPersistMoniker_IsDirty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistMoniker_Load_Proxy(IPersistMoniker *This,WINBOOL fFullyAvailable,IMoniker *pimkName,LPBC pibc,DWORD grfMode);
  void __RPC_STUB IPersistMoniker_Load_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistMoniker_Save_Proxy(IPersistMoniker *This,IMoniker *pimkName,LPBC pbc,WINBOOL fRemember);
  void __RPC_STUB IPersistMoniker_Save_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistMoniker_SaveCompleted_Proxy(IPersistMoniker *This,IMoniker *pimkName,LPBC pibc);
  void __RPC_STUB IPersistMoniker_SaveCompleted_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPersistMoniker_GetCurMoniker_Proxy(IPersistMoniker *This,IMoniker **ppimkName);
  void __RPC_STUB IPersistMoniker_GetCurMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPMONIKERPROP_DEFINED
#define _LPMONIKERPROP_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0178_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0178_v0_0_s_ifspec;
#ifndef __IMonikerProp_INTERFACE_DEFINED__
#define __IMonikerProp_INTERFACE_DEFINED__
  typedef IMonikerProp *LPMONIKERPROP;

  typedef enum __MIDL_IMonikerProp_0001 {
    MIMETYPEPROP = 0,USE_SRC_URL = 0x1,CLASSIDPROP = 0x2,TRUSTEDDOWNLOADPROP = 0x3,POPUPLEVELPROP = 0x4
  } MONIKERPROPERTY;

  EXTERN_C const IID IID_IMonikerProp;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IMonikerProp : public IUnknown {
  public:
    virtual HRESULT WINAPI PutProperty(MONIKERPROPERTY mkp,LPCWSTR val) = 0;
  };
#else
  typedef struct IMonikerPropVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IMonikerProp *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IMonikerProp *This);
      ULONG (WINAPI *Release)(IMonikerProp *This);
      HRESULT (WINAPI *PutProperty)(IMonikerProp *This,MONIKERPROPERTY mkp,LPCWSTR val);
    END_INTERFACE
  } IMonikerPropVtbl;
  struct IMonikerProp {
    CONST_VTBL struct IMonikerPropVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IMonikerProp_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IMonikerProp_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IMonikerProp_Release(This) (This)->lpVtbl->Release(This)
#define IMonikerProp_PutProperty(This,mkp,val) (This)->lpVtbl->PutProperty(This,mkp,val)
#endif
#endif
  HRESULT WINAPI IMonikerProp_PutProperty_Proxy(IMonikerProp *This,MONIKERPROPERTY mkp,LPCWSTR val);
  void __RPC_STUB IMonikerProp_PutProperty_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPBINDPROTOCOL_DEFINED
#define _LPBINDPROTOCOL_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0179_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0179_v0_0_s_ifspec;
#ifndef __IBindProtocol_INTERFACE_DEFINED__
#define __IBindProtocol_INTERFACE_DEFINED__
  typedef IBindProtocol *LPBINDPROTOCOL;

  EXTERN_C const IID IID_IBindProtocol;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBindProtocol : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateBinding(LPCWSTR szUrl,IBindCtx *pbc,IBinding **ppb) = 0;
  };
#else
  typedef struct IBindProtocolVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBindProtocol *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBindProtocol *This);
      ULONG (WINAPI *Release)(IBindProtocol *This);
      HRESULT (WINAPI *CreateBinding)(IBindProtocol *This,LPCWSTR szUrl,IBindCtx *pbc,IBinding **ppb);
    END_INTERFACE
  } IBindProtocolVtbl;
  struct IBindProtocol {
    CONST_VTBL struct IBindProtocolVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBindProtocol_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBindProtocol_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBindProtocol_Release(This) (This)->lpVtbl->Release(This)
#define IBindProtocol_CreateBinding(This,szUrl,pbc,ppb) (This)->lpVtbl->CreateBinding(This,szUrl,pbc,ppb)
#endif
#endif
  HRESULT WINAPI IBindProtocol_CreateBinding_Proxy(IBindProtocol *This,LPCWSTR szUrl,IBindCtx *pbc,IBinding **ppb);
  void __RPC_STUB IBindProtocol_CreateBinding_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPBINDING_DEFINED
#define _LPBINDING_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0180_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0180_v0_0_s_ifspec;
#ifndef __IBinding_INTERFACE_DEFINED__
#define __IBinding_INTERFACE_DEFINED__
  typedef IBinding *LPBINDING;

  EXTERN_C const IID IID_IBinding;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBinding : public IUnknown {
  public:
    virtual HRESULT WINAPI Abort(void) = 0;
    virtual HRESULT WINAPI Suspend(void) = 0;
    virtual HRESULT WINAPI Resume(void) = 0;
    virtual HRESULT WINAPI SetPriority(LONG nPriority) = 0;
    virtual HRESULT WINAPI GetPriority(LONG *pnPriority) = 0;
    virtual HRESULT WINAPI GetBindResult(CLSID *pclsidProtocol,DWORD *pdwResult,LPOLESTR *pszResult,DWORD *pdwReserved) = 0;
  };
#else
  typedef struct IBindingVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBinding *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBinding *This);
      ULONG (WINAPI *Release)(IBinding *This);
      HRESULT (WINAPI *Abort)(IBinding *This);
      HRESULT (WINAPI *Suspend)(IBinding *This);
      HRESULT (WINAPI *Resume)(IBinding *This);
      HRESULT (WINAPI *SetPriority)(IBinding *This,LONG nPriority);
      HRESULT (WINAPI *GetPriority)(IBinding *This,LONG *pnPriority);
      HRESULT (WINAPI *GetBindResult)(IBinding *This,CLSID *pclsidProtocol,DWORD *pdwResult,LPOLESTR *pszResult,DWORD *pdwReserved);
    END_INTERFACE
  } IBindingVtbl;
  struct IBinding {
    CONST_VTBL struct IBindingVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBinding_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBinding_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBinding_Release(This) (This)->lpVtbl->Release(This)
#define IBinding_Abort(This) (This)->lpVtbl->Abort(This)
#define IBinding_Suspend(This) (This)->lpVtbl->Suspend(This)
#define IBinding_Resume(This) (This)->lpVtbl->Resume(This)
#define IBinding_SetPriority(This,nPriority) (This)->lpVtbl->SetPriority(This,nPriority)
#define IBinding_GetPriority(This,pnPriority) (This)->lpVtbl->GetPriority(This,pnPriority)
#define IBinding_GetBindResult(This,pclsidProtocol,pdwResult,pszResult,pdwReserved) (This)->lpVtbl->GetBindResult(This,pclsidProtocol,pdwResult,pszResult,pdwReserved)
#endif
#endif
  HRESULT WINAPI IBinding_Abort_Proxy(IBinding *This);
  void __RPC_STUB IBinding_Abort_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBinding_Suspend_Proxy(IBinding *This);
  void __RPC_STUB IBinding_Suspend_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBinding_Resume_Proxy(IBinding *This);
  void __RPC_STUB IBinding_Resume_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBinding_SetPriority_Proxy(IBinding *This,LONG nPriority);
  void __RPC_STUB IBinding_SetPriority_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBinding_GetPriority_Proxy(IBinding *This,LONG *pnPriority);
  void __RPC_STUB IBinding_GetPriority_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBinding_RemoteGetBindResult_Proxy(IBinding *This,CLSID *pclsidProtocol,DWORD *pdwResult,LPOLESTR *pszResult,DWORD dwReserved);
  void __RPC_STUB IBinding_RemoteGetBindResult_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPBINDSTATUSCALLBACK_DEFINED
#define _LPBINDSTATUSCALLBACK_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0181_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0181_v0_0_s_ifspec;
#ifndef __IBindStatusCallback_INTERFACE_DEFINED__
#define __IBindStatusCallback_INTERFACE_DEFINED__
  typedef IBindStatusCallback *LPBINDSTATUSCALLBACK;

  typedef enum __MIDL_IBindStatusCallback_0001 {
    BINDVERB_GET = 0,BINDVERB_POST = 0x1,BINDVERB_PUT = 0x2,BINDVERB_CUSTOM = 0x3
  } BINDVERB;

  typedef enum __MIDL_IBindStatusCallback_0002 {
    BINDINFOF_URLENCODESTGMEDDATA = 0x1,BINDINFOF_URLENCODEDEXTRAINFO = 0x2
  } BINDINFOF;

  typedef enum __MIDL_IBindStatusCallback_0003 {
    BINDF_ASYNCHRONOUS = 0x1,BINDF_ASYNCSTORAGE = 0x2,BINDF_NOPROGRESSIVERENDERING = 0x4,BINDF_OFFLINEOPERATION = 0x8,BINDF_GETNEWESTVERSION = 0x10,
    BINDF_NOWRITECACHE = 0x20,BINDF_NEEDFILE = 0x40,BINDF_PULLDATA = 0x80,BINDF_IGNORESECURITYPROBLEM = 0x100,BINDF_RESYNCHRONIZE = 0x200,
    BINDF_HYPERLINK = 0x400,BINDF_NO_UI = 0x800,BINDF_SILENTOPERATION = 0x1000,BINDF_PRAGMA_NO_CACHE = 0x2000,BINDF_GETCLASSOBJECT = 0x4000,
    BINDF_RESERVED_1 = 0x8000,BINDF_FREE_THREADED = 0x10000,BINDF_DIRECT_READ = 0x20000,BINDF_FORMS_SUBMIT = 0x40000,
    BINDF_GETFROMCACHE_IF_NET_FAIL = 0x80000,BINDF_FROMURLMON = 0x100000,BINDF_FWD_BACK = 0x200000,BINDF_PREFERDEFAULTHANDLER = 0x400000,
    BINDF_ENFORCERESTRICTED = 0x800000
  } BINDF;

  typedef enum __MIDL_IBindStatusCallback_0004 {
    URL_ENCODING_NONE = 0,URL_ENCODING_ENABLE_UTF8 = 0x10000000,URL_ENCODING_DISABLE_UTF8 = 0x20000000
  } URL_ENCODING;

  typedef struct _tagBINDINFO {
    ULONG cbSize;
    LPWSTR szExtraInfo;
    STGMEDIUM stgmedData;
    DWORD grfBindInfoF;
    DWORD dwBindVerb;
    LPWSTR szCustomVerb;
    DWORD cbstgmedData;
    DWORD dwOptions;
    DWORD dwOptionsFlags;
    DWORD dwCodePage;
    SECURITY_ATTRIBUTES securityAttributes;
    IID iid;
    IUnknown *pUnk;
    DWORD dwReserved;
  } BINDINFO;

  typedef struct _REMSECURITY_ATTRIBUTES {
    DWORD nLength;
    DWORD lpSecurityDescriptor;
    WINBOOL bInheritHandle;
  } REMSECURITY_ATTRIBUTES;

  typedef struct _REMSECURITY_ATTRIBUTES *PREMSECURITY_ATTRIBUTES;
  typedef struct _REMSECURITY_ATTRIBUTES *LPREMSECURITY_ATTRIBUTES;

  typedef struct _tagRemBINDINFO {
    ULONG cbSize;
    LPWSTR szExtraInfo;
    DWORD grfBindInfoF;
    DWORD dwBindVerb;
    LPWSTR szCustomVerb;
    DWORD cbstgmedData;
    DWORD dwOptions;
    DWORD dwOptionsFlags;
    DWORD dwCodePage;
    REMSECURITY_ATTRIBUTES securityAttributes;
    IID iid;
    IUnknown *pUnk;
    DWORD dwReserved;
  } RemBINDINFO;

  typedef struct tagRemFORMATETC {
    DWORD cfFormat;
    DWORD ptd;
    DWORD dwAspect;
    LONG lindex;
    DWORD tymed;
  } RemFORMATETC;

  typedef struct tagRemFORMATETC *LPREMFORMATETC;

  typedef enum __MIDL_IBindStatusCallback_0005 {
    BINDINFO_OPTIONS_WININETFLAG = 0x10000,BINDINFO_OPTIONS_ENABLE_UTF8 = 0x20000,BINDINFO_OPTIONS_DISABLE_UTF8 = 0x40000,
    BINDINFO_OPTIONS_USE_IE_ENCODING = 0x80000,BINDINFO_OPTIONS_BINDTOOBJECT = 0x100000,BINDINFO_OPTIONS_SECURITYOPTOUT = 0x200000,
    BINDINFO_OPTIONS_IGNOREMIMETEXTPLAIN = 0x400000,BINDINFO_OPTIONS_USEBINDSTRINGCREDS = 0x800000,BINDINFO_OPTIONS_IGNOREHTTPHTTPSREDIRECTS = 0x1000000,
    BINDINFO_OPTIONS_SHDOCVW_NAVIGATE = 0x80000000
  } BINDINFO_OPTIONS;

  typedef enum __MIDL_IBindStatusCallback_0006 {
    BSCF_FIRSTDATANOTIFICATION = 0x1,BSCF_INTERMEDIATEDATANOTIFICATION = 0x2,BSCF_LASTDATANOTIFICATION = 0x4,BSCF_DATAFULLYAVAILABLE = 0x8,
    BSCF_AVAILABLEDATASIZEUNKNOWN = 0x10
  } BSCF;

  typedef enum tagBINDSTATUS {
    BINDSTATUS_FINDINGRESOURCE = 1,
    BINDSTATUS_CONNECTING,BINDSTATUS_REDIRECTING,BINDSTATUS_BEGINDOWNLOADDATA,
    BINDSTATUS_DOWNLOADINGDATA,BINDSTATUS_ENDDOWNLOADDATA,BINDSTATUS_BEGINDOWNLOADCOMPONENTS,
    BINDSTATUS_INSTALLINGCOMPONENTS,BINDSTATUS_ENDDOWNLOADCOMPONENTS,
    BINDSTATUS_USINGCACHEDCOPY,BINDSTATUS_SENDINGREQUEST,BINDSTATUS_CLASSIDAVAILABLE,
    BINDSTATUS_MIMETYPEAVAILABLE,BINDSTATUS_CACHEFILENAMEAVAILABLE,
    BINDSTATUS_BEGINSYNCOPERATION,BINDSTATUS_ENDSYNCOPERATION,BINDSTATUS_BEGINUPLOADDATA,
    BINDSTATUS_UPLOADINGDATA,BINDSTATUS_ENDUPLOADDATA,BINDSTATUS_PROTOCOLCLASSID,
    BINDSTATUS_ENCODING,BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE,BINDSTATUS_CLASSINSTALLLOCATION,
    BINDSTATUS_DECODING,BINDSTATUS_LOADINGMIMEHANDLER,BINDSTATUS_CONTENTDISPOSITIONATTACH,
    BINDSTATUS_FILTERREPORTMIMETYPE,BINDSTATUS_CLSIDCANINSTANTIATE,BINDSTATUS_IUNKNOWNAVAILABLE,
    BINDSTATUS_DIRECTBIND,BINDSTATUS_RAWMIMETYPE,BINDSTATUS_PROXYDETECTING,
    BINDSTATUS_ACCEPTRANGES,BINDSTATUS_COOKIE_SENT,BINDSTATUS_COMPACT_POLICY_RECEIVED,
    BINDSTATUS_COOKIE_SUPPRESSED,BINDSTATUS_COOKIE_STATE_UNKNOWN,BINDSTATUS_COOKIE_STATE_ACCEPT,
    BINDSTATUS_COOKIE_STATE_REJECT,BINDSTATUS_COOKIE_STATE_PROMPT,BINDSTATUS_COOKIE_STATE_LEASH,
    BINDSTATUS_COOKIE_STATE_DOWNGRADE,BINDSTATUS_POLICY_HREF,BINDSTATUS_P3P_HEADER,
    BINDSTATUS_SESSION_COOKIE_RECEIVED,BINDSTATUS_PERSISTENT_COOKIE_RECEIVED,
    BINDSTATUS_SESSION_COOKIES_ALLOWED,BINDSTATUS_CACHECONTROL,BINDSTATUS_CONTENTDISPOSITIONFILENAME,
    BINDSTATUS_MIMETEXTPLAINMISMATCH,BINDSTATUS_PUBLISHERAVAILABLE,BINDSTATUS_DISPLAYNAMEAVAILABLE
  } BINDSTATUS;

  EXTERN_C const IID IID_IBindStatusCallback;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBindStatusCallback : public IUnknown {
  public:
    virtual HRESULT WINAPI OnStartBinding(DWORD dwReserved,IBinding *pib) = 0;
    virtual HRESULT WINAPI GetPriority(LONG *pnPriority) = 0;
    virtual HRESULT WINAPI OnLowResource(DWORD reserved) = 0;
    virtual HRESULT WINAPI OnProgress(ULONG ulProgress,ULONG ulProgressMax,ULONG ulStatusCode,LPCWSTR szStatusText) = 0;
    virtual HRESULT WINAPI OnStopBinding(HRESULT hresult,LPCWSTR szError) = 0;
    virtual HRESULT WINAPI GetBindInfo(DWORD *grfBINDF,BINDINFO *pbindinfo) = 0;
    virtual HRESULT WINAPI OnDataAvailable(DWORD grfBSCF,DWORD dwSize,FORMATETC *pformatetc,STGMEDIUM *pstgmed) = 0;
    virtual HRESULT WINAPI OnObjectAvailable(REFIID riid,IUnknown *punk) = 0;
  };
#else
  typedef struct IBindStatusCallbackVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBindStatusCallback *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBindStatusCallback *This);
      ULONG (WINAPI *Release)(IBindStatusCallback *This);
      HRESULT (WINAPI *OnStartBinding)(IBindStatusCallback *This,DWORD dwReserved,IBinding *pib);
      HRESULT (WINAPI *GetPriority)(IBindStatusCallback *This,LONG *pnPriority);
      HRESULT (WINAPI *OnLowResource)(IBindStatusCallback *This,DWORD reserved);
      HRESULT (WINAPI *OnProgress)(IBindStatusCallback *This,ULONG ulProgress,ULONG ulProgressMax,ULONG ulStatusCode,LPCWSTR szStatusText);
      HRESULT (WINAPI *OnStopBinding)(IBindStatusCallback *This,HRESULT hresult,LPCWSTR szError);
      HRESULT (WINAPI *GetBindInfo)(IBindStatusCallback *This,DWORD *grfBINDF,BINDINFO *pbindinfo);
      HRESULT (WINAPI *OnDataAvailable)(IBindStatusCallback *This,DWORD grfBSCF,DWORD dwSize,FORMATETC *pformatetc,STGMEDIUM *pstgmed);
      HRESULT (WINAPI *OnObjectAvailable)(IBindStatusCallback *This,REFIID riid,IUnknown *punk);
    END_INTERFACE
  } IBindStatusCallbackVtbl;
  struct IBindStatusCallback {
    CONST_VTBL struct IBindStatusCallbackVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBindStatusCallback_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBindStatusCallback_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBindStatusCallback_Release(This) (This)->lpVtbl->Release(This)
#define IBindStatusCallback_OnStartBinding(This,dwReserved,pib) (This)->lpVtbl->OnStartBinding(This,dwReserved,pib)
#define IBindStatusCallback_GetPriority(This,pnPriority) (This)->lpVtbl->GetPriority(This,pnPriority)
#define IBindStatusCallback_OnLowResource(This,reserved) (This)->lpVtbl->OnLowResource(This,reserved)
#define IBindStatusCallback_OnProgress(This,ulProgress,ulProgressMax,ulStatusCode,szStatusText) (This)->lpVtbl->OnProgress(This,ulProgress,ulProgressMax,ulStatusCode,szStatusText)
#define IBindStatusCallback_OnStopBinding(This,hresult,szError) (This)->lpVtbl->OnStopBinding(This,hresult,szError)
#define IBindStatusCallback_GetBindInfo(This,grfBINDF,pbindinfo) (This)->lpVtbl->GetBindInfo(This,grfBINDF,pbindinfo)
#define IBindStatusCallback_OnDataAvailable(This,grfBSCF,dwSize,pformatetc,pstgmed) (This)->lpVtbl->OnDataAvailable(This,grfBSCF,dwSize,pformatetc,pstgmed)
#define IBindStatusCallback_OnObjectAvailable(This,riid,punk) (This)->lpVtbl->OnObjectAvailable(This,riid,punk)
#endif
#endif
  HRESULT WINAPI IBindStatusCallback_OnStartBinding_Proxy(IBindStatusCallback *This,DWORD dwReserved,IBinding *pib);
  void __RPC_STUB IBindStatusCallback_OnStartBinding_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_GetPriority_Proxy(IBindStatusCallback *This,LONG *pnPriority);
  void __RPC_STUB IBindStatusCallback_GetPriority_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_OnLowResource_Proxy(IBindStatusCallback *This,DWORD reserved);
  void __RPC_STUB IBindStatusCallback_OnLowResource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_OnProgress_Proxy(IBindStatusCallback *This,ULONG ulProgress,ULONG ulProgressMax,ULONG ulStatusCode,LPCWSTR szStatusText);
  void __RPC_STUB IBindStatusCallback_OnProgress_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_OnStopBinding_Proxy(IBindStatusCallback *This,HRESULT hresult,LPCWSTR szError);
  void __RPC_STUB IBindStatusCallback_OnStopBinding_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_RemoteGetBindInfo_Proxy(IBindStatusCallback *This,DWORD *grfBINDF,RemBINDINFO *pbindinfo,RemSTGMEDIUM *pstgmed);
  void __RPC_STUB IBindStatusCallback_RemoteGetBindInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_RemoteOnDataAvailable_Proxy(IBindStatusCallback *This,DWORD grfBSCF,DWORD dwSize,RemFORMATETC *pformatetc,RemSTGMEDIUM *pstgmed);
  void __RPC_STUB IBindStatusCallback_RemoteOnDataAvailable_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindStatusCallback_OnObjectAvailable_Proxy(IBindStatusCallback *This,REFIID riid,IUnknown *punk);
  void __RPC_STUB IBindStatusCallback_OnObjectAvailable_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPAUTHENTICATION_DEFINED
#define _LPAUTHENTICATION_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0182_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0182_v0_0_s_ifspec;
#ifndef __IAuthenticate_INTERFACE_DEFINED__
#define __IAuthenticate_INTERFACE_DEFINED__
  typedef IAuthenticate *LPAUTHENTICATION;

  EXTERN_C const IID IID_IAuthenticate;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IAuthenticate : public IUnknown {
  public:
    virtual HRESULT WINAPI Authenticate(HWND *phwnd,LPWSTR *pszUsername,LPWSTR *pszPassword) = 0;
  };
#else
  typedef struct IAuthenticateVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IAuthenticate *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IAuthenticate *This);
      ULONG (WINAPI *Release)(IAuthenticate *This);
      HRESULT (WINAPI *Authenticate)(IAuthenticate *This,HWND *phwnd,LPWSTR *pszUsername,LPWSTR *pszPassword);
    END_INTERFACE
  } IAuthenticateVtbl;
  struct IAuthenticate {
    CONST_VTBL struct IAuthenticateVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IAuthenticate_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IAuthenticate_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IAuthenticate_Release(This) (This)->lpVtbl->Release(This)
#define IAuthenticate_Authenticate(This,phwnd,pszUsername,pszPassword) (This)->lpVtbl->Authenticate(This,phwnd,pszUsername,pszPassword)
#endif
#endif
  HRESULT WINAPI IAuthenticate_Authenticate_Proxy(IAuthenticate *This,HWND *phwnd,LPWSTR *pszUsername,LPWSTR *pszPassword);
  void __RPC_STUB IAuthenticate_Authenticate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPHTTPNEGOTIATE_DEFINED
#define _LPHTTPNEGOTIATE_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0183_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0183_v0_0_s_ifspec;
#ifndef __IHttpNegotiate_INTERFACE_DEFINED__
#define __IHttpNegotiate_INTERFACE_DEFINED__
  typedef IHttpNegotiate *LPHTTPNEGOTIATE;

  EXTERN_C const IID IID_IHttpNegotiate;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IHttpNegotiate : public IUnknown {
  public:
    virtual HRESULT WINAPI BeginningTransaction(LPCWSTR szURL,LPCWSTR szHeaders,DWORD dwReserved,LPWSTR *pszAdditionalHeaders) = 0;
    virtual HRESULT WINAPI OnResponse(DWORD dwResponseCode,LPCWSTR szResponseHeaders,LPCWSTR szRequestHeaders,LPWSTR *pszAdditionalRequestHeaders) = 0;
  };
#else
  typedef struct IHttpNegotiateVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IHttpNegotiate *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IHttpNegotiate *This);
      ULONG (WINAPI *Release)(IHttpNegotiate *This);
      HRESULT (WINAPI *BeginningTransaction)(IHttpNegotiate *This,LPCWSTR szURL,LPCWSTR szHeaders,DWORD dwReserved,LPWSTR *pszAdditionalHeaders);
      HRESULT (WINAPI *OnResponse)(IHttpNegotiate *This,DWORD dwResponseCode,LPCWSTR szResponseHeaders,LPCWSTR szRequestHeaders,LPWSTR *pszAdditionalRequestHeaders);
    END_INTERFACE
  } IHttpNegotiateVtbl;
  struct IHttpNegotiate {
    CONST_VTBL struct IHttpNegotiateVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IHttpNegotiate_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IHttpNegotiate_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IHttpNegotiate_Release(This) (This)->lpVtbl->Release(This)
#define IHttpNegotiate_BeginningTransaction(This,szURL,szHeaders,dwReserved,pszAdditionalHeaders) (This)->lpVtbl->BeginningTransaction(This,szURL,szHeaders,dwReserved,pszAdditionalHeaders)
#define IHttpNegotiate_OnResponse(This,dwResponseCode,szResponseHeaders,szRequestHeaders,pszAdditionalRequestHeaders) (This)->lpVtbl->OnResponse(This,dwResponseCode,szResponseHeaders,szRequestHeaders,pszAdditionalRequestHeaders)
#endif
#endif
  HRESULT WINAPI IHttpNegotiate_BeginningTransaction_Proxy(IHttpNegotiate *This,LPCWSTR szURL,LPCWSTR szHeaders,DWORD dwReserved,LPWSTR *pszAdditionalHeaders);
  void __RPC_STUB IHttpNegotiate_BeginningTransaction_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IHttpNegotiate_OnResponse_Proxy(IHttpNegotiate *This,DWORD dwResponseCode,LPCWSTR szResponseHeaders,LPCWSTR szRequestHeaders,LPWSTR *pszAdditionalRequestHeaders);
  void __RPC_STUB IHttpNegotiate_OnResponse_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPHTTPNEGOTIATE2_DEFINED
#define _LPHTTPNEGOTIATE2_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0184_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0184_v0_0_s_ifspec;
#ifndef __IHttpNegotiate2_INTERFACE_DEFINED__
#define __IHttpNegotiate2_INTERFACE_DEFINED__
  typedef IHttpNegotiate2 *LPHTTPNEGOTIATE2;

  EXTERN_C const IID IID_IHttpNegotiate2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IHttpNegotiate2 : public IHttpNegotiate {
  public:
    virtual HRESULT WINAPI GetRootSecurityId(BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved) = 0;
  };
#else
  typedef struct IHttpNegotiate2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IHttpNegotiate2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IHttpNegotiate2 *This);
      ULONG (WINAPI *Release)(IHttpNegotiate2 *This);
      HRESULT (WINAPI *BeginningTransaction)(IHttpNegotiate2 *This,LPCWSTR szURL,LPCWSTR szHeaders,DWORD dwReserved,LPWSTR *pszAdditionalHeaders);
      HRESULT (WINAPI *OnResponse)(IHttpNegotiate2 *This,DWORD dwResponseCode,LPCWSTR szResponseHeaders,LPCWSTR szRequestHeaders,LPWSTR *pszAdditionalRequestHeaders);
      HRESULT (WINAPI *GetRootSecurityId)(IHttpNegotiate2 *This,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
    END_INTERFACE
  } IHttpNegotiate2Vtbl;
  struct IHttpNegotiate2 {
    CONST_VTBL struct IHttpNegotiate2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IHttpNegotiate2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IHttpNegotiate2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IHttpNegotiate2_Release(This) (This)->lpVtbl->Release(This)
#define IHttpNegotiate2_BeginningTransaction(This,szURL,szHeaders,dwReserved,pszAdditionalHeaders) (This)->lpVtbl->BeginningTransaction(This,szURL,szHeaders,dwReserved,pszAdditionalHeaders)
#define IHttpNegotiate2_OnResponse(This,dwResponseCode,szResponseHeaders,szRequestHeaders,pszAdditionalRequestHeaders) (This)->lpVtbl->OnResponse(This,dwResponseCode,szResponseHeaders,szRequestHeaders,pszAdditionalRequestHeaders)
#define IHttpNegotiate2_GetRootSecurityId(This,pbSecurityId,pcbSecurityId,dwReserved) (This)->lpVtbl->GetRootSecurityId(This,pbSecurityId,pcbSecurityId,dwReserved)
#endif
#endif
  HRESULT WINAPI IHttpNegotiate2_GetRootSecurityId_Proxy(IHttpNegotiate2 *This,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
  void __RPC_STUB IHttpNegotiate2_GetRootSecurityId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPWININETFILESTREAM_DEFINED
#define _LPWININETFILESTREAM_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0185_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0185_v0_0_s_ifspec;
#ifndef __IWinInetFileStream_INTERFACE_DEFINED__
#define __IWinInetFileStream_INTERFACE_DEFINED__
  typedef IWinInetFileStream *LPWININETFILESTREAM;

  EXTERN_C const IID IID_IWinInetFileStream;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWinInetFileStream : public IUnknown {
  public:
    virtual HRESULT WINAPI SetHandleForUnlock(DWORD_PTR hWinInetLockHandle,DWORD_PTR dwReserved) = 0;
    virtual HRESULT WINAPI SetDeleteFile(DWORD_PTR dwReserved) = 0;
  };
#else
  typedef struct IWinInetFileStreamVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWinInetFileStream *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWinInetFileStream *This);
      ULONG (WINAPI *Release)(IWinInetFileStream *This);
      HRESULT (WINAPI *SetHandleForUnlock)(IWinInetFileStream *This,DWORD_PTR hWinInetLockHandle,DWORD_PTR dwReserved);
      HRESULT (WINAPI *SetDeleteFile)(IWinInetFileStream *This,DWORD_PTR dwReserved);
    END_INTERFACE
  } IWinInetFileStreamVtbl;
  struct IWinInetFileStream {
    CONST_VTBL struct IWinInetFileStreamVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWinInetFileStream_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWinInetFileStream_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWinInetFileStream_Release(This) (This)->lpVtbl->Release(This)
#define IWinInetFileStream_SetHandleForUnlock(This,hWinInetLockHandle,dwReserved) (This)->lpVtbl->SetHandleForUnlock(This,hWinInetLockHandle,dwReserved)
#define IWinInetFileStream_SetDeleteFile(This,dwReserved) (This)->lpVtbl->SetDeleteFile(This,dwReserved)
#endif
#endif
  HRESULT WINAPI IWinInetFileStream_SetHandleForUnlock_Proxy(IWinInetFileStream *This,DWORD_PTR hWinInetLockHandle,DWORD_PTR dwReserved);
  void __RPC_STUB IWinInetFileStream_SetHandleForUnlock_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IWinInetFileStream_SetDeleteFile_Proxy(IWinInetFileStream *This,DWORD_PTR dwReserved);
  void __RPC_STUB IWinInetFileStream_SetDeleteFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPWINDOWFORBINDINGUI_DEFINED
#define _LPWINDOWFORBINDINGUI_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0186_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0186_v0_0_s_ifspec;
#ifndef __IWindowForBindingUI_INTERFACE_DEFINED__
#define __IWindowForBindingUI_INTERFACE_DEFINED__
  typedef IWindowForBindingUI *LPWINDOWFORBINDINGUI;

  EXTERN_C const IID IID_IWindowForBindingUI;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWindowForBindingUI : public IUnknown {
  public:
    virtual HRESULT WINAPI GetWindow(REFGUID rguidReason,HWND *phwnd) = 0;
  };
#else
  typedef struct IWindowForBindingUIVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWindowForBindingUI *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWindowForBindingUI *This);
      ULONG (WINAPI *Release)(IWindowForBindingUI *This);
      HRESULT (WINAPI *GetWindow)(IWindowForBindingUI *This,REFGUID rguidReason,HWND *phwnd);
    END_INTERFACE
  } IWindowForBindingUIVtbl;
  struct IWindowForBindingUI {
    CONST_VTBL struct IWindowForBindingUIVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWindowForBindingUI_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWindowForBindingUI_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWindowForBindingUI_Release(This) (This)->lpVtbl->Release(This)
#define IWindowForBindingUI_GetWindow(This,rguidReason,phwnd) (This)->lpVtbl->GetWindow(This,rguidReason,phwnd)
#endif
#endif
  HRESULT WINAPI IWindowForBindingUI_GetWindow_Proxy(IWindowForBindingUI *This,REFGUID rguidReason,HWND *phwnd);
  void __RPC_STUB IWindowForBindingUI_GetWindow_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPCODEINSTALL_DEFINED
#define _LPCODEINSTALL_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0187_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0187_v0_0_s_ifspec;
#ifndef __ICodeInstall_INTERFACE_DEFINED__
#define __ICodeInstall_INTERFACE_DEFINED__
  typedef ICodeInstall *LPCODEINSTALL;

  typedef enum __MIDL_ICodeInstall_0001 {
    CIP_DISK_FULL = 0,
    CIP_ACCESS_DENIED,CIP_NEWER_VERSION_EXISTS,CIP_OLDER_VERSION_EXISTS,
    CIP_NAME_CONFLICT,CIP_TRUST_VERIFICATION_COMPONENT_MISSING,CIP_EXE_SELF_REGISTERATION_TIMEOUT,
    CIP_UNSAFE_TO_ABORT,CIP_NEED_REBOOT,CIP_NEED_REBOOT_UI_PERMISSION
  } CIP_STATUS;

  EXTERN_C const IID IID_ICodeInstall;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICodeInstall : public IWindowForBindingUI {
  public:
    virtual HRESULT WINAPI OnCodeInstallProblem(ULONG ulStatusCode,LPCWSTR szDestination,LPCWSTR szSource,DWORD dwReserved) = 0;
  };
#else
  typedef struct ICodeInstallVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICodeInstall *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICodeInstall *This);
      ULONG (WINAPI *Release)(ICodeInstall *This);
      HRESULT (WINAPI *GetWindow)(ICodeInstall *This,REFGUID rguidReason,HWND *phwnd);
      HRESULT (WINAPI *OnCodeInstallProblem)(ICodeInstall *This,ULONG ulStatusCode,LPCWSTR szDestination,LPCWSTR szSource,DWORD dwReserved);
    END_INTERFACE
  } ICodeInstallVtbl;
  struct ICodeInstall {
    CONST_VTBL struct ICodeInstallVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICodeInstall_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICodeInstall_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICodeInstall_Release(This) (This)->lpVtbl->Release(This)
#define ICodeInstall_GetWindow(This,rguidReason,phwnd) (This)->lpVtbl->GetWindow(This,rguidReason,phwnd)
#define ICodeInstall_OnCodeInstallProblem(This,ulStatusCode,szDestination,szSource,dwReserved) (This)->lpVtbl->OnCodeInstallProblem(This,ulStatusCode,szDestination,szSource,dwReserved)
#endif
#endif
  HRESULT WINAPI ICodeInstall_OnCodeInstallProblem_Proxy(ICodeInstall *This,ULONG ulStatusCode,LPCWSTR szDestination,LPCWSTR szSource,DWORD dwReserved);
  void __RPC_STUB ICodeInstall_OnCodeInstallProblem_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPWININETINFO_DEFINED
#define _LPWININETINFO_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0188_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0188_v0_0_s_ifspec;
#ifndef __IWinInetInfo_INTERFACE_DEFINED__
#define __IWinInetInfo_INTERFACE_DEFINED__
  typedef IWinInetInfo *LPWININETINFO;

  EXTERN_C const IID IID_IWinInetInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWinInetInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI QueryOption(DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf) = 0;
  };
#else
  typedef struct IWinInetInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWinInetInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWinInetInfo *This);
      ULONG (WINAPI *Release)(IWinInetInfo *This);
      HRESULT (WINAPI *QueryOption)(IWinInetInfo *This,DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf);
    END_INTERFACE
  } IWinInetInfoVtbl;
  struct IWinInetInfo {
    CONST_VTBL struct IWinInetInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWinInetInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWinInetInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWinInetInfo_Release(This) (This)->lpVtbl->Release(This)
#define IWinInetInfo_QueryOption(This,dwOption,pBuffer,pcbBuf) (This)->lpVtbl->QueryOption(This,dwOption,pBuffer,pcbBuf)
#endif
#endif
  HRESULT WINAPI IWinInetInfo_RemoteQueryOption_Proxy(IWinInetInfo *This,DWORD dwOption,BYTE *pBuffer,DWORD *pcbBuf);
  void __RPC_STUB IWinInetInfo_RemoteQueryOption_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif
#define WININETINFO_OPTION_LOCK_HANDLE 65534

#ifndef _LPHTTPSECURITY_DEFINED
#define _LPHTTPSECURITY_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0189_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0189_v0_0_s_ifspec;
#ifndef __IHttpSecurity_INTERFACE_DEFINED__
#define __IHttpSecurity_INTERFACE_DEFINED__

  typedef IHttpSecurity *LPHTTPSECURITY;

  EXTERN_C const IID IID_IHttpSecurity;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IHttpSecurity : public IWindowForBindingUI {
  public:
    virtual HRESULT WINAPI OnSecurityProblem(DWORD dwProblem) = 0;
  };
#else
  typedef struct IHttpSecurityVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IHttpSecurity *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IHttpSecurity *This);
      ULONG (WINAPI *Release)(IHttpSecurity *This);
      HRESULT (WINAPI *GetWindow)(IHttpSecurity *This,REFGUID rguidReason,HWND *phwnd);
      HRESULT (WINAPI *OnSecurityProblem)(IHttpSecurity *This,DWORD dwProblem);
    END_INTERFACE
  } IHttpSecurityVtbl;
  struct IHttpSecurity {
    CONST_VTBL struct IHttpSecurityVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IHttpSecurity_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IHttpSecurity_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IHttpSecurity_Release(This) (This)->lpVtbl->Release(This)
#define IHttpSecurity_GetWindow(This,rguidReason,phwnd) (This)->lpVtbl->GetWindow(This,rguidReason,phwnd)
#define IHttpSecurity_OnSecurityProblem(This,dwProblem) (This)->lpVtbl->OnSecurityProblem(This,dwProblem)
#endif
#endif
  HRESULT WINAPI IHttpSecurity_OnSecurityProblem_Proxy(IHttpSecurity *This,DWORD dwProblem);
  void __RPC_STUB IHttpSecurity_OnSecurityProblem_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPWININETHTTPINFO_DEFINED
#define _LPWININETHTTPINFO_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0190_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0190_v0_0_s_ifspec;
#ifndef __IWinInetHttpInfo_INTERFACE_DEFINED__
#define __IWinInetHttpInfo_INTERFACE_DEFINED__
  typedef IWinInetHttpInfo *LPWININETHTTPINFO;

  EXTERN_C const IID IID_IWinInetHttpInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWinInetHttpInfo : public IWinInetInfo {
  public:
    virtual HRESULT WINAPI QueryInfo(DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf,DWORD *pdwFlags,DWORD *pdwReserved) = 0;
  };
#else
  typedef struct IWinInetHttpInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWinInetHttpInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWinInetHttpInfo *This);
      ULONG (WINAPI *Release)(IWinInetHttpInfo *This);
      HRESULT (WINAPI *QueryOption)(IWinInetHttpInfo *This,DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf);
      HRESULT (WINAPI *QueryInfo)(IWinInetHttpInfo *This,DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf,DWORD *pdwFlags,DWORD *pdwReserved);
    END_INTERFACE
  } IWinInetHttpInfoVtbl;
  struct IWinInetHttpInfo {
    CONST_VTBL struct IWinInetHttpInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWinInetHttpInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWinInetHttpInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWinInetHttpInfo_Release(This) (This)->lpVtbl->Release(This)
#define IWinInetHttpInfo_QueryOption(This,dwOption,pBuffer,pcbBuf) (This)->lpVtbl->QueryOption(This,dwOption,pBuffer,pcbBuf)
#define IWinInetHttpInfo_QueryInfo(This,dwOption,pBuffer,pcbBuf,pdwFlags,pdwReserved) (This)->lpVtbl->QueryInfo(This,dwOption,pBuffer,pcbBuf,pdwFlags,pdwReserved)
#endif
#endif
  HRESULT WINAPI IWinInetHttpInfo_RemoteQueryInfo_Proxy(IWinInetHttpInfo *This,DWORD dwOption,BYTE *pBuffer,DWORD *pcbBuf,DWORD *pdwFlags,DWORD *pdwReserved);
  void __RPC_STUB IWinInetHttpInfo_RemoteQueryInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPWININETCACHEHINTS_DEFINED
#define _LPWININETCACHEHINTS_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0191_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0191_v0_0_s_ifspec;
#ifndef __IWinInetCacheHints_INTERFACE_DEFINED__
#define __IWinInetCacheHints_INTERFACE_DEFINED__
  typedef IWinInetCacheHints *LPWININETCACHEHINTS;

  EXTERN_C const IID IID_IWinInetCacheHints;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWinInetCacheHints : public IUnknown {
  public:
    virtual HRESULT WINAPI SetCacheExtension(LPCWSTR pwzExt,LPVOID pszCacheFile,DWORD *pcbCacheFile,DWORD *pdwWinInetError,DWORD *pdwReserved) = 0;
  };
#else
  typedef struct IWinInetCacheHintsVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWinInetCacheHints *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWinInetCacheHints *This);
      ULONG (WINAPI *Release)(IWinInetCacheHints *This);
      HRESULT (WINAPI *SetCacheExtension)(IWinInetCacheHints *This,LPCWSTR pwzExt,LPVOID pszCacheFile,DWORD *pcbCacheFile,DWORD *pdwWinInetError,DWORD *pdwReserved);
    END_INTERFACE
  } IWinInetCacheHintsVtbl;
  struct IWinInetCacheHints {
    CONST_VTBL struct IWinInetCacheHintsVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWinInetCacheHints_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWinInetCacheHints_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWinInetCacheHints_Release(This) (This)->lpVtbl->Release(This)
#define IWinInetCacheHints_SetCacheExtension(This,pwzExt,pszCacheFile,pcbCacheFile,pdwWinInetError,pdwReserved) (This)->lpVtbl->SetCacheExtension(This,pwzExt,pszCacheFile,pcbCacheFile,pdwWinInetError,pdwReserved)
#endif
#endif
  HRESULT WINAPI IWinInetCacheHints_SetCacheExtension_Proxy(IWinInetCacheHints *This,LPCWSTR pwzExt,LPVOID pszCacheFile,DWORD *pcbCacheFile,DWORD *pdwWinInetError,DWORD *pdwReserved);
  void __RPC_STUB IWinInetCacheHints_SetCacheExtension_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#define SID_IBindHost IID_IBindHost
#define SID_SBindHost IID_IBindHost
#ifndef _LPBINDHOST_DEFINED
#define _LPBINDHOST_DEFINED
  EXTERN_C const GUID SID_BindHost;

  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0192_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0192_v0_0_s_ifspec;
#ifndef __IBindHost_INTERFACE_DEFINED__
#define __IBindHost_INTERFACE_DEFINED__
  typedef IBindHost *LPBINDHOST;

  EXTERN_C const IID IID_IBindHost;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IBindHost : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateMoniker(LPOLESTR szName,IBindCtx *pBC,IMoniker **ppmk,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI MonikerBindToStorage(IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj) = 0;
    virtual HRESULT WINAPI MonikerBindToObject(IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj) = 0;
  };
#else
  typedef struct IBindHostVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IBindHost *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IBindHost *This);
      ULONG (WINAPI *Release)(IBindHost *This);
      HRESULT (WINAPI *CreateMoniker)(IBindHost *This,LPOLESTR szName,IBindCtx *pBC,IMoniker **ppmk,DWORD dwReserved);
      HRESULT (WINAPI *MonikerBindToStorage)(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj);
      HRESULT (WINAPI *MonikerBindToObject)(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj);
    END_INTERFACE
  } IBindHostVtbl;
  struct IBindHost {
    CONST_VTBL struct IBindHostVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IBindHost_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IBindHost_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IBindHost_Release(This) (This)->lpVtbl->Release(This)
#define IBindHost_CreateMoniker(This,szName,pBC,ppmk,dwReserved) (This)->lpVtbl->CreateMoniker(This,szName,pBC,ppmk,dwReserved)
#define IBindHost_MonikerBindToStorage(This,pMk,pBC,pBSC,riid,ppvObj) (This)->lpVtbl->MonikerBindToStorage(This,pMk,pBC,pBSC,riid,ppvObj)
#define IBindHost_MonikerBindToObject(This,pMk,pBC,pBSC,riid,ppvObj) (This)->lpVtbl->MonikerBindToObject(This,pMk,pBC,pBSC,riid,ppvObj)
#endif
#endif
  HRESULT WINAPI IBindHost_CreateMoniker_Proxy(IBindHost *This,LPOLESTR szName,IBindCtx *pBC,IMoniker **ppmk,DWORD dwReserved);
  void __RPC_STUB IBindHost_CreateMoniker_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindHost_RemoteMonikerBindToStorage_Proxy(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,IUnknown **ppvObj);
  void __RPC_STUB IBindHost_RemoteMonikerBindToStorage_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IBindHost_RemoteMonikerBindToObject_Proxy(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,IUnknown **ppvObj);
  void __RPC_STUB IBindHost_RemoteMonikerBindToObject_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#define URLOSTRM_USECACHEDCOPY_ONLY 0x1
#define URLOSTRM_USECACHEDCOPY 0x2
#define URLOSTRM_GETNEWESTVERSION 0x3

  struct IBindStatusCallback;

  STDAPI HlinkSimpleNavigateToString(LPCWSTR szTarget,LPCWSTR szLocation,LPCWSTR szTargetFrameName,IUnknown *pUnk,IBindCtx *pbc,IBindStatusCallback *,DWORD grfHLNF,DWORD dwReserved);
  STDAPI HlinkSimpleNavigateToMoniker(IMoniker *pmkTarget,LPCWSTR szLocation,LPCWSTR szTargetFrameName,IUnknown *pUnk,IBindCtx *pbc,IBindStatusCallback *,DWORD grfHLNF,DWORD dwReserved);
  STDAPI URLOpenStreamA(LPUNKNOWN,LPCSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLOpenStreamW(LPUNKNOWN,LPCWSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLOpenPullStreamA(LPUNKNOWN,LPCSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLOpenPullStreamW(LPUNKNOWN,LPCWSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLDownloadToFileA(LPUNKNOWN,LPCSTR,LPCSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLDownloadToFileW(LPUNKNOWN,LPCWSTR,LPCWSTR,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLDownloadToCacheFileA(LPUNKNOWN,LPCSTR,LPTSTR,DWORD,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLDownloadToCacheFileW(LPUNKNOWN,LPCWSTR,LPWSTR,DWORD,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLOpenBlockingStreamA(LPUNKNOWN,LPCSTR,LPSTREAM*,DWORD,LPBINDSTATUSCALLBACK);
  STDAPI URLOpenBlockingStreamW(LPUNKNOWN,LPCWSTR,LPSTREAM*,DWORD,LPBINDSTATUSCALLBACK);

#ifdef UNICODE
#define URLOpenStream URLOpenStreamW
#define URLOpenPullStream URLOpenPullStreamW
#define URLDownloadToFile URLDownloadToFileW
#define URLDownloadToCacheFile URLDownloadToCacheFileW
#define URLOpenBlockingStream URLOpenBlockingStreamW
#else
#define URLOpenStream URLOpenStreamA
#define URLOpenPullStream URLOpenPullStreamA
#define URLDownloadToFile URLDownloadToFileA
#define URLDownloadToCacheFile URLDownloadToCacheFileA
#define URLOpenBlockingStream URLOpenBlockingStreamA
#endif

  STDAPI HlinkGoBack(IUnknown *pUnk);
  STDAPI HlinkGoForward(IUnknown *pUnk);
  STDAPI HlinkNavigateString(IUnknown *pUnk,LPCWSTR szTarget);
  STDAPI HlinkNavigateMoniker(IUnknown *pUnk,IMoniker *pmkTarget);

#ifndef _URLMON_NO_ASYNC_PLUGABLE_PROTOCOLS_

#ifndef _LPIINTERNET
#define _LPIINTERNET
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0193_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0193_v0_0_s_ifspec;
#ifndef __IInternet_INTERFACE_DEFINED__
#define __IInternet_INTERFACE_DEFINED__
  typedef IInternet *LPIINTERNET;

  EXTERN_C const IID IID_IInternet;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternet : public IUnknown {
  };
#else
  typedef struct IInternetVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternet *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternet *This);
      ULONG (WINAPI *Release)(IInternet *This);
    END_INTERFACE
  } IInternetVtbl;
  struct IInternet {
    CONST_VTBL struct IInternetVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternet_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternet_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternet_Release(This) (This)->lpVtbl->Release(This)
#endif
#endif
#endif
#endif

#ifndef _LPIINTERNETBINDINFO
#define _LPIINTERNETBINDINFO
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0194_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0194_v0_0_s_ifspec;
#ifndef __IInternetBindInfo_INTERFACE_DEFINED__
#define __IInternetBindInfo_INTERFACE_DEFINED__

  typedef IInternetBindInfo *LPIINTERNETBINDINFO;

  typedef enum tagBINDSTRING {
    BINDSTRING_HEADERS = 1,
    BINDSTRING_ACCEPT_MIMES,BINDSTRING_EXTRA_URL,BINDSTRING_LANGUAGE,BINDSTRING_USERNAME,
    BINDSTRING_PASSWORD,BINDSTRING_UA_PIXELS,BINDSTRING_UA_COLOR,BINDSTRING_OS,
    BINDSTRING_USER_AGENT,BINDSTRING_ACCEPT_ENCODINGS,BINDSTRING_POST_COOKIE,
    BINDSTRING_POST_DATA_MIME,BINDSTRING_URL,BINDSTRING_IID,BINDSTRING_FLAG_BIND_TO_OBJECT,
    BINDSTRING_PTR_BIND_CONTEXT
  } BINDSTRING;

  EXTERN_C const IID IID_IInternetBindInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetBindInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetBindInfo(DWORD *grfBINDF,BINDINFO *pbindinfo) = 0;
    virtual HRESULT WINAPI GetBindString(ULONG ulStringType,LPOLESTR *ppwzStr,ULONG cEl,ULONG *pcElFetched) = 0;
  };
#else
  typedef struct IInternetBindInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetBindInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetBindInfo *This);
      ULONG (WINAPI *Release)(IInternetBindInfo *This);
      HRESULT (WINAPI *GetBindInfo)(IInternetBindInfo *This,DWORD *grfBINDF,BINDINFO *pbindinfo);
      HRESULT (WINAPI *GetBindString)(IInternetBindInfo *This,ULONG ulStringType,LPOLESTR *ppwzStr,ULONG cEl,ULONG *pcElFetched);
    END_INTERFACE
  } IInternetBindInfoVtbl;
  struct IInternetBindInfo {
    CONST_VTBL struct IInternetBindInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetBindInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetBindInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetBindInfo_Release(This) (This)->lpVtbl->Release(This)
#define IInternetBindInfo_GetBindInfo(This,grfBINDF,pbindinfo) (This)->lpVtbl->GetBindInfo(This,grfBINDF,pbindinfo)
#define IInternetBindInfo_GetBindString(This,ulStringType,ppwzStr,cEl,pcElFetched) (This)->lpVtbl->GetBindString(This,ulStringType,ppwzStr,cEl,pcElFetched)
#endif
#endif
  HRESULT WINAPI IInternetBindInfo_GetBindInfo_Proxy(IInternetBindInfo *This,DWORD *grfBINDF,BINDINFO *pbindinfo);
  void __RPC_STUB IInternetBindInfo_GetBindInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetBindInfo_GetBindString_Proxy(IInternetBindInfo *This,ULONG ulStringType,LPOLESTR *ppwzStr,ULONG cEl,ULONG *pcElFetched);
  void __RPC_STUB IInternetBindInfo_GetBindString_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPROTOCOLROOT_DEFINED
#define _LPIINTERNETPROTOCOLROOT_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0195_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0195_v0_0_s_ifspec;
#ifndef __IInternetProtocolRoot_INTERFACE_DEFINED__
#define __IInternetProtocolRoot_INTERFACE_DEFINED__
  typedef IInternetProtocolRoot *LPIINTERNETPROTOCOLROOT;

  typedef enum _tagPI_FLAGS {
    PI_PARSE_URL = 0x1,PI_FILTER_MODE = 0x2,PI_FORCE_ASYNC = 0x4,PI_USE_WORKERTHREAD = 0x8,PI_MIMEVERIFICATION = 0x10,PI_CLSIDLOOKUP = 0x20,
    PI_DATAPROGRESS = 0x40,PI_SYNCHRONOUS = 0x80,PI_APARTMENTTHREADED = 0x100,PI_CLASSINSTALL = 0x200,PI_PASSONBINDCTX = 0x2000,
    PI_NOMIMEHANDLER = 0x8000,PI_LOADAPPDIRECT = 0x4000,PD_FORCE_SWITCH = 0x10000,PI_PREFERDEFAULTHANDLER = 0x20000
  } PI_FLAGS;

  typedef struct _tagPROTOCOLDATA {
    DWORD grfFlags;
    DWORD dwState;
    LPVOID pData;
    ULONG cbData;
  } PROTOCOLDATA;

  typedef struct _tagStartParam {
    IID iid;
    IBindCtx *pIBindCtx;
    IUnknown *pItf;
  } StartParam;

  EXTERN_C const IID IID_IInternetProtocolRoot;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetProtocolRoot : public IUnknown {
  public:
    virtual HRESULT WINAPI Start(LPCWSTR szUrl,IInternetProtocolSink *pOIProtSink,IInternetBindInfo *pOIBindInfo,DWORD grfPI,HANDLE_PTR dwReserved) = 0;
    virtual HRESULT WINAPI Continue(PROTOCOLDATA *pProtocolData) = 0;
    virtual HRESULT WINAPI Abort(HRESULT hrReason,DWORD dwOptions) = 0;
    virtual HRESULT WINAPI Terminate(DWORD dwOptions) = 0;
    virtual HRESULT WINAPI Suspend(void) = 0;
    virtual HRESULT WINAPI Resume(void) = 0;
  };
#else
  typedef struct IInternetProtocolRootVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetProtocolRoot *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetProtocolRoot *This);
      ULONG (WINAPI *Release)(IInternetProtocolRoot *This);
      HRESULT (WINAPI *Start)(IInternetProtocolRoot *This,LPCWSTR szUrl,IInternetProtocolSink *pOIProtSink,IInternetBindInfo *pOIBindInfo,DWORD grfPI,HANDLE_PTR dwReserved);
      HRESULT (WINAPI *Continue)(IInternetProtocolRoot *This,PROTOCOLDATA *pProtocolData);
      HRESULT (WINAPI *Abort)(IInternetProtocolRoot *This,HRESULT hrReason,DWORD dwOptions);
      HRESULT (WINAPI *Terminate)(IInternetProtocolRoot *This,DWORD dwOptions);
      HRESULT (WINAPI *Suspend)(IInternetProtocolRoot *This);
      HRESULT (WINAPI *Resume)(IInternetProtocolRoot *This);
    END_INTERFACE
  } IInternetProtocolRootVtbl;
  struct IInternetProtocolRoot {
    CONST_VTBL struct IInternetProtocolRootVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetProtocolRoot_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetProtocolRoot_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetProtocolRoot_Release(This) (This)->lpVtbl->Release(This)
#define IInternetProtocolRoot_Start(This,szUrl,pOIProtSink,pOIBindInfo,grfPI,dwReserved) (This)->lpVtbl->Start(This,szUrl,pOIProtSink,pOIBindInfo,grfPI,dwReserved)
#define IInternetProtocolRoot_Continue(This,pProtocolData) (This)->lpVtbl->Continue(This,pProtocolData)
#define IInternetProtocolRoot_Abort(This,hrReason,dwOptions) (This)->lpVtbl->Abort(This,hrReason,dwOptions)
#define IInternetProtocolRoot_Terminate(This,dwOptions) (This)->lpVtbl->Terminate(This,dwOptions)
#define IInternetProtocolRoot_Suspend(This) (This)->lpVtbl->Suspend(This)
#define IInternetProtocolRoot_Resume(This) (This)->lpVtbl->Resume(This)
#endif
#endif
  HRESULT WINAPI IInternetProtocolRoot_Start_Proxy(IInternetProtocolRoot *This,LPCWSTR szUrl,IInternetProtocolSink *pOIProtSink,IInternetBindInfo *pOIBindInfo,DWORD grfPI,HANDLE_PTR dwReserved);
  void __RPC_STUB IInternetProtocolRoot_Start_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolRoot_Continue_Proxy(IInternetProtocolRoot *This,PROTOCOLDATA *pProtocolData);
  void __RPC_STUB IInternetProtocolRoot_Continue_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolRoot_Abort_Proxy(IInternetProtocolRoot *This,HRESULT hrReason,DWORD dwOptions);
  void __RPC_STUB IInternetProtocolRoot_Abort_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolRoot_Terminate_Proxy(IInternetProtocolRoot *This,DWORD dwOptions);
  void __RPC_STUB IInternetProtocolRoot_Terminate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolRoot_Suspend_Proxy(IInternetProtocolRoot *This);
  void __RPC_STUB IInternetProtocolRoot_Suspend_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolRoot_Resume_Proxy(IInternetProtocolRoot *This);
  void __RPC_STUB IInternetProtocolRoot_Resume_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPROTOCOL_DEFINED
#define _LPIINTERNETPROTOCOL_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0196_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0196_v0_0_s_ifspec;
#ifndef __IInternetProtocol_INTERFACE_DEFINED__
#define __IInternetProtocol_INTERFACE_DEFINED__
  typedef IInternetProtocol *LPIINTERNETPROTOCOL;

  EXTERN_C const IID IID_IInternetProtocol;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetProtocol : public IInternetProtocolRoot {
  public:
    virtual HRESULT WINAPI Read(void *pv,ULONG cb,ULONG *pcbRead) = 0;
    virtual HRESULT WINAPI Seek(LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition) = 0;
    virtual HRESULT WINAPI LockRequest(DWORD dwOptions) = 0;
    virtual HRESULT WINAPI UnlockRequest(void) = 0;
  };
#else
  typedef struct IInternetProtocolVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetProtocol *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetProtocol *This);
      ULONG (WINAPI *Release)(IInternetProtocol *This);
      HRESULT (WINAPI *Start)(IInternetProtocol *This,LPCWSTR szUrl,IInternetProtocolSink *pOIProtSink,IInternetBindInfo *pOIBindInfo,DWORD grfPI,HANDLE_PTR dwReserved);
      HRESULT (WINAPI *Continue)(IInternetProtocol *This,PROTOCOLDATA *pProtocolData);
      HRESULT (WINAPI *Abort)(IInternetProtocol *This,HRESULT hrReason,DWORD dwOptions);
      HRESULT (WINAPI *Terminate)(IInternetProtocol *This,DWORD dwOptions);
      HRESULT (WINAPI *Suspend)(IInternetProtocol *This);
      HRESULT (WINAPI *Resume)(IInternetProtocol *This);
      HRESULT (WINAPI *Read)(IInternetProtocol *This,void *pv,ULONG cb,ULONG *pcbRead);
      HRESULT (WINAPI *Seek)(IInternetProtocol *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
      HRESULT (WINAPI *LockRequest)(IInternetProtocol *This,DWORD dwOptions);
      HRESULT (WINAPI *UnlockRequest)(IInternetProtocol *This);
    END_INTERFACE
  } IInternetProtocolVtbl;
  struct IInternetProtocol {
    CONST_VTBL struct IInternetProtocolVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetProtocol_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetProtocol_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetProtocol_Release(This) (This)->lpVtbl->Release(This)
#define IInternetProtocol_Start(This,szUrl,pOIProtSink,pOIBindInfo,grfPI,dwReserved) (This)->lpVtbl->Start(This,szUrl,pOIProtSink,pOIBindInfo,grfPI,dwReserved)
#define IInternetProtocol_Continue(This,pProtocolData) (This)->lpVtbl->Continue(This,pProtocolData)
#define IInternetProtocol_Abort(This,hrReason,dwOptions) (This)->lpVtbl->Abort(This,hrReason,dwOptions)
#define IInternetProtocol_Terminate(This,dwOptions) (This)->lpVtbl->Terminate(This,dwOptions)
#define IInternetProtocol_Suspend(This) (This)->lpVtbl->Suspend(This)
#define IInternetProtocol_Resume(This) (This)->lpVtbl->Resume(This)
#define IInternetProtocol_Read(This,pv,cb,pcbRead) (This)->lpVtbl->Read(This,pv,cb,pcbRead)
#define IInternetProtocol_Seek(This,dlibMove,dwOrigin,plibNewPosition) (This)->lpVtbl->Seek(This,dlibMove,dwOrigin,plibNewPosition)
#define IInternetProtocol_LockRequest(This,dwOptions) (This)->lpVtbl->LockRequest(This,dwOptions)
#define IInternetProtocol_UnlockRequest(This) (This)->lpVtbl->UnlockRequest(This)
#endif
#endif
  HRESULT WINAPI IInternetProtocol_Read_Proxy(IInternetProtocol *This,void *pv,ULONG cb,ULONG *pcbRead);
  void __RPC_STUB IInternetProtocol_Read_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocol_Seek_Proxy(IInternetProtocol *This,LARGE_INTEGER dlibMove,DWORD dwOrigin,ULARGE_INTEGER *plibNewPosition);
  void __RPC_STUB IInternetProtocol_Seek_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocol_LockRequest_Proxy(IInternetProtocol *This,DWORD dwOptions);
  void __RPC_STUB IInternetProtocol_LockRequest_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocol_UnlockRequest_Proxy(IInternetProtocol *This);
  void __RPC_STUB IInternetProtocol_UnlockRequest_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPROTOCOLSINK_DEFINED
#define _LPIINTERNETPROTOCOLSINK_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0197_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0197_v0_0_s_ifspec;
#ifndef __IInternetProtocolSink_INTERFACE_DEFINED__
#define __IInternetProtocolSink_INTERFACE_DEFINED__
  typedef IInternetProtocolSink *LPIINTERNETPROTOCOLSINK;

  EXTERN_C const IID IID_IInternetProtocolSink;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetProtocolSink : public IUnknown {
  public:
    virtual HRESULT WINAPI Switch(PROTOCOLDATA *pProtocolData) = 0;
    virtual HRESULT WINAPI ReportProgress(ULONG ulStatusCode,LPCWSTR szStatusText) = 0;
    virtual HRESULT WINAPI ReportData(DWORD grfBSCF,ULONG ulProgress,ULONG ulProgressMax) = 0;
    virtual HRESULT WINAPI ReportResult(HRESULT hrResult,DWORD dwError,LPCWSTR szResult) = 0;
  };
#else
  typedef struct IInternetProtocolSinkVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetProtocolSink *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetProtocolSink *This);
      ULONG (WINAPI *Release)(IInternetProtocolSink *This);
      HRESULT (WINAPI *Switch)(IInternetProtocolSink *This,PROTOCOLDATA *pProtocolData);
      HRESULT (WINAPI *ReportProgress)(IInternetProtocolSink *This,ULONG ulStatusCode,LPCWSTR szStatusText);
      HRESULT (WINAPI *ReportData)(IInternetProtocolSink *This,DWORD grfBSCF,ULONG ulProgress,ULONG ulProgressMax);
      HRESULT (WINAPI *ReportResult)(IInternetProtocolSink *This,HRESULT hrResult,DWORD dwError,LPCWSTR szResult);
    END_INTERFACE
  } IInternetProtocolSinkVtbl;
  struct IInternetProtocolSink {
    CONST_VTBL struct IInternetProtocolSinkVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetProtocolSink_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetProtocolSink_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetProtocolSink_Release(This) (This)->lpVtbl->Release(This)
#define IInternetProtocolSink_Switch(This,pProtocolData) (This)->lpVtbl->Switch(This,pProtocolData)
#define IInternetProtocolSink_ReportProgress(This,ulStatusCode,szStatusText) (This)->lpVtbl->ReportProgress(This,ulStatusCode,szStatusText)
#define IInternetProtocolSink_ReportData(This,grfBSCF,ulProgress,ulProgressMax) (This)->lpVtbl->ReportData(This,grfBSCF,ulProgress,ulProgressMax)
#define IInternetProtocolSink_ReportResult(This,hrResult,dwError,szResult) (This)->lpVtbl->ReportResult(This,hrResult,dwError,szResult)
#endif
#endif
  HRESULT WINAPI IInternetProtocolSink_Switch_Proxy(IInternetProtocolSink *This,PROTOCOLDATA *pProtocolData);
  void __RPC_STUB IInternetProtocolSink_Switch_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolSink_ReportProgress_Proxy(IInternetProtocolSink *This,ULONG ulStatusCode,LPCWSTR szStatusText);
  void __RPC_STUB IInternetProtocolSink_ReportProgress_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolSink_ReportData_Proxy(IInternetProtocolSink *This,DWORD grfBSCF,ULONG ulProgress,ULONG ulProgressMax);
  void __RPC_STUB IInternetProtocolSink_ReportData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolSink_ReportResult_Proxy(IInternetProtocolSink *This,HRESULT hrResult,DWORD dwError,LPCWSTR szResult);
  void __RPC_STUB IInternetProtocolSink_ReportResult_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPROTOCOLSINKSTACKABLE_DEFINED
#define _LPIINTERNETPROTOCOLSINKSTACKABLE_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0198_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0198_v0_0_s_ifspec;
#ifndef __IInternetProtocolSinkStackable_INTERFACE_DEFINED__
#define __IInternetProtocolSinkStackable_INTERFACE_DEFINED__
  typedef IInternetProtocolSinkStackable *LPIINTERNETPROTOCOLSINKStackable;

  EXTERN_C const IID IID_IInternetProtocolSinkStackable;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetProtocolSinkStackable : public IUnknown {
  public:
    virtual HRESULT WINAPI SwitchSink(IInternetProtocolSink *pOIProtSink) = 0;
    virtual HRESULT WINAPI CommitSwitch(void) = 0;
    virtual HRESULT WINAPI RollbackSwitch(void) = 0;
  };
#else
  typedef struct IInternetProtocolSinkStackableVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetProtocolSinkStackable *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetProtocolSinkStackable *This);
      ULONG (WINAPI *Release)(IInternetProtocolSinkStackable *This);
      HRESULT (WINAPI *SwitchSink)(IInternetProtocolSinkStackable *This,IInternetProtocolSink *pOIProtSink);
      HRESULT (WINAPI *CommitSwitch)(IInternetProtocolSinkStackable *This);
      HRESULT (WINAPI *RollbackSwitch)(IInternetProtocolSinkStackable *This);
    END_INTERFACE
  } IInternetProtocolSinkStackableVtbl;
  struct IInternetProtocolSinkStackable {
    CONST_VTBL struct IInternetProtocolSinkStackableVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetProtocolSinkStackable_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetProtocolSinkStackable_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetProtocolSinkStackable_Release(This) (This)->lpVtbl->Release(This)
#define IInternetProtocolSinkStackable_SwitchSink(This,pOIProtSink) (This)->lpVtbl->SwitchSink(This,pOIProtSink)
#define IInternetProtocolSinkStackable_CommitSwitch(This) (This)->lpVtbl->CommitSwitch(This)
#define IInternetProtocolSinkStackable_RollbackSwitch(This) (This)->lpVtbl->RollbackSwitch(This)
#endif
#endif
  HRESULT WINAPI IInternetProtocolSinkStackable_SwitchSink_Proxy(IInternetProtocolSinkStackable *This,IInternetProtocolSink *pOIProtSink);
  void __RPC_STUB IInternetProtocolSinkStackable_SwitchSink_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolSinkStackable_CommitSwitch_Proxy(IInternetProtocolSinkStackable *This);
  void __RPC_STUB IInternetProtocolSinkStackable_CommitSwitch_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolSinkStackable_RollbackSwitch_Proxy(IInternetProtocolSinkStackable *This);
  void __RPC_STUB IInternetProtocolSinkStackable_RollbackSwitch_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETSESSION_DEFINED
#define _LPIINTERNETSESSION_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0199_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0199_v0_0_s_ifspec;
#ifndef __IInternetSession_INTERFACE_DEFINED__
#define __IInternetSession_INTERFACE_DEFINED__
  typedef IInternetSession *LPIINTERNETSESSION;

  typedef enum _tagOIBDG_FLAGS {
    OIBDG_APARTMENTTHREADED = 0x100,OIBDG_DATAONLY = 0x1000
  } OIBDG_FLAGS;

  EXTERN_C const IID IID_IInternetSession;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetSession : public IUnknown {
  public:
    virtual HRESULT WINAPI RegisterNameSpace(IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzProtocol,ULONG cPatterns,const LPCWSTR *ppwzPatterns,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI UnregisterNameSpace(IClassFactory *pCF,LPCWSTR pszProtocol) = 0;
    virtual HRESULT WINAPI RegisterMimeFilter(IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzType) = 0;
    virtual HRESULT WINAPI UnregisterMimeFilter(IClassFactory *pCF,LPCWSTR pwzType) = 0;
    virtual HRESULT WINAPI CreateBinding(LPBC pBC,LPCWSTR szUrl,IUnknown *pUnkOuter,IUnknown **ppUnk,IInternetProtocol **ppOInetProt,DWORD dwOption) = 0;
    virtual HRESULT WINAPI SetSessionOption(DWORD dwOption,LPVOID pBuffer,DWORD dwBufferLength,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI GetSessionOption(DWORD dwOption,LPVOID pBuffer,DWORD *pdwBufferLength,DWORD dwReserved) = 0;
  };
#else
  typedef struct IInternetSessionVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetSession *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetSession *This);
      ULONG (WINAPI *Release)(IInternetSession *This);
      HRESULT (WINAPI *RegisterNameSpace)(IInternetSession *This,IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzProtocol,ULONG cPatterns,const LPCWSTR *ppwzPatterns,DWORD dwReserved);
      HRESULT (WINAPI *UnregisterNameSpace)(IInternetSession *This,IClassFactory *pCF,LPCWSTR pszProtocol);
      HRESULT (WINAPI *RegisterMimeFilter)(IInternetSession *This,IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzType);
      HRESULT (WINAPI *UnregisterMimeFilter)(IInternetSession *This,IClassFactory *pCF,LPCWSTR pwzType);
      HRESULT (WINAPI *CreateBinding)(IInternetSession *This,LPBC pBC,LPCWSTR szUrl,IUnknown *pUnkOuter,IUnknown **ppUnk,IInternetProtocol **ppOInetProt,DWORD dwOption);
      HRESULT (WINAPI *SetSessionOption)(IInternetSession *This,DWORD dwOption,LPVOID pBuffer,DWORD dwBufferLength,DWORD dwReserved);
      HRESULT (WINAPI *GetSessionOption)(IInternetSession *This,DWORD dwOption,LPVOID pBuffer,DWORD *pdwBufferLength,DWORD dwReserved);
    END_INTERFACE
  } IInternetSessionVtbl;
  struct IInternetSession {
    CONST_VTBL struct IInternetSessionVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetSession_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetSession_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetSession_Release(This) (This)->lpVtbl->Release(This)
#define IInternetSession_RegisterNameSpace(This,pCF,rclsid,pwzProtocol,cPatterns,ppwzPatterns,dwReserved) (This)->lpVtbl->RegisterNameSpace(This,pCF,rclsid,pwzProtocol,cPatterns,ppwzPatterns,dwReserved)
#define IInternetSession_UnregisterNameSpace(This,pCF,pszProtocol) (This)->lpVtbl->UnregisterNameSpace(This,pCF,pszProtocol)
#define IInternetSession_RegisterMimeFilter(This,pCF,rclsid,pwzType) (This)->lpVtbl->RegisterMimeFilter(This,pCF,rclsid,pwzType)
#define IInternetSession_UnregisterMimeFilter(This,pCF,pwzType) (This)->lpVtbl->UnregisterMimeFilter(This,pCF,pwzType)
#define IInternetSession_CreateBinding(This,pBC,szUrl,pUnkOuter,ppUnk,ppOInetProt,dwOption) (This)->lpVtbl->CreateBinding(This,pBC,szUrl,pUnkOuter,ppUnk,ppOInetProt,dwOption)
#define IInternetSession_SetSessionOption(This,dwOption,pBuffer,dwBufferLength,dwReserved) (This)->lpVtbl->SetSessionOption(This,dwOption,pBuffer,dwBufferLength,dwReserved)
#define IInternetSession_GetSessionOption(This,dwOption,pBuffer,pdwBufferLength,dwReserved) (This)->lpVtbl->GetSessionOption(This,dwOption,pBuffer,pdwBufferLength,dwReserved)
#endif
#endif
  HRESULT WINAPI IInternetSession_RegisterNameSpace_Proxy(IInternetSession *This,IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzProtocol,ULONG cPatterns,const LPCWSTR *ppwzPatterns,DWORD dwReserved);
  void __RPC_STUB IInternetSession_RegisterNameSpace_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_UnregisterNameSpace_Proxy(IInternetSession *This,IClassFactory *pCF,LPCWSTR pszProtocol);
  void __RPC_STUB IInternetSession_UnregisterNameSpace_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_RegisterMimeFilter_Proxy(IInternetSession *This,IClassFactory *pCF,REFCLSID rclsid,LPCWSTR pwzType);
  void __RPC_STUB IInternetSession_RegisterMimeFilter_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_UnregisterMimeFilter_Proxy(IInternetSession *This,IClassFactory *pCF,LPCWSTR pwzType);
  void __RPC_STUB IInternetSession_UnregisterMimeFilter_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_CreateBinding_Proxy(IInternetSession *This,LPBC pBC,LPCWSTR szUrl,IUnknown *pUnkOuter,IUnknown **ppUnk,IInternetProtocol **ppOInetProt,DWORD dwOption);
  void __RPC_STUB IInternetSession_CreateBinding_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_SetSessionOption_Proxy(IInternetSession *This,DWORD dwOption,LPVOID pBuffer,DWORD dwBufferLength,DWORD dwReserved);
  void __RPC_STUB IInternetSession_SetSessionOption_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSession_GetSessionOption_Proxy(IInternetSession *This,DWORD dwOption,LPVOID pBuffer,DWORD *pdwBufferLength,DWORD dwReserved);
  void __RPC_STUB IInternetSession_GetSessionOption_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETTHREADSWITCH_DEFINED
#define _LPIINTERNETTHREADSWITCH_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0200_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0200_v0_0_s_ifspec;
#ifndef __IInternetThreadSwitch_INTERFACE_DEFINED__
#define __IInternetThreadSwitch_INTERFACE_DEFINED__
  typedef IInternetThreadSwitch *LPIINTERNETTHREADSWITCH;

  EXTERN_C const IID IID_IInternetThreadSwitch;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetThreadSwitch : public IUnknown {
  public:
    virtual HRESULT WINAPI Prepare(void) = 0;
    virtual HRESULT WINAPI Continue(void) = 0;
  };
#else
  typedef struct IInternetThreadSwitchVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetThreadSwitch *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetThreadSwitch *This);
      ULONG (WINAPI *Release)(IInternetThreadSwitch *This);
      HRESULT (WINAPI *Prepare)(IInternetThreadSwitch *This);
      HRESULT (WINAPI *Continue)(IInternetThreadSwitch *This);
    END_INTERFACE
  } IInternetThreadSwitchVtbl;
  struct IInternetThreadSwitch {
    CONST_VTBL struct IInternetThreadSwitchVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetThreadSwitch_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetThreadSwitch_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetThreadSwitch_Release(This) (This)->lpVtbl->Release(This)
#define IInternetThreadSwitch_Prepare(This) (This)->lpVtbl->Prepare(This)
#define IInternetThreadSwitch_Continue(This) (This)->lpVtbl->Continue(This)
#endif
#endif
  HRESULT WINAPI IInternetThreadSwitch_Prepare_Proxy(IInternetThreadSwitch *This);
  void __RPC_STUB IInternetThreadSwitch_Prepare_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetThreadSwitch_Continue_Proxy(IInternetThreadSwitch *This);
  void __RPC_STUB IInternetThreadSwitch_Continue_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPRIORITY_DEFINED
#define _LPIINTERNETPRIORITY_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0201_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0201_v0_0_s_ifspec;
#ifndef __IInternetPriority_INTERFACE_DEFINED__
#define __IInternetPriority_INTERFACE_DEFINED__
  typedef IInternetPriority *LPIINTERNETPRIORITY;

  EXTERN_C const IID IID_IInternetPriority;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetPriority : public IUnknown {
  public:
    virtual HRESULT WINAPI SetPriority(LONG nPriority) = 0;
    virtual HRESULT WINAPI GetPriority(LONG *pnPriority) = 0;
  };
#else
  typedef struct IInternetPriorityVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetPriority *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetPriority *This);
      ULONG (WINAPI *Release)(IInternetPriority *This);
      HRESULT (WINAPI *SetPriority)(IInternetPriority *This,LONG nPriority);
      HRESULT (WINAPI *GetPriority)(IInternetPriority *This,LONG *pnPriority);
    END_INTERFACE
  } IInternetPriorityVtbl;
  struct IInternetPriority {
    CONST_VTBL struct IInternetPriorityVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetPriority_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetPriority_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetPriority_Release(This) (This)->lpVtbl->Release(This)
#define IInternetPriority_SetPriority(This,nPriority) (This)->lpVtbl->SetPriority(This,nPriority)
#define IInternetPriority_GetPriority(This,pnPriority) (This)->lpVtbl->GetPriority(This,pnPriority)
#endif
#endif
  HRESULT WINAPI IInternetPriority_SetPriority_Proxy(IInternetPriority *This,LONG nPriority);
  void __RPC_STUB IInternetPriority_SetPriority_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetPriority_GetPriority_Proxy(IInternetPriority *This,LONG *pnPriority);
  void __RPC_STUB IInternetPriority_GetPriority_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPIINTERNETPROTOCOLINFO_DEFINED
#define _LPIINTERNETPROTOCOLINFO_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0202_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0202_v0_0_s_ifspec;
#ifndef __IInternetProtocolInfo_INTERFACE_DEFINED__
#define __IInternetProtocolInfo_INTERFACE_DEFINED__
  typedef IInternetProtocolInfo *LPIINTERNETPROTOCOLINFO;

  typedef enum _tagPARSEACTION {
    PARSE_CANONICALIZE = 1,
    PARSE_FRIENDLY,PARSE_SECURITY_URL,PARSE_ROOTDOCUMENT,PARSE_DOCUMENT,PARSE_ANCHOR,
    PARSE_ENCODE,PARSE_DECODE,PARSE_PATH_FROM_URL,PARSE_URL_FROM_PATH,PARSE_MIME,
    PARSE_SERVER,PARSE_SCHEMA,PARSE_SITE,PARSE_DOMAIN,PARSE_LOCATION,PARSE_SECURITY_DOMAIN,
    PARSE_ESCAPE,PARSE_UNESCAPE
  } PARSEACTION;

  typedef enum _tagPSUACTION {
    PSU_DEFAULT = 1,
    PSU_SECURITY_URL_ONLY
  } PSUACTION;

  typedef enum _tagQUERYOPTION {
    QUERY_EXPIRATION_DATE = 1,
    QUERY_TIME_OF_LAST_CHANGE,QUERY_CONTENT_ENCODING,QUERY_CONTENT_TYPE,QUERY_REFRESH,
    QUERY_RECOMBINE,QUERY_CAN_NAVIGATE,QUERY_USES_NETWORK,QUERY_IS_CACHED,QUERY_IS_INSTALLEDENTRY,
    QUERY_IS_CACHED_OR_MAPPED,QUERY_USES_CACHE,QUERY_IS_SECURE,QUERY_IS_SAFE
  } QUERYOPTION;

  EXTERN_C const IID IID_IInternetProtocolInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetProtocolInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI ParseUrl(LPCWSTR pwzUrl,PARSEACTION ParseAction,DWORD dwParseFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI CombineUrl(LPCWSTR pwzBaseUrl,LPCWSTR pwzRelativeUrl,DWORD dwCombineFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI CompareUrl(LPCWSTR pwzUrl1,LPCWSTR pwzUrl2,DWORD dwCompareFlags) = 0;
    virtual HRESULT WINAPI QueryInfo(LPCWSTR pwzUrl,QUERYOPTION OueryOption,DWORD dwQueryFlags,LPVOID pBuffer,DWORD cbBuffer,DWORD *pcbBuf,DWORD dwReserved) = 0;
  };
#else
  typedef struct IInternetProtocolInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetProtocolInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetProtocolInfo *This);
      ULONG (WINAPI *Release)(IInternetProtocolInfo *This);
      HRESULT (WINAPI *ParseUrl)(IInternetProtocolInfo *This,LPCWSTR pwzUrl,PARSEACTION ParseAction,DWORD dwParseFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
      HRESULT (WINAPI *CombineUrl)(IInternetProtocolInfo *This,LPCWSTR pwzBaseUrl,LPCWSTR pwzRelativeUrl,DWORD dwCombineFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
      HRESULT (WINAPI *CompareUrl)(IInternetProtocolInfo *This,LPCWSTR pwzUrl1,LPCWSTR pwzUrl2,DWORD dwCompareFlags);
      HRESULT (WINAPI *QueryInfo)(IInternetProtocolInfo *This,LPCWSTR pwzUrl,QUERYOPTION OueryOption,DWORD dwQueryFlags,LPVOID pBuffer,DWORD cbBuffer,DWORD *pcbBuf,DWORD dwReserved);
    END_INTERFACE
  } IInternetProtocolInfoVtbl;
  struct IInternetProtocolInfo {
    CONST_VTBL struct IInternetProtocolInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetProtocolInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetProtocolInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetProtocolInfo_Release(This) (This)->lpVtbl->Release(This)
#define IInternetProtocolInfo_ParseUrl(This,pwzUrl,ParseAction,dwParseFlags,pwzResult,cchResult,pcchResult,dwReserved) (This)->lpVtbl->ParseUrl(This,pwzUrl,ParseAction,dwParseFlags,pwzResult,cchResult,pcchResult,dwReserved)
#define IInternetProtocolInfo_CombineUrl(This,pwzBaseUrl,pwzRelativeUrl,dwCombineFlags,pwzResult,cchResult,pcchResult,dwReserved) (This)->lpVtbl->CombineUrl(This,pwzBaseUrl,pwzRelativeUrl,dwCombineFlags,pwzResult,cchResult,pcchResult,dwReserved)
#define IInternetProtocolInfo_CompareUrl(This,pwzUrl1,pwzUrl2,dwCompareFlags) (This)->lpVtbl->CompareUrl(This,pwzUrl1,pwzUrl2,dwCompareFlags)
#define IInternetProtocolInfo_QueryInfo(This,pwzUrl,OueryOption,dwQueryFlags,pBuffer,cbBuffer,pcbBuf,dwReserved) (This)->lpVtbl->QueryInfo(This,pwzUrl,OueryOption,dwQueryFlags,pBuffer,cbBuffer,pcbBuf,dwReserved)
#endif
#endif
  HRESULT WINAPI IInternetProtocolInfo_ParseUrl_Proxy(IInternetProtocolInfo *This,LPCWSTR pwzUrl,PARSEACTION ParseAction,DWORD dwParseFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
  void __RPC_STUB IInternetProtocolInfo_ParseUrl_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolInfo_CombineUrl_Proxy(IInternetProtocolInfo *This,LPCWSTR pwzBaseUrl,LPCWSTR pwzRelativeUrl,DWORD dwCombineFlags,LPWSTR pwzResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
  void __RPC_STUB IInternetProtocolInfo_CombineUrl_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolInfo_CompareUrl_Proxy(IInternetProtocolInfo *This,LPCWSTR pwzUrl1,LPCWSTR pwzUrl2,DWORD dwCompareFlags);
  void __RPC_STUB IInternetProtocolInfo_CompareUrl_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetProtocolInfo_QueryInfo_Proxy(IInternetProtocolInfo *This,LPCWSTR pwzUrl,QUERYOPTION OueryOption,DWORD dwQueryFlags,LPVOID pBuffer,DWORD cbBuffer,DWORD *pcbBuf,DWORD dwReserved);
  void __RPC_STUB IInternetProtocolInfo_QueryInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#define IOInet IInternet
#define IOInetBindInfo IInternetBindInfo
#define IOInetProtocolRoot IInternetProtocolRoot
#define IOInetProtocol IInternetProtocol
#define IOInetProtocolSink IInternetProtocolSink
#define IOInetProtocolInfo IInternetProtocolInfo
#define IOInetSession IInternetSession
#define IOInetPriority IInternetPriority
#define IOInetThreadSwitch IInternetThreadSwitch
#define IOInetProtocolSinkStackable IInternetProtocolSinkStackable
#define LPOINET LPIINTERNET
#define LPOINETPROTOCOLINFO LPIINTERNETPROTOCOLINFO
#define LPOINETBINDINFO LPIINTERNETBINDINFO
#define LPOINETPROTOCOLROOT LPIINTERNETPROTOCOLROOT
#define LPOINETPROTOCOL LPIINTERNETPROTOCOL
#define LPOINETPROTOCOLSINK LPIINTERNETPROTOCOLSINK
#define LPOINETSESSION LPIINTERNETSESSION
#define LPOINETTHREADSWITCH LPIINTERNETTHREADSWITCH
#define LPOINETPRIORITY LPIINTERNETPRIORITY
#define LPOINETPROTOCOLINFO LPIINTERNETPROTOCOLINFO
#define LPOINETPROTOCOLSINKSTACKABLE LPIINTERNETPROTOCOLSINKSTACKABLE
#define IID_IOInet IID_IInternet
#define IID_IOInetBindInfo IID_IInternetBindInfo
#define IID_IOInetProtocolRoot IID_IInternetProtocolRoot
#define IID_IOInetProtocol IID_IInternetProtocol
#define IID_IOInetProtocolSink IID_IInternetProtocolSink
#define IID_IOInetProtocolInfo IID_IInternetProtocolInfo
#define IID_IOInetSession IID_IInternetSession
#define IID_IOInetPriority IID_IInternetPriority
#define IID_IOInetThreadSwitch IID_IInternetThreadSwitch
#define IID_IOInetProtocolSinkStackable IID_IInternetProtocolSinkStackable
  STDAPI CoInternetParseUrl(LPCWSTR pwzUrl,PARSEACTION ParseAction,DWORD dwFlags,LPWSTR pszResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
  STDAPI CoInternetCombineUrl(LPCWSTR pwzBaseUrl,LPCWSTR pwzRelativeUrl,DWORD dwCombineFlags,LPWSTR pszResult,DWORD cchResult,DWORD *pcchResult,DWORD dwReserved);
  STDAPI CoInternetCompareUrl(LPCWSTR pwzUrl1,LPCWSTR pwzUrl2,DWORD dwFlags);
  STDAPI CoInternetGetProtocolFlags(LPCWSTR pwzUrl,DWORD *pdwFlags,DWORD dwReserved);
  STDAPI CoInternetQueryInfo(LPCWSTR pwzUrl,QUERYOPTION QueryOptions,DWORD dwQueryFlags,LPVOID pvBuffer,DWORD cbBuffer,DWORD *pcbBuffer,DWORD dwReserved);
  STDAPI CoInternetGetSession(DWORD dwSessionMode,IInternetSession **ppIInternetSession,DWORD dwReserved);
  STDAPI CoInternetGetSecurityUrl(LPCWSTR pwzUrl,LPWSTR *ppwzSecUrl,PSUACTION psuAction,DWORD dwReserved);
  STDAPI AsyncInstallDistributionUnit(LPCWSTR szDistUnit,LPCWSTR szTYPE,LPCWSTR szExt,DWORD dwFileVersionMS,DWORD dwFileVersionLS,LPCWSTR szURL,IBindCtx *pbc,LPVOID pvReserved,DWORD flags);
#ifndef _INTERNETFEATURELIST_DEFINED
#define _INTERNETFEATURELIST_DEFINED

  typedef enum _tagINTERNETFEATURELIST {
    FEATURE_OBJECT_CACHING = 0,
    FEATURE_ZONE_ELEVATION,FEATURE_MIME_HANDLING,FEATURE_MIME_SNIFFING,
    FEATURE_WINDOW_RESTRICTIONS,FEATURE_WEBOC_POPUPMANAGEMENT,
    FEATURE_BEHAVIORS,FEATURE_DISABLE_MK_PROTOCOL,FEATURE_LOCALMACHINE_LOCKDOWN,
    FEATURE_SECURITYBAND,FEATURE_RESTRICT_ACTIVEXINSTALL,FEATURE_VALIDATE_NAVIGATE_URL,
    FEATURE_RESTRICT_FILEDOWNLOAD,FEATURE_ADDON_MANAGEMENT,FEATURE_PROTOCOL_LOCKDOWN,
    FEATURE_HTTP_USERNAME_PASSWORD_DISABLE,FEATURE_SAFE_BINDTOOBJECT,
    FEATURE_UNC_SAVEDFILECHECK,FEATURE_GET_URL_DOM_FILEPATH_UNENCODED,
    FEATURE_ENTRY_COUNT
  } INTERNETFEATURELIST;

#define SET_FEATURE_ON_THREAD 0x00000001
#define SET_FEATURE_ON_PROCESS 0x00000002
#define SET_FEATURE_IN_REGISTRY 0x00000004
#define SET_FEATURE_ON_THREAD_LOCALMACHINE 0x00000008
#define SET_FEATURE_ON_THREAD_INTRANET 0x00000010
#define SET_FEATURE_ON_THREAD_TRUSTED 0x00000020
#define SET_FEATURE_ON_THREAD_INTERNET 0x00000040
#define SET_FEATURE_ON_THREAD_RESTRICTED 0x00000080

#define GET_FEATURE_FROM_THREAD 0x00000001
#define GET_FEATURE_FROM_PROCESS 0x00000002
#define GET_FEATURE_FROM_REGISTRY 0x00000004
#define GET_FEATURE_FROM_THREAD_LOCALMACHINE 0x00000008
#define GET_FEATURE_FROM_THREAD_INTRANET 0x00000010
#define GET_FEATURE_FROM_THREAD_TRUSTED 0x00000020
#define GET_FEATURE_FROM_THREAD_INTERNET 0x00000040
#define GET_FEATURE_FROM_THREAD_RESTRICTED 0x00000080
#endif
  STDAPI CoInternetSetFeatureEnabled(INTERNETFEATURELIST FeatureEntry,DWORD dwFlags,WINBOOL fEnable);
  STDAPI CoInternetIsFeatureEnabled(INTERNETFEATURELIST FeatureEntry,DWORD dwFlags);
  STDAPI CoInternetIsFeatureEnabledForUrl(INTERNETFEATURELIST FeatureEntry,DWORD dwFlags,LPCWSTR szURL,IInternetSecurityManager *pSecMgr);
  STDAPI CoInternetIsFeatureZoneElevationEnabled(LPCWSTR szFromURL,LPCWSTR szToURL,IInternetSecurityManager *pSecMgr,DWORD dwFlags);
  STDAPI CopyStgMedium(const STGMEDIUM *pcstgmedSrc,STGMEDIUM *pstgmedDest);
  STDAPI CopyBindInfo(const BINDINFO *pcbiSrc,BINDINFO *pbiDest);
  STDAPI_(void) ReleaseBindInfo(BINDINFO *pbindinfo);

#define INET_E_USE_DEFAULT_PROTOCOLHANDLER _HRESULT_TYPEDEF_(0x800C0011L)
#define INET_E_USE_DEFAULT_SETTING _HRESULT_TYPEDEF_(0x800C0012L)
#define INET_E_DEFAULT_ACTION INET_E_USE_DEFAULT_PROTOCOLHANDLER
#define INET_E_QUERYOPTION_UNKNOWN _HRESULT_TYPEDEF_(0x800C0013L)
#define INET_E_REDIRECTING _HRESULT_TYPEDEF_(0x800C0014L)
#define OInetParseUrl CoInternetParseUrl
#define OInetCombineUrl CoInternetCombineUrl
#define OInetCompareUrl CoInternetCompareUrl
#define OInetQueryInfo CoInternetQueryInfo
#define OInetGetSession CoInternetGetSession
#endif

#define PROTOCOLFLAG_NO_PICS_CHECK 0x00000001

  STDAPI CoInternetCreateSecurityManager(IServiceProvider *pSP,IInternetSecurityManager **ppSM,DWORD dwReserved);
  STDAPI CoInternetCreateZoneManager(IServiceProvider *pSP,IInternetZoneManager **ppZM,DWORD dwReserved);

  EXTERN_C const IID CLSID_InternetSecurityManager;
  EXTERN_C const IID CLSID_InternetZoneManager;
  EXTERN_C const IID CLSID_PersistentZoneIdentifier;

#define SID_SInternetSecurityManager IID_IInternetSecurityManager
#define SID_SInternetSecurityManagerEx IID_IInternetSecurityManagerEx
#define SID_SInternetHostSecurityManager IID_IInternetHostSecurityManager

#ifndef _LPINTERNETSECURITYMGRSITE_DEFINED
#define _LPINTERNETSECURITYMGRSITE_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0203_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0203_v0_0_s_ifspec;
#ifndef __IInternetSecurityMgrSite_INTERFACE_DEFINED__
#define __IInternetSecurityMgrSite_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IInternetSecurityMgrSite;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetSecurityMgrSite : public IUnknown {
  public:
    virtual HRESULT WINAPI GetWindow(HWND *phwnd) = 0;
    virtual HRESULT WINAPI EnableModeless(WINBOOL fEnable) = 0;
  };
#else
  typedef struct IInternetSecurityMgrSiteVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetSecurityMgrSite *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetSecurityMgrSite *This);
      ULONG (WINAPI *Release)(IInternetSecurityMgrSite *This);
      HRESULT (WINAPI *GetWindow)(IInternetSecurityMgrSite *This,HWND *phwnd);
      HRESULT (WINAPI *EnableModeless)(IInternetSecurityMgrSite *This,WINBOOL fEnable);
    END_INTERFACE
  } IInternetSecurityMgrSiteVtbl;
  struct IInternetSecurityMgrSite {
    CONST_VTBL struct IInternetSecurityMgrSiteVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetSecurityMgrSite_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetSecurityMgrSite_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetSecurityMgrSite_Release(This) (This)->lpVtbl->Release(This)
#define IInternetSecurityMgrSite_GetWindow(This,phwnd) (This)->lpVtbl->GetWindow(This,phwnd)
#define IInternetSecurityMgrSite_EnableModeless(This,fEnable) (This)->lpVtbl->EnableModeless(This,fEnable)
#endif
#endif
  HRESULT WINAPI IInternetSecurityMgrSite_GetWindow_Proxy(IInternetSecurityMgrSite *This,HWND *phwnd);
  void __RPC_STUB IInternetSecurityMgrSite_GetWindow_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityMgrSite_EnableModeless_Proxy(IInternetSecurityMgrSite *This,WINBOOL fEnable);
  void __RPC_STUB IInternetSecurityMgrSite_EnableModeless_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPINTERNETSECURITYMANANGEREX_DEFINED
#define _LPINTERNETSECURITYMANANGEREX_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0204_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0204_v0_0_s_ifspec;
#ifndef __IInternetSecurityManager_INTERFACE_DEFINED__
#define __IInternetSecurityManager_INTERFACE_DEFINED__

#define MUTZ_NOSAVEDFILECHECK 0x00000001
#define MUTZ_ISFILE 0x00000002
#define MUTZ_ACCEPT_WILDCARD_SCHEME 0x00000080
#define MUTZ_ENFORCERESTRICTED 0x00000100
#define MUTZ_REQUIRESAVEDFILECHECK 0x00000400
#define MUTZ_DONT_UNESCAPE 0x00000800

#define MAX_SIZE_SECURITY_ID 512

  typedef enum __MIDL_IInternetSecurityManager_0001 {
    PUAF_DEFAULT = 0,PUAF_NOUI = 0x1,PUAF_ISFILE = 0x2,PUAF_WARN_IF_DENIED = 0x4,PUAF_FORCEUI_FOREGROUND = 0x8,PUAF_CHECK_TIFS = 0x10,
    PUAF_DONTCHECKBOXINDIALOG = 0x20,PUAF_TRUSTED = 0x40,PUAF_ACCEPT_WILDCARD_SCHEME = 0x80,PUAF_ENFORCERESTRICTED = 0x100,
    PUAF_NOSAVEDFILECHECK = 0x200,PUAF_REQUIRESAVEDFILECHECK = 0x400,PUAF_LMZ_UNLOCKED = 0x10000,PUAF_LMZ_LOCKED = 0x20000,
    PUAF_DEFAULTZONEPOL = 0x40000,PUAF_NPL_USE_LOCKED_IF_RESTRICTED = 0x80000,PUAF_NOUIIFLOCKED = 0x100000,PUAF_DRAGPROTOCOLCHECK = 0x200000
  } PUAF;

  typedef enum __MIDL_IInternetSecurityManager_0002 {
    PUAFOUT_DEFAULT = 0,PUAFOUT_ISLOCKZONEPOLICY = 0x1
  } PUAFOUT;

  typedef enum __MIDL_IInternetSecurityManager_0003 {
    SZM_CREATE = 0,SZM_DELETE = 0x1
  } SZM_FLAGS;

  EXTERN_C const IID IID_IInternetSecurityManager;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetSecurityManager : public IUnknown {
  public:
    virtual HRESULT WINAPI SetSecuritySite(IInternetSecurityMgrSite *pSite) = 0;
    virtual HRESULT WINAPI GetSecuritySite(IInternetSecurityMgrSite **ppSite) = 0;
    virtual HRESULT WINAPI MapUrlToZone(LPCWSTR pwszUrl,DWORD *pdwZone,DWORD dwFlags) = 0;
    virtual HRESULT WINAPI GetSecurityId(LPCWSTR pwszUrl,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved) = 0;
    virtual HRESULT WINAPI ProcessUrlAction(LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI QueryCustomPolicy(LPCWSTR pwszUrl,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI SetZoneMapping(DWORD dwZone,LPCWSTR lpszPattern,DWORD dwFlags) = 0;
    virtual HRESULT WINAPI GetZoneMappings(DWORD dwZone,IEnumString **ppenumString,DWORD dwFlags) = 0;
  };
#else
  typedef struct IInternetSecurityManagerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetSecurityManager *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetSecurityManager *This);
      ULONG (WINAPI *Release)(IInternetSecurityManager *This);
      HRESULT (WINAPI *SetSecuritySite)(IInternetSecurityManager *This,IInternetSecurityMgrSite *pSite);
      HRESULT (WINAPI *GetSecuritySite)(IInternetSecurityManager *This,IInternetSecurityMgrSite **ppSite);
      HRESULT (WINAPI *MapUrlToZone)(IInternetSecurityManager *This,LPCWSTR pwszUrl,DWORD *pdwZone,DWORD dwFlags);
      HRESULT (WINAPI *GetSecurityId)(IInternetSecurityManager *This,LPCWSTR pwszUrl,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
      HRESULT (WINAPI *ProcessUrlAction)(IInternetSecurityManager *This,LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved);
      HRESULT (WINAPI *QueryCustomPolicy)(IInternetSecurityManager *This,LPCWSTR pwszUrl,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved);
      HRESULT (WINAPI *SetZoneMapping)(IInternetSecurityManager *This,DWORD dwZone,LPCWSTR lpszPattern,DWORD dwFlags);
      HRESULT (WINAPI *GetZoneMappings)(IInternetSecurityManager *This,DWORD dwZone,IEnumString **ppenumString,DWORD dwFlags);
    END_INTERFACE
  } IInternetSecurityManagerVtbl;
  struct IInternetSecurityManager {
    CONST_VTBL struct IInternetSecurityManagerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetSecurityManager_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetSecurityManager_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetSecurityManager_Release(This) (This)->lpVtbl->Release(This)
#define IInternetSecurityManager_SetSecuritySite(This,pSite) (This)->lpVtbl->SetSecuritySite(This,pSite)
#define IInternetSecurityManager_GetSecuritySite(This,ppSite) (This)->lpVtbl->GetSecuritySite(This,ppSite)
#define IInternetSecurityManager_MapUrlToZone(This,pwszUrl,pdwZone,dwFlags) (This)->lpVtbl->MapUrlToZone(This,pwszUrl,pdwZone,dwFlags)
#define IInternetSecurityManager_GetSecurityId(This,pwszUrl,pbSecurityId,pcbSecurityId,dwReserved) (This)->lpVtbl->GetSecurityId(This,pwszUrl,pbSecurityId,pcbSecurityId,dwReserved)
#define IInternetSecurityManager_ProcessUrlAction(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved) (This)->lpVtbl->ProcessUrlAction(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved)
#define IInternetSecurityManager_QueryCustomPolicy(This,pwszUrl,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved) (This)->lpVtbl->QueryCustomPolicy(This,pwszUrl,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved)
#define IInternetSecurityManager_SetZoneMapping(This,dwZone,lpszPattern,dwFlags) (This)->lpVtbl->SetZoneMapping(This,dwZone,lpszPattern,dwFlags)
#define IInternetSecurityManager_GetZoneMappings(This,dwZone,ppenumString,dwFlags) (This)->lpVtbl->GetZoneMappings(This,dwZone,ppenumString,dwFlags)
#endif
#endif
  HRESULT WINAPI IInternetSecurityManager_SetSecuritySite_Proxy(IInternetSecurityManager *This,IInternetSecurityMgrSite *pSite);
  void __RPC_STUB IInternetSecurityManager_SetSecuritySite_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_GetSecuritySite_Proxy(IInternetSecurityManager *This,IInternetSecurityMgrSite **ppSite);
  void __RPC_STUB IInternetSecurityManager_GetSecuritySite_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_MapUrlToZone_Proxy(IInternetSecurityManager *This,LPCWSTR pwszUrl,DWORD *pdwZone,DWORD dwFlags);
  void __RPC_STUB IInternetSecurityManager_MapUrlToZone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_GetSecurityId_Proxy(IInternetSecurityManager *This,LPCWSTR pwszUrl,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
  void __RPC_STUB IInternetSecurityManager_GetSecurityId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_ProcessUrlAction_Proxy(IInternetSecurityManager *This,LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved);
  void __RPC_STUB IInternetSecurityManager_ProcessUrlAction_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_QueryCustomPolicy_Proxy(IInternetSecurityManager *This,LPCWSTR pwszUrl,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved);
  void __RPC_STUB IInternetSecurityManager_QueryCustomPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_SetZoneMapping_Proxy(IInternetSecurityManager *This,DWORD dwZone,LPCWSTR lpszPattern,DWORD dwFlags);
  void __RPC_STUB IInternetSecurityManager_SetZoneMapping_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetSecurityManager_GetZoneMappings_Proxy(IInternetSecurityManager *This,DWORD dwZone,IEnumString **ppenumString,DWORD dwFlags);
  void __RPC_STUB IInternetSecurityManager_GetZoneMappings_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IInternetSecurityManagerEx_INTERFACE_DEFINED__
#define __IInternetSecurityManagerEx_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IInternetSecurityManagerEx;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetSecurityManagerEx : public IInternetSecurityManager {
  public:
    virtual HRESULT WINAPI ProcessUrlActionEx(LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved,DWORD *pdwOutFlags) = 0;
  };
#else
  typedef struct IInternetSecurityManagerExVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetSecurityManagerEx *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetSecurityManagerEx *This);
      ULONG (WINAPI *Release)(IInternetSecurityManagerEx *This);
      HRESULT (WINAPI *SetSecuritySite)(IInternetSecurityManagerEx *This,IInternetSecurityMgrSite *pSite);
      HRESULT (WINAPI *GetSecuritySite)(IInternetSecurityManagerEx *This,IInternetSecurityMgrSite **ppSite);
      HRESULT (WINAPI *MapUrlToZone)(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,DWORD *pdwZone,DWORD dwFlags);
      HRESULT (WINAPI *GetSecurityId)(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
      HRESULT (WINAPI *ProcessUrlAction)(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved);
      HRESULT (WINAPI *QueryCustomPolicy)(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved);
      HRESULT (WINAPI *SetZoneMapping)(IInternetSecurityManagerEx *This,DWORD dwZone,LPCWSTR lpszPattern,DWORD dwFlags);
      HRESULT (WINAPI *GetZoneMappings)(IInternetSecurityManagerEx *This,DWORD dwZone,IEnumString **ppenumString,DWORD dwFlags);
      HRESULT (WINAPI *ProcessUrlActionEx)(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved,DWORD *pdwOutFlags);
    END_INTERFACE
  } IInternetSecurityManagerExVtbl;
  struct IInternetSecurityManagerEx {
    CONST_VTBL struct IInternetSecurityManagerExVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetSecurityManagerEx_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetSecurityManagerEx_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetSecurityManagerEx_Release(This) (This)->lpVtbl->Release(This)
#define IInternetSecurityManagerEx_SetSecuritySite(This,pSite) (This)->lpVtbl->SetSecuritySite(This,pSite)
#define IInternetSecurityManagerEx_GetSecuritySite(This,ppSite) (This)->lpVtbl->GetSecuritySite(This,ppSite)
#define IInternetSecurityManagerEx_MapUrlToZone(This,pwszUrl,pdwZone,dwFlags) (This)->lpVtbl->MapUrlToZone(This,pwszUrl,pdwZone,dwFlags)
#define IInternetSecurityManagerEx_GetSecurityId(This,pwszUrl,pbSecurityId,pcbSecurityId,dwReserved) (This)->lpVtbl->GetSecurityId(This,pwszUrl,pbSecurityId,pcbSecurityId,dwReserved)
#define IInternetSecurityManagerEx_ProcessUrlAction(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved) (This)->lpVtbl->ProcessUrlAction(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved)
#define IInternetSecurityManagerEx_QueryCustomPolicy(This,pwszUrl,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved) (This)->lpVtbl->QueryCustomPolicy(This,pwszUrl,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved)
#define IInternetSecurityManagerEx_SetZoneMapping(This,dwZone,lpszPattern,dwFlags) (This)->lpVtbl->SetZoneMapping(This,dwZone,lpszPattern,dwFlags)
#define IInternetSecurityManagerEx_GetZoneMappings(This,dwZone,ppenumString,dwFlags) (This)->lpVtbl->GetZoneMappings(This,dwZone,ppenumString,dwFlags)
#define IInternetSecurityManagerEx_ProcessUrlActionEx(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved,pdwOutFlags) (This)->lpVtbl->ProcessUrlActionEx(This,pwszUrl,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved,pdwOutFlags)
#endif
#endif
  HRESULT WINAPI IInternetSecurityManagerEx_ProcessUrlActionEx_Proxy(IInternetSecurityManagerEx *This,LPCWSTR pwszUrl,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved,DWORD *pdwOutFlags);
  void __RPC_STUB IInternetSecurityManagerEx_ProcessUrlActionEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPINTERNETSECURITYMANANGER_DEFINED
#define _LPINTERNETSECURITYMANANGER_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0205_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0205_v0_0_s_ifspec;
#ifndef __IZoneIdentifier_INTERFACE_DEFINED__
#define __IZoneIdentifier_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IZoneIdentifier;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IZoneIdentifier : public IUnknown {
  public:
    virtual HRESULT WINAPI GetId(DWORD *pdwZone) = 0;
    virtual HRESULT WINAPI SetId(DWORD dwZone) = 0;
    virtual HRESULT WINAPI Remove(void) = 0;
  };
#else
  typedef struct IZoneIdentifierVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IZoneIdentifier *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IZoneIdentifier *This);
      ULONG (WINAPI *Release)(IZoneIdentifier *This);
      HRESULT (WINAPI *GetId)(IZoneIdentifier *This,DWORD *pdwZone);
      HRESULT (WINAPI *SetId)(IZoneIdentifier *This,DWORD dwZone);
      HRESULT (WINAPI *Remove)(IZoneIdentifier *This);
    END_INTERFACE
  } IZoneIdentifierVtbl;
  struct IZoneIdentifier {
    CONST_VTBL struct IZoneIdentifierVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IZoneIdentifier_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IZoneIdentifier_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IZoneIdentifier_Release(This) (This)->lpVtbl->Release(This)
#define IZoneIdentifier_GetId(This,pdwZone) (This)->lpVtbl->GetId(This,pdwZone)
#define IZoneIdentifier_SetId(This,dwZone) (This)->lpVtbl->SetId(This,dwZone)
#define IZoneIdentifier_Remove(This) (This)->lpVtbl->Remove(This)
#endif
#endif
  HRESULT WINAPI IZoneIdentifier_GetId_Proxy(IZoneIdentifier *This,DWORD *pdwZone);
  void __RPC_STUB IZoneIdentifier_GetId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IZoneIdentifier_SetId_Proxy(IZoneIdentifier *This,DWORD dwZone);
  void __RPC_STUB IZoneIdentifier_SetId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IZoneIdentifier_Remove_Proxy(IZoneIdentifier *This);
  void __RPC_STUB IZoneIdentifier_Remove_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPINTERNETHOSTSECURITYMANANGER_DEFINED
#define _LPINTERNETHOSTSECURITYMANANGER_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0207_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0207_v0_0_s_ifspec;
#ifndef __IInternetHostSecurityManager_INTERFACE_DEFINED__
#define __IInternetHostSecurityManager_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IInternetHostSecurityManager;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetHostSecurityManager : public IUnknown {
  public:
    virtual HRESULT WINAPI GetSecurityId(BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved) = 0;
    virtual HRESULT WINAPI ProcessUrlAction(DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI QueryCustomPolicy(REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved) = 0;
  };
#else
  typedef struct IInternetHostSecurityManagerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetHostSecurityManager *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetHostSecurityManager *This);
      ULONG (WINAPI *Release)(IInternetHostSecurityManager *This);
      HRESULT (WINAPI *GetSecurityId)(IInternetHostSecurityManager *This,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
      HRESULT (WINAPI *ProcessUrlAction)(IInternetHostSecurityManager *This,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved);
      HRESULT (WINAPI *QueryCustomPolicy)(IInternetHostSecurityManager *This,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved);
    END_INTERFACE
  } IInternetHostSecurityManagerVtbl;
  struct IInternetHostSecurityManager {
    CONST_VTBL struct IInternetHostSecurityManagerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetHostSecurityManager_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetHostSecurityManager_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetHostSecurityManager_Release(This) (This)->lpVtbl->Release(This)
#define IInternetHostSecurityManager_GetSecurityId(This,pbSecurityId,pcbSecurityId,dwReserved) (This)->lpVtbl->GetSecurityId(This,pbSecurityId,pcbSecurityId,dwReserved)
#define IInternetHostSecurityManager_ProcessUrlAction(This,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved) (This)->lpVtbl->ProcessUrlAction(This,dwAction,pPolicy,cbPolicy,pContext,cbContext,dwFlags,dwReserved)
#define IInternetHostSecurityManager_QueryCustomPolicy(This,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved) (This)->lpVtbl->QueryCustomPolicy(This,guidKey,ppPolicy,pcbPolicy,pContext,cbContext,dwReserved)
#endif
#endif
  HRESULT WINAPI IInternetHostSecurityManager_GetSecurityId_Proxy(IInternetHostSecurityManager *This,BYTE *pbSecurityId,DWORD *pcbSecurityId,DWORD_PTR dwReserved);
  void __RPC_STUB IInternetHostSecurityManager_GetSecurityId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetHostSecurityManager_ProcessUrlAction_Proxy(IInternetHostSecurityManager *This,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwFlags,DWORD dwReserved);
  void __RPC_STUB IInternetHostSecurityManager_ProcessUrlAction_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetHostSecurityManager_QueryCustomPolicy_Proxy(IInternetHostSecurityManager *This,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,BYTE *pContext,DWORD cbContext,DWORD dwReserved);
  void __RPC_STUB IInternetHostSecurityManager_QueryCustomPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#define URLACTION_MIN 0x00001000
#define URLACTION_DOWNLOAD_MIN 0x00001000
#define URLACTION_DOWNLOAD_SIGNED_ACTIVEX 0x00001001
#define URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX 0x00001004
#define URLACTION_DOWNLOAD_CURR_MAX 0x00001004
#define URLACTION_DOWNLOAD_MAX 0x000011FF

#define URLACTION_ACTIVEX_MIN 0x00001200
#define URLACTION_ACTIVEX_RUN 0x00001200
#define URLPOLICY_ACTIVEX_CHECK_LIST 0x00010000
#define URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY 0x00001201
#define URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY 0x00001202
#define URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY 0x00001203
#define URLACTION_SCRIPT_OVERRIDE_SAFETY 0x00001401
#define URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY 0x00001204
#define URLACTION_ACTIVEX_TREATASUNTRUSTED 0x00001205
#define URLACTION_ACTIVEX_NO_WEBOC_SCRIPT 0x00001206
#define URLACTION_ACTIVEX_CURR_MAX 0x00001206
#define URLACTION_ACTIVEX_MAX 0x000013ff

#define URLACTION_SCRIPT_MIN 0x00001400
#define URLACTION_SCRIPT_RUN 0x00001400
#define URLACTION_SCRIPT_JAVA_USE 0x00001402
#define URLACTION_SCRIPT_SAFE_ACTIVEX 0x00001405
#define URLACTION_CROSS_DOMAIN_DATA 0x00001406
#define URLACTION_SCRIPT_PASTE 0x00001407
#define URLACTION_SCRIPT_CURR_MAX 0x00001407
#define URLACTION_SCRIPT_MAX 0x000015ff

#define URLACTION_HTML_MIN 0x00001600
#define URLACTION_HTML_SUBMIT_FORMS 0x00001601
#define URLACTION_HTML_SUBMIT_FORMS_FROM 0x00001602
#define URLACTION_HTML_SUBMIT_FORMS_TO 0x00001603
#define URLACTION_HTML_FONT_DOWNLOAD 0x00001604
#define URLACTION_HTML_JAVA_RUN 0x00001605
#define URLACTION_HTML_USERDATA_SAVE 0x00001606
#define URLACTION_HTML_SUBFRAME_NAVIGATE 0x00001607
#define URLACTION_HTML_META_REFRESH 0x00001608
#define URLACTION_HTML_MIXED_CONTENT 0x00001609
#define URLACTION_HTML_MAX 0x000017ff

#define URLACTION_SHELL_MIN 0x00001800
#define URLACTION_SHELL_INSTALL_DTITEMS 0x00001800
#define URLACTION_SHELL_MOVE_OR_COPY 0x00001802
#define URLACTION_SHELL_FILE_DOWNLOAD 0x00001803
#define URLACTION_SHELL_VERB 0x00001804
#define URLACTION_SHELL_WEBVIEW_VERB 0x00001805
#define URLACTION_SHELL_SHELLEXECUTE 0x00001806
#define URLACTION_SHELL_EXECUTE_HIGHRISK 0x00001806
#define URLACTION_SHELL_EXECUTE_MODRISK 0x00001807
#define URLACTION_SHELL_EXECUTE_LOWRISK 0x00001808
#define URLACTION_SHELL_POPUPMGR 0x00001809
#define URLACTION_SHELL_RTF_OBJECTS_LOAD 0x0000180A
#define URLACTION_SHELL_ENHANCED_DRAGDROP_SECURITY 0x0000180B
#define URLACTION_SHELL_CURR_MAX 0x0000180B
#define URLACTION_SHELL_MAX 0x000019ff

#define URLACTION_NETWORK_MIN 0x00001A00

#define URLACTION_CREDENTIALS_USE 0x00001A00
#define URLPOLICY_CREDENTIALS_SILENT_LOGON_OK 0x00000000
#define URLPOLICY_CREDENTIALS_MUST_PROMPT_USER 0x00010000
#define URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT 0x00020000
#define URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY 0x00030000

#define URLACTION_AUTHENTICATE_CLIENT 0x00001A01
#define URLPOLICY_AUTHENTICATE_CLEARTEXT_OK 0x00000000
#define URLPOLICY_AUTHENTICATE_CHALLENGE_RESPONSE 0x00010000
#define URLPOLICY_AUTHENTICATE_MUTUAL_ONLY 0x00030000

#define URLACTION_COOKIES 0x00001A02
#define URLACTION_COOKIES_SESSION 0x00001A03

#define URLACTION_CLIENT_CERT_PROMPT 0x00001A04

#define URLACTION_COOKIES_THIRD_PARTY 0x00001A05
#define URLACTION_COOKIES_SESSION_THIRD_PARTY 0x00001A06

#define URLACTION_COOKIES_ENABLED 0x00001A10

#define URLACTION_NETWORK_CURR_MAX 0x00001A10
#define URLACTION_NETWORK_MAX 0x00001Bff

#define URLACTION_JAVA_MIN 0x00001C00
#define URLACTION_JAVA_PERMISSIONS 0x00001C00
#define URLPOLICY_JAVA_PROHIBIT 0x00000000
#define URLPOLICY_JAVA_HIGH 0x00010000
#define URLPOLICY_JAVA_MEDIUM 0x00020000
#define URLPOLICY_JAVA_LOW 0x00030000
#define URLPOLICY_JAVA_CUSTOM 0x00800000
#define URLACTION_JAVA_CURR_MAX 0x00001C00
#define URLACTION_JAVA_MAX 0x00001Cff

#define URLACTION_INFODELIVERY_MIN 0x00001D00
#define URLACTION_INFODELIVERY_NO_ADDING_CHANNELS 0x00001D00
#define URLACTION_INFODELIVERY_NO_EDITING_CHANNELS 0x00001D01
#define URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS 0x00001D02
#define URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS 0x00001D03
#define URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS 0x00001D04
#define URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS 0x00001D05
#define URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING 0x00001D06
#define URLACTION_INFODELIVERY_CURR_MAX 0x00001D06
#define URLACTION_INFODELIVERY_MAX 0x00001Dff
#define URLACTION_CHANNEL_SOFTDIST_MIN 0x00001E00
#define URLACTION_CHANNEL_SOFTDIST_PERMISSIONS 0x00001E05
#define URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT 0x00010000
#define URLPOLICY_CHANNEL_SOFTDIST_PRECACHE 0x00020000
#define URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL 0x00030000
#define URLACTION_CHANNEL_SOFTDIST_MAX 0x00001Eff
#define URLACTION_BEHAVIOR_MIN 0x00002000
#define URLACTION_BEHAVIOR_RUN 0x00002000
#define URLPOLICY_BEHAVIOR_CHECK_LIST 0x00010000

#define URLACTION_FEATURE_MIN 0x00002100
#define URLACTION_FEATURE_MIME_SNIFFING 0x00002100
#define URLACTION_FEATURE_ZONE_ELEVATION 0x00002101
#define URLACTION_FEATURE_WINDOW_RESTRICTIONS 0x00002102

#define URLACTION_AUTOMATIC_DOWNLOAD_UI_MIN 0x00002200
#define URLACTION_AUTOMATIC_DOWNLOAD_UI 0x00002200
#define URLACTION_AUTOMATIC_ACTIVEX_UI 0x00002201

#define URLACTION_ALLOW_RESTRICTEDPROTOCOLS 0x00002300

#define URLPOLICY_ALLOW 0x00
#define URLPOLICY_QUERY 0x01
#define URLPOLICY_DISALLOW 0x03

#define URLPOLICY_NOTIFY_ON_ALLOW 0x10
#define URLPOLICY_NOTIFY_ON_DISALLOW 0x20

#define URLPOLICY_LOG_ON_ALLOW 0x40
#define URLPOLICY_LOG_ON_DISALLOW 0x80

#define URLPOLICY_MASK_PERMISSIONS 0x0f
#define GetUrlPolicyPermissions(dw) (dw & URLPOLICY_MASK_PERMISSIONS)
#define SetUrlPolicyPermissions(dw,dw2) ((dw) = ((dw) & ~(URLPOLICY_MASK_PERMISSIONS)) | (dw2))

#define URLPOLICY_DONTCHECKDLGBOX 0x100

  EXTERN_C const GUID GUID_CUSTOM_LOCALMACHINEZONEUNLOCKED;
#ifndef _LPINTERNETZONEMANAGER_DEFINED
#define _LPINTERNETZONEMANAGER_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0208_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0208_v0_0_s_ifspec;
#ifndef __IInternetZoneManager_INTERFACE_DEFINED__
#define __IInternetZoneManager_INTERFACE_DEFINED__

  typedef IInternetZoneManager *LPURLZONEMANAGER;

  typedef enum tagURLZONE {
    URLZONE_PREDEFINED_MIN = 0,URLZONE_LOCAL_MACHINE = 0,
    URLZONE_INTRANET,URLZONE_TRUSTED,URLZONE_INTERNET,URLZONE_UNTRUSTED,
    URLZONE_PREDEFINED_MAX = 999,URLZONE_USER_MIN = 1000,
    URLZONE_USER_MAX = 10000
  } URLZONE;

#define URLZONE_ESC_FLAG 0x100

  typedef enum tagURLTEMPLATE {
    URLTEMPLATE_CUSTOM = 0,
    URLTEMPLATE_PREDEFINED_MIN = 0x10000,
    URLTEMPLATE_LOW = 0x10000,
    URLTEMPLATE_MEDLOW = 0x10500,
    URLTEMPLATE_MEDIUM = 0x11000,
    URLTEMPLATE_HIGH = 0x12000,
    URLTEMPLATE_PREDEFINED_MAX = 0x20000
  } URLTEMPLATE;

  enum __MIDL_IInternetZoneManager_0001 {
    MAX_ZONE_PATH = 260,MAX_ZONE_DESCRIPTION = 200
  };
  typedef enum __MIDL_IInternetZoneManager_0002 {
    ZAFLAGS_CUSTOM_EDIT = 0x1,ZAFLAGS_ADD_SITES = 0x2,ZAFLAGS_REQUIRE_VERIFICATION = 0x4,ZAFLAGS_INCLUDE_PROXY_OVERRIDE = 0x8,
    ZAFLAGS_INCLUDE_INTRANET_SITES = 0x10,ZAFLAGS_NO_UI = 0x20,ZAFLAGS_SUPPORTS_VERIFICATION = 0x40,ZAFLAGS_UNC_AS_INTRANET = 0x80,
    ZAFLAGS_USE_LOCKED_ZONES = 0x10000
  } ZAFLAGS;

  typedef struct _ZONEATTRIBUTES {
    ULONG cbSize;
    WCHAR szDisplayName[260 ];
    WCHAR szDescription[200 ];
    WCHAR szIconPath[260 ];
    DWORD dwTemplateMinLevel;
    DWORD dwTemplateRecommended;
    DWORD dwTemplateCurrentLevel;
    DWORD dwFlags;
  } ZONEATTRIBUTES;

  typedef struct _ZONEATTRIBUTES *LPZONEATTRIBUTES;

  typedef enum _URLZONEREG {
    URLZONEREG_DEFAULT = 0,
    URLZONEREG_HKLM,URLZONEREG_HKCU
  } URLZONEREG;

  EXTERN_C const IID IID_IInternetZoneManager;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetZoneManager : public IUnknown {
  public:
    virtual HRESULT WINAPI GetZoneAttributes(DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes) = 0;
    virtual HRESULT WINAPI SetZoneAttributes(DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes) = 0;
    virtual HRESULT WINAPI GetZoneCustomPolicy(DWORD dwZone,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,URLZONEREG urlZoneReg) = 0;
    virtual HRESULT WINAPI SetZoneCustomPolicy(DWORD dwZone,REFGUID guidKey,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg) = 0;
    virtual HRESULT WINAPI GetZoneActionPolicy(DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg) = 0;
    virtual HRESULT WINAPI SetZoneActionPolicy(DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg) = 0;
    virtual HRESULT WINAPI PromptAction(DWORD dwAction,HWND hwndParent,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwPromptFlags) = 0;
    virtual HRESULT WINAPI LogAction(DWORD dwAction,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwLogFlags) = 0;
    virtual HRESULT WINAPI CreateZoneEnumerator(DWORD *pdwEnum,DWORD *pdwCount,DWORD dwFlags) = 0;
    virtual HRESULT WINAPI GetZoneAt(DWORD dwEnum,DWORD dwIndex,DWORD *pdwZone) = 0;
    virtual HRESULT WINAPI DestroyZoneEnumerator(DWORD dwEnum) = 0;
    virtual HRESULT WINAPI CopyTemplatePoliciesToZone(DWORD dwTemplate,DWORD dwZone,DWORD dwReserved) = 0;
  };
#else
  typedef struct IInternetZoneManagerVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetZoneManager *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetZoneManager *This);
      ULONG (WINAPI *Release)(IInternetZoneManager *This);
      HRESULT (WINAPI *GetZoneAttributes)(IInternetZoneManager *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
      HRESULT (WINAPI *SetZoneAttributes)(IInternetZoneManager *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
      HRESULT (WINAPI *GetZoneCustomPolicy)(IInternetZoneManager *This,DWORD dwZone,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *SetZoneCustomPolicy)(IInternetZoneManager *This,DWORD dwZone,REFGUID guidKey,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *GetZoneActionPolicy)(IInternetZoneManager *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *SetZoneActionPolicy)(IInternetZoneManager *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *PromptAction)(IInternetZoneManager *This,DWORD dwAction,HWND hwndParent,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwPromptFlags);
      HRESULT (WINAPI *LogAction)(IInternetZoneManager *This,DWORD dwAction,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwLogFlags);
      HRESULT (WINAPI *CreateZoneEnumerator)(IInternetZoneManager *This,DWORD *pdwEnum,DWORD *pdwCount,DWORD dwFlags);
      HRESULT (WINAPI *GetZoneAt)(IInternetZoneManager *This,DWORD dwEnum,DWORD dwIndex,DWORD *pdwZone);
      HRESULT (WINAPI *DestroyZoneEnumerator)(IInternetZoneManager *This,DWORD dwEnum);
      HRESULT (WINAPI *CopyTemplatePoliciesToZone)(IInternetZoneManager *This,DWORD dwTemplate,DWORD dwZone,DWORD dwReserved);
    END_INTERFACE
  } IInternetZoneManagerVtbl;
  struct IInternetZoneManager {
    CONST_VTBL struct IInternetZoneManagerVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetZoneManager_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetZoneManager_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetZoneManager_Release(This) (This)->lpVtbl->Release(This)
#define IInternetZoneManager_GetZoneAttributes(This,dwZone,pZoneAttributes) (This)->lpVtbl->GetZoneAttributes(This,dwZone,pZoneAttributes)
#define IInternetZoneManager_SetZoneAttributes(This,dwZone,pZoneAttributes) (This)->lpVtbl->SetZoneAttributes(This,dwZone,pZoneAttributes)
#define IInternetZoneManager_GetZoneCustomPolicy(This,dwZone,guidKey,ppPolicy,pcbPolicy,urlZoneReg) (This)->lpVtbl->GetZoneCustomPolicy(This,dwZone,guidKey,ppPolicy,pcbPolicy,urlZoneReg)
#define IInternetZoneManager_SetZoneCustomPolicy(This,dwZone,guidKey,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->SetZoneCustomPolicy(This,dwZone,guidKey,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManager_GetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->GetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManager_SetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->SetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManager_PromptAction(This,dwAction,hwndParent,pwszUrl,pwszText,dwPromptFlags) (This)->lpVtbl->PromptAction(This,dwAction,hwndParent,pwszUrl,pwszText,dwPromptFlags)
#define IInternetZoneManager_LogAction(This,dwAction,pwszUrl,pwszText,dwLogFlags) (This)->lpVtbl->LogAction(This,dwAction,pwszUrl,pwszText,dwLogFlags)
#define IInternetZoneManager_CreateZoneEnumerator(This,pdwEnum,pdwCount,dwFlags) (This)->lpVtbl->CreateZoneEnumerator(This,pdwEnum,pdwCount,dwFlags)
#define IInternetZoneManager_GetZoneAt(This,dwEnum,dwIndex,pdwZone) (This)->lpVtbl->GetZoneAt(This,dwEnum,dwIndex,pdwZone)
#define IInternetZoneManager_DestroyZoneEnumerator(This,dwEnum) (This)->lpVtbl->DestroyZoneEnumerator(This,dwEnum)
#define IInternetZoneManager_CopyTemplatePoliciesToZone(This,dwTemplate,dwZone,dwReserved) (This)->lpVtbl->CopyTemplatePoliciesToZone(This,dwTemplate,dwZone,dwReserved)
#endif
#endif
  HRESULT WINAPI IInternetZoneManager_GetZoneAttributes_Proxy(IInternetZoneManager *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
  void __RPC_STUB IInternetZoneManager_GetZoneAttributes_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_SetZoneAttributes_Proxy(IInternetZoneManager *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
  void __RPC_STUB IInternetZoneManager_SetZoneAttributes_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_GetZoneCustomPolicy_Proxy(IInternetZoneManager *This,DWORD dwZone,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,URLZONEREG urlZoneReg);
  void __RPC_STUB IInternetZoneManager_GetZoneCustomPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_SetZoneCustomPolicy_Proxy(IInternetZoneManager *This,DWORD dwZone,REFGUID guidKey,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
  void __RPC_STUB IInternetZoneManager_SetZoneCustomPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_GetZoneActionPolicy_Proxy(IInternetZoneManager *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
  void __RPC_STUB IInternetZoneManager_GetZoneActionPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_SetZoneActionPolicy_Proxy(IInternetZoneManager *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
  void __RPC_STUB IInternetZoneManager_SetZoneActionPolicy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_PromptAction_Proxy(IInternetZoneManager *This,DWORD dwAction,HWND hwndParent,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwPromptFlags);
  void __RPC_STUB IInternetZoneManager_PromptAction_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_LogAction_Proxy(IInternetZoneManager *This,DWORD dwAction,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwLogFlags);
  void __RPC_STUB IInternetZoneManager_LogAction_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_CreateZoneEnumerator_Proxy(IInternetZoneManager *This,DWORD *pdwEnum,DWORD *pdwCount,DWORD dwFlags);
  void __RPC_STUB IInternetZoneManager_CreateZoneEnumerator_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_GetZoneAt_Proxy(IInternetZoneManager *This,DWORD dwEnum,DWORD dwIndex,DWORD *pdwZone);
  void __RPC_STUB IInternetZoneManager_GetZoneAt_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_DestroyZoneEnumerator_Proxy(IInternetZoneManager *This,DWORD dwEnum);
  void __RPC_STUB IInternetZoneManager_DestroyZoneEnumerator_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManager_CopyTemplatePoliciesToZone_Proxy(IInternetZoneManager *This,DWORD dwTemplate,DWORD dwZone,DWORD dwReserved);
  void __RPC_STUB IInternetZoneManager_CopyTemplatePoliciesToZone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPINTERNETZONEMANAGEREX_DEFINED
#define _LPINTERNETZONEMANAGEREX_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0209_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0209_v0_0_s_ifspec;
#ifndef __IInternetZoneManagerEx_INTERFACE_DEFINED__
#define __IInternetZoneManagerEx_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IInternetZoneManagerEx;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IInternetZoneManagerEx : public IInternetZoneManager {
  public:
    virtual HRESULT WINAPI GetZoneActionPolicyEx(DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags) = 0;
    virtual HRESULT WINAPI SetZoneActionPolicyEx(DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags) = 0;
  };
#else
  typedef struct IInternetZoneManagerExVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IInternetZoneManagerEx *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IInternetZoneManagerEx *This);
      ULONG (WINAPI *Release)(IInternetZoneManagerEx *This);
      HRESULT (WINAPI *GetZoneAttributes)(IInternetZoneManagerEx *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
      HRESULT (WINAPI *SetZoneAttributes)(IInternetZoneManagerEx *This,DWORD dwZone,ZONEATTRIBUTES *pZoneAttributes);
      HRESULT (WINAPI *GetZoneCustomPolicy)(IInternetZoneManagerEx *This,DWORD dwZone,REFGUID guidKey,BYTE **ppPolicy,DWORD *pcbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *SetZoneCustomPolicy)(IInternetZoneManagerEx *This,DWORD dwZone,REFGUID guidKey,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *GetZoneActionPolicy)(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *SetZoneActionPolicy)(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg);
      HRESULT (WINAPI *PromptAction)(IInternetZoneManagerEx *This,DWORD dwAction,HWND hwndParent,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwPromptFlags);
      HRESULT (WINAPI *LogAction)(IInternetZoneManagerEx *This,DWORD dwAction,LPCWSTR pwszUrl,LPCWSTR pwszText,DWORD dwLogFlags);
      HRESULT (WINAPI *CreateZoneEnumerator)(IInternetZoneManagerEx *This,DWORD *pdwEnum,DWORD *pdwCount,DWORD dwFlags);
      HRESULT (WINAPI *GetZoneAt)(IInternetZoneManagerEx *This,DWORD dwEnum,DWORD dwIndex,DWORD *pdwZone);
      HRESULT (WINAPI *DestroyZoneEnumerator)(IInternetZoneManagerEx *This,DWORD dwEnum);
      HRESULT (WINAPI *CopyTemplatePoliciesToZone)(IInternetZoneManagerEx *This,DWORD dwTemplate,DWORD dwZone,DWORD dwReserved);
      HRESULT (WINAPI *GetZoneActionPolicyEx)(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags);
      HRESULT (WINAPI *SetZoneActionPolicyEx)(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags);
    END_INTERFACE
  } IInternetZoneManagerExVtbl;
  struct IInternetZoneManagerEx {
    CONST_VTBL struct IInternetZoneManagerExVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IInternetZoneManagerEx_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IInternetZoneManagerEx_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IInternetZoneManagerEx_Release(This) (This)->lpVtbl->Release(This)
#define IInternetZoneManagerEx_GetZoneAttributes(This,dwZone,pZoneAttributes) (This)->lpVtbl->GetZoneAttributes(This,dwZone,pZoneAttributes)
#define IInternetZoneManagerEx_SetZoneAttributes(This,dwZone,pZoneAttributes) (This)->lpVtbl->SetZoneAttributes(This,dwZone,pZoneAttributes)
#define IInternetZoneManagerEx_GetZoneCustomPolicy(This,dwZone,guidKey,ppPolicy,pcbPolicy,urlZoneReg) (This)->lpVtbl->GetZoneCustomPolicy(This,dwZone,guidKey,ppPolicy,pcbPolicy,urlZoneReg)
#define IInternetZoneManagerEx_SetZoneCustomPolicy(This,dwZone,guidKey,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->SetZoneCustomPolicy(This,dwZone,guidKey,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManagerEx_GetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->GetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManagerEx_SetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg) (This)->lpVtbl->SetZoneActionPolicy(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg)
#define IInternetZoneManagerEx_PromptAction(This,dwAction,hwndParent,pwszUrl,pwszText,dwPromptFlags) (This)->lpVtbl->PromptAction(This,dwAction,hwndParent,pwszUrl,pwszText,dwPromptFlags)
#define IInternetZoneManagerEx_LogAction(This,dwAction,pwszUrl,pwszText,dwLogFlags) (This)->lpVtbl->LogAction(This,dwAction,pwszUrl,pwszText,dwLogFlags)
#define IInternetZoneManagerEx_CreateZoneEnumerator(This,pdwEnum,pdwCount,dwFlags) (This)->lpVtbl->CreateZoneEnumerator(This,pdwEnum,pdwCount,dwFlags)
#define IInternetZoneManagerEx_GetZoneAt(This,dwEnum,dwIndex,pdwZone) (This)->lpVtbl->GetZoneAt(This,dwEnum,dwIndex,pdwZone)
#define IInternetZoneManagerEx_DestroyZoneEnumerator(This,dwEnum) (This)->lpVtbl->DestroyZoneEnumerator(This,dwEnum)
#define IInternetZoneManagerEx_CopyTemplatePoliciesToZone(This,dwTemplate,dwZone,dwReserved) (This)->lpVtbl->CopyTemplatePoliciesToZone(This,dwTemplate,dwZone,dwReserved)
#define IInternetZoneManagerEx_GetZoneActionPolicyEx(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg,dwFlags) (This)->lpVtbl->GetZoneActionPolicyEx(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg,dwFlags)
#define IInternetZoneManagerEx_SetZoneActionPolicyEx(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg,dwFlags) (This)->lpVtbl->SetZoneActionPolicyEx(This,dwZone,dwAction,pPolicy,cbPolicy,urlZoneReg,dwFlags)
#endif
#endif
  HRESULT WINAPI IInternetZoneManagerEx_GetZoneActionPolicyEx_Proxy(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags);
  void __RPC_STUB IInternetZoneManagerEx_GetZoneActionPolicyEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IInternetZoneManagerEx_SetZoneActionPolicyEx_Proxy(IInternetZoneManagerEx *This,DWORD dwZone,DWORD dwAction,BYTE *pPolicy,DWORD cbPolicy,URLZONEREG urlZoneReg,DWORD dwFlags);
  void __RPC_STUB IInternetZoneManagerEx_SetZoneActionPolicyEx_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

  EXTERN_C const IID CLSID_SoftDistExt;

#ifndef _LPSOFTDISTEXT_DEFINED
#define _LPSOFTDISTEXT_DEFINED

#define SOFTDIST_FLAG_USAGE_EMAIL 0x00000001
#define SOFTDIST_FLAG_USAGE_PRECACHE 0x00000002
#define SOFTDIST_FLAG_USAGE_AUTOINSTALL 0x00000004
#define SOFTDIST_FLAG_DELETE_SUBSCRIPTION 0x00000008

#define SOFTDIST_ADSTATE_NONE 0x00000000
#define SOFTDIST_ADSTATE_AVAILABLE 0x00000001
#define SOFTDIST_ADSTATE_DOWNLOADED 0x00000002
#define SOFTDIST_ADSTATE_INSTALLED 0x00000003

  typedef struct _tagCODEBASEHOLD {
    ULONG cbSize;
    LPWSTR szDistUnit;
    LPWSTR szCodeBase;
    DWORD dwVersionMS;
    DWORD dwVersionLS;
    DWORD dwStyle;
  } CODEBASEHOLD;

  typedef struct _tagCODEBASEHOLD *LPCODEBASEHOLD;

  typedef struct _tagSOFTDISTINFO {
    ULONG cbSize;
    DWORD dwFlags;
    DWORD dwAdState;
    LPWSTR szTitle;
    LPWSTR szAbstract;
    LPWSTR szHREF;
    DWORD dwInstalledVersionMS;
    DWORD dwInstalledVersionLS;
    DWORD dwUpdateVersionMS;
    DWORD dwUpdateVersionLS;
    DWORD dwAdvertisedVersionMS;
    DWORD dwAdvertisedVersionLS;
    DWORD dwReserved;
  } SOFTDISTINFO;

  typedef struct _tagSOFTDISTINFO *LPSOFTDISTINFO;

  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0210_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0210_v0_0_s_ifspec;
#ifndef __ISoftDistExt_INTERFACE_DEFINED__
#define __ISoftDistExt_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ISoftDistExt;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISoftDistExt : public IUnknown {
  public:
    virtual HRESULT WINAPI ProcessSoftDist(LPCWSTR szCDFURL,IXMLElement *pSoftDistElement,LPSOFTDISTINFO lpsdi) = 0;
    virtual HRESULT WINAPI GetFirstCodeBase(LPWSTR *szCodeBase,LPDWORD dwMaxSize) = 0;
    virtual HRESULT WINAPI GetNextCodeBase(LPWSTR *szCodeBase,LPDWORD dwMaxSize) = 0;
    virtual HRESULT WINAPI AsyncInstallDistributionUnit(IBindCtx *pbc,LPVOID pvReserved,DWORD flags,LPCODEBASEHOLD lpcbh) = 0;
  };
#else
  typedef struct ISoftDistExtVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISoftDistExt *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISoftDistExt *This);
      ULONG (WINAPI *Release)(ISoftDistExt *This);
      HRESULT (WINAPI *ProcessSoftDist)(ISoftDistExt *This,LPCWSTR szCDFURL,IXMLElement *pSoftDistElement,LPSOFTDISTINFO lpsdi);
      HRESULT (WINAPI *GetFirstCodeBase)(ISoftDistExt *This,LPWSTR *szCodeBase,LPDWORD dwMaxSize);
      HRESULT (WINAPI *GetNextCodeBase)(ISoftDistExt *This,LPWSTR *szCodeBase,LPDWORD dwMaxSize);
      HRESULT (WINAPI *AsyncInstallDistributionUnit)(ISoftDistExt *This,IBindCtx *pbc,LPVOID pvReserved,DWORD flags,LPCODEBASEHOLD lpcbh);
    END_INTERFACE
  } ISoftDistExtVtbl;
  struct ISoftDistExt {
    CONST_VTBL struct ISoftDistExtVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISoftDistExt_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISoftDistExt_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISoftDistExt_Release(This) (This)->lpVtbl->Release(This)
#define ISoftDistExt_ProcessSoftDist(This,szCDFURL,pSoftDistElement,lpsdi) (This)->lpVtbl->ProcessSoftDist(This,szCDFURL,pSoftDistElement,lpsdi)
#define ISoftDistExt_GetFirstCodeBase(This,szCodeBase,dwMaxSize) (This)->lpVtbl->GetFirstCodeBase(This,szCodeBase,dwMaxSize)
#define ISoftDistExt_GetNextCodeBase(This,szCodeBase,dwMaxSize) (This)->lpVtbl->GetNextCodeBase(This,szCodeBase,dwMaxSize)
#define ISoftDistExt_AsyncInstallDistributionUnit(This,pbc,pvReserved,flags,lpcbh) (This)->lpVtbl->AsyncInstallDistributionUnit(This,pbc,pvReserved,flags,lpcbh)
#endif
#endif
  HRESULT WINAPI ISoftDistExt_ProcessSoftDist_Proxy(ISoftDistExt *This,LPCWSTR szCDFURL,IXMLElement *pSoftDistElement,LPSOFTDISTINFO lpsdi);
  void __RPC_STUB ISoftDistExt_ProcessSoftDist_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISoftDistExt_GetFirstCodeBase_Proxy(ISoftDistExt *This,LPWSTR *szCodeBase,LPDWORD dwMaxSize);
  void __RPC_STUB ISoftDistExt_GetFirstCodeBase_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISoftDistExt_GetNextCodeBase_Proxy(ISoftDistExt *This,LPWSTR *szCodeBase,LPDWORD dwMaxSize);
  void __RPC_STUB ISoftDistExt_GetNextCodeBase_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ISoftDistExt_AsyncInstallDistributionUnit_Proxy(ISoftDistExt *This,IBindCtx *pbc,LPVOID pvReserved,DWORD flags,LPCODEBASEHOLD lpcbh);
  void __RPC_STUB ISoftDistExt_AsyncInstallDistributionUnit_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  STDAPI GetSoftwareUpdateInfo(LPCWSTR szDistUnit,LPSOFTDISTINFO psdi);
  STDAPI SetSoftwareUpdateAdvertisementState(LPCWSTR szDistUnit,DWORD dwAdState,DWORD dwAdvertisedVersionMS,DWORD dwAdvertisedVersionLS);
#endif

#ifndef _LPCATALOGFILEINFO_DEFINED
#define _LPCATALOGFILEINFO_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0211_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0211_v0_0_s_ifspec;
#ifndef __ICatalogFileInfo_INTERFACE_DEFINED__
#define __ICatalogFileInfo_INTERFACE_DEFINED__
  typedef ICatalogFileInfo *LPCATALOGFILEINFO;

  EXTERN_C const IID IID_ICatalogFileInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICatalogFileInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetCatalogFile(LPSTR *ppszCatalogFile) = 0;
    virtual HRESULT WINAPI GetJavaTrust(void **ppJavaTrust) = 0;
  };
#else
  typedef struct ICatalogFileInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICatalogFileInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICatalogFileInfo *This);
      ULONG (WINAPI *Release)(ICatalogFileInfo *This);
      HRESULT (WINAPI *GetCatalogFile)(ICatalogFileInfo *This,LPSTR *ppszCatalogFile);
      HRESULT (WINAPI *GetJavaTrust)(ICatalogFileInfo *This,void **ppJavaTrust);
    END_INTERFACE
  } ICatalogFileInfoVtbl;
  struct ICatalogFileInfo {
    CONST_VTBL struct ICatalogFileInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICatalogFileInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICatalogFileInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICatalogFileInfo_Release(This) (This)->lpVtbl->Release(This)
#define ICatalogFileInfo_GetCatalogFile(This,ppszCatalogFile) (This)->lpVtbl->GetCatalogFile(This,ppszCatalogFile)
#define ICatalogFileInfo_GetJavaTrust(This,ppJavaTrust) (This)->lpVtbl->GetJavaTrust(This,ppJavaTrust)
#endif
#endif
  HRESULT WINAPI ICatalogFileInfo_GetCatalogFile_Proxy(ICatalogFileInfo *This,LPSTR *ppszCatalogFile);
  void __RPC_STUB ICatalogFileInfo_GetCatalogFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICatalogFileInfo_GetJavaTrust_Proxy(ICatalogFileInfo *This,void **ppJavaTrust);
  void __RPC_STUB ICatalogFileInfo_GetJavaTrust_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPDATAFILTER_DEFINED
#define _LPDATAFILTER_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0212_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0212_v0_0_s_ifspec;
#ifndef __IDataFilter_INTERFACE_DEFINED__
#define __IDataFilter_INTERFACE_DEFINED__
  typedef IDataFilter *LPDATAFILTER;

  EXTERN_C const IID IID_IDataFilter;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDataFilter : public IUnknown {
  public:
    virtual HRESULT WINAPI DoEncode(DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI DoDecode(DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved) = 0;
    virtual HRESULT WINAPI SetEncodingLevel(DWORD dwEncLevel) = 0;
  };
#else
  typedef struct IDataFilterVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDataFilter *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDataFilter *This);
      ULONG (WINAPI *Release)(IDataFilter *This);
      HRESULT (WINAPI *DoEncode)(IDataFilter *This,DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved);
      HRESULT (WINAPI *DoDecode)(IDataFilter *This,DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved);
      HRESULT (WINAPI *SetEncodingLevel)(IDataFilter *This,DWORD dwEncLevel);
    END_INTERFACE
  } IDataFilterVtbl;
  struct IDataFilter {
    CONST_VTBL struct IDataFilterVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDataFilter_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDataFilter_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDataFilter_Release(This) (This)->lpVtbl->Release(This)
#define IDataFilter_DoEncode(This,dwFlags,lInBufferSize,pbInBuffer,lOutBufferSize,pbOutBuffer,lInBytesAvailable,plInBytesRead,plOutBytesWritten,dwReserved) (This)->lpVtbl->DoEncode(This,dwFlags,lInBufferSize,pbInBuffer,lOutBufferSize,pbOutBuffer,lInBytesAvailable,plInBytesRead,plOutBytesWritten,dwReserved)
#define IDataFilter_DoDecode(This,dwFlags,lInBufferSize,pbInBuffer,lOutBufferSize,pbOutBuffer,lInBytesAvailable,plInBytesRead,plOutBytesWritten,dwReserved) (This)->lpVtbl->DoDecode(This,dwFlags,lInBufferSize,pbInBuffer,lOutBufferSize,pbOutBuffer,lInBytesAvailable,plInBytesRead,plOutBytesWritten,dwReserved)
#define IDataFilter_SetEncodingLevel(This,dwEncLevel) (This)->lpVtbl->SetEncodingLevel(This,dwEncLevel)
#endif
#endif
  HRESULT WINAPI IDataFilter_DoEncode_Proxy(IDataFilter *This,DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved);
  void __RPC_STUB IDataFilter_DoEncode_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataFilter_DoDecode_Proxy(IDataFilter *This,DWORD dwFlags,LONG lInBufferSize,BYTE *pbInBuffer,LONG lOutBufferSize,BYTE *pbOutBuffer,LONG lInBytesAvailable,LONG *plInBytesRead,LONG *plOutBytesWritten,DWORD dwReserved);
  void __RPC_STUB IDataFilter_DoDecode_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDataFilter_SetEncodingLevel_Proxy(IDataFilter *This,DWORD dwEncLevel);
  void __RPC_STUB IDataFilter_SetEncodingLevel_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _LPENCODINGFILTERFACTORY_DEFINED
#define _LPENCODINGFILTERFACTORY_DEFINED
  typedef struct _tagPROTOCOLFILTERDATA {
    DWORD cbSize;
    IInternetProtocolSink *pProtocolSink;
    IInternetProtocol *pProtocol;
    IUnknown *pUnk;
    DWORD dwFilterFlags;
  } PROTOCOLFILTERDATA;

  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0213_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0213_v0_0_s_ifspec;
#ifndef __IEncodingFilterFactory_INTERFACE_DEFINED__
#define __IEncodingFilterFactory_INTERFACE_DEFINED__
  typedef IEncodingFilterFactory *LPENCODINGFILTERFACTORY;

  typedef struct _tagDATAINFO {
    ULONG ulTotalSize;
    ULONG ulavrPacketSize;
    ULONG ulConnectSpeed;
    ULONG ulProcessorSpeed;
  } DATAINFO;

  EXTERN_C const IID IID_IEncodingFilterFactory;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEncodingFilterFactory : public IUnknown {
  public:
    virtual HRESULT WINAPI FindBestFilter(LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,DATAINFO info,IDataFilter **ppDF) = 0;
    virtual HRESULT WINAPI GetDefaultFilter(LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,IDataFilter **ppDF) = 0;
  };
#else
  typedef struct IEncodingFilterFactoryVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEncodingFilterFactory *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEncodingFilterFactory *This);
      ULONG (WINAPI *Release)(IEncodingFilterFactory *This);
      HRESULT (WINAPI *FindBestFilter)(IEncodingFilterFactory *This,LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,DATAINFO info,IDataFilter **ppDF);
      HRESULT (WINAPI *GetDefaultFilter)(IEncodingFilterFactory *This,LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,IDataFilter **ppDF);
    END_INTERFACE
  } IEncodingFilterFactoryVtbl;
  struct IEncodingFilterFactory {
    CONST_VTBL struct IEncodingFilterFactoryVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEncodingFilterFactory_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEncodingFilterFactory_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEncodingFilterFactory_Release(This) (This)->lpVtbl->Release(This)
#define IEncodingFilterFactory_FindBestFilter(This,pwzCodeIn,pwzCodeOut,info,ppDF) (This)->lpVtbl->FindBestFilter(This,pwzCodeIn,pwzCodeOut,info,ppDF)
#define IEncodingFilterFactory_GetDefaultFilter(This,pwzCodeIn,pwzCodeOut,ppDF) (This)->lpVtbl->GetDefaultFilter(This,pwzCodeIn,pwzCodeOut,ppDF)
#endif
#endif
  HRESULT WINAPI IEncodingFilterFactory_FindBestFilter_Proxy(IEncodingFilterFactory *This,LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,DATAINFO info,IDataFilter **ppDF);
  void __RPC_STUB IEncodingFilterFactory_FindBestFilter_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEncodingFilterFactory_GetDefaultFilter_Proxy(IEncodingFilterFactory *This,LPCWSTR pwzCodeIn,LPCWSTR pwzCodeOut,IDataFilter **ppDF);
  void __RPC_STUB IEncodingFilterFactory_GetDefaultFilter_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

#ifndef _HITLOGGING_DEFINED
#define _HITLOGGING_DEFINED
  WINBOOL WINAPI IsLoggingEnabledA(LPCSTR pszUrl);
  WINBOOL WINAPI IsLoggingEnabledW(LPCWSTR pwszUrl);
#ifdef UNICODE
#define IsLoggingEnabled IsLoggingEnabledW
#else
#define IsLoggingEnabled IsLoggingEnabledA
#endif
  typedef struct _tagHIT_LOGGING_INFO {
    DWORD dwStructSize;
    LPSTR lpszLoggedUrlName;
    SYSTEMTIME StartTime;
    SYSTEMTIME EndTime;
    LPSTR lpszExtendedInfo;
  } HIT_LOGGING_INFO;

  typedef struct _tagHIT_LOGGING_INFO *LPHIT_LOGGING_INFO;

  WINBOOL WINAPI WriteHitLogging(LPHIT_LOGGING_INFO lpLogginginfo);
#define CONFIRMSAFETYACTION_LOADOBJECT 0x00000001

  struct CONFIRMSAFETY {
    CLSID clsid;
    IUnknown *pUnk;
    DWORD dwFlags;
  };
  EXTERN_C const GUID GUID_CUSTOM_CONFIRMOBJECTSAFETY;
#endif

#ifndef _LPIWRAPPEDPROTOCOL_DEFINED
#define _LPIWRAPPEDPROTOCOL_DEFINED
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0214_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0214_v0_0_s_ifspec;
#ifndef __IWrappedProtocol_INTERFACE_DEFINED__
#define __IWrappedProtocol_INTERFACE_DEFINED__
  typedef IWrappedProtocol *LPIWRAPPEDPROTOCOL;

  EXTERN_C const IID IID_IWrappedProtocol;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IWrappedProtocol : public IUnknown {
  public:
    virtual HRESULT WINAPI GetWrapperCode(LONG *pnCode,DWORD_PTR dwReserved) = 0;
  };
#else
  typedef struct IWrappedProtocolVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IWrappedProtocol *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IWrappedProtocol *This);
      ULONG (WINAPI *Release)(IWrappedProtocol *This);
      HRESULT (WINAPI *GetWrapperCode)(IWrappedProtocol *This,LONG *pnCode,DWORD_PTR dwReserved);
    END_INTERFACE
  } IWrappedProtocolVtbl;
  struct IWrappedProtocol {
    CONST_VTBL struct IWrappedProtocolVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IWrappedProtocol_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IWrappedProtocol_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IWrappedProtocol_Release(This) (This)->lpVtbl->Release(This)
#define IWrappedProtocol_GetWrapperCode(This,pnCode,dwReserved) (This)->lpVtbl->GetWrapperCode(This,pnCode,dwReserved)
#endif
#endif
  HRESULT WINAPI IWrappedProtocol_GetWrapperCode_Proxy(IWrappedProtocol *This,LONG *pnCode,DWORD_PTR dwReserved);
  void __RPC_STUB IWrappedProtocol_GetWrapperCode_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif
#endif

  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0215_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_urlmon_0215_v0_0_s_ifspec;

  unsigned long __RPC_API HWND_UserSize(unsigned long *,unsigned long,HWND *);
  unsigned char *__RPC_API HWND_UserMarshal(unsigned long *,unsigned char *,HWND *);
  unsigned char *__RPC_API HWND_UserUnmarshal(unsigned long *,unsigned char *,HWND *);
  void __RPC_API HWND_UserFree(unsigned long *,HWND *);

  HRESULT WINAPI IBinding_GetBindResult_Proxy(IBinding *This,CLSID *pclsidProtocol,DWORD *pdwResult,LPOLESTR *pszResult,DWORD *pdwReserved);
  HRESULT WINAPI IBinding_GetBindResult_Stub(IBinding *This,CLSID *pclsidProtocol,DWORD *pdwResult,LPOLESTR *pszResult,DWORD dwReserved);
  HRESULT WINAPI IBindStatusCallback_GetBindInfo_Proxy(IBindStatusCallback *This,DWORD *grfBINDF,BINDINFO *pbindinfo);
  HRESULT WINAPI IBindStatusCallback_GetBindInfo_Stub(IBindStatusCallback *This,DWORD *grfBINDF,RemBINDINFO *pbindinfo,RemSTGMEDIUM *pstgmed);
  HRESULT WINAPI IBindStatusCallback_OnDataAvailable_Proxy(IBindStatusCallback *This,DWORD grfBSCF,DWORD dwSize,FORMATETC *pformatetc,STGMEDIUM *pstgmed);
  HRESULT WINAPI IBindStatusCallback_OnDataAvailable_Stub(IBindStatusCallback *This,DWORD grfBSCF,DWORD dwSize,RemFORMATETC *pformatetc,RemSTGMEDIUM *pstgmed);
  HRESULT WINAPI IWinInetInfo_QueryOption_Proxy(IWinInetInfo *This,DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf);
  HRESULT WINAPI IWinInetInfo_QueryOption_Stub(IWinInetInfo *This,DWORD dwOption,BYTE *pBuffer,DWORD *pcbBuf);
  HRESULT WINAPI IWinInetHttpInfo_QueryInfo_Proxy(IWinInetHttpInfo *This,DWORD dwOption,LPVOID pBuffer,DWORD *pcbBuf,DWORD *pdwFlags,DWORD *pdwReserved);
  HRESULT WINAPI IWinInetHttpInfo_QueryInfo_Stub(IWinInetHttpInfo *This,DWORD dwOption,BYTE *pBuffer,DWORD *pcbBuf,DWORD *pdwFlags,DWORD *pdwReserved);
  HRESULT WINAPI IBindHost_MonikerBindToStorage_Proxy(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj);
  HRESULT WINAPI IBindHost_MonikerBindToStorage_Stub(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,IUnknown **ppvObj);
  HRESULT WINAPI IBindHost_MonikerBindToObject_Proxy(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,void **ppvObj);
  HRESULT WINAPI IBindHost_MonikerBindToObject_Stub(IBindHost *This,IMoniker *pMk,IBindCtx *pBC,IBindStatusCallback *pBSC,REFIID riid,IUnknown **ppvObj);

#ifdef __cplusplus
}
#endif
#endif
