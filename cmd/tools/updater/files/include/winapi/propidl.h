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

#ifndef __propidl_h__
#define __propidl_h__

#ifndef __IPropertyStorage_FWD_DEFINED__
#define __IPropertyStorage_FWD_DEFINED__
typedef struct IPropertyStorage IPropertyStorage;
#endif

#ifndef __IPropertySetStorage_FWD_DEFINED__
#define __IPropertySetStorage_FWD_DEFINED__
typedef struct IPropertySetStorage IPropertySetStorage;
#endif

#ifndef __IEnumSTATPROPSTG_FWD_DEFINED__
#define __IEnumSTATPROPSTG_FWD_DEFINED__
typedef struct IEnumSTATPROPSTG IEnumSTATPROPSTG;
#endif

#ifndef __IEnumSTATPROPSETSTG_FWD_DEFINED__
#define __IEnumSTATPROPSETSTG_FWD_DEFINED__
typedef struct IEnumSTATPROPSETSTG IEnumSTATPROPSETSTG;
#endif

#include "objidl.h"
#include "oaidl.h"

#ifdef __cplusplus
extern "C"{
#endif

#ifndef __MIDL_user_allocate_free_DEFINED__
#define __MIDL_user_allocate_free_DEFINED__
  void *__RPC_API MIDL_user_allocate(size_t);
  void __RPC_API MIDL_user_free(void *);
#endif

  typedef struct tagVersionedStream {
    GUID guidVersion;
    IStream *pStream;
  } VERSIONEDSTREAM;

  typedef struct tagVersionedStream *LPVERSIONEDSTREAM;

#define PROPSETFLAG_DEFAULT (0)
#define PROPSETFLAG_NONSIMPLE (1)
#define PROPSETFLAG_ANSI (2)
#define PROPSETFLAG_UNBUFFERED (4)
#define PROPSETFLAG_CASE_SENSITIVE (8)

#define PROPSET_BEHAVIOR_CASE_SENSITIVE (1)

  typedef struct tagPROPVARIANT PROPVARIANT;
  typedef struct tagCAC {
    ULONG cElems;
    CHAR *pElems;
  } CAC;

  typedef struct tagCAUB {
    ULONG cElems;
    UCHAR *pElems;
  } CAUB;

  typedef struct tagCAI {
    ULONG cElems;
    SHORT *pElems;
  } CAI;

  typedef struct tagCAUI {
    ULONG cElems;
    USHORT *pElems;
  } CAUI;

  typedef struct tagCAL {
    ULONG cElems;
    LONG *pElems;
  } CAL;

  typedef struct tagCAUL {
    ULONG cElems;
    ULONG *pElems;
  } CAUL;

  typedef struct tagCAFLT {
    ULONG cElems;
    FLOAT *pElems;
  } CAFLT;

  typedef struct tagCADBL {
    ULONG cElems;
    DOUBLE *pElems;
  } CADBL;

  typedef struct tagCACY {
    ULONG cElems;
    CY *pElems;
  } CACY;

  typedef struct tagCADATE {
    ULONG cElems;
    DATE *pElems;
  } CADATE;

  typedef struct tagCABSTR {
    ULONG cElems;
    BSTR *pElems;
  } CABSTR;

  typedef struct tagCABSTRBLOB {
    ULONG cElems;
    BSTRBLOB *pElems;
  } CABSTRBLOB;

  typedef struct tagCABOOL {
    ULONG cElems;
    VARIANT_BOOL *pElems;
  } CABOOL;

  typedef struct tagCASCODE {
    ULONG cElems;
    SCODE *pElems;
  } CASCODE;

  typedef struct tagCAPROPVARIANT {
    ULONG cElems;
    PROPVARIANT *pElems;
  } CAPROPVARIANT;

  typedef struct tagCAH {
    ULONG cElems;
    LARGE_INTEGER *pElems;
  } CAH;

  typedef struct tagCAUH {
    ULONG cElems;
    ULARGE_INTEGER *pElems;
  } CAUH;

  typedef struct tagCALPSTR {
    ULONG cElems;
    LPSTR *pElems;
  } CALPSTR;

  typedef struct tagCALPWSTR {
    ULONG cElems;
    LPWSTR *pElems;
  } CALPWSTR;

  typedef struct tagCAFILETIME {
    ULONG cElems;
    FILETIME *pElems;
  } CAFILETIME;

  typedef struct tagCACLIPDATA {
    ULONG cElems;
    CLIPDATA *pElems;
  } CACLIPDATA;

  typedef struct tagCACLSID {
    ULONG cElems;
    CLSID *pElems;
  } CACLSID;

  typedef WORD PROPVAR_PAD1;
  typedef WORD PROPVAR_PAD2;
  typedef WORD PROPVAR_PAD3;
#define tag_inner_PROPVARIANT

  struct tagPROPVARIANT {
    __MINGW_EXTENSION union {
      __MINGW_EXTENSION struct tag_inner_PROPVARIANT {
	VARTYPE vt;
	PROPVAR_PAD1 wReserved1;
	PROPVAR_PAD2 wReserved2;
	PROPVAR_PAD3 wReserved3;
	__MINGW_EXTENSION union {
	  CHAR cVal;
	  UCHAR bVal;
	  SHORT iVal;
	  USHORT uiVal;
	  LONG lVal;
	  ULONG ulVal;
	  INT intVal;
	  UINT uintVal;
	  LARGE_INTEGER hVal;
	  ULARGE_INTEGER uhVal;
	  FLOAT fltVal;
	  DOUBLE dblVal;
	  VARIANT_BOOL boolVal;
	  /* _VARIANT_BOOL bool; */
	  SCODE scode;
	  CY cyVal;
	  DATE date;
	  FILETIME filetime;
	  CLSID *puuid;
	  CLIPDATA *pclipdata;
	  BSTR bstrVal;
	  BSTRBLOB bstrblobVal;
	  BLOB blob;
	  LPSTR pszVal;
	  LPWSTR pwszVal;
	  IUnknown *punkVal;
	  IDispatch *pdispVal;
	  IStream *pStream;
	  IStorage *pStorage;
	  LPVERSIONEDSTREAM pVersionedStream;
	  LPSAFEARRAY parray;
	  CAC cac;
	  CAUB caub;
	  CAI cai;
	  CAUI caui;
	  CAL cal;
	  CAUL caul;
	  CAH cah;
	  CAUH cauh;
	  CAFLT caflt;
	  CADBL cadbl;
	  CABOOL cabool;
	  CASCODE cascode;
	  CACY cacy;
	  CADATE cadate;
	  CAFILETIME cafiletime;
	  CACLSID cauuid;
	  CACLIPDATA caclipdata;
	  CABSTR cabstr;
	  CABSTRBLOB cabstrblob;
	  CALPSTR calpstr;
	  CALPWSTR calpwstr;
	  CAPROPVARIANT capropvar;
	  CHAR *pcVal;
	  UCHAR *pbVal;
	  SHORT *piVal;
	  USHORT *puiVal;
	  LONG *plVal;
	  ULONG *pulVal;
	  INT *pintVal;
	  UINT *puintVal;
	  FLOAT *pfltVal;
	  DOUBLE *pdblVal;
	  VARIANT_BOOL *pboolVal;
	  DECIMAL *pdecVal;
	  SCODE *pscode;
	  CY *pcyVal;
	  DATE *pdate;
	  BSTR *pbstrVal;
	  IUnknown **ppunkVal;
	  IDispatch **ppdispVal;
	  LPSAFEARRAY *pparray;
	  PROPVARIANT *pvarVal;
	};
      };
      DECIMAL decVal;
    };
  };

  typedef struct tagPROPVARIANT *LPPROPVARIANT;

#define PID_DICTIONARY (0)
#define PID_CODEPAGE (0x1)
#define PID_FIRST_USABLE (0x2)
#define PID_FIRST_NAME_DEFAULT (0xfff)
#define PID_LOCALE (0x80000000)
#define PID_MODIFY_TIME (0x80000001)
#define PID_SECURITY (0x80000002)
#define PID_BEHAVIOR (0x80000003)
#define PID_ILLEGAL (0xffffffff)
#define PID_MIN_READONLY (0x80000000)
#define PID_MAX_READONLY (0xbfffffff)

#define PIDDI_THUMBNAIL 0x00000002L

#define PIDSI_TITLE 0x00000002L
#define PIDSI_SUBJECT 0x00000003L
#define PIDSI_AUTHOR 0x00000004L
#define PIDSI_KEYWORDS 0x00000005L
#define PIDSI_COMMENTS 0x00000006L
#define PIDSI_TEMPLATE 0x00000007L
#define PIDSI_LASTAUTHOR 0x00000008L
#define PIDSI_REVNUMBER 0x00000009L
#define PIDSI_EDITTIME 0x0000000aL
#define PIDSI_LASTPRINTED 0x0000000bL
#define PIDSI_CREATE_DTM 0x0000000cL
#define PIDSI_LASTSAVE_DTM 0x0000000dL
#define PIDSI_PAGECOUNT 0x0000000eL
#define PIDSI_WORDCOUNT 0x0000000fL
#define PIDSI_CHARCOUNT 0x00000010L
#define PIDSI_THUMBNAIL 0x00000011L
#define PIDSI_APPNAME 0x00000012L
#define PIDSI_DOC_SECURITY 0x00000013L

#define PIDDSI_CATEGORY 0x00000002
#define PIDDSI_PRESFORMAT 0x00000003
#define PIDDSI_BYTECOUNT 0x00000004
#define PIDDSI_LINECOUNT 0x00000005
#define PIDDSI_PARCOUNT 0x00000006
#define PIDDSI_SLIDECOUNT 0x00000007
#define PIDDSI_NOTECOUNT 0x00000008
#define PIDDSI_HIDDENCOUNT 0x00000009
#define PIDDSI_MMCLIPCOUNT 0x0000000A
#define PIDDSI_SCALE 0x0000000B
#define PIDDSI_HEADINGPAIR 0x0000000C
#define PIDDSI_DOCPARTS 0x0000000D
#define PIDDSI_MANAGER 0x0000000E
#define PIDDSI_COMPANY 0x0000000F
#define PIDDSI_LINKSDIRTY 0x00000010

#define PIDMSI_EDITOR 0x00000002L
#define PIDMSI_SUPPLIER 0x00000003L
#define PIDMSI_SOURCE 0x00000004L
#define PIDMSI_SEQUENCE_NO 0x00000005L
#define PIDMSI_PROJECT 0x00000006L
#define PIDMSI_STATUS 0x00000007L
#define PIDMSI_OWNER 0x00000008L
#define PIDMSI_RATING 0x00000009L
#define PIDMSI_PRODUCTION 0x0000000AL
#define PIDMSI_COPYRIGHT 0x0000000BL

  enum PIDMSI_STATUS_VALUE {
    PIDMSI_STATUS_NORMAL = 0,PIDMSI_STATUS_NEW,PIDMSI_STATUS_PRELIM,
    PIDMSI_STATUS_DRAFT,PIDMSI_STATUS_INPROGRESS,PIDMSI_STATUS_EDIT,
    PIDMSI_STATUS_REVIEW,PIDMSI_STATUS_PROOF,PIDMSI_STATUS_FINAL,
    PIDMSI_STATUS_OTHER = 0x7fff
  };
#define PRSPEC_INVALID (0xffffffff)
#define PRSPEC_LPWSTR (0)
#define PRSPEC_PROPID (1)

  typedef struct tagPROPSPEC {
    ULONG ulKind;
    __MINGW_EXTENSION union {
      PROPID propid;
      LPOLESTR lpwstr;
    };
  } PROPSPEC;

  typedef struct tagSTATPROPSTG {
    LPOLESTR lpwstrName;
    PROPID propid;
    VARTYPE vt;
  } STATPROPSTG;

#define PROPSETHDR_OSVER_KIND(dwOSVer) HIWORD((dwOSVer))
#define PROPSETHDR_OSVER_MAJOR(dwOSVer) LOBYTE(LOWORD((dwOSVer)))
#define PROPSETHDR_OSVER_MINOR(dwOSVer) HIBYTE(LOWORD((dwOSVer)))
#define PROPSETHDR_OSVERSION_UNKNOWN 0xFFFFFFFF
  typedef struct tagSTATPROPSETSTG {
    FMTID fmtid;
    CLSID clsid;
    DWORD grfFlags;
    FILETIME mtime;
    FILETIME ctime;
    FILETIME atime;
    DWORD dwOSVersion;
  } STATPROPSETSTG;

  extern RPC_IF_HANDLE __MIDL_itf_propidl_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_propidl_0000_v0_0_s_ifspec;

#ifndef __IPropertyStorage_INTERFACE_DEFINED__
#define __IPropertyStorage_INTERFACE_DEFINED__
  EXTERN_C const IID IID_IPropertyStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPropertyStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI ReadMultiple(ULONG cpspec,const PROPSPEC rgpspec[],PROPVARIANT rgpropvar[]) = 0;
    virtual HRESULT WINAPI WriteMultiple(ULONG cpspec,const PROPSPEC rgpspec[],const PROPVARIANT rgpropvar[],PROPID propidNameFirst) = 0;
    virtual HRESULT WINAPI DeleteMultiple(ULONG cpspec,const PROPSPEC rgpspec[]) = 0;
    virtual HRESULT WINAPI ReadPropertyNames(ULONG cpropid,const PROPID rgpropid[],LPOLESTR rglpwstrName[]) = 0;
    virtual HRESULT WINAPI WritePropertyNames(ULONG cpropid,const PROPID rgpropid[],const LPOLESTR rglpwstrName[]) = 0;
    virtual HRESULT WINAPI DeletePropertyNames(ULONG cpropid,const PROPID rgpropid[]) = 0;
    virtual HRESULT WINAPI Commit(DWORD grfCommitFlags) = 0;
    virtual HRESULT WINAPI Revert(void) = 0;
    virtual HRESULT WINAPI Enum(IEnumSTATPROPSTG **ppenum) = 0;
    virtual HRESULT WINAPI SetTimes(const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime) = 0;
    virtual HRESULT WINAPI SetClass(REFCLSID clsid) = 0;
    virtual HRESULT WINAPI Stat(STATPROPSETSTG *pstatpsstg) = 0;
  };
#else
  typedef struct IPropertyStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPropertyStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPropertyStorage *This);
      ULONG (WINAPI *Release)(IPropertyStorage *This);
      HRESULT (WINAPI *ReadMultiple)(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[],PROPVARIANT rgpropvar[]);
      HRESULT (WINAPI *WriteMultiple)(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[],const PROPVARIANT rgpropvar[],PROPID propidNameFirst);
      HRESULT (WINAPI *DeleteMultiple)(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[]);
      HRESULT (WINAPI *ReadPropertyNames)(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[],LPOLESTR rglpwstrName[]);
      HRESULT (WINAPI *WritePropertyNames)(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[],const LPOLESTR rglpwstrName[]);
      HRESULT (WINAPI *DeletePropertyNames)(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[]);
      HRESULT (WINAPI *Commit)(IPropertyStorage *This,DWORD grfCommitFlags);
      HRESULT (WINAPI *Revert)(IPropertyStorage *This);
      HRESULT (WINAPI *Enum)(IPropertyStorage *This,IEnumSTATPROPSTG **ppenum);
      HRESULT (WINAPI *SetTimes)(IPropertyStorage *This,const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime);
      HRESULT (WINAPI *SetClass)(IPropertyStorage *This,REFCLSID clsid);
      HRESULT (WINAPI *Stat)(IPropertyStorage *This,STATPROPSETSTG *pstatpsstg);
    END_INTERFACE
  } IPropertyStorageVtbl;
  struct IPropertyStorage {
    CONST_VTBL struct IPropertyStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPropertyStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPropertyStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPropertyStorage_Release(This) (This)->lpVtbl->Release(This)
#define IPropertyStorage_ReadMultiple(This,cpspec,rgpspec,rgpropvar) (This)->lpVtbl->ReadMultiple(This,cpspec,rgpspec,rgpropvar)
#define IPropertyStorage_WriteMultiple(This,cpspec,rgpspec,rgpropvar,propidNameFirst) (This)->lpVtbl->WriteMultiple(This,cpspec,rgpspec,rgpropvar,propidNameFirst)
#define IPropertyStorage_DeleteMultiple(This,cpspec,rgpspec) (This)->lpVtbl->DeleteMultiple(This,cpspec,rgpspec)
#define IPropertyStorage_ReadPropertyNames(This,cpropid,rgpropid,rglpwstrName) (This)->lpVtbl->ReadPropertyNames(This,cpropid,rgpropid,rglpwstrName)
#define IPropertyStorage_WritePropertyNames(This,cpropid,rgpropid,rglpwstrName) (This)->lpVtbl->WritePropertyNames(This,cpropid,rgpropid,rglpwstrName)
#define IPropertyStorage_DeletePropertyNames(This,cpropid,rgpropid) (This)->lpVtbl->DeletePropertyNames(This,cpropid,rgpropid)
#define IPropertyStorage_Commit(This,grfCommitFlags) (This)->lpVtbl->Commit(This,grfCommitFlags)
#define IPropertyStorage_Revert(This) (This)->lpVtbl->Revert(This)
#define IPropertyStorage_Enum(This,ppenum) (This)->lpVtbl->Enum(This,ppenum)
#define IPropertyStorage_SetTimes(This,pctime,patime,pmtime) (This)->lpVtbl->SetTimes(This,pctime,patime,pmtime)
#define IPropertyStorage_SetClass(This,clsid) (This)->lpVtbl->SetClass(This,clsid)
#define IPropertyStorage_Stat(This,pstatpsstg) (This)->lpVtbl->Stat(This,pstatpsstg)
#endif
#endif
  HRESULT WINAPI IPropertyStorage_ReadMultiple_Proxy(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[],PROPVARIANT rgpropvar[]);
  void __RPC_STUB IPropertyStorage_ReadMultiple_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_WriteMultiple_Proxy(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[],const PROPVARIANT rgpropvar[],PROPID propidNameFirst);
  void __RPC_STUB IPropertyStorage_WriteMultiple_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_DeleteMultiple_Proxy(IPropertyStorage *This,ULONG cpspec,const PROPSPEC rgpspec[]);
  void __RPC_STUB IPropertyStorage_DeleteMultiple_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_ReadPropertyNames_Proxy(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[],LPOLESTR rglpwstrName[]);
  void __RPC_STUB IPropertyStorage_ReadPropertyNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_WritePropertyNames_Proxy(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[],const LPOLESTR rglpwstrName[]);
  void __RPC_STUB IPropertyStorage_WritePropertyNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_DeletePropertyNames_Proxy(IPropertyStorage *This,ULONG cpropid,const PROPID rgpropid[]);
  void __RPC_STUB IPropertyStorage_DeletePropertyNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_Commit_Proxy(IPropertyStorage *This,DWORD grfCommitFlags);
  void __RPC_STUB IPropertyStorage_Commit_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_Revert_Proxy(IPropertyStorage *This);
  void __RPC_STUB IPropertyStorage_Revert_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_Enum_Proxy(IPropertyStorage *This,IEnumSTATPROPSTG **ppenum);
  void __RPC_STUB IPropertyStorage_Enum_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_SetTimes_Proxy(IPropertyStorage *This,const FILETIME *pctime,const FILETIME *patime,const FILETIME *pmtime);
  void __RPC_STUB IPropertyStorage_SetTimes_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_SetClass_Proxy(IPropertyStorage *This,REFCLSID clsid);
  void __RPC_STUB IPropertyStorage_SetClass_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyStorage_Stat_Proxy(IPropertyStorage *This,STATPROPSETSTG *pstatpsstg);
  void __RPC_STUB IPropertyStorage_Stat_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPropertySetStorage_INTERFACE_DEFINED__
#define __IPropertySetStorage_INTERFACE_DEFINED__
  typedef IPropertySetStorage *LPPROPERTYSETSTORAGE;

  EXTERN_C const IID IID_IPropertySetStorage;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPropertySetStorage : public IUnknown {
  public:
    virtual HRESULT WINAPI Create(REFFMTID rfmtid,const CLSID *pclsid,DWORD grfFlags,DWORD grfMode,IPropertyStorage **ppprstg) = 0;
    virtual HRESULT WINAPI Open(REFFMTID rfmtid,DWORD grfMode,IPropertyStorage **ppprstg) = 0;
    virtual HRESULT WINAPI Delete(REFFMTID rfmtid) = 0;
    virtual HRESULT WINAPI Enum(IEnumSTATPROPSETSTG **ppenum) = 0;
  };
#else
  typedef struct IPropertySetStorageVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPropertySetStorage *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPropertySetStorage *This);
      ULONG (WINAPI *Release)(IPropertySetStorage *This);
      HRESULT (WINAPI *Create)(IPropertySetStorage *This,REFFMTID rfmtid,const CLSID *pclsid,DWORD grfFlags,DWORD grfMode,IPropertyStorage **ppprstg);
      HRESULT (WINAPI *Open)(IPropertySetStorage *This,REFFMTID rfmtid,DWORD grfMode,IPropertyStorage **ppprstg);
      HRESULT (WINAPI *Delete)(IPropertySetStorage *This,REFFMTID rfmtid);
      HRESULT (WINAPI *Enum)(IPropertySetStorage *This,IEnumSTATPROPSETSTG **ppenum);
    END_INTERFACE
  } IPropertySetStorageVtbl;
  struct IPropertySetStorage {
    CONST_VTBL struct IPropertySetStorageVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPropertySetStorage_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPropertySetStorage_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPropertySetStorage_Release(This) (This)->lpVtbl->Release(This)
#define IPropertySetStorage_Create(This,rfmtid,pclsid,grfFlags,grfMode,ppprstg) (This)->lpVtbl->Create(This,rfmtid,pclsid,grfFlags,grfMode,ppprstg)
#define IPropertySetStorage_Open(This,rfmtid,grfMode,ppprstg) (This)->lpVtbl->Open(This,rfmtid,grfMode,ppprstg)
#define IPropertySetStorage_Delete(This,rfmtid) (This)->lpVtbl->Delete(This,rfmtid)
#define IPropertySetStorage_Enum(This,ppenum) (This)->lpVtbl->Enum(This,ppenum)
#endif
#endif
  HRESULT WINAPI IPropertySetStorage_Create_Proxy(IPropertySetStorage *This,REFFMTID rfmtid,const CLSID *pclsid,DWORD grfFlags,DWORD grfMode,IPropertyStorage **ppprstg);
  void __RPC_STUB IPropertySetStorage_Create_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertySetStorage_Open_Proxy(IPropertySetStorage *This,REFFMTID rfmtid,DWORD grfMode,IPropertyStorage **ppprstg);
  void __RPC_STUB IPropertySetStorage_Open_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertySetStorage_Delete_Proxy(IPropertySetStorage *This,REFFMTID rfmtid);
  void __RPC_STUB IPropertySetStorage_Delete_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertySetStorage_Enum_Proxy(IPropertySetStorage *This,IEnumSTATPROPSETSTG **ppenum);
  void __RPC_STUB IPropertySetStorage_Enum_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumSTATPROPSTG_INTERFACE_DEFINED__
#define __IEnumSTATPROPSTG_INTERFACE_DEFINED__
  typedef IEnumSTATPROPSTG *LPENUMSTATPROPSTG;

  EXTERN_C const IID IID_IEnumSTATPROPSTG;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumSTATPROPSTG : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,STATPROPSTG *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumSTATPROPSTG **ppenum) = 0;
  };
#else
  typedef struct IEnumSTATPROPSTGVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumSTATPROPSTG *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumSTATPROPSTG *This);
      ULONG (WINAPI *Release)(IEnumSTATPROPSTG *This);
      HRESULT (WINAPI *Next)(IEnumSTATPROPSTG *This,ULONG celt,STATPROPSTG *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumSTATPROPSTG *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumSTATPROPSTG *This);
      HRESULT (WINAPI *Clone)(IEnumSTATPROPSTG *This,IEnumSTATPROPSTG **ppenum);
    END_INTERFACE
  } IEnumSTATPROPSTGVtbl;
  struct IEnumSTATPROPSTG {
    CONST_VTBL struct IEnumSTATPROPSTGVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumSTATPROPSTG_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATPROPSTG_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumSTATPROPSTG_Release(This) (This)->lpVtbl->Release(This)
#define IEnumSTATPROPSTG_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATPROPSTG_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATPROPSTG_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumSTATPROPSTG_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumSTATPROPSTG_RemoteNext_Proxy(IEnumSTATPROPSTG *This,ULONG celt,STATPROPSTG *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumSTATPROPSTG_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSTG_Skip_Proxy(IEnumSTATPROPSTG *This,ULONG celt);
  void __RPC_STUB IEnumSTATPROPSTG_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSTG_Reset_Proxy(IEnumSTATPROPSTG *This);
  void __RPC_STUB IEnumSTATPROPSTG_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSTG_Clone_Proxy(IEnumSTATPROPSTG *This,IEnumSTATPROPSTG **ppenum);
  void __RPC_STUB IEnumSTATPROPSTG_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumSTATPROPSETSTG_INTERFACE_DEFINED__
#define __IEnumSTATPROPSETSTG_INTERFACE_DEFINED__
  typedef IEnumSTATPROPSETSTG *LPENUMSTATPROPSETSTG;

  EXTERN_C const IID IID_IEnumSTATPROPSETSTG;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumSTATPROPSETSTG : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,STATPROPSETSTG *rgelt,ULONG *pceltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumSTATPROPSETSTG **ppenum) = 0;
  };
#else
  typedef struct IEnumSTATPROPSETSTGVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumSTATPROPSETSTG *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumSTATPROPSETSTG *This);
      ULONG (WINAPI *Release)(IEnumSTATPROPSETSTG *This);
      HRESULT (WINAPI *Next)(IEnumSTATPROPSETSTG *This,ULONG celt,STATPROPSETSTG *rgelt,ULONG *pceltFetched);
      HRESULT (WINAPI *Skip)(IEnumSTATPROPSETSTG *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumSTATPROPSETSTG *This);
      HRESULT (WINAPI *Clone)(IEnumSTATPROPSETSTG *This,IEnumSTATPROPSETSTG **ppenum);
    END_INTERFACE
  } IEnumSTATPROPSETSTGVtbl;
  struct IEnumSTATPROPSETSTG {
    CONST_VTBL struct IEnumSTATPROPSETSTGVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumSTATPROPSETSTG_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATPROPSETSTG_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumSTATPROPSETSTG_Release(This) (This)->lpVtbl->Release(This)
#define IEnumSTATPROPSETSTG_Next(This,celt,rgelt,pceltFetched) (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATPROPSETSTG_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATPROPSETSTG_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumSTATPROPSETSTG_Clone(This,ppenum) (This)->lpVtbl->Clone(This,ppenum)
#endif
#endif
  HRESULT WINAPI IEnumSTATPROPSETSTG_RemoteNext_Proxy(IEnumSTATPROPSETSTG *This,ULONG celt,STATPROPSETSTG *rgelt,ULONG *pceltFetched);
  void __RPC_STUB IEnumSTATPROPSETSTG_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSETSTG_Skip_Proxy(IEnumSTATPROPSETSTG *This,ULONG celt);
  void __RPC_STUB IEnumSTATPROPSETSTG_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSETSTG_Reset_Proxy(IEnumSTATPROPSETSTG *This);
  void __RPC_STUB IEnumSTATPROPSETSTG_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumSTATPROPSETSTG_Clone_Proxy(IEnumSTATPROPSETSTG *This,IEnumSTATPROPSETSTG **ppenum);
  void __RPC_STUB IEnumSTATPROPSETSTG_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  typedef IPropertyStorage *LPPROPERTYSTORAGE;

  WINOLEAPI PropVariantCopy (PROPVARIANT *pvarDest,const PROPVARIANT *pvarSrc);
  WINOLEAPI PropVariantClear (PROPVARIANT *pvar);
  WINOLEAPI FreePropVariantArray (ULONG cVariants,PROPVARIANT *rgvars);

#define _PROPVARIANTINIT_DEFINED_
#ifdef __cplusplus
  static inline void PropVariantInit(PROPVARIANT *pvar) { memset (pvar,0,sizeof(PROPVARIANT)); }
#else
#define PropVariantInit(pvar) memset((pvar),0,sizeof(PROPVARIANT))
#endif

#ifndef _STGCREATEPROPSTG_DEFINED_
  WINOLEAPI StgCreatePropStg(IUnknown *pUnk,REFFMTID fmtid,const CLSID *pclsid,DWORD grfFlags,DWORD dwReserved,IPropertyStorage **ppPropStg);
  WINOLEAPI StgOpenPropStg(IUnknown *pUnk,REFFMTID fmtid,DWORD grfFlags,DWORD dwReserved,IPropertyStorage **ppPropStg);
  WINOLEAPI StgCreatePropSetStg(IStorage *pStorage,DWORD dwReserved,IPropertySetStorage **ppPropSetStg);

#define CCH_MAX_PROPSTG_NAME 31
  WINOLEAPI FmtIdToPropStgName(const FMTID *pfmtid,LPOLESTR oszName);
  WINOLEAPI PropStgNameToFmtId(const LPOLESTR oszName,FMTID *pfmtid);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_propidl_0120_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_propidl_0120_v0_0_s_ifspec;

  unsigned long __RPC_API BSTR_UserSize(unsigned long *,unsigned long,BSTR *);
  unsigned char *__RPC_API BSTR_UserMarshal(unsigned long *,unsigned char *,BSTR *);
  unsigned char *__RPC_API BSTR_UserUnmarshal(unsigned long *,unsigned char *,BSTR *);
  void __RPC_API BSTR_UserFree(unsigned long *,BSTR *);
  unsigned long __RPC_API LPSAFEARRAY_UserSize(unsigned long *,unsigned long,LPSAFEARRAY *);
  unsigned char *__RPC_API LPSAFEARRAY_UserMarshal(unsigned long *,unsigned char *,LPSAFEARRAY *);
  unsigned char *__RPC_API LPSAFEARRAY_UserUnmarshal(unsigned long *,unsigned char *,LPSAFEARRAY *);
  void __RPC_API LPSAFEARRAY_UserFree(unsigned long *,LPSAFEARRAY *);

  HRESULT WINAPI IEnumSTATPROPSTG_Next_Proxy(IEnumSTATPROPSTG *This,ULONG celt,STATPROPSTG *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATPROPSTG_Next_Stub(IEnumSTATPROPSTG *This,ULONG celt,STATPROPSTG *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATPROPSETSTG_Next_Proxy(IEnumSTATPROPSETSTG *This,ULONG celt,STATPROPSETSTG *rgelt,ULONG *pceltFetched);
  HRESULT WINAPI IEnumSTATPROPSETSTG_Next_Stub(IEnumSTATPROPSETSTG *This,ULONG celt,STATPROPSETSTG *rgelt,ULONG *pceltFetched);

#ifdef __cplusplus
}
#endif
#endif
