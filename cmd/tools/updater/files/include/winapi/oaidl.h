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

#ifndef __oaidl_h__
#define __oaidl_h__

#ifndef __ICreateTypeInfo_FWD_DEFINED__
#define __ICreateTypeInfo_FWD_DEFINED__
typedef struct ICreateTypeInfo ICreateTypeInfo;
#endif

#ifndef __ICreateTypeInfo2_FWD_DEFINED__
#define __ICreateTypeInfo2_FWD_DEFINED__
typedef struct ICreateTypeInfo2 ICreateTypeInfo2;
#endif

#ifndef __ICreateTypeLib_FWD_DEFINED__
#define __ICreateTypeLib_FWD_DEFINED__
typedef struct ICreateTypeLib ICreateTypeLib;
#endif

#ifndef __ICreateTypeLib2_FWD_DEFINED__
#define __ICreateTypeLib2_FWD_DEFINED__
typedef struct ICreateTypeLib2 ICreateTypeLib2;
#endif

#ifndef __IDispatch_FWD_DEFINED__
#define __IDispatch_FWD_DEFINED__
typedef struct IDispatch IDispatch;
#endif

#ifndef __IEnumVARIANT_FWD_DEFINED__
#define __IEnumVARIANT_FWD_DEFINED__
typedef struct IEnumVARIANT IEnumVARIANT;
#endif

#ifndef __ITypeComp_FWD_DEFINED__
#define __ITypeComp_FWD_DEFINED__
typedef struct ITypeComp ITypeComp;
#endif

#ifndef __ITypeInfo_FWD_DEFINED__
#define __ITypeInfo_FWD_DEFINED__
typedef struct ITypeInfo ITypeInfo;
#endif

#ifndef __ITypeInfo2_FWD_DEFINED__
#define __ITypeInfo2_FWD_DEFINED__
typedef struct ITypeInfo2 ITypeInfo2;
#endif

#ifndef __ITypeLib_FWD_DEFINED__
#define __ITypeLib_FWD_DEFINED__
typedef struct ITypeLib ITypeLib;
#endif

#ifndef __ITypeLib2_FWD_DEFINED__
#define __ITypeLib2_FWD_DEFINED__
typedef struct ITypeLib2 ITypeLib2;
#endif

#ifndef __ITypeChangeEvents_FWD_DEFINED__
#define __ITypeChangeEvents_FWD_DEFINED__
typedef struct ITypeChangeEvents ITypeChangeEvents;
#endif

#ifndef __IErrorInfo_FWD_DEFINED__
#define __IErrorInfo_FWD_DEFINED__
typedef struct IErrorInfo IErrorInfo;
#endif

#ifndef __ICreateErrorInfo_FWD_DEFINED__
#define __ICreateErrorInfo_FWD_DEFINED__
typedef struct ICreateErrorInfo ICreateErrorInfo;
#endif

#ifndef __ISupportErrorInfo_FWD_DEFINED__
#define __ISupportErrorInfo_FWD_DEFINED__
typedef struct ISupportErrorInfo ISupportErrorInfo;
#endif

#ifndef __ITypeFactory_FWD_DEFINED__
#define __ITypeFactory_FWD_DEFINED__
typedef struct ITypeFactory ITypeFactory;
#endif

#ifndef __ITypeMarshal_FWD_DEFINED__
#define __ITypeMarshal_FWD_DEFINED__
typedef struct ITypeMarshal ITypeMarshal;
#endif

#ifndef __IRecordInfo_FWD_DEFINED__
#define __IRecordInfo_FWD_DEFINED__
typedef struct IRecordInfo IRecordInfo;
#endif

#ifndef __IErrorLog_FWD_DEFINED__
#define __IErrorLog_FWD_DEFINED__
typedef struct IErrorLog IErrorLog;
#endif

#ifndef __IPropertyBag_FWD_DEFINED__
#define __IPropertyBag_FWD_DEFINED__
typedef struct IPropertyBag IPropertyBag;
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

  extern RPC_IF_HANDLE __MIDL_itf_oaidl_0000_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_oaidl_0000_v0_0_s_ifspec;

#ifndef __IOleAutomationTypes_INTERFACE_DEFINED__
#define __IOleAutomationTypes_INTERFACE_DEFINED__

  typedef CY CURRENCY;

  typedef struct tagSAFEARRAYBOUND {
    ULONG cElements;
    LONG lLbound;
  } SAFEARRAYBOUND;

  typedef struct tagSAFEARRAYBOUND *LPSAFEARRAYBOUND;
  typedef struct _wireVARIANT *wireVARIANT;
  typedef struct _wireBRECORD *wireBRECORD;

  typedef struct _wireSAFEARR_BSTR {
    ULONG Size;
    wireBSTR *aBstr;
  } SAFEARR_BSTR;

  typedef struct _wireSAFEARR_UNKNOWN {
    ULONG Size;
    IUnknown **apUnknown;
  } SAFEARR_UNKNOWN;

  typedef struct _wireSAFEARR_DISPATCH {
    ULONG Size;
    IDispatch **apDispatch;
  } SAFEARR_DISPATCH;

  typedef struct _wireSAFEARR_VARIANT {
    ULONG Size;
    wireVARIANT *aVariant;
  } SAFEARR_VARIANT;

  typedef struct _wireSAFEARR_BRECORD {
    ULONG Size;
    wireBRECORD *aRecord;
  } SAFEARR_BRECORD;

  typedef struct _wireSAFEARR_HAVEIID {
    ULONG Size;
    IUnknown **apUnknown;
    IID iid;
  } SAFEARR_HAVEIID;

  typedef enum tagSF_TYPE {
    SF_ERROR = VT_ERROR,
    SF_I1 = VT_I1,SF_I2 = VT_I2,SF_I4 = VT_I4,SF_I8 = VT_I8,
    SF_BSTR = VT_BSTR,
    SF_UNKNOWN = VT_UNKNOWN,SF_DISPATCH = VT_DISPATCH,
    SF_VARIANT = VT_VARIANT,SF_RECORD = VT_RECORD,
    SF_HAVEIID = VT_UNKNOWN | VT_RESERVED
  } SF_TYPE;

  typedef struct _wireSAFEARRAY_UNION {
    ULONG sfType;
    union __MIDL_IOleAutomationTypes_0001 {
      SAFEARR_BSTR BstrStr;
      SAFEARR_UNKNOWN UnknownStr;
      SAFEARR_DISPATCH DispatchStr;
      SAFEARR_VARIANT VariantStr;
      SAFEARR_BRECORD RecordStr;
      SAFEARR_HAVEIID HaveIidStr;
      BYTE_SIZEDARR ByteStr;
      WORD_SIZEDARR WordStr;
      DWORD_SIZEDARR LongStr;
      HYPER_SIZEDARR HyperStr;
    } u;
  } SAFEARRAYUNION;

  typedef struct _wireSAFEARRAY {
    USHORT cDims;
    USHORT fFeatures;
    ULONG cbElements;
    ULONG cLocks;
    SAFEARRAYUNION uArrayStructs;
    SAFEARRAYBOUND rgsabound[1 ];
  } *wireSAFEARRAY;

  typedef wireSAFEARRAY *wirePSAFEARRAY;

  typedef struct tagSAFEARRAY {
    USHORT cDims;
    USHORT fFeatures;
    ULONG cbElements;
    ULONG cLocks;
    PVOID pvData;
    SAFEARRAYBOUND rgsabound[1 ];
  } SAFEARRAY;

  typedef SAFEARRAY *LPSAFEARRAY;

#define FADF_AUTO (0x1)
#define FADF_STATIC (0x2)
#define FADF_EMBEDDED (0x4)
#define FADF_FIXEDSIZE (0x10)
#define FADF_RECORD (0x20)
#define FADF_HAVEIID (0x40)
#define FADF_HAVEVARTYPE (0x80)
#define FADF_BSTR (0x100)
#define FADF_UNKNOWN (0x200)
#define FADF_DISPATCH (0x400)
#define FADF_VARIANT (0x800)
#define FADF_RESERVED (0xf008)

#if defined(NONAMELESSUNION)
#define __VARIANT_NAME_1 n1
#define __VARIANT_NAME_2 n2
#define __VARIANT_NAME_3 n3
#define __VARIANT_NAME_4 brecVal
#else
#define __tagVARIANT
#define __VARIANT_NAME_1
#define __VARIANT_NAME_2
#define __VARIANT_NAME_3
#define __tagBRECORD
#define __VARIANT_NAME_4
#endif
  typedef struct tagVARIANT VARIANT;

  struct tagVARIANT {
    __MINGW_EXTENSION union {
      __MINGW_EXTENSION struct __tagVARIANT
      {
	VARTYPE vt;
	WORD wReserved1;
	WORD wReserved2;
	WORD wReserved3;
	__MINGW_EXTENSION union {
	  LONGLONG llVal;
	  LONG lVal;
	  BYTE bVal;
	  SHORT iVal;
	  FLOAT fltVal;
	  DOUBLE dblVal;
	  VARIANT_BOOL boolVal;
	  /* _VARIANT_BOOL bool; */
	  SCODE scode;
	  CY cyVal;
	  DATE date;
	  BSTR bstrVal;
	  IUnknown *punkVal;
	  IDispatch *pdispVal;
	  SAFEARRAY *parray;
	  BYTE *pbVal;
	  SHORT *piVal;
	  LONG *plVal;
	  LONGLONG *pllVal;
	  FLOAT *pfltVal;
	  DOUBLE *pdblVal;
	  VARIANT_BOOL *pboolVal;
	  /* _VARIANT_BOOL *pbool; */
	  SCODE *pscode;
	  CY *pcyVal;
	  DATE *pdate;
	  BSTR *pbstrVal;
	  IUnknown **ppunkVal;
	  IDispatch **ppdispVal;
	  SAFEARRAY **pparray;
	  VARIANT *pvarVal;
	  PVOID byref;
	  CHAR cVal;
	  USHORT uiVal;
	  ULONG ulVal;
	  ULONGLONG ullVal;
	  INT intVal;
	  UINT uintVal;
	  DECIMAL *pdecVal;
	  CHAR *pcVal;
	  USHORT *puiVal;
	  ULONG *pulVal;
	  ULONGLONG *pullVal;
	  INT *pintVal;
	  UINT *puintVal;
	  struct __tagBRECORD {
	    PVOID pvRecord;
	    IRecordInfo *pRecInfo;
	  } __VARIANT_NAME_4;
	} __VARIANT_NAME_3;
      } __VARIANT_NAME_2;
      DECIMAL decVal;
    } __VARIANT_NAME_1;
  };

  typedef VARIANT *LPVARIANT;
  typedef VARIANT VARIANTARG;
  typedef VARIANT *LPVARIANTARG;

  struct _wireBRECORD {
    ULONG fFlags;
    ULONG clSize;
    IRecordInfo *pRecInfo;
    byte *pRecord;
  };
  struct _wireVARIANT {
    DWORD clSize;
    DWORD rpcReserved;
    USHORT vt;
    USHORT wReserved1;
    USHORT wReserved2;
    USHORT wReserved3;
    __MINGW_EXTENSION union {
      LONGLONG llVal;
      LONG lVal;
      BYTE bVal;
      SHORT iVal;
      FLOAT fltVal;
      DOUBLE dblVal;
      VARIANT_BOOL boolVal;
      SCODE scode;
      CY cyVal;
      DATE date;
      wireBSTR bstrVal;
      IUnknown *punkVal;
      IDispatch *pdispVal;
      wirePSAFEARRAY parray;
      wireBRECORD brecVal;
      BYTE *pbVal;
      SHORT *piVal;
      LONG *plVal;
      LONGLONG *pllVal;
      FLOAT *pfltVal;
      DOUBLE *pdblVal;
      VARIANT_BOOL *pboolVal;
      SCODE *pscode;
      CY *pcyVal;
      DATE *pdate;
      wireBSTR *pbstrVal;
      IUnknown **ppunkVal;
      IDispatch **ppdispVal;
      wirePSAFEARRAY *pparray;
      wireVARIANT *pvarVal;
      CHAR cVal;
      USHORT uiVal;
      ULONG ulVal;
      ULONGLONG ullVal;
      INT intVal;
      UINT uintVal;
      DECIMAL decVal;
      DECIMAL *pdecVal;
      CHAR *pcVal;
      USHORT *puiVal;
      ULONG *pulVal;
      ULONGLONG *pullVal;
      INT *pintVal;
      UINT *puintVal;
    };
  };
#ifndef DEFINED_DISPID_MEMBERID
#define DEFINED_DISPID_MEMBERID
  typedef LONG DISPID;
  typedef DISPID MEMBERID;
  typedef DWORD HREFTYPE;
#endif

  typedef enum tagTYPEKIND {
    TKIND_ENUM = 0,TKIND_RECORD,TKIND_MODULE,TKIND_INTERFACE,
    TKIND_DISPATCH,TKIND_COCLASS,TKIND_ALIAS,TKIND_UNION,
    TKIND_MAX
  } TYPEKIND;

  typedef struct tagTYPEDESC {
    __MINGW_EXTENSION union {
      struct tagTYPEDESC *lptdesc;
      struct tagARRAYDESC *lpadesc;
      HREFTYPE hreftype;
    };
    VARTYPE vt;
  } TYPEDESC;

  typedef struct tagARRAYDESC {
    TYPEDESC tdescElem;
    USHORT cDims;
    SAFEARRAYBOUND rgbounds[1 ];
  } ARRAYDESC;

  typedef struct tagPARAMDESCEX {
    ULONG cBytes;
    VARIANTARG varDefaultValue;
  } PARAMDESCEX;

  typedef struct tagPARAMDESCEX *LPPARAMDESCEX;

  typedef struct tagPARAMDESC {
    LPPARAMDESCEX pparamdescex;
    USHORT wParamFlags;
  } PARAMDESC;

  typedef struct tagPARAMDESC *LPPARAMDESC;

#define PARAMFLAG_NONE (0)
#define PARAMFLAG_FIN (0x1)
#define PARAMFLAG_FOUT (0x2)
#define PARAMFLAG_FLCID (0x4)
#define PARAMFLAG_FRETVAL (0x8)
#define PARAMFLAG_FOPT (0x10)
#define PARAMFLAG_FHASDEFAULT (0x20)
#define PARAMFLAG_FHASCUSTDATA (0x40)

  typedef struct tagIDLDESC {
    ULONG_PTR dwReserved;
    USHORT wIDLFlags;
  } IDLDESC;

  typedef struct tagIDLDESC *LPIDLDESC;

#define IDLFLAG_NONE (PARAMFLAG_NONE)
#define IDLFLAG_FIN (PARAMFLAG_FIN)
#define IDLFLAG_FOUT (PARAMFLAG_FOUT)
#define IDLFLAG_FLCID (PARAMFLAG_FLCID)
#define IDLFLAG_FRETVAL (PARAMFLAG_FRETVAL)

  typedef struct tagELEMDESC {
    TYPEDESC tdesc;
    __MINGW_EXTENSION union {
      IDLDESC idldesc;
      PARAMDESC paramdesc;
    };
  } ELEMDESC,*LPELEMDESC;

  typedef struct tagTYPEATTR {
    GUID guid;
    LCID lcid;
    DWORD dwReserved;
    MEMBERID memidConstructor;
    MEMBERID memidDestructor;
    LPOLESTR lpstrSchema;
    ULONG cbSizeInstance;
    TYPEKIND typekind;
    WORD cFuncs;
    WORD cVars;
    WORD cImplTypes;
    WORD cbSizeVft;
    WORD cbAlignment;
    WORD wTypeFlags;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    TYPEDESC tdescAlias;
    IDLDESC idldescType;
  } TYPEATTR;

  typedef struct tagTYPEATTR *LPTYPEATTR;

  typedef struct tagDISPPARAMS {
    VARIANTARG *rgvarg;
    DISPID *rgdispidNamedArgs;
    UINT cArgs;
    UINT cNamedArgs;
  } DISPPARAMS;

  typedef struct tagEXCEPINFO {
    WORD wCode;
    WORD wReserved;
    BSTR bstrSource;
    BSTR bstrDescription;
    BSTR bstrHelpFile;
    DWORD dwHelpContext;
    PVOID pvReserved;
    HRESULT (WINAPI *pfnDeferredFillIn)(struct tagEXCEPINFO *);
    SCODE scode;
  } EXCEPINFO,*LPEXCEPINFO;

  typedef enum tagCALLCONV {
    CC_FASTCALL = 0,
    CC_CDECL = 1,CC_MSCPASCAL = 2,
    CC_PASCAL = CC_MSCPASCAL,
    CC_MACPASCAL = 3,
    CC_STDCALL = 4,
    CC_FPFASTCALL = 5,
    CC_SYSCALL = 6,
    CC_MPWCDECL = 7,
    CC_MPWPASCAL = 8,
    CC_MAX = 9
  } CALLCONV;

  typedef enum tagFUNCKIND {
    FUNC_VIRTUAL = 0,
    FUNC_PUREVIRTUAL = 1,
    FUNC_NONVIRTUAL = 2,
    FUNC_STATIC = 3,
    FUNC_DISPATCH = 4
  } FUNCKIND;

  typedef enum tagINVOKEKIND {
    INVOKE_FUNC = 1,
    INVOKE_PROPERTYGET = 2,
    INVOKE_PROPERTYPUT = 4,
    INVOKE_PROPERTYPUTREF = 8
  } INVOKEKIND;

  typedef struct tagFUNCDESC {
    MEMBERID memid;
    SCODE *lprgscode;
    ELEMDESC *lprgelemdescParam;
    FUNCKIND funckind;
    INVOKEKIND invkind;
    CALLCONV callconv;
    SHORT cParams;
    SHORT cParamsOpt;
    SHORT oVft;
    SHORT cScodes;
    ELEMDESC elemdescFunc;
    WORD wFuncFlags;
  } FUNCDESC;

  typedef struct tagFUNCDESC *LPFUNCDESC;

  typedef enum tagVARKIND {
    VAR_PERINSTANCE = 0,VAR_STATIC,VAR_CONST,VAR_DISPATCH
  } VARKIND;

#define IMPLTYPEFLAG_FDEFAULT (0x1)
#define IMPLTYPEFLAG_FSOURCE (0x2)
#define IMPLTYPEFLAG_FRESTRICTED (0x4)
#define IMPLTYPEFLAG_FDEFAULTVTABLE (0x8)

  typedef struct tagVARDESC {
    MEMBERID memid;
    LPOLESTR lpstrSchema;
    __MINGW_EXTENSION union {
      ULONG oInst;
      VARIANT *lpvarValue;
    };
    ELEMDESC elemdescVar;
    WORD wVarFlags;
    VARKIND varkind;
  } VARDESC;

  typedef struct tagVARDESC *LPVARDESC;

  typedef enum tagTYPEFLAGS {
    TYPEFLAG_FAPPOBJECT = 0x1,TYPEFLAG_FCANCREATE = 0x2,TYPEFLAG_FLICENSED = 0x4,
    TYPEFLAG_FPREDECLID = 0x8,TYPEFLAG_FHIDDEN = 0x10,
    TYPEFLAG_FCONTROL = 0x20,TYPEFLAG_FDUAL = 0x40,TYPEFLAG_FNONEXTENSIBLE = 0x80,
    TYPEFLAG_FOLEAUTOMATION = 0x100,TYPEFLAG_FRESTRICTED = 0x200,
    TYPEFLAG_FAGGREGATABLE = 0x400,TYPEFLAG_FREPLACEABLE = 0x800,
    TYPEFLAG_FDISPATCHABLE = 0x1000,TYPEFLAG_FREVERSEBIND = 0x2000,
    TYPEFLAG_FPROXY = 0x4000
  } TYPEFLAGS;

  typedef enum tagFUNCFLAGS {
    FUNCFLAG_FRESTRICTED = 0x1,FUNCFLAG_FSOURCE = 0x2,FUNCFLAG_FBINDABLE = 0x4,
    FUNCFLAG_FREQUESTEDIT = 0x8,FUNCFLAG_FDISPLAYBIND = 0x10,
    FUNCFLAG_FDEFAULTBIND = 0x20,FUNCFLAG_FHIDDEN = 0x40,
    FUNCFLAG_FUSESGETLASTERROR = 0x80,FUNCFLAG_FDEFAULTCOLLELEM = 0x100,
    FUNCFLAG_FUIDEFAULT = 0x200,
    FUNCFLAG_FNONBROWSABLE = 0x400,FUNCFLAG_FREPLACEABLE = 0x800,
    FUNCFLAG_FIMMEDIATEBIND = 0x1000
  } FUNCFLAGS;

  typedef enum tagVARFLAGS {
    VARFLAG_FREADONLY = 0x1,VARFLAG_FSOURCE = 0x2,VARFLAG_FBINDABLE = 0x4,
    VARFLAG_FREQUESTEDIT = 0x8,VARFLAG_FDISPLAYBIND = 0x10,
    VARFLAG_FDEFAULTBIND = 0x20,VARFLAG_FHIDDEN = 0x40,VARFLAG_FRESTRICTED = 0x80,
    VARFLAG_FDEFAULTCOLLELEM = 0x100,VARFLAG_FUIDEFAULT = 0x200,
    VARFLAG_FNONBROWSABLE = 0x400,VARFLAG_FREPLACEABLE = 0x800,VARFLAG_FIMMEDIATEBIND = 0x1000
  } VARFLAGS;

  typedef struct tagCLEANLOCALSTORAGE {
    IUnknown *pInterface;
    PVOID pStorage;
    DWORD flags;
  } CLEANLOCALSTORAGE;

  typedef struct tagCUSTDATAITEM {
    GUID guid;
    VARIANTARG varValue;
  } CUSTDATAITEM;

  typedef struct tagCUSTDATAITEM *LPCUSTDATAITEM;

  typedef struct tagCUSTDATA {
    DWORD cCustData;
    LPCUSTDATAITEM prgCustData;
  } CUSTDATA;

  typedef struct tagCUSTDATA *LPCUSTDATA;

  extern RPC_IF_HANDLE IOleAutomationTypes_v1_0_c_ifspec;
  extern RPC_IF_HANDLE IOleAutomationTypes_v1_0_s_ifspec;
#endif

#ifndef __ICreateTypeInfo_INTERFACE_DEFINED__
#define __ICreateTypeInfo_INTERFACE_DEFINED__
#ifndef DEFINE_LPCREATETYPEINFO
#define DEFINE_LPCREATETYPEINFO
  typedef ICreateTypeInfo *LPCREATETYPEINFO;
#endif

  EXTERN_C const IID IID_ICreateTypeInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICreateTypeInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI SetGuid(REFGUID guid) = 0;
    virtual HRESULT WINAPI SetTypeFlags(UINT uTypeFlags) = 0;
    virtual HRESULT WINAPI SetDocString(LPOLESTR pStrDoc) = 0;
    virtual HRESULT WINAPI SetHelpContext(DWORD dwHelpContext) = 0;
    virtual HRESULT WINAPI SetVersion(WORD wMajorVerNum,WORD wMinorVerNum) = 0;
    virtual HRESULT WINAPI AddRefTypeInfo(ITypeInfo *pTInfo,HREFTYPE *phRefType) = 0;
    virtual HRESULT WINAPI AddFuncDesc(UINT index,FUNCDESC *pFuncDesc) = 0;
    virtual HRESULT WINAPI AddImplType(UINT index,HREFTYPE hRefType) = 0;
    virtual HRESULT WINAPI SetImplTypeFlags(UINT index,INT implTypeFlags) = 0;
    virtual HRESULT WINAPI SetAlignment(WORD cbAlignment) = 0;
    virtual HRESULT WINAPI SetSchema(LPOLESTR pStrSchema) = 0;
    virtual HRESULT WINAPI AddVarDesc(UINT index,VARDESC *pVarDesc) = 0;
    virtual HRESULT WINAPI SetFuncAndParamNames(UINT index,LPOLESTR *rgszNames,UINT cNames) = 0;
    virtual HRESULT WINAPI SetVarName(UINT index,LPOLESTR szName) = 0;
    virtual HRESULT WINAPI SetTypeDescAlias(TYPEDESC *pTDescAlias) = 0;
    virtual HRESULT WINAPI DefineFuncAsDllEntry(UINT index,LPOLESTR szDllName,LPOLESTR szProcName) = 0;
    virtual HRESULT WINAPI SetFuncDocString(UINT index,LPOLESTR szDocString) = 0;
    virtual HRESULT WINAPI SetVarDocString(UINT index,LPOLESTR szDocString) = 0;
    virtual HRESULT WINAPI SetFuncHelpContext(UINT index,DWORD dwHelpContext) = 0;
    virtual HRESULT WINAPI SetVarHelpContext(UINT index,DWORD dwHelpContext) = 0;
    virtual HRESULT WINAPI SetMops(UINT index,BSTR bstrMops) = 0;
    virtual HRESULT WINAPI SetTypeIdldesc(IDLDESC *pIdlDesc) = 0;
    virtual HRESULT WINAPI LayOut(void) = 0;
  };
#else
  typedef struct ICreateTypeInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICreateTypeInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICreateTypeInfo *This);
      ULONG (WINAPI *Release)(ICreateTypeInfo *This);
      HRESULT (WINAPI *SetGuid)(ICreateTypeInfo *This,REFGUID guid);
      HRESULT (WINAPI *SetTypeFlags)(ICreateTypeInfo *This,UINT uTypeFlags);
      HRESULT (WINAPI *SetDocString)(ICreateTypeInfo *This,LPOLESTR pStrDoc);
      HRESULT (WINAPI *SetHelpContext)(ICreateTypeInfo *This,DWORD dwHelpContext);
      HRESULT (WINAPI *SetVersion)(ICreateTypeInfo *This,WORD wMajorVerNum,WORD wMinorVerNum);
      HRESULT (WINAPI *AddRefTypeInfo)(ICreateTypeInfo *This,ITypeInfo *pTInfo,HREFTYPE *phRefType);
      HRESULT (WINAPI *AddFuncDesc)(ICreateTypeInfo *This,UINT index,FUNCDESC *pFuncDesc);
      HRESULT (WINAPI *AddImplType)(ICreateTypeInfo *This,UINT index,HREFTYPE hRefType);
      HRESULT (WINAPI *SetImplTypeFlags)(ICreateTypeInfo *This,UINT index,INT implTypeFlags);
      HRESULT (WINAPI *SetAlignment)(ICreateTypeInfo *This,WORD cbAlignment);
      HRESULT (WINAPI *SetSchema)(ICreateTypeInfo *This,LPOLESTR pStrSchema);
      HRESULT (WINAPI *AddVarDesc)(ICreateTypeInfo *This,UINT index,VARDESC *pVarDesc);
      HRESULT (WINAPI *SetFuncAndParamNames)(ICreateTypeInfo *This,UINT index,LPOLESTR *rgszNames,UINT cNames);
      HRESULT (WINAPI *SetVarName)(ICreateTypeInfo *This,UINT index,LPOLESTR szName);
      HRESULT (WINAPI *SetTypeDescAlias)(ICreateTypeInfo *This,TYPEDESC *pTDescAlias);
      HRESULT (WINAPI *DefineFuncAsDllEntry)(ICreateTypeInfo *This,UINT index,LPOLESTR szDllName,LPOLESTR szProcName);
      HRESULT (WINAPI *SetFuncDocString)(ICreateTypeInfo *This,UINT index,LPOLESTR szDocString);
      HRESULT (WINAPI *SetVarDocString)(ICreateTypeInfo *This,UINT index,LPOLESTR szDocString);
      HRESULT (WINAPI *SetFuncHelpContext)(ICreateTypeInfo *This,UINT index,DWORD dwHelpContext);
      HRESULT (WINAPI *SetVarHelpContext)(ICreateTypeInfo *This,UINT index,DWORD dwHelpContext);
      HRESULT (WINAPI *SetMops)(ICreateTypeInfo *This,UINT index,BSTR bstrMops);
      HRESULT (WINAPI *SetTypeIdldesc)(ICreateTypeInfo *This,IDLDESC *pIdlDesc);
      HRESULT (WINAPI *LayOut)(ICreateTypeInfo *This);
    END_INTERFACE
  } ICreateTypeInfoVtbl;
  struct ICreateTypeInfo {
    CONST_VTBL struct ICreateTypeInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICreateTypeInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICreateTypeInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICreateTypeInfo_Release(This) (This)->lpVtbl->Release(This)
#define ICreateTypeInfo_SetGuid(This,guid) (This)->lpVtbl->SetGuid(This,guid)
#define ICreateTypeInfo_SetTypeFlags(This,uTypeFlags) (This)->lpVtbl->SetTypeFlags(This,uTypeFlags)
#define ICreateTypeInfo_SetDocString(This,pStrDoc) (This)->lpVtbl->SetDocString(This,pStrDoc)
#define ICreateTypeInfo_SetHelpContext(This,dwHelpContext) (This)->lpVtbl->SetHelpContext(This,dwHelpContext)
#define ICreateTypeInfo_SetVersion(This,wMajorVerNum,wMinorVerNum) (This)->lpVtbl->SetVersion(This,wMajorVerNum,wMinorVerNum)
#define ICreateTypeInfo_AddRefTypeInfo(This,pTInfo,phRefType) (This)->lpVtbl->AddRefTypeInfo(This,pTInfo,phRefType)
#define ICreateTypeInfo_AddFuncDesc(This,index,pFuncDesc) (This)->lpVtbl->AddFuncDesc(This,index,pFuncDesc)
#define ICreateTypeInfo_AddImplType(This,index,hRefType) (This)->lpVtbl->AddImplType(This,index,hRefType)
#define ICreateTypeInfo_SetImplTypeFlags(This,index,implTypeFlags) (This)->lpVtbl->SetImplTypeFlags(This,index,implTypeFlags)
#define ICreateTypeInfo_SetAlignment(This,cbAlignment) (This)->lpVtbl->SetAlignment(This,cbAlignment)
#define ICreateTypeInfo_SetSchema(This,pStrSchema) (This)->lpVtbl->SetSchema(This,pStrSchema)
#define ICreateTypeInfo_AddVarDesc(This,index,pVarDesc) (This)->lpVtbl->AddVarDesc(This,index,pVarDesc)
#define ICreateTypeInfo_SetFuncAndParamNames(This,index,rgszNames,cNames) (This)->lpVtbl->SetFuncAndParamNames(This,index,rgszNames,cNames)
#define ICreateTypeInfo_SetVarName(This,index,szName) (This)->lpVtbl->SetVarName(This,index,szName)
#define ICreateTypeInfo_SetTypeDescAlias(This,pTDescAlias) (This)->lpVtbl->SetTypeDescAlias(This,pTDescAlias)
#define ICreateTypeInfo_DefineFuncAsDllEntry(This,index,szDllName,szProcName) (This)->lpVtbl->DefineFuncAsDllEntry(This,index,szDllName,szProcName)
#define ICreateTypeInfo_SetFuncDocString(This,index,szDocString) (This)->lpVtbl->SetFuncDocString(This,index,szDocString)
#define ICreateTypeInfo_SetVarDocString(This,index,szDocString) (This)->lpVtbl->SetVarDocString(This,index,szDocString)
#define ICreateTypeInfo_SetFuncHelpContext(This,index,dwHelpContext) (This)->lpVtbl->SetFuncHelpContext(This,index,dwHelpContext)
#define ICreateTypeInfo_SetVarHelpContext(This,index,dwHelpContext) (This)->lpVtbl->SetVarHelpContext(This,index,dwHelpContext)
#define ICreateTypeInfo_SetMops(This,index,bstrMops) (This)->lpVtbl->SetMops(This,index,bstrMops)
#define ICreateTypeInfo_SetTypeIdldesc(This,pIdlDesc) (This)->lpVtbl->SetTypeIdldesc(This,pIdlDesc)
#define ICreateTypeInfo_LayOut(This) (This)->lpVtbl->LayOut(This)
#endif
#endif
  HRESULT WINAPI ICreateTypeInfo_SetGuid_Proxy(ICreateTypeInfo *This,REFGUID guid);
  void __RPC_STUB ICreateTypeInfo_SetGuid_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetTypeFlags_Proxy(ICreateTypeInfo *This,UINT uTypeFlags);
  void __RPC_STUB ICreateTypeInfo_SetTypeFlags_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetDocString_Proxy(ICreateTypeInfo *This,LPOLESTR pStrDoc);
  void __RPC_STUB ICreateTypeInfo_SetDocString_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetHelpContext_Proxy(ICreateTypeInfo *This,DWORD dwHelpContext);
  void __RPC_STUB ICreateTypeInfo_SetHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetVersion_Proxy(ICreateTypeInfo *This,WORD wMajorVerNum,WORD wMinorVerNum);
  void __RPC_STUB ICreateTypeInfo_SetVersion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_AddRefTypeInfo_Proxy(ICreateTypeInfo *This,ITypeInfo *pTInfo,HREFTYPE *phRefType);
  void __RPC_STUB ICreateTypeInfo_AddRefTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_AddFuncDesc_Proxy(ICreateTypeInfo *This,UINT index,FUNCDESC *pFuncDesc);
  void __RPC_STUB ICreateTypeInfo_AddFuncDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_AddImplType_Proxy(ICreateTypeInfo *This,UINT index,HREFTYPE hRefType);
  void __RPC_STUB ICreateTypeInfo_AddImplType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetImplTypeFlags_Proxy(ICreateTypeInfo *This,UINT index,INT implTypeFlags);
  void __RPC_STUB ICreateTypeInfo_SetImplTypeFlags_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetAlignment_Proxy(ICreateTypeInfo *This,WORD cbAlignment);
  void __RPC_STUB ICreateTypeInfo_SetAlignment_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetSchema_Proxy(ICreateTypeInfo *This,LPOLESTR pStrSchema);
  void __RPC_STUB ICreateTypeInfo_SetSchema_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_AddVarDesc_Proxy(ICreateTypeInfo *This,UINT index,VARDESC *pVarDesc);
  void __RPC_STUB ICreateTypeInfo_AddVarDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetFuncAndParamNames_Proxy(ICreateTypeInfo *This,UINT index,LPOLESTR *rgszNames,UINT cNames);
  void __RPC_STUB ICreateTypeInfo_SetFuncAndParamNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetVarName_Proxy(ICreateTypeInfo *This,UINT index,LPOLESTR szName);
  void __RPC_STUB ICreateTypeInfo_SetVarName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetTypeDescAlias_Proxy(ICreateTypeInfo *This,TYPEDESC *pTDescAlias);
  void __RPC_STUB ICreateTypeInfo_SetTypeDescAlias_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_DefineFuncAsDllEntry_Proxy(ICreateTypeInfo *This,UINT index,LPOLESTR szDllName,LPOLESTR szProcName);
  void __RPC_STUB ICreateTypeInfo_DefineFuncAsDllEntry_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetFuncDocString_Proxy(ICreateTypeInfo *This,UINT index,LPOLESTR szDocString);
  void __RPC_STUB ICreateTypeInfo_SetFuncDocString_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetVarDocString_Proxy(ICreateTypeInfo *This,UINT index,LPOLESTR szDocString);
  void __RPC_STUB ICreateTypeInfo_SetVarDocString_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetFuncHelpContext_Proxy(ICreateTypeInfo *This,UINT index,DWORD dwHelpContext);
  void __RPC_STUB ICreateTypeInfo_SetFuncHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetVarHelpContext_Proxy(ICreateTypeInfo *This,UINT index,DWORD dwHelpContext);
  void __RPC_STUB ICreateTypeInfo_SetVarHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetMops_Proxy(ICreateTypeInfo *This,UINT index,BSTR bstrMops);
  void __RPC_STUB ICreateTypeInfo_SetMops_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_SetTypeIdldesc_Proxy(ICreateTypeInfo *This,IDLDESC *pIdlDesc);
  void __RPC_STUB ICreateTypeInfo_SetTypeIdldesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo_LayOut_Proxy(ICreateTypeInfo *This);
  void __RPC_STUB ICreateTypeInfo_LayOut_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICreateTypeInfo2_INTERFACE_DEFINED__
#define __ICreateTypeInfo2_INTERFACE_DEFINED__
  typedef ICreateTypeInfo2 *LPCREATETYPEINFO2;

  EXTERN_C const IID IID_ICreateTypeInfo2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICreateTypeInfo2 : public ICreateTypeInfo {
  public:
    virtual HRESULT WINAPI DeleteFuncDesc(UINT index) = 0;
    virtual HRESULT WINAPI DeleteFuncDescByMemId(MEMBERID memid,INVOKEKIND invKind) = 0;
    virtual HRESULT WINAPI DeleteVarDesc(UINT index) = 0;
    virtual HRESULT WINAPI DeleteVarDescByMemId(MEMBERID memid) = 0;
    virtual HRESULT WINAPI DeleteImplType(UINT index) = 0;
    virtual HRESULT WINAPI SetCustData(REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetFuncCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetParamCustData(UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetVarCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetImplTypeCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetHelpStringContext(ULONG dwHelpStringContext) = 0;
    virtual HRESULT WINAPI SetFuncHelpStringContext(UINT index,ULONG dwHelpStringContext) = 0;
    virtual HRESULT WINAPI SetVarHelpStringContext(UINT index,ULONG dwHelpStringContext) = 0;
    virtual HRESULT WINAPI Invalidate(void) = 0;
    virtual HRESULT WINAPI SetName(LPOLESTR szName) = 0;
  };
#else
  typedef struct ICreateTypeInfo2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICreateTypeInfo2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICreateTypeInfo2 *This);
      ULONG (WINAPI *Release)(ICreateTypeInfo2 *This);
      HRESULT (WINAPI *SetGuid)(ICreateTypeInfo2 *This,REFGUID guid);
      HRESULT (WINAPI *SetTypeFlags)(ICreateTypeInfo2 *This,UINT uTypeFlags);
      HRESULT (WINAPI *SetDocString)(ICreateTypeInfo2 *This,LPOLESTR pStrDoc);
      HRESULT (WINAPI *SetHelpContext)(ICreateTypeInfo2 *This,DWORD dwHelpContext);
      HRESULT (WINAPI *SetVersion)(ICreateTypeInfo2 *This,WORD wMajorVerNum,WORD wMinorVerNum);
      HRESULT (WINAPI *AddRefTypeInfo)(ICreateTypeInfo2 *This,ITypeInfo *pTInfo,HREFTYPE *phRefType);
      HRESULT (WINAPI *AddFuncDesc)(ICreateTypeInfo2 *This,UINT index,FUNCDESC *pFuncDesc);
      HRESULT (WINAPI *AddImplType)(ICreateTypeInfo2 *This,UINT index,HREFTYPE hRefType);
      HRESULT (WINAPI *SetImplTypeFlags)(ICreateTypeInfo2 *This,UINT index,INT implTypeFlags);
      HRESULT (WINAPI *SetAlignment)(ICreateTypeInfo2 *This,WORD cbAlignment);
      HRESULT (WINAPI *SetSchema)(ICreateTypeInfo2 *This,LPOLESTR pStrSchema);
      HRESULT (WINAPI *AddVarDesc)(ICreateTypeInfo2 *This,UINT index,VARDESC *pVarDesc);
      HRESULT (WINAPI *SetFuncAndParamNames)(ICreateTypeInfo2 *This,UINT index,LPOLESTR *rgszNames,UINT cNames);
      HRESULT (WINAPI *SetVarName)(ICreateTypeInfo2 *This,UINT index,LPOLESTR szName);
      HRESULT (WINAPI *SetTypeDescAlias)(ICreateTypeInfo2 *This,TYPEDESC *pTDescAlias);
      HRESULT (WINAPI *DefineFuncAsDllEntry)(ICreateTypeInfo2 *This,UINT index,LPOLESTR szDllName,LPOLESTR szProcName);
      HRESULT (WINAPI *SetFuncDocString)(ICreateTypeInfo2 *This,UINT index,LPOLESTR szDocString);
      HRESULT (WINAPI *SetVarDocString)(ICreateTypeInfo2 *This,UINT index,LPOLESTR szDocString);
      HRESULT (WINAPI *SetFuncHelpContext)(ICreateTypeInfo2 *This,UINT index,DWORD dwHelpContext);
      HRESULT (WINAPI *SetVarHelpContext)(ICreateTypeInfo2 *This,UINT index,DWORD dwHelpContext);
      HRESULT (WINAPI *SetMops)(ICreateTypeInfo2 *This,UINT index,BSTR bstrMops);
      HRESULT (WINAPI *SetTypeIdldesc)(ICreateTypeInfo2 *This,IDLDESC *pIdlDesc);
      HRESULT (WINAPI *LayOut)(ICreateTypeInfo2 *This);
      HRESULT (WINAPI *DeleteFuncDesc)(ICreateTypeInfo2 *This,UINT index);
      HRESULT (WINAPI *DeleteFuncDescByMemId)(ICreateTypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind);
      HRESULT (WINAPI *DeleteVarDesc)(ICreateTypeInfo2 *This,UINT index);
      HRESULT (WINAPI *DeleteVarDescByMemId)(ICreateTypeInfo2 *This,MEMBERID memid);
      HRESULT (WINAPI *DeleteImplType)(ICreateTypeInfo2 *This,UINT index);
      HRESULT (WINAPI *SetCustData)(ICreateTypeInfo2 *This,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetFuncCustData)(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetParamCustData)(ICreateTypeInfo2 *This,UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetVarCustData)(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetImplTypeCustData)(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetHelpStringContext)(ICreateTypeInfo2 *This,ULONG dwHelpStringContext);
      HRESULT (WINAPI *SetFuncHelpStringContext)(ICreateTypeInfo2 *This,UINT index,ULONG dwHelpStringContext);
      HRESULT (WINAPI *SetVarHelpStringContext)(ICreateTypeInfo2 *This,UINT index,ULONG dwHelpStringContext);
      HRESULT (WINAPI *Invalidate)(ICreateTypeInfo2 *This);
      HRESULT (WINAPI *SetName)(ICreateTypeInfo2 *This,LPOLESTR szName);
    END_INTERFACE
  } ICreateTypeInfo2Vtbl;
  struct ICreateTypeInfo2 {
    CONST_VTBL struct ICreateTypeInfo2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICreateTypeInfo2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICreateTypeInfo2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICreateTypeInfo2_Release(This) (This)->lpVtbl->Release(This)
#define ICreateTypeInfo2_SetGuid(This,guid) (This)->lpVtbl->SetGuid(This,guid)
#define ICreateTypeInfo2_SetTypeFlags(This,uTypeFlags) (This)->lpVtbl->SetTypeFlags(This,uTypeFlags)
#define ICreateTypeInfo2_SetDocString(This,pStrDoc) (This)->lpVtbl->SetDocString(This,pStrDoc)
#define ICreateTypeInfo2_SetHelpContext(This,dwHelpContext) (This)->lpVtbl->SetHelpContext(This,dwHelpContext)
#define ICreateTypeInfo2_SetVersion(This,wMajorVerNum,wMinorVerNum) (This)->lpVtbl->SetVersion(This,wMajorVerNum,wMinorVerNum)
#define ICreateTypeInfo2_AddRefTypeInfo(This,pTInfo,phRefType) (This)->lpVtbl->AddRefTypeInfo(This,pTInfo,phRefType)
#define ICreateTypeInfo2_AddFuncDesc(This,index,pFuncDesc) (This)->lpVtbl->AddFuncDesc(This,index,pFuncDesc)
#define ICreateTypeInfo2_AddImplType(This,index,hRefType) (This)->lpVtbl->AddImplType(This,index,hRefType)
#define ICreateTypeInfo2_SetImplTypeFlags(This,index,implTypeFlags) (This)->lpVtbl->SetImplTypeFlags(This,index,implTypeFlags)
#define ICreateTypeInfo2_SetAlignment(This,cbAlignment) (This)->lpVtbl->SetAlignment(This,cbAlignment)
#define ICreateTypeInfo2_SetSchema(This,pStrSchema) (This)->lpVtbl->SetSchema(This,pStrSchema)
#define ICreateTypeInfo2_AddVarDesc(This,index,pVarDesc) (This)->lpVtbl->AddVarDesc(This,index,pVarDesc)
#define ICreateTypeInfo2_SetFuncAndParamNames(This,index,rgszNames,cNames) (This)->lpVtbl->SetFuncAndParamNames(This,index,rgszNames,cNames)
#define ICreateTypeInfo2_SetVarName(This,index,szName) (This)->lpVtbl->SetVarName(This,index,szName)
#define ICreateTypeInfo2_SetTypeDescAlias(This,pTDescAlias) (This)->lpVtbl->SetTypeDescAlias(This,pTDescAlias)
#define ICreateTypeInfo2_DefineFuncAsDllEntry(This,index,szDllName,szProcName) (This)->lpVtbl->DefineFuncAsDllEntry(This,index,szDllName,szProcName)
#define ICreateTypeInfo2_SetFuncDocString(This,index,szDocString) (This)->lpVtbl->SetFuncDocString(This,index,szDocString)
#define ICreateTypeInfo2_SetVarDocString(This,index,szDocString) (This)->lpVtbl->SetVarDocString(This,index,szDocString)
#define ICreateTypeInfo2_SetFuncHelpContext(This,index,dwHelpContext) (This)->lpVtbl->SetFuncHelpContext(This,index,dwHelpContext)
#define ICreateTypeInfo2_SetVarHelpContext(This,index,dwHelpContext) (This)->lpVtbl->SetVarHelpContext(This,index,dwHelpContext)
#define ICreateTypeInfo2_SetMops(This,index,bstrMops) (This)->lpVtbl->SetMops(This,index,bstrMops)
#define ICreateTypeInfo2_SetTypeIdldesc(This,pIdlDesc) (This)->lpVtbl->SetTypeIdldesc(This,pIdlDesc)
#define ICreateTypeInfo2_LayOut(This) (This)->lpVtbl->LayOut(This)
#define ICreateTypeInfo2_DeleteFuncDesc(This,index) (This)->lpVtbl->DeleteFuncDesc(This,index)
#define ICreateTypeInfo2_DeleteFuncDescByMemId(This,memid,invKind) (This)->lpVtbl->DeleteFuncDescByMemId(This,memid,invKind)
#define ICreateTypeInfo2_DeleteVarDesc(This,index) (This)->lpVtbl->DeleteVarDesc(This,index)
#define ICreateTypeInfo2_DeleteVarDescByMemId(This,memid) (This)->lpVtbl->DeleteVarDescByMemId(This,memid)
#define ICreateTypeInfo2_DeleteImplType(This,index) (This)->lpVtbl->DeleteImplType(This,index)
#define ICreateTypeInfo2_SetCustData(This,guid,pVarVal) (This)->lpVtbl->SetCustData(This,guid,pVarVal)
#define ICreateTypeInfo2_SetFuncCustData(This,index,guid,pVarVal) (This)->lpVtbl->SetFuncCustData(This,index,guid,pVarVal)
#define ICreateTypeInfo2_SetParamCustData(This,indexFunc,indexParam,guid,pVarVal) (This)->lpVtbl->SetParamCustData(This,indexFunc,indexParam,guid,pVarVal)
#define ICreateTypeInfo2_SetVarCustData(This,index,guid,pVarVal) (This)->lpVtbl->SetVarCustData(This,index,guid,pVarVal)
#define ICreateTypeInfo2_SetImplTypeCustData(This,index,guid,pVarVal) (This)->lpVtbl->SetImplTypeCustData(This,index,guid,pVarVal)
#define ICreateTypeInfo2_SetHelpStringContext(This,dwHelpStringContext) (This)->lpVtbl->SetHelpStringContext(This,dwHelpStringContext)
#define ICreateTypeInfo2_SetFuncHelpStringContext(This,index,dwHelpStringContext) (This)->lpVtbl->SetFuncHelpStringContext(This,index,dwHelpStringContext)
#define ICreateTypeInfo2_SetVarHelpStringContext(This,index,dwHelpStringContext) (This)->lpVtbl->SetVarHelpStringContext(This,index,dwHelpStringContext)
#define ICreateTypeInfo2_Invalidate(This) (This)->lpVtbl->Invalidate(This)
#define ICreateTypeInfo2_SetName(This,szName) (This)->lpVtbl->SetName(This,szName)
#endif
#endif
  HRESULT WINAPI ICreateTypeInfo2_DeleteFuncDesc_Proxy(ICreateTypeInfo2 *This,UINT index);
  void __RPC_STUB ICreateTypeInfo2_DeleteFuncDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_DeleteFuncDescByMemId_Proxy(ICreateTypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind);
  void __RPC_STUB ICreateTypeInfo2_DeleteFuncDescByMemId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_DeleteVarDesc_Proxy(ICreateTypeInfo2 *This,UINT index);
  void __RPC_STUB ICreateTypeInfo2_DeleteVarDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_DeleteVarDescByMemId_Proxy(ICreateTypeInfo2 *This,MEMBERID memid);
  void __RPC_STUB ICreateTypeInfo2_DeleteVarDescByMemId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_DeleteImplType_Proxy(ICreateTypeInfo2 *This,UINT index);
  void __RPC_STUB ICreateTypeInfo2_DeleteImplType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetCustData_Proxy(ICreateTypeInfo2 *This,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeInfo2_SetCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetFuncCustData_Proxy(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeInfo2_SetFuncCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetParamCustData_Proxy(ICreateTypeInfo2 *This,UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeInfo2_SetParamCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetVarCustData_Proxy(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeInfo2_SetVarCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetImplTypeCustData_Proxy(ICreateTypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeInfo2_SetImplTypeCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetHelpStringContext_Proxy(ICreateTypeInfo2 *This,ULONG dwHelpStringContext);
  void __RPC_STUB ICreateTypeInfo2_SetHelpStringContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetFuncHelpStringContext_Proxy(ICreateTypeInfo2 *This,UINT index,ULONG dwHelpStringContext);
  void __RPC_STUB ICreateTypeInfo2_SetFuncHelpStringContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetVarHelpStringContext_Proxy(ICreateTypeInfo2 *This,UINT index,ULONG dwHelpStringContext);
  void __RPC_STUB ICreateTypeInfo2_SetVarHelpStringContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_Invalidate_Proxy(ICreateTypeInfo2 *This);
  void __RPC_STUB ICreateTypeInfo2_Invalidate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeInfo2_SetName_Proxy(ICreateTypeInfo2 *This,LPOLESTR szName);
  void __RPC_STUB ICreateTypeInfo2_SetName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICreateTypeLib_INTERFACE_DEFINED__
#define __ICreateTypeLib_INTERFACE_DEFINED__
#ifndef DEFINED_LPCREATETYPELIB
#define DEFINED_LPCREATETYPELIB
  typedef ICreateTypeLib *LPCREATETYPELIB;
#endif

  EXTERN_C const IID IID_ICreateTypeLib;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICreateTypeLib : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateTypeInfo(LPOLESTR szName,TYPEKIND tkind,ICreateTypeInfo **ppCTInfo) = 0;
    virtual HRESULT WINAPI SetName(LPOLESTR szName) = 0;
    virtual HRESULT WINAPI SetVersion(WORD wMajorVerNum,WORD wMinorVerNum) = 0;
    virtual HRESULT WINAPI SetGuid(REFGUID guid) = 0;
    virtual HRESULT WINAPI SetDocString(LPOLESTR szDoc) = 0;
    virtual HRESULT WINAPI SetHelpFileName(LPOLESTR szHelpFileName) = 0;
    virtual HRESULT WINAPI SetHelpContext(DWORD dwHelpContext) = 0;
    virtual HRESULT WINAPI SetLcid(LCID lcid) = 0;
    virtual HRESULT WINAPI SetLibFlags(UINT uLibFlags) = 0;
    virtual HRESULT WINAPI SaveAllChanges(void) = 0;
  };
#else
  typedef struct ICreateTypeLibVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICreateTypeLib *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICreateTypeLib *This);
      ULONG (WINAPI *Release)(ICreateTypeLib *This);
      HRESULT (WINAPI *CreateTypeInfo)(ICreateTypeLib *This,LPOLESTR szName,TYPEKIND tkind,ICreateTypeInfo **ppCTInfo);
      HRESULT (WINAPI *SetName)(ICreateTypeLib *This,LPOLESTR szName);
      HRESULT (WINAPI *SetVersion)(ICreateTypeLib *This,WORD wMajorVerNum,WORD wMinorVerNum);
      HRESULT (WINAPI *SetGuid)(ICreateTypeLib *This,REFGUID guid);
      HRESULT (WINAPI *SetDocString)(ICreateTypeLib *This,LPOLESTR szDoc);
      HRESULT (WINAPI *SetHelpFileName)(ICreateTypeLib *This,LPOLESTR szHelpFileName);
      HRESULT (WINAPI *SetHelpContext)(ICreateTypeLib *This,DWORD dwHelpContext);
      HRESULT (WINAPI *SetLcid)(ICreateTypeLib *This,LCID lcid);
      HRESULT (WINAPI *SetLibFlags)(ICreateTypeLib *This,UINT uLibFlags);
      HRESULT (WINAPI *SaveAllChanges)(ICreateTypeLib *This);
    END_INTERFACE
  } ICreateTypeLibVtbl;
  struct ICreateTypeLib {
    CONST_VTBL struct ICreateTypeLibVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICreateTypeLib_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICreateTypeLib_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICreateTypeLib_Release(This) (This)->lpVtbl->Release(This)
#define ICreateTypeLib_CreateTypeInfo(This,szName,tkind,ppCTInfo) (This)->lpVtbl->CreateTypeInfo(This,szName,tkind,ppCTInfo)
#define ICreateTypeLib_SetName(This,szName) (This)->lpVtbl->SetName(This,szName)
#define ICreateTypeLib_SetVersion(This,wMajorVerNum,wMinorVerNum) (This)->lpVtbl->SetVersion(This,wMajorVerNum,wMinorVerNum)
#define ICreateTypeLib_SetGuid(This,guid) (This)->lpVtbl->SetGuid(This,guid)
#define ICreateTypeLib_SetDocString(This,szDoc) (This)->lpVtbl->SetDocString(This,szDoc)
#define ICreateTypeLib_SetHelpFileName(This,szHelpFileName) (This)->lpVtbl->SetHelpFileName(This,szHelpFileName)
#define ICreateTypeLib_SetHelpContext(This,dwHelpContext) (This)->lpVtbl->SetHelpContext(This,dwHelpContext)
#define ICreateTypeLib_SetLcid(This,lcid) (This)->lpVtbl->SetLcid(This,lcid)
#define ICreateTypeLib_SetLibFlags(This,uLibFlags) (This)->lpVtbl->SetLibFlags(This,uLibFlags)
#define ICreateTypeLib_SaveAllChanges(This) (This)->lpVtbl->SaveAllChanges(This)
#endif
#endif
  HRESULT WINAPI ICreateTypeLib_CreateTypeInfo_Proxy(ICreateTypeLib *This,LPOLESTR szName,TYPEKIND tkind,ICreateTypeInfo **ppCTInfo);
  void __RPC_STUB ICreateTypeLib_CreateTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetName_Proxy(ICreateTypeLib *This,LPOLESTR szName);
  void __RPC_STUB ICreateTypeLib_SetName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetVersion_Proxy(ICreateTypeLib *This,WORD wMajorVerNum,WORD wMinorVerNum);
  void __RPC_STUB ICreateTypeLib_SetVersion_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetGuid_Proxy(ICreateTypeLib *This,REFGUID guid);
  void __RPC_STUB ICreateTypeLib_SetGuid_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetDocString_Proxy(ICreateTypeLib *This,LPOLESTR szDoc);
  void __RPC_STUB ICreateTypeLib_SetDocString_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetHelpFileName_Proxy(ICreateTypeLib *This,LPOLESTR szHelpFileName);
  void __RPC_STUB ICreateTypeLib_SetHelpFileName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetHelpContext_Proxy(ICreateTypeLib *This,DWORD dwHelpContext);
  void __RPC_STUB ICreateTypeLib_SetHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetLcid_Proxy(ICreateTypeLib *This,LCID lcid);
  void __RPC_STUB ICreateTypeLib_SetLcid_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SetLibFlags_Proxy(ICreateTypeLib *This,UINT uLibFlags);
  void __RPC_STUB ICreateTypeLib_SetLibFlags_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib_SaveAllChanges_Proxy(ICreateTypeLib *This);
  void __RPC_STUB ICreateTypeLib_SaveAllChanges_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICreateTypeLib2_INTERFACE_DEFINED__
#define __ICreateTypeLib2_INTERFACE_DEFINED__
  typedef ICreateTypeLib2 *LPCREATETYPELIB2;

  EXTERN_C const IID IID_ICreateTypeLib2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICreateTypeLib2 : public ICreateTypeLib {
  public:
    virtual HRESULT WINAPI DeleteTypeInfo(LPOLESTR szName) = 0;
    virtual HRESULT WINAPI SetCustData(REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI SetHelpStringContext(ULONG dwHelpStringContext) = 0;
    virtual HRESULT WINAPI SetHelpStringDll(LPOLESTR szFileName) = 0;
  };
#else
  typedef struct ICreateTypeLib2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICreateTypeLib2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICreateTypeLib2 *This);
      ULONG (WINAPI *Release)(ICreateTypeLib2 *This);
      HRESULT (WINAPI *CreateTypeInfo)(ICreateTypeLib2 *This,LPOLESTR szName,TYPEKIND tkind,ICreateTypeInfo **ppCTInfo);
      HRESULT (WINAPI *SetName)(ICreateTypeLib2 *This,LPOLESTR szName);
      HRESULT (WINAPI *SetVersion)(ICreateTypeLib2 *This,WORD wMajorVerNum,WORD wMinorVerNum);
      HRESULT (WINAPI *SetGuid)(ICreateTypeLib2 *This,REFGUID guid);
      HRESULT (WINAPI *SetDocString)(ICreateTypeLib2 *This,LPOLESTR szDoc);
      HRESULT (WINAPI *SetHelpFileName)(ICreateTypeLib2 *This,LPOLESTR szHelpFileName);
      HRESULT (WINAPI *SetHelpContext)(ICreateTypeLib2 *This,DWORD dwHelpContext);
      HRESULT (WINAPI *SetLcid)(ICreateTypeLib2 *This,LCID lcid);
      HRESULT (WINAPI *SetLibFlags)(ICreateTypeLib2 *This,UINT uLibFlags);
      HRESULT (WINAPI *SaveAllChanges)(ICreateTypeLib2 *This);
      HRESULT (WINAPI *DeleteTypeInfo)(ICreateTypeLib2 *This,LPOLESTR szName);
      HRESULT (WINAPI *SetCustData)(ICreateTypeLib2 *This,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *SetHelpStringContext)(ICreateTypeLib2 *This,ULONG dwHelpStringContext);
      HRESULT (WINAPI *SetHelpStringDll)(ICreateTypeLib2 *This,LPOLESTR szFileName);
    END_INTERFACE
  } ICreateTypeLib2Vtbl;
  struct ICreateTypeLib2 {
    CONST_VTBL struct ICreateTypeLib2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICreateTypeLib2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICreateTypeLib2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICreateTypeLib2_Release(This) (This)->lpVtbl->Release(This)
#define ICreateTypeLib2_CreateTypeInfo(This,szName,tkind,ppCTInfo) (This)->lpVtbl->CreateTypeInfo(This,szName,tkind,ppCTInfo)
#define ICreateTypeLib2_SetName(This,szName) (This)->lpVtbl->SetName(This,szName)
#define ICreateTypeLib2_SetVersion(This,wMajorVerNum,wMinorVerNum) (This)->lpVtbl->SetVersion(This,wMajorVerNum,wMinorVerNum)
#define ICreateTypeLib2_SetGuid(This,guid) (This)->lpVtbl->SetGuid(This,guid)
#define ICreateTypeLib2_SetDocString(This,szDoc) (This)->lpVtbl->SetDocString(This,szDoc)
#define ICreateTypeLib2_SetHelpFileName(This,szHelpFileName) (This)->lpVtbl->SetHelpFileName(This,szHelpFileName)
#define ICreateTypeLib2_SetHelpContext(This,dwHelpContext) (This)->lpVtbl->SetHelpContext(This,dwHelpContext)
#define ICreateTypeLib2_SetLcid(This,lcid) (This)->lpVtbl->SetLcid(This,lcid)
#define ICreateTypeLib2_SetLibFlags(This,uLibFlags) (This)->lpVtbl->SetLibFlags(This,uLibFlags)
#define ICreateTypeLib2_SaveAllChanges(This) (This)->lpVtbl->SaveAllChanges(This)
#define ICreateTypeLib2_DeleteTypeInfo(This,szName) (This)->lpVtbl->DeleteTypeInfo(This,szName)
#define ICreateTypeLib2_SetCustData(This,guid,pVarVal) (This)->lpVtbl->SetCustData(This,guid,pVarVal)
#define ICreateTypeLib2_SetHelpStringContext(This,dwHelpStringContext) (This)->lpVtbl->SetHelpStringContext(This,dwHelpStringContext)
#define ICreateTypeLib2_SetHelpStringDll(This,szFileName) (This)->lpVtbl->SetHelpStringDll(This,szFileName)
#endif
#endif
  HRESULT WINAPI ICreateTypeLib2_DeleteTypeInfo_Proxy(ICreateTypeLib2 *This,LPOLESTR szName);
  void __RPC_STUB ICreateTypeLib2_DeleteTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib2_SetCustData_Proxy(ICreateTypeLib2 *This,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ICreateTypeLib2_SetCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib2_SetHelpStringContext_Proxy(ICreateTypeLib2 *This,ULONG dwHelpStringContext);
  void __RPC_STUB ICreateTypeLib2_SetHelpStringContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateTypeLib2_SetHelpStringDll_Proxy(ICreateTypeLib2 *This,LPOLESTR szFileName);
  void __RPC_STUB ICreateTypeLib2_SetHelpStringDll_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IDispatch_INTERFACE_DEFINED__
#define __IDispatch_INTERFACE_DEFINED__
#ifndef DEFINED_LPDISPATCH
#define DEFINED_LPDISPATCH
  typedef IDispatch *LPDISPATCH;
#endif

#define DISPID_UNKNOWN (-1)
#define DISPID_VALUE (0)
#define DISPID_PROPERTYPUT (-3)
#define DISPID_NEWENUM (-4)
#define DISPID_EVALUATE (-5)
#define DISPID_CONSTRUCTOR (-6)
#define DISPID_DESTRUCTOR (-7)
#define DISPID_COLLECT (-8)

  EXTERN_C const IID IID_IDispatch;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IDispatch : public IUnknown {
  public:
    virtual HRESULT WINAPI GetTypeInfoCount(UINT *pctinfo) = 0;
    virtual HRESULT WINAPI GetTypeInfo(UINT iTInfo,LCID lcid,ITypeInfo **ppTInfo) = 0;
    virtual HRESULT WINAPI GetIDsOfNames(REFIID riid,LPOLESTR *rgszNames,UINT cNames,LCID lcid,DISPID *rgDispId) = 0;
    virtual HRESULT WINAPI Invoke(DISPID dispIdMember,REFIID riid,LCID lcid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr) = 0;
  };
#else
  typedef struct IDispatchVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IDispatch *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IDispatch *This);
      ULONG (WINAPI *Release)(IDispatch *This);
      HRESULT (WINAPI *GetTypeInfoCount)(IDispatch *This,UINT *pctinfo);
      HRESULT (WINAPI *GetTypeInfo)(IDispatch *This,UINT iTInfo,LCID lcid,ITypeInfo **ppTInfo);
      HRESULT (WINAPI *GetIDsOfNames)(IDispatch *This,REFIID riid,LPOLESTR *rgszNames,UINT cNames,LCID lcid,DISPID *rgDispId);
      HRESULT (WINAPI *Invoke)(IDispatch *This,DISPID dispIdMember,REFIID riid,LCID lcid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr);
    END_INTERFACE
  } IDispatchVtbl;
  struct IDispatch {
    CONST_VTBL struct IDispatchVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IDispatch_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IDispatch_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IDispatch_Release(This) (This)->lpVtbl->Release(This)
#define IDispatch_GetTypeInfoCount(This,pctinfo) (This)->lpVtbl->GetTypeInfoCount(This,pctinfo)
#define IDispatch_GetTypeInfo(This,iTInfo,lcid,ppTInfo) (This)->lpVtbl->GetTypeInfo(This,iTInfo,lcid,ppTInfo)
#define IDispatch_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) (This)->lpVtbl->GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)
#define IDispatch_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) (This)->lpVtbl->Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)
#endif
#endif
  HRESULT WINAPI IDispatch_GetTypeInfoCount_Proxy(IDispatch *This,UINT *pctinfo);
  void __RPC_STUB IDispatch_GetTypeInfoCount_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDispatch_GetTypeInfo_Proxy(IDispatch *This,UINT iTInfo,LCID lcid,ITypeInfo **ppTInfo);
  void __RPC_STUB IDispatch_GetTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDispatch_GetIDsOfNames_Proxy(IDispatch *This,REFIID riid,LPOLESTR *rgszNames,UINT cNames,LCID lcid,DISPID *rgDispId);
  void __RPC_STUB IDispatch_GetIDsOfNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IDispatch_RemoteInvoke_Proxy(IDispatch *This,DISPID dispIdMember,REFIID riid,LCID lcid,DWORD dwFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *pArgErr,UINT cVarRef,UINT *rgVarRefIdx,VARIANTARG *rgVarRef);
  void __RPC_STUB IDispatch_RemoteInvoke_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IEnumVARIANT_INTERFACE_DEFINED__
#define __IEnumVARIANT_INTERFACE_DEFINED__
  typedef IEnumVARIANT *LPENUMVARIANT;

  EXTERN_C const IID IID_IEnumVARIANT;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IEnumVARIANT : public IUnknown {
  public:
    virtual HRESULT WINAPI Next(ULONG celt,VARIANT *rgVar,ULONG *pCeltFetched) = 0;
    virtual HRESULT WINAPI Skip(ULONG celt) = 0;
    virtual HRESULT WINAPI Reset(void) = 0;
    virtual HRESULT WINAPI Clone(IEnumVARIANT **ppEnum) = 0;
  };
#else
  typedef struct IEnumVARIANTVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IEnumVARIANT *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IEnumVARIANT *This);
      ULONG (WINAPI *Release)(IEnumVARIANT *This);
      HRESULT (WINAPI *Next)(IEnumVARIANT *This,ULONG celt,VARIANT *rgVar,ULONG *pCeltFetched);
      HRESULT (WINAPI *Skip)(IEnumVARIANT *This,ULONG celt);
      HRESULT (WINAPI *Reset)(IEnumVARIANT *This);
      HRESULT (WINAPI *Clone)(IEnumVARIANT *This,IEnumVARIANT **ppEnum);
    END_INTERFACE
  } IEnumVARIANTVtbl;
  struct IEnumVARIANT {
    CONST_VTBL struct IEnumVARIANTVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IEnumVARIANT_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumVARIANT_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IEnumVARIANT_Release(This) (This)->lpVtbl->Release(This)
#define IEnumVARIANT_Next(This,celt,rgVar,pCeltFetched) (This)->lpVtbl->Next(This,celt,rgVar,pCeltFetched)
#define IEnumVARIANT_Skip(This,celt) (This)->lpVtbl->Skip(This,celt)
#define IEnumVARIANT_Reset(This) (This)->lpVtbl->Reset(This)
#define IEnumVARIANT_Clone(This,ppEnum) (This)->lpVtbl->Clone(This,ppEnum)
#endif
#endif
  HRESULT WINAPI IEnumVARIANT_RemoteNext_Proxy(IEnumVARIANT *This,ULONG celt,VARIANT *rgVar,ULONG *pCeltFetched);
  void __RPC_STUB IEnumVARIANT_RemoteNext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumVARIANT_Skip_Proxy(IEnumVARIANT *This,ULONG celt);
  void __RPC_STUB IEnumVARIANT_Skip_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumVARIANT_Reset_Proxy(IEnumVARIANT *This);
  void __RPC_STUB IEnumVARIANT_Reset_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IEnumVARIANT_Clone_Proxy(IEnumVARIANT *This,IEnumVARIANT **ppEnum);
  void __RPC_STUB IEnumVARIANT_Clone_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeComp_INTERFACE_DEFINED__
#define __ITypeComp_INTERFACE_DEFINED__
#ifndef DEFINED_LPTYPECOMP
#define DEFINED_LPTYPECOMP
  typedef ITypeComp *LPTYPECOMP;
#endif

  typedef enum tagDESCKIND {
    DESCKIND_NONE = 0,
    DESCKIND_FUNCDESC,DESCKIND_VARDESC,DESCKIND_TYPECOMP,DESCKIND_IMPLICITAPPOBJ,
    DESCKIND_MAX
  } DESCKIND;

  typedef union tagBINDPTR {
    FUNCDESC *lpfuncdesc;
    VARDESC *lpvardesc;
    ITypeComp *lptcomp;
  } BINDPTR;

  typedef union tagBINDPTR *LPBINDPTR;

  EXTERN_C const IID IID_ITypeComp;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeComp : public IUnknown {
  public:
    virtual HRESULT WINAPI Bind(LPOLESTR szName,ULONG lHashVal,WORD wFlags,ITypeInfo **ppTInfo,DESCKIND *pDescKind,BINDPTR *pBindPtr) = 0;
    virtual HRESULT WINAPI BindType(LPOLESTR szName,ULONG lHashVal,ITypeInfo **ppTInfo,ITypeComp **ppTComp) = 0;
  };
#else
  typedef struct ITypeCompVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeComp *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeComp *This);
      ULONG (WINAPI *Release)(ITypeComp *This);
      HRESULT (WINAPI *Bind)(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,WORD wFlags,ITypeInfo **ppTInfo,DESCKIND *pDescKind,BINDPTR *pBindPtr);
      HRESULT (WINAPI *BindType)(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,ITypeInfo **ppTInfo,ITypeComp **ppTComp);
    END_INTERFACE
  } ITypeCompVtbl;
  struct ITypeComp {
    CONST_VTBL struct ITypeCompVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeComp_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeComp_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeComp_Release(This) (This)->lpVtbl->Release(This)
#define ITypeComp_Bind(This,szName,lHashVal,wFlags,ppTInfo,pDescKind,pBindPtr) (This)->lpVtbl->Bind(This,szName,lHashVal,wFlags,ppTInfo,pDescKind,pBindPtr)
#define ITypeComp_BindType(This,szName,lHashVal,ppTInfo,ppTComp) (This)->lpVtbl->BindType(This,szName,lHashVal,ppTInfo,ppTComp)
#endif
#endif
  HRESULT WINAPI ITypeComp_RemoteBind_Proxy(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,WORD wFlags,ITypeInfo **ppTInfo,DESCKIND *pDescKind,LPFUNCDESC *ppFuncDesc,LPVARDESC *ppVarDesc,ITypeComp **ppTypeComp,CLEANLOCALSTORAGE *pDummy);
  void __RPC_STUB ITypeComp_RemoteBind_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeComp_RemoteBindType_Proxy(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,ITypeInfo **ppTInfo);
  void __RPC_STUB ITypeComp_RemoteBindType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeInfo_INTERFACE_DEFINED__
#define __ITypeInfo_INTERFACE_DEFINED__
#ifndef DEFINDE_LPTYPEINFO
#define DEFINDE_LPTYPEINFO
  typedef ITypeInfo *LPTYPEINFO;
#endif

  EXTERN_C const IID IID_ITypeInfo;

#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetTypeAttr(TYPEATTR **ppTypeAttr) = 0;
    virtual HRESULT WINAPI GetTypeComp(ITypeComp **ppTComp) = 0;
    virtual HRESULT WINAPI GetFuncDesc(UINT index,FUNCDESC **ppFuncDesc) = 0;
    virtual HRESULT WINAPI GetVarDesc(UINT index,VARDESC **ppVarDesc) = 0;
    virtual HRESULT WINAPI GetNames(MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames) = 0;
    virtual HRESULT WINAPI GetRefTypeOfImplType(UINT index,HREFTYPE *pRefType) = 0;
    virtual HRESULT WINAPI GetImplTypeFlags(UINT index,INT *pImplTypeFlags) = 0;
    virtual HRESULT WINAPI GetIDsOfNames(LPOLESTR *rgszNames,UINT cNames,MEMBERID *pMemId) = 0;
    virtual HRESULT WINAPI Invoke(PVOID pvInstance,MEMBERID memid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr) = 0;
    virtual HRESULT WINAPI GetDocumentation(MEMBERID memid,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile) = 0;
    virtual HRESULT WINAPI GetDllEntry(MEMBERID memid,INVOKEKIND invKind,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal) = 0;
    virtual HRESULT WINAPI GetRefTypeInfo(HREFTYPE hRefType,ITypeInfo **ppTInfo) = 0;
    virtual HRESULT WINAPI AddressOfMember(MEMBERID memid,INVOKEKIND invKind,PVOID *ppv) = 0;
    virtual HRESULT WINAPI CreateInstance(IUnknown *pUnkOuter,REFIID riid,PVOID *ppvObj) = 0;
    virtual HRESULT WINAPI GetMops(MEMBERID memid,BSTR *pBstrMops) = 0;
    virtual HRESULT WINAPI GetContainingTypeLib(ITypeLib **ppTLib,UINT *pIndex) = 0;
    virtual void WINAPI ReleaseTypeAttr(TYPEATTR *pTypeAttr) = 0;
    virtual void WINAPI ReleaseFuncDesc(FUNCDESC *pFuncDesc) = 0;
    virtual void WINAPI ReleaseVarDesc(VARDESC *pVarDesc) = 0;
  };
#else
  typedef struct ITypeInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeInfo *This);
      ULONG (WINAPI *Release)(ITypeInfo *This);
      HRESULT (WINAPI *GetTypeAttr)(ITypeInfo *This,TYPEATTR **ppTypeAttr);
      HRESULT (WINAPI *GetTypeComp)(ITypeInfo *This,ITypeComp **ppTComp);
      HRESULT (WINAPI *GetFuncDesc)(ITypeInfo *This,UINT index,FUNCDESC **ppFuncDesc);
      HRESULT (WINAPI *GetVarDesc)(ITypeInfo *This,UINT index,VARDESC **ppVarDesc);
      HRESULT (WINAPI *GetNames)(ITypeInfo *This,MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames);
      HRESULT (WINAPI *GetRefTypeOfImplType)(ITypeInfo *This,UINT index,HREFTYPE *pRefType);
      HRESULT (WINAPI *GetImplTypeFlags)(ITypeInfo *This,UINT index,INT *pImplTypeFlags);
      HRESULT (WINAPI *GetIDsOfNames)(ITypeInfo *This,LPOLESTR *rgszNames,UINT cNames,MEMBERID *pMemId);
      HRESULT (WINAPI *Invoke)(ITypeInfo *This,PVOID pvInstance,MEMBERID memid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr);
      HRESULT (WINAPI *GetDocumentation)(ITypeInfo *This,MEMBERID memid,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
      HRESULT (WINAPI *GetDllEntry)(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal);
      HRESULT (WINAPI *GetRefTypeInfo)(ITypeInfo *This,HREFTYPE hRefType,ITypeInfo **ppTInfo);
      HRESULT (WINAPI *AddressOfMember)(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,PVOID *ppv);
      HRESULT (WINAPI *CreateInstance)(ITypeInfo *This,IUnknown *pUnkOuter,REFIID riid,PVOID *ppvObj);
      HRESULT (WINAPI *GetMops)(ITypeInfo *This,MEMBERID memid,BSTR *pBstrMops);
      HRESULT (WINAPI *GetContainingTypeLib)(ITypeInfo *This,ITypeLib **ppTLib,UINT *pIndex);
      void (WINAPI *ReleaseTypeAttr)(ITypeInfo *This,TYPEATTR *pTypeAttr);
      void (WINAPI *ReleaseFuncDesc)(ITypeInfo *This,FUNCDESC *pFuncDesc);
      void (WINAPI *ReleaseVarDesc)(ITypeInfo *This,VARDESC *pVarDesc);
    END_INTERFACE
  } ITypeInfoVtbl;
  struct ITypeInfo {
    CONST_VTBL struct ITypeInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeInfo_Release(This) (This)->lpVtbl->Release(This)
#define ITypeInfo_GetTypeAttr(This,ppTypeAttr) (This)->lpVtbl->GetTypeAttr(This,ppTypeAttr)
#define ITypeInfo_GetTypeComp(This,ppTComp) (This)->lpVtbl->GetTypeComp(This,ppTComp)
#define ITypeInfo_GetFuncDesc(This,index,ppFuncDesc) (This)->lpVtbl->GetFuncDesc(This,index,ppFuncDesc)
#define ITypeInfo_GetVarDesc(This,index,ppVarDesc) (This)->lpVtbl->GetVarDesc(This,index,ppVarDesc)
#define ITypeInfo_GetNames(This,memid,rgBstrNames,cMaxNames,pcNames) (This)->lpVtbl->GetNames(This,memid,rgBstrNames,cMaxNames,pcNames)
#define ITypeInfo_GetRefTypeOfImplType(This,index,pRefType) (This)->lpVtbl->GetRefTypeOfImplType(This,index,pRefType)
#define ITypeInfo_GetImplTypeFlags(This,index,pImplTypeFlags) (This)->lpVtbl->GetImplTypeFlags(This,index,pImplTypeFlags)
#define ITypeInfo_GetIDsOfNames(This,rgszNames,cNames,pMemId) (This)->lpVtbl->GetIDsOfNames(This,rgszNames,cNames,pMemId)
#define ITypeInfo_Invoke(This,pvInstance,memid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) (This)->lpVtbl->Invoke(This,pvInstance,memid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)
#define ITypeInfo_GetDocumentation(This,memid,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile) (This)->lpVtbl->GetDocumentation(This,memid,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile)
#define ITypeInfo_GetDllEntry(This,memid,invKind,pBstrDllName,pBstrName,pwOrdinal) (This)->lpVtbl->GetDllEntry(This,memid,invKind,pBstrDllName,pBstrName,pwOrdinal)
#define ITypeInfo_GetRefTypeInfo(This,hRefType,ppTInfo) (This)->lpVtbl->GetRefTypeInfo(This,hRefType,ppTInfo)
#define ITypeInfo_AddressOfMember(This,memid,invKind,ppv) (This)->lpVtbl->AddressOfMember(This,memid,invKind,ppv)
#define ITypeInfo_CreateInstance(This,pUnkOuter,riid,ppvObj) (This)->lpVtbl->CreateInstance(This,pUnkOuter,riid,ppvObj)
#define ITypeInfo_GetMops(This,memid,pBstrMops) (This)->lpVtbl->GetMops(This,memid,pBstrMops)
#define ITypeInfo_GetContainingTypeLib(This,ppTLib,pIndex) (This)->lpVtbl->GetContainingTypeLib(This,ppTLib,pIndex)
#define ITypeInfo_ReleaseTypeAttr(This,pTypeAttr) (This)->lpVtbl->ReleaseTypeAttr(This,pTypeAttr)
#define ITypeInfo_ReleaseFuncDesc(This,pFuncDesc) (This)->lpVtbl->ReleaseFuncDesc(This,pFuncDesc)
#define ITypeInfo_ReleaseVarDesc(This,pVarDesc) (This)->lpVtbl->ReleaseVarDesc(This,pVarDesc)
#endif
#endif
  HRESULT WINAPI ITypeInfo_RemoteGetTypeAttr_Proxy(ITypeInfo *This,LPTYPEATTR *ppTypeAttr,CLEANLOCALSTORAGE *pDummy);
  void __RPC_STUB ITypeInfo_RemoteGetTypeAttr_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_GetTypeComp_Proxy(ITypeInfo *This,ITypeComp **ppTComp);
  void __RPC_STUB ITypeInfo_GetTypeComp_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetFuncDesc_Proxy(ITypeInfo *This,UINT index,LPFUNCDESC *ppFuncDesc,CLEANLOCALSTORAGE *pDummy);
  void __RPC_STUB ITypeInfo_RemoteGetFuncDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetVarDesc_Proxy(ITypeInfo *This,UINT index,LPVARDESC *ppVarDesc,CLEANLOCALSTORAGE *pDummy);
  void __RPC_STUB ITypeInfo_RemoteGetVarDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetNames_Proxy(ITypeInfo *This,MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames);
  void __RPC_STUB ITypeInfo_RemoteGetNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_GetRefTypeOfImplType_Proxy(ITypeInfo *This,UINT index,HREFTYPE *pRefType);
  void __RPC_STUB ITypeInfo_GetRefTypeOfImplType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_GetImplTypeFlags_Proxy(ITypeInfo *This,UINT index,INT *pImplTypeFlags);
  void __RPC_STUB ITypeInfo_GetImplTypeFlags_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalGetIDsOfNames_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalGetIDsOfNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalInvoke_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalInvoke_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetDocumentation_Proxy(ITypeInfo *This,MEMBERID memid,DWORD refPtrFlags,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  void __RPC_STUB ITypeInfo_RemoteGetDocumentation_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetDllEntry_Proxy(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,DWORD refPtrFlags,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal);
  void __RPC_STUB ITypeInfo_RemoteGetDllEntry_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_GetRefTypeInfo_Proxy(ITypeInfo *This,HREFTYPE hRefType,ITypeInfo **ppTInfo);
  void __RPC_STUB ITypeInfo_GetRefTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalAddressOfMember_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalAddressOfMember_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteCreateInstance_Proxy(ITypeInfo *This,REFIID riid,IUnknown **ppvObj);
  void __RPC_STUB ITypeInfo_RemoteCreateInstance_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_GetMops_Proxy(ITypeInfo *This,MEMBERID memid,BSTR *pBstrMops);
  void __RPC_STUB ITypeInfo_GetMops_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_RemoteGetContainingTypeLib_Proxy(ITypeInfo *This,ITypeLib **ppTLib,UINT *pIndex);
  void __RPC_STUB ITypeInfo_RemoteGetContainingTypeLib_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalReleaseTypeAttr_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalReleaseTypeAttr_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalReleaseFuncDesc_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalReleaseFuncDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo_LocalReleaseVarDesc_Proxy(ITypeInfo *This);
  void __RPC_STUB ITypeInfo_LocalReleaseVarDesc_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeInfo2_INTERFACE_DEFINED__
#define __ITypeInfo2_INTERFACE_DEFINED__
  typedef ITypeInfo2 *LPTYPEINFO2;

  EXTERN_C const IID IID_ITypeInfo2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeInfo2 : public ITypeInfo {
  public:
    virtual HRESULT WINAPI GetTypeKind(TYPEKIND *pTypeKind) = 0;
    virtual HRESULT WINAPI GetTypeFlags(ULONG *pTypeFlags) = 0;
    virtual HRESULT WINAPI GetFuncIndexOfMemId(MEMBERID memid,INVOKEKIND invKind,UINT *pFuncIndex) = 0;
    virtual HRESULT WINAPI GetVarIndexOfMemId(MEMBERID memid,UINT *pVarIndex) = 0;
    virtual HRESULT WINAPI GetCustData(REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetFuncCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetParamCustData(UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetVarCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetImplTypeCustData(UINT index,REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetDocumentation2(MEMBERID memid,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll) = 0;
    virtual HRESULT WINAPI GetAllCustData(CUSTDATA *pCustData) = 0;
    virtual HRESULT WINAPI GetAllFuncCustData(UINT index,CUSTDATA *pCustData) = 0;
    virtual HRESULT WINAPI GetAllParamCustData(UINT indexFunc,UINT indexParam,CUSTDATA *pCustData) = 0;
    virtual HRESULT WINAPI GetAllVarCustData(UINT index,CUSTDATA *pCustData) = 0;
    virtual HRESULT WINAPI GetAllImplTypeCustData(UINT index,CUSTDATA *pCustData) = 0;
  };
#else
  typedef struct ITypeInfo2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeInfo2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeInfo2 *This);
      ULONG (WINAPI *Release)(ITypeInfo2 *This);
      HRESULT (WINAPI *GetTypeAttr)(ITypeInfo2 *This,TYPEATTR **ppTypeAttr);
      HRESULT (WINAPI *GetTypeComp)(ITypeInfo2 *This,ITypeComp **ppTComp);
      HRESULT (WINAPI *GetFuncDesc)(ITypeInfo2 *This,UINT index,FUNCDESC **ppFuncDesc);
      HRESULT (WINAPI *GetVarDesc)(ITypeInfo2 *This,UINT index,VARDESC **ppVarDesc);
      HRESULT (WINAPI *GetNames)(ITypeInfo2 *This,MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames);
      HRESULT (WINAPI *GetRefTypeOfImplType)(ITypeInfo2 *This,UINT index,HREFTYPE *pRefType);
      HRESULT (WINAPI *GetImplTypeFlags)(ITypeInfo2 *This,UINT index,INT *pImplTypeFlags);
      HRESULT (WINAPI *GetIDsOfNames)(ITypeInfo2 *This,LPOLESTR *rgszNames,UINT cNames,MEMBERID *pMemId);
      HRESULT (WINAPI *Invoke)(ITypeInfo2 *This,PVOID pvInstance,MEMBERID memid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr);
      HRESULT (WINAPI *GetDocumentation)(ITypeInfo2 *This,MEMBERID memid,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
      HRESULT (WINAPI *GetDllEntry)(ITypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal);
      HRESULT (WINAPI *GetRefTypeInfo)(ITypeInfo2 *This,HREFTYPE hRefType,ITypeInfo **ppTInfo);
      HRESULT (WINAPI *AddressOfMember)(ITypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind,PVOID *ppv);
      HRESULT (WINAPI *CreateInstance)(ITypeInfo2 *This,IUnknown *pUnkOuter,REFIID riid,PVOID *ppvObj);
      HRESULT (WINAPI *GetMops)(ITypeInfo2 *This,MEMBERID memid,BSTR *pBstrMops);
      HRESULT (WINAPI *GetContainingTypeLib)(ITypeInfo2 *This,ITypeLib **ppTLib,UINT *pIndex);
      void (WINAPI *ReleaseTypeAttr)(ITypeInfo2 *This,TYPEATTR *pTypeAttr);
      void (WINAPI *ReleaseFuncDesc)(ITypeInfo2 *This,FUNCDESC *pFuncDesc);
      void (WINAPI *ReleaseVarDesc)(ITypeInfo2 *This,VARDESC *pVarDesc);
      HRESULT (WINAPI *GetTypeKind)(ITypeInfo2 *This,TYPEKIND *pTypeKind);
      HRESULT (WINAPI *GetTypeFlags)(ITypeInfo2 *This,ULONG *pTypeFlags);
      HRESULT (WINAPI *GetFuncIndexOfMemId)(ITypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind,UINT *pFuncIndex);
      HRESULT (WINAPI *GetVarIndexOfMemId)(ITypeInfo2 *This,MEMBERID memid,UINT *pVarIndex);
      HRESULT (WINAPI *GetCustData)(ITypeInfo2 *This,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetFuncCustData)(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetParamCustData)(ITypeInfo2 *This,UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetVarCustData)(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetImplTypeCustData)(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetDocumentation2)(ITypeInfo2 *This,MEMBERID memid,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
      HRESULT (WINAPI *GetAllCustData)(ITypeInfo2 *This,CUSTDATA *pCustData);
      HRESULT (WINAPI *GetAllFuncCustData)(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
      HRESULT (WINAPI *GetAllParamCustData)(ITypeInfo2 *This,UINT indexFunc,UINT indexParam,CUSTDATA *pCustData);
      HRESULT (WINAPI *GetAllVarCustData)(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
      HRESULT (WINAPI *GetAllImplTypeCustData)(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
    END_INTERFACE
  } ITypeInfo2Vtbl;
  struct ITypeInfo2 {
    CONST_VTBL struct ITypeInfo2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeInfo2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeInfo2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeInfo2_Release(This) (This)->lpVtbl->Release(This)
#define ITypeInfo2_GetTypeAttr(This,ppTypeAttr) (This)->lpVtbl->GetTypeAttr(This,ppTypeAttr)
#define ITypeInfo2_GetTypeComp(This,ppTComp) (This)->lpVtbl->GetTypeComp(This,ppTComp)
#define ITypeInfo2_GetFuncDesc(This,index,ppFuncDesc) (This)->lpVtbl->GetFuncDesc(This,index,ppFuncDesc)
#define ITypeInfo2_GetVarDesc(This,index,ppVarDesc) (This)->lpVtbl->GetVarDesc(This,index,ppVarDesc)
#define ITypeInfo2_GetNames(This,memid,rgBstrNames,cMaxNames,pcNames) (This)->lpVtbl->GetNames(This,memid,rgBstrNames,cMaxNames,pcNames)
#define ITypeInfo2_GetRefTypeOfImplType(This,index,pRefType) (This)->lpVtbl->GetRefTypeOfImplType(This,index,pRefType)
#define ITypeInfo2_GetImplTypeFlags(This,index,pImplTypeFlags) (This)->lpVtbl->GetImplTypeFlags(This,index,pImplTypeFlags)
#define ITypeInfo2_GetIDsOfNames(This,rgszNames,cNames,pMemId) (This)->lpVtbl->GetIDsOfNames(This,rgszNames,cNames,pMemId)
#define ITypeInfo2_Invoke(This,pvInstance,memid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) (This)->lpVtbl->Invoke(This,pvInstance,memid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)
#define ITypeInfo2_GetDocumentation(This,memid,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile) (This)->lpVtbl->GetDocumentation(This,memid,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile)
#define ITypeInfo2_GetDllEntry(This,memid,invKind,pBstrDllName,pBstrName,pwOrdinal) (This)->lpVtbl->GetDllEntry(This,memid,invKind,pBstrDllName,pBstrName,pwOrdinal)
#define ITypeInfo2_GetRefTypeInfo(This,hRefType,ppTInfo) (This)->lpVtbl->GetRefTypeInfo(This,hRefType,ppTInfo)
#define ITypeInfo2_AddressOfMember(This,memid,invKind,ppv) (This)->lpVtbl->AddressOfMember(This,memid,invKind,ppv)
#define ITypeInfo2_CreateInstance(This,pUnkOuter,riid,ppvObj) (This)->lpVtbl->CreateInstance(This,pUnkOuter,riid,ppvObj)
#define ITypeInfo2_GetMops(This,memid,pBstrMops) (This)->lpVtbl->GetMops(This,memid,pBstrMops)
#define ITypeInfo2_GetContainingTypeLib(This,ppTLib,pIndex) (This)->lpVtbl->GetContainingTypeLib(This,ppTLib,pIndex)
#define ITypeInfo2_ReleaseTypeAttr(This,pTypeAttr) (This)->lpVtbl->ReleaseTypeAttr(This,pTypeAttr)
#define ITypeInfo2_ReleaseFuncDesc(This,pFuncDesc) (This)->lpVtbl->ReleaseFuncDesc(This,pFuncDesc)
#define ITypeInfo2_ReleaseVarDesc(This,pVarDesc) (This)->lpVtbl->ReleaseVarDesc(This,pVarDesc)
#define ITypeInfo2_GetTypeKind(This,pTypeKind) (This)->lpVtbl->GetTypeKind(This,pTypeKind)
#define ITypeInfo2_GetTypeFlags(This,pTypeFlags) (This)->lpVtbl->GetTypeFlags(This,pTypeFlags)
#define ITypeInfo2_GetFuncIndexOfMemId(This,memid,invKind,pFuncIndex) (This)->lpVtbl->GetFuncIndexOfMemId(This,memid,invKind,pFuncIndex)
#define ITypeInfo2_GetVarIndexOfMemId(This,memid,pVarIndex) (This)->lpVtbl->GetVarIndexOfMemId(This,memid,pVarIndex)
#define ITypeInfo2_GetCustData(This,guid,pVarVal) (This)->lpVtbl->GetCustData(This,guid,pVarVal)
#define ITypeInfo2_GetFuncCustData(This,index,guid,pVarVal) (This)->lpVtbl->GetFuncCustData(This,index,guid,pVarVal)
#define ITypeInfo2_GetParamCustData(This,indexFunc,indexParam,guid,pVarVal) (This)->lpVtbl->GetParamCustData(This,indexFunc,indexParam,guid,pVarVal)
#define ITypeInfo2_GetVarCustData(This,index,guid,pVarVal) (This)->lpVtbl->GetVarCustData(This,index,guid,pVarVal)
#define ITypeInfo2_GetImplTypeCustData(This,index,guid,pVarVal) (This)->lpVtbl->GetImplTypeCustData(This,index,guid,pVarVal)
#define ITypeInfo2_GetDocumentation2(This,memid,lcid,pbstrHelpString,pdwHelpStringContext,pbstrHelpStringDll) (This)->lpVtbl->GetDocumentation2(This,memid,lcid,pbstrHelpString,pdwHelpStringContext,pbstrHelpStringDll)
#define ITypeInfo2_GetAllCustData(This,pCustData) (This)->lpVtbl->GetAllCustData(This,pCustData)
#define ITypeInfo2_GetAllFuncCustData(This,index,pCustData) (This)->lpVtbl->GetAllFuncCustData(This,index,pCustData)
#define ITypeInfo2_GetAllParamCustData(This,indexFunc,indexParam,pCustData) (This)->lpVtbl->GetAllParamCustData(This,indexFunc,indexParam,pCustData)
#define ITypeInfo2_GetAllVarCustData(This,index,pCustData) (This)->lpVtbl->GetAllVarCustData(This,index,pCustData)
#define ITypeInfo2_GetAllImplTypeCustData(This,index,pCustData) (This)->lpVtbl->GetAllImplTypeCustData(This,index,pCustData)
#endif
#endif
  HRESULT WINAPI ITypeInfo2_GetTypeKind_Proxy(ITypeInfo2 *This,TYPEKIND *pTypeKind);
  void __RPC_STUB ITypeInfo2_GetTypeKind_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetTypeFlags_Proxy(ITypeInfo2 *This,ULONG *pTypeFlags);
  void __RPC_STUB ITypeInfo2_GetTypeFlags_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetFuncIndexOfMemId_Proxy(ITypeInfo2 *This,MEMBERID memid,INVOKEKIND invKind,UINT *pFuncIndex);
  void __RPC_STUB ITypeInfo2_GetFuncIndexOfMemId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetVarIndexOfMemId_Proxy(ITypeInfo2 *This,MEMBERID memid,UINT *pVarIndex);
  void __RPC_STUB ITypeInfo2_GetVarIndexOfMemId_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetCustData_Proxy(ITypeInfo2 *This,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeInfo2_GetCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetFuncCustData_Proxy(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeInfo2_GetFuncCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetParamCustData_Proxy(ITypeInfo2 *This,UINT indexFunc,UINT indexParam,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeInfo2_GetParamCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetVarCustData_Proxy(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeInfo2_GetVarCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetImplTypeCustData_Proxy(ITypeInfo2 *This,UINT index,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeInfo2_GetImplTypeCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_RemoteGetDocumentation2_Proxy(ITypeInfo2 *This,MEMBERID memid,LCID lcid,DWORD refPtrFlags,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  void __RPC_STUB ITypeInfo2_RemoteGetDocumentation2_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetAllCustData_Proxy(ITypeInfo2 *This,CUSTDATA *pCustData);
  void __RPC_STUB ITypeInfo2_GetAllCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetAllFuncCustData_Proxy(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
  void __RPC_STUB ITypeInfo2_GetAllFuncCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetAllParamCustData_Proxy(ITypeInfo2 *This,UINT indexFunc,UINT indexParam,CUSTDATA *pCustData);
  void __RPC_STUB ITypeInfo2_GetAllParamCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetAllVarCustData_Proxy(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
  void __RPC_STUB ITypeInfo2_GetAllVarCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeInfo2_GetAllImplTypeCustData_Proxy(ITypeInfo2 *This,UINT index,CUSTDATA *pCustData);
  void __RPC_STUB ITypeInfo2_GetAllImplTypeCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeLib_INTERFACE_DEFINED__
#define __ITypeLib_INTERFACE_DEFINED__
  typedef enum tagSYSKIND {
    SYS_WIN16 = 0,
    SYS_WIN32,SYS_MAC,SYS_WIN64
  } SYSKIND;

  typedef enum tagLIBFLAGS {
    LIBFLAG_FRESTRICTED = 0x1,LIBFLAG_FCONTROL = 0x2,LIBFLAG_FHIDDEN = 0x4,LIBFLAG_FHASDISKIMAGE = 0x8
  } LIBFLAGS;

#ifndef DEFINED_LPTYPELIB
#define DEFINED_LPTYPELIB
  typedef ITypeLib *LPTYPELIB;
#endif

  typedef struct tagTLIBATTR {
    GUID guid;
    LCID lcid;
    SYSKIND syskind;
    WORD wMajorVerNum;
    WORD wMinorVerNum;
    WORD wLibFlags;
  } TLIBATTR;

  typedef struct tagTLIBATTR *LPTLIBATTR;

  EXTERN_C const IID IID_ITypeLib;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeLib : public IUnknown {
  public:
    virtual UINT WINAPI GetTypeInfoCount(void) = 0;
    virtual HRESULT WINAPI GetTypeInfo(UINT index,ITypeInfo **ppTInfo) = 0;
    virtual HRESULT WINAPI GetTypeInfoType(UINT index,TYPEKIND *pTKind) = 0;
    virtual HRESULT WINAPI GetTypeInfoOfGuid(REFGUID guid,ITypeInfo **ppTinfo) = 0;
    virtual HRESULT WINAPI GetLibAttr(TLIBATTR **ppTLibAttr) = 0;
    virtual HRESULT WINAPI GetTypeComp(ITypeComp **ppTComp) = 0;
    virtual HRESULT WINAPI GetDocumentation(INT index,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile) = 0;
    virtual HRESULT WINAPI IsName(LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName) = 0;
    virtual HRESULT WINAPI FindName(LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound) = 0;
    virtual void WINAPI ReleaseTLibAttr(TLIBATTR *pTLibAttr) = 0;
  };
#else
  typedef struct ITypeLibVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeLib *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeLib *This);
      ULONG (WINAPI *Release)(ITypeLib *This);
      UINT (WINAPI *GetTypeInfoCount)(ITypeLib *This);
      HRESULT (WINAPI *GetTypeInfo)(ITypeLib *This,UINT index,ITypeInfo **ppTInfo);
      HRESULT (WINAPI *GetTypeInfoType)(ITypeLib *This,UINT index,TYPEKIND *pTKind);
      HRESULT (WINAPI *GetTypeInfoOfGuid)(ITypeLib *This,REFGUID guid,ITypeInfo **ppTinfo);
      HRESULT (WINAPI *GetLibAttr)(ITypeLib *This,TLIBATTR **ppTLibAttr);
      HRESULT (WINAPI *GetTypeComp)(ITypeLib *This,ITypeComp **ppTComp);
      HRESULT (WINAPI *GetDocumentation)(ITypeLib *This,INT index,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
      HRESULT (WINAPI *IsName)(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName);
      HRESULT (WINAPI *FindName)(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound);
      void (WINAPI *ReleaseTLibAttr)(ITypeLib *This,TLIBATTR *pTLibAttr);
    END_INTERFACE
  } ITypeLibVtbl;
  struct ITypeLib {
    CONST_VTBL struct ITypeLibVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeLib_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeLib_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeLib_Release(This) (This)->lpVtbl->Release(This)
#define ITypeLib_GetTypeInfoCount(This) (This)->lpVtbl->GetTypeInfoCount(This)
#define ITypeLib_GetTypeInfo(This,index,ppTInfo) (This)->lpVtbl->GetTypeInfo(This,index,ppTInfo)
#define ITypeLib_GetTypeInfoType(This,index,pTKind) (This)->lpVtbl->GetTypeInfoType(This,index,pTKind)
#define ITypeLib_GetTypeInfoOfGuid(This,guid,ppTinfo) (This)->lpVtbl->GetTypeInfoOfGuid(This,guid,ppTinfo)
#define ITypeLib_GetLibAttr(This,ppTLibAttr) (This)->lpVtbl->GetLibAttr(This,ppTLibAttr)
#define ITypeLib_GetTypeComp(This,ppTComp) (This)->lpVtbl->GetTypeComp(This,ppTComp)
#define ITypeLib_GetDocumentation(This,index,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile) (This)->lpVtbl->GetDocumentation(This,index,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile)
#define ITypeLib_IsName(This,szNameBuf,lHashVal,pfName) (This)->lpVtbl->IsName(This,szNameBuf,lHashVal,pfName)
#define ITypeLib_FindName(This,szNameBuf,lHashVal,ppTInfo,rgMemId,pcFound) (This)->lpVtbl->FindName(This,szNameBuf,lHashVal,ppTInfo,rgMemId,pcFound)
#define ITypeLib_ReleaseTLibAttr(This,pTLibAttr) (This)->lpVtbl->ReleaseTLibAttr(This,pTLibAttr)
#endif
#endif
  HRESULT WINAPI ITypeLib_RemoteGetTypeInfoCount_Proxy(ITypeLib *This,UINT *pcTInfo);
  void __RPC_STUB ITypeLib_RemoteGetTypeInfoCount_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_GetTypeInfo_Proxy(ITypeLib *This,UINT index,ITypeInfo **ppTInfo);
  void __RPC_STUB ITypeLib_GetTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_GetTypeInfoType_Proxy(ITypeLib *This,UINT index,TYPEKIND *pTKind);
  void __RPC_STUB ITypeLib_GetTypeInfoType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_GetTypeInfoOfGuid_Proxy(ITypeLib *This,REFGUID guid,ITypeInfo **ppTinfo);
  void __RPC_STUB ITypeLib_GetTypeInfoOfGuid_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_RemoteGetLibAttr_Proxy(ITypeLib *This,LPTLIBATTR *ppTLibAttr,CLEANLOCALSTORAGE *pDummy);
  void __RPC_STUB ITypeLib_RemoteGetLibAttr_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_GetTypeComp_Proxy(ITypeLib *This,ITypeComp **ppTComp);
  void __RPC_STUB ITypeLib_GetTypeComp_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_RemoteGetDocumentation_Proxy(ITypeLib *This,INT index,DWORD refPtrFlags,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  void __RPC_STUB ITypeLib_RemoteGetDocumentation_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_RemoteIsName_Proxy(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName,BSTR *pBstrLibName);
  void __RPC_STUB ITypeLib_RemoteIsName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_RemoteFindName_Proxy(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound,BSTR *pBstrLibName);
  void __RPC_STUB ITypeLib_RemoteFindName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib_LocalReleaseTLibAttr_Proxy(ITypeLib *This);
  void __RPC_STUB ITypeLib_LocalReleaseTLibAttr_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeLib2_INTERFACE_DEFINED__
#define __ITypeLib2_INTERFACE_DEFINED__
  typedef ITypeLib2 *LPTYPELIB2;

  EXTERN_C const IID IID_ITypeLib2;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeLib2 : public ITypeLib {
  public:
    virtual HRESULT WINAPI GetCustData(REFGUID guid,VARIANT *pVarVal) = 0;
    virtual HRESULT WINAPI GetLibStatistics(ULONG *pcUniqueNames,ULONG *pcchUniqueNames) = 0;
    virtual HRESULT WINAPI GetDocumentation2(INT index,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll) = 0;
    virtual HRESULT WINAPI GetAllCustData(CUSTDATA *pCustData) = 0;
  };
#else
  typedef struct ITypeLib2Vtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeLib2 *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeLib2 *This);
      ULONG (WINAPI *Release)(ITypeLib2 *This);
      UINT (WINAPI *GetTypeInfoCount)(ITypeLib2 *This);
      HRESULT (WINAPI *GetTypeInfo)(ITypeLib2 *This,UINT index,ITypeInfo **ppTInfo);
      HRESULT (WINAPI *GetTypeInfoType)(ITypeLib2 *This,UINT index,TYPEKIND *pTKind);
      HRESULT (WINAPI *GetTypeInfoOfGuid)(ITypeLib2 *This,REFGUID guid,ITypeInfo **ppTinfo);
      HRESULT (WINAPI *GetLibAttr)(ITypeLib2 *This,TLIBATTR **ppTLibAttr);
      HRESULT (WINAPI *GetTypeComp)(ITypeLib2 *This,ITypeComp **ppTComp);
      HRESULT (WINAPI *GetDocumentation)(ITypeLib2 *This,INT index,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
      HRESULT (WINAPI *IsName)(ITypeLib2 *This,LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName);
      HRESULT (WINAPI *FindName)(ITypeLib2 *This,LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound);
      void (WINAPI *ReleaseTLibAttr)(ITypeLib2 *This,TLIBATTR *pTLibAttr);
      HRESULT (WINAPI *GetCustData)(ITypeLib2 *This,REFGUID guid,VARIANT *pVarVal);
      HRESULT (WINAPI *GetLibStatistics)(ITypeLib2 *This,ULONG *pcUniqueNames,ULONG *pcchUniqueNames);
      HRESULT (WINAPI *GetDocumentation2)(ITypeLib2 *This,INT index,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
      HRESULT (WINAPI *GetAllCustData)(ITypeLib2 *This,CUSTDATA *pCustData);
    END_INTERFACE
  } ITypeLib2Vtbl;
  struct ITypeLib2 {
    CONST_VTBL struct ITypeLib2Vtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeLib2_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeLib2_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeLib2_Release(This) (This)->lpVtbl->Release(This)
#define ITypeLib2_GetTypeInfoCount(This) (This)->lpVtbl->GetTypeInfoCount(This)
#define ITypeLib2_GetTypeInfo(This,index,ppTInfo) (This)->lpVtbl->GetTypeInfo(This,index,ppTInfo)
#define ITypeLib2_GetTypeInfoType(This,index,pTKind) (This)->lpVtbl->GetTypeInfoType(This,index,pTKind)
#define ITypeLib2_GetTypeInfoOfGuid(This,guid,ppTinfo) (This)->lpVtbl->GetTypeInfoOfGuid(This,guid,ppTinfo)
#define ITypeLib2_GetLibAttr(This,ppTLibAttr) (This)->lpVtbl->GetLibAttr(This,ppTLibAttr)
#define ITypeLib2_GetTypeComp(This,ppTComp) (This)->lpVtbl->GetTypeComp(This,ppTComp)
#define ITypeLib2_GetDocumentation(This,index,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile) (This)->lpVtbl->GetDocumentation(This,index,pBstrName,pBstrDocString,pdwHelpContext,pBstrHelpFile)
#define ITypeLib2_IsName(This,szNameBuf,lHashVal,pfName) (This)->lpVtbl->IsName(This,szNameBuf,lHashVal,pfName)
#define ITypeLib2_FindName(This,szNameBuf,lHashVal,ppTInfo,rgMemId,pcFound) (This)->lpVtbl->FindName(This,szNameBuf,lHashVal,ppTInfo,rgMemId,pcFound)
#define ITypeLib2_ReleaseTLibAttr(This,pTLibAttr) (This)->lpVtbl->ReleaseTLibAttr(This,pTLibAttr)
#define ITypeLib2_GetCustData(This,guid,pVarVal) (This)->lpVtbl->GetCustData(This,guid,pVarVal)
#define ITypeLib2_GetLibStatistics(This,pcUniqueNames,pcchUniqueNames) (This)->lpVtbl->GetLibStatistics(This,pcUniqueNames,pcchUniqueNames)
#define ITypeLib2_GetDocumentation2(This,index,lcid,pbstrHelpString,pdwHelpStringContext,pbstrHelpStringDll) (This)->lpVtbl->GetDocumentation2(This,index,lcid,pbstrHelpString,pdwHelpStringContext,pbstrHelpStringDll)
#define ITypeLib2_GetAllCustData(This,pCustData) (This)->lpVtbl->GetAllCustData(This,pCustData)
#endif
#endif
  HRESULT WINAPI ITypeLib2_GetCustData_Proxy(ITypeLib2 *This,REFGUID guid,VARIANT *pVarVal);
  void __RPC_STUB ITypeLib2_GetCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib2_RemoteGetLibStatistics_Proxy(ITypeLib2 *This,ULONG *pcUniqueNames,ULONG *pcchUniqueNames);
  void __RPC_STUB ITypeLib2_RemoteGetLibStatistics_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib2_RemoteGetDocumentation2_Proxy(ITypeLib2 *This,INT index,LCID lcid,DWORD refPtrFlags,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  void __RPC_STUB ITypeLib2_RemoteGetDocumentation2_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeLib2_GetAllCustData_Proxy(ITypeLib2 *This,CUSTDATA *pCustData);
  void __RPC_STUB ITypeLib2_GetAllCustData_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeChangeEvents_INTERFACE_DEFINED__
#define __ITypeChangeEvents_INTERFACE_DEFINED__
  typedef ITypeChangeEvents *LPTYPECHANGEEVENTS;

  typedef enum tagCHANGEKIND {
    CHANGEKIND_ADDMEMBER = 0,
    CHANGEKIND_DELETEMEMBER,CHANGEKIND_SETNAMES,CHANGEKIND_SETDOCUMENTATION,
    CHANGEKIND_GENERAL,CHANGEKIND_INVALIDATE,CHANGEKIND_CHANGEFAILED,
    CHANGEKIND_MAX
  } CHANGEKIND;

  EXTERN_C const IID IID_ITypeChangeEvents;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeChangeEvents : public IUnknown {
  public:
    virtual HRESULT WINAPI RequestTypeChange(CHANGEKIND changeKind,ITypeInfo *pTInfoBefore,LPOLESTR pStrName,INT *pfCancel) = 0;
    virtual HRESULT WINAPI AfterTypeChange(CHANGEKIND changeKind,ITypeInfo *pTInfoAfter,LPOLESTR pStrName) = 0;
  };
#else
  typedef struct ITypeChangeEventsVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeChangeEvents *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeChangeEvents *This);
      ULONG (WINAPI *Release)(ITypeChangeEvents *This);
      HRESULT (WINAPI *RequestTypeChange)(ITypeChangeEvents *This,CHANGEKIND changeKind,ITypeInfo *pTInfoBefore,LPOLESTR pStrName,INT *pfCancel);
      HRESULT (WINAPI *AfterTypeChange)(ITypeChangeEvents *This,CHANGEKIND changeKind,ITypeInfo *pTInfoAfter,LPOLESTR pStrName);
    END_INTERFACE
  } ITypeChangeEventsVtbl;
  struct ITypeChangeEvents {
    CONST_VTBL struct ITypeChangeEventsVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeChangeEvents_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeChangeEvents_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeChangeEvents_Release(This) (This)->lpVtbl->Release(This)
#define ITypeChangeEvents_RequestTypeChange(This,changeKind,pTInfoBefore,pStrName,pfCancel) (This)->lpVtbl->RequestTypeChange(This,changeKind,pTInfoBefore,pStrName,pfCancel)
#define ITypeChangeEvents_AfterTypeChange(This,changeKind,pTInfoAfter,pStrName) (This)->lpVtbl->AfterTypeChange(This,changeKind,pTInfoAfter,pStrName)
#endif
#endif
  HRESULT WINAPI ITypeChangeEvents_RequestTypeChange_Proxy(ITypeChangeEvents *This,CHANGEKIND changeKind,ITypeInfo *pTInfoBefore,LPOLESTR pStrName,INT *pfCancel);
  void __RPC_STUB ITypeChangeEvents_RequestTypeChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeChangeEvents_AfterTypeChange_Proxy(ITypeChangeEvents *This,CHANGEKIND changeKind,ITypeInfo *pTInfoAfter,LPOLESTR pStrName);
  void __RPC_STUB ITypeChangeEvents_AfterTypeChange_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IErrorInfo_INTERFACE_DEFINED__
#define __IErrorInfo_INTERFACE_DEFINED__
  typedef IErrorInfo *LPERRORINFO;

  EXTERN_C const IID IID_IErrorInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IErrorInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI GetGUID(GUID *pGUID) = 0;
    virtual HRESULT WINAPI GetSource(BSTR *pBstrSource) = 0;
    virtual HRESULT WINAPI GetDescription(BSTR *pBstrDescription) = 0;
    virtual HRESULT WINAPI GetHelpFile(BSTR *pBstrHelpFile) = 0;
    virtual HRESULT WINAPI GetHelpContext(DWORD *pdwHelpContext) = 0;
  };
#else
  typedef struct IErrorInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IErrorInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IErrorInfo *This);
      ULONG (WINAPI *Release)(IErrorInfo *This);
      HRESULT (WINAPI *GetGUID)(IErrorInfo *This,GUID *pGUID);
      HRESULT (WINAPI *GetSource)(IErrorInfo *This,BSTR *pBstrSource);
      HRESULT (WINAPI *GetDescription)(IErrorInfo *This,BSTR *pBstrDescription);
      HRESULT (WINAPI *GetHelpFile)(IErrorInfo *This,BSTR *pBstrHelpFile);
      HRESULT (WINAPI *GetHelpContext)(IErrorInfo *This,DWORD *pdwHelpContext);
    END_INTERFACE
  } IErrorInfoVtbl;
  struct IErrorInfo {
    CONST_VTBL struct IErrorInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IErrorInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IErrorInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IErrorInfo_Release(This) (This)->lpVtbl->Release(This)
#define IErrorInfo_GetGUID(This,pGUID) (This)->lpVtbl->GetGUID(This,pGUID)
#define IErrorInfo_GetSource(This,pBstrSource) (This)->lpVtbl->GetSource(This,pBstrSource)
#define IErrorInfo_GetDescription(This,pBstrDescription) (This)->lpVtbl->GetDescription(This,pBstrDescription)
#define IErrorInfo_GetHelpFile(This,pBstrHelpFile) (This)->lpVtbl->GetHelpFile(This,pBstrHelpFile)
#define IErrorInfo_GetHelpContext(This,pdwHelpContext) (This)->lpVtbl->GetHelpContext(This,pdwHelpContext)
#endif
#endif
  HRESULT WINAPI IErrorInfo_GetGUID_Proxy(IErrorInfo *This,GUID *pGUID);
  void __RPC_STUB IErrorInfo_GetGUID_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IErrorInfo_GetSource_Proxy(IErrorInfo *This,BSTR *pBstrSource);
  void __RPC_STUB IErrorInfo_GetSource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IErrorInfo_GetDescription_Proxy(IErrorInfo *This,BSTR *pBstrDescription);
  void __RPC_STUB IErrorInfo_GetDescription_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IErrorInfo_GetHelpFile_Proxy(IErrorInfo *This,BSTR *pBstrHelpFile);
  void __RPC_STUB IErrorInfo_GetHelpFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IErrorInfo_GetHelpContext_Proxy(IErrorInfo *This,DWORD *pdwHelpContext);
  void __RPC_STUB IErrorInfo_GetHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ICreateErrorInfo_INTERFACE_DEFINED__
#define __ICreateErrorInfo_INTERFACE_DEFINED__
  typedef ICreateErrorInfo *LPCREATEERRORINFO;

  EXTERN_C const IID IID_ICreateErrorInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ICreateErrorInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI SetGUID(REFGUID rguid) = 0;
    virtual HRESULT WINAPI SetSource(LPOLESTR szSource) = 0;
    virtual HRESULT WINAPI SetDescription(LPOLESTR szDescription) = 0;
    virtual HRESULT WINAPI SetHelpFile(LPOLESTR szHelpFile) = 0;
    virtual HRESULT WINAPI SetHelpContext(DWORD dwHelpContext) = 0;
  };
#else
  typedef struct ICreateErrorInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ICreateErrorInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ICreateErrorInfo *This);
      ULONG (WINAPI *Release)(ICreateErrorInfo *This);
      HRESULT (WINAPI *SetGUID)(ICreateErrorInfo *This,REFGUID rguid);
      HRESULT (WINAPI *SetSource)(ICreateErrorInfo *This,LPOLESTR szSource);
      HRESULT (WINAPI *SetDescription)(ICreateErrorInfo *This,LPOLESTR szDescription);
      HRESULT (WINAPI *SetHelpFile)(ICreateErrorInfo *This,LPOLESTR szHelpFile);
      HRESULT (WINAPI *SetHelpContext)(ICreateErrorInfo *This,DWORD dwHelpContext);
    END_INTERFACE
  } ICreateErrorInfoVtbl;
  struct ICreateErrorInfo {
    CONST_VTBL struct ICreateErrorInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ICreateErrorInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ICreateErrorInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ICreateErrorInfo_Release(This) (This)->lpVtbl->Release(This)
#define ICreateErrorInfo_SetGUID(This,rguid) (This)->lpVtbl->SetGUID(This,rguid)
#define ICreateErrorInfo_SetSource(This,szSource) (This)->lpVtbl->SetSource(This,szSource)
#define ICreateErrorInfo_SetDescription(This,szDescription) (This)->lpVtbl->SetDescription(This,szDescription)
#define ICreateErrorInfo_SetHelpFile(This,szHelpFile) (This)->lpVtbl->SetHelpFile(This,szHelpFile)
#define ICreateErrorInfo_SetHelpContext(This,dwHelpContext) (This)->lpVtbl->SetHelpContext(This,dwHelpContext)
#endif
#endif
  HRESULT WINAPI ICreateErrorInfo_SetGUID_Proxy(ICreateErrorInfo *This,REFGUID rguid);
  void __RPC_STUB ICreateErrorInfo_SetGUID_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateErrorInfo_SetSource_Proxy(ICreateErrorInfo *This,LPOLESTR szSource);
  void __RPC_STUB ICreateErrorInfo_SetSource_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateErrorInfo_SetDescription_Proxy(ICreateErrorInfo *This,LPOLESTR szDescription);
  void __RPC_STUB ICreateErrorInfo_SetDescription_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateErrorInfo_SetHelpFile_Proxy(ICreateErrorInfo *This,LPOLESTR szHelpFile);
  void __RPC_STUB ICreateErrorInfo_SetHelpFile_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ICreateErrorInfo_SetHelpContext_Proxy(ICreateErrorInfo *This,DWORD dwHelpContext);
  void __RPC_STUB ICreateErrorInfo_SetHelpContext_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ISupportErrorInfo_INTERFACE_DEFINED__
#define __ISupportErrorInfo_INTERFACE_DEFINED__
  typedef ISupportErrorInfo *LPSUPPORTERRORINFO;

  EXTERN_C const IID IID_ISupportErrorInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ISupportErrorInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI InterfaceSupportsErrorInfo(REFIID riid) = 0;
  };
#else
  typedef struct ISupportErrorInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ISupportErrorInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ISupportErrorInfo *This);
      ULONG (WINAPI *Release)(ISupportErrorInfo *This);
      HRESULT (WINAPI *InterfaceSupportsErrorInfo)(ISupportErrorInfo *This,REFIID riid);
    END_INTERFACE
  } ISupportErrorInfoVtbl;
  struct ISupportErrorInfo {
    CONST_VTBL struct ISupportErrorInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ISupportErrorInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ISupportErrorInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ISupportErrorInfo_Release(This) (This)->lpVtbl->Release(This)
#define ISupportErrorInfo_InterfaceSupportsErrorInfo(This,riid) (This)->lpVtbl->InterfaceSupportsErrorInfo(This,riid)
#endif
#endif
  HRESULT WINAPI ISupportErrorInfo_InterfaceSupportsErrorInfo_Proxy(ISupportErrorInfo *This,REFIID riid);
  void __RPC_STUB ISupportErrorInfo_InterfaceSupportsErrorInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeFactory_INTERFACE_DEFINED__
#define __ITypeFactory_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ITypeFactory;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeFactory : public IUnknown {
  public:
    virtual HRESULT WINAPI CreateFromTypeInfo(ITypeInfo *pTypeInfo,REFIID riid,IUnknown **ppv) = 0;
  };
#else
  typedef struct ITypeFactoryVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeFactory *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeFactory *This);
      ULONG (WINAPI *Release)(ITypeFactory *This);
      HRESULT (WINAPI *CreateFromTypeInfo)(ITypeFactory *This,ITypeInfo *pTypeInfo,REFIID riid,IUnknown **ppv);
    END_INTERFACE
  } ITypeFactoryVtbl;
  struct ITypeFactory {
    CONST_VTBL struct ITypeFactoryVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeFactory_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeFactory_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeFactory_Release(This) (This)->lpVtbl->Release(This)
#define ITypeFactory_CreateFromTypeInfo(This,pTypeInfo,riid,ppv) (This)->lpVtbl->CreateFromTypeInfo(This,pTypeInfo,riid,ppv)
#endif
#endif
  HRESULT WINAPI ITypeFactory_CreateFromTypeInfo_Proxy(ITypeFactory *This,ITypeInfo *pTypeInfo,REFIID riid,IUnknown **ppv);
  void __RPC_STUB ITypeFactory_CreateFromTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __ITypeMarshal_INTERFACE_DEFINED__
#define __ITypeMarshal_INTERFACE_DEFINED__
  EXTERN_C const IID IID_ITypeMarshal;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct ITypeMarshal : public IUnknown {
  public:
    virtual HRESULT WINAPI Size(PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG *pSize) = 0;
    virtual HRESULT WINAPI Marshal(PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbWritten) = 0;
    virtual HRESULT WINAPI Unmarshal(PVOID pvType,DWORD dwFlags,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbRead) = 0;
    virtual HRESULT WINAPI Free(PVOID pvType) = 0;
  };
#else
  typedef struct ITypeMarshalVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(ITypeMarshal *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(ITypeMarshal *This);
      ULONG (WINAPI *Release)(ITypeMarshal *This);
      HRESULT (WINAPI *Size)(ITypeMarshal *This,PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG *pSize);
      HRESULT (WINAPI *Marshal)(ITypeMarshal *This,PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbWritten);
      HRESULT (WINAPI *Unmarshal)(ITypeMarshal *This,PVOID pvType,DWORD dwFlags,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbRead);
      HRESULT (WINAPI *Free)(ITypeMarshal *This,PVOID pvType);
    END_INTERFACE
  } ITypeMarshalVtbl;
  struct ITypeMarshal {
    CONST_VTBL struct ITypeMarshalVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define ITypeMarshal_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define ITypeMarshal_AddRef(This) (This)->lpVtbl->AddRef(This)
#define ITypeMarshal_Release(This) (This)->lpVtbl->Release(This)
#define ITypeMarshal_Size(This,pvType,dwDestContext,pvDestContext,pSize) (This)->lpVtbl->Size(This,pvType,dwDestContext,pvDestContext,pSize)
#define ITypeMarshal_Marshal(This,pvType,dwDestContext,pvDestContext,cbBufferLength,pBuffer,pcbWritten) (This)->lpVtbl->Marshal(This,pvType,dwDestContext,pvDestContext,cbBufferLength,pBuffer,pcbWritten)
#define ITypeMarshal_Unmarshal(This,pvType,dwFlags,cbBufferLength,pBuffer,pcbRead) (This)->lpVtbl->Unmarshal(This,pvType,dwFlags,cbBufferLength,pBuffer,pcbRead)
#define ITypeMarshal_Free(This,pvType) (This)->lpVtbl->Free(This,pvType)
#endif
#endif
  HRESULT WINAPI ITypeMarshal_Size_Proxy(ITypeMarshal *This,PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG *pSize);
  void __RPC_STUB ITypeMarshal_Size_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeMarshal_Marshal_Proxy(ITypeMarshal *This,PVOID pvType,DWORD dwDestContext,PVOID pvDestContext,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbWritten);
  void __RPC_STUB ITypeMarshal_Marshal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeMarshal_Unmarshal_Proxy(ITypeMarshal *This,PVOID pvType,DWORD dwFlags,ULONG cbBufferLength,BYTE *pBuffer,ULONG *pcbRead);
  void __RPC_STUB ITypeMarshal_Unmarshal_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI ITypeMarshal_Free_Proxy(ITypeMarshal *This,PVOID pvType);
  void __RPC_STUB ITypeMarshal_Free_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IRecordInfo_INTERFACE_DEFINED__
#define __IRecordInfo_INTERFACE_DEFINED__
  typedef IRecordInfo *LPRECORDINFO;

  EXTERN_C const IID IID_IRecordInfo;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IRecordInfo : public IUnknown {
  public:
    virtual HRESULT WINAPI RecordInit(PVOID pvNew) = 0;
    virtual HRESULT WINAPI RecordClear(PVOID pvExisting) = 0;
    virtual HRESULT WINAPI RecordCopy(PVOID pvExisting,PVOID pvNew) = 0;
    virtual HRESULT WINAPI GetGuid(GUID *pguid) = 0;
    virtual HRESULT WINAPI GetName(BSTR *pbstrName) = 0;
    virtual HRESULT WINAPI GetSize(ULONG *pcbSize) = 0;
    virtual HRESULT WINAPI GetTypeInfo(ITypeInfo **ppTypeInfo) = 0;
    virtual HRESULT WINAPI GetField(PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField) = 0;
    virtual HRESULT WINAPI GetFieldNoCopy(PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField,PVOID *ppvDataCArray) = 0;
    virtual HRESULT WINAPI PutField(ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField) = 0;
    virtual HRESULT WINAPI PutFieldNoCopy(ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField) = 0;
    virtual HRESULT WINAPI GetFieldNames(ULONG *pcNames,BSTR *rgBstrNames) = 0;
    virtual WINBOOL WINAPI IsMatchingType(IRecordInfo *pRecordInfo) = 0;
    virtual PVOID WINAPI RecordCreate(void) = 0;
    virtual HRESULT WINAPI RecordCreateCopy(PVOID pvSource,PVOID *ppvDest) = 0;
    virtual HRESULT WINAPI RecordDestroy(PVOID pvRecord) = 0;
  };
#else
  typedef struct IRecordInfoVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IRecordInfo *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IRecordInfo *This);
      ULONG (WINAPI *Release)(IRecordInfo *This);
      HRESULT (WINAPI *RecordInit)(IRecordInfo *This,PVOID pvNew);
      HRESULT (WINAPI *RecordClear)(IRecordInfo *This,PVOID pvExisting);
      HRESULT (WINAPI *RecordCopy)(IRecordInfo *This,PVOID pvExisting,PVOID pvNew);
      HRESULT (WINAPI *GetGuid)(IRecordInfo *This,GUID *pguid);
      HRESULT (WINAPI *GetName)(IRecordInfo *This,BSTR *pbstrName);
      HRESULT (WINAPI *GetSize)(IRecordInfo *This,ULONG *pcbSize);
      HRESULT (WINAPI *GetTypeInfo)(IRecordInfo *This,ITypeInfo **ppTypeInfo);
      HRESULT (WINAPI *GetField)(IRecordInfo *This,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
      HRESULT (WINAPI *GetFieldNoCopy)(IRecordInfo *This,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField,PVOID *ppvDataCArray);
      HRESULT (WINAPI *PutField)(IRecordInfo *This,ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
      HRESULT (WINAPI *PutFieldNoCopy)(IRecordInfo *This,ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
      HRESULT (WINAPI *GetFieldNames)(IRecordInfo *This,ULONG *pcNames,BSTR *rgBstrNames);
      WINBOOL (WINAPI *IsMatchingType)(IRecordInfo *This,IRecordInfo *pRecordInfo);
      PVOID (WINAPI *RecordCreate)(IRecordInfo *This);
      HRESULT (WINAPI *RecordCreateCopy)(IRecordInfo *This,PVOID pvSource,PVOID *ppvDest);
      HRESULT (WINAPI *RecordDestroy)(IRecordInfo *This,PVOID pvRecord);
    END_INTERFACE
  } IRecordInfoVtbl;
  struct IRecordInfo {
    CONST_VTBL struct IRecordInfoVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IRecordInfo_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IRecordInfo_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IRecordInfo_Release(This) (This)->lpVtbl->Release(This)
#define IRecordInfo_RecordInit(This,pvNew) (This)->lpVtbl->RecordInit(This,pvNew)
#define IRecordInfo_RecordClear(This,pvExisting) (This)->lpVtbl->RecordClear(This,pvExisting)
#define IRecordInfo_RecordCopy(This,pvExisting,pvNew) (This)->lpVtbl->RecordCopy(This,pvExisting,pvNew)
#define IRecordInfo_GetGuid(This,pguid) (This)->lpVtbl->GetGuid(This,pguid)
#define IRecordInfo_GetName(This,pbstrName) (This)->lpVtbl->GetName(This,pbstrName)
#define IRecordInfo_GetSize(This,pcbSize) (This)->lpVtbl->GetSize(This,pcbSize)
#define IRecordInfo_GetTypeInfo(This,ppTypeInfo) (This)->lpVtbl->GetTypeInfo(This,ppTypeInfo)
#define IRecordInfo_GetField(This,pvData,szFieldName,pvarField) (This)->lpVtbl->GetField(This,pvData,szFieldName,pvarField)
#define IRecordInfo_GetFieldNoCopy(This,pvData,szFieldName,pvarField,ppvDataCArray) (This)->lpVtbl->GetFieldNoCopy(This,pvData,szFieldName,pvarField,ppvDataCArray)
#define IRecordInfo_PutField(This,wFlags,pvData,szFieldName,pvarField) (This)->lpVtbl->PutField(This,wFlags,pvData,szFieldName,pvarField)
#define IRecordInfo_PutFieldNoCopy(This,wFlags,pvData,szFieldName,pvarField) (This)->lpVtbl->PutFieldNoCopy(This,wFlags,pvData,szFieldName,pvarField)
#define IRecordInfo_GetFieldNames(This,pcNames,rgBstrNames) (This)->lpVtbl->GetFieldNames(This,pcNames,rgBstrNames)
#define IRecordInfo_IsMatchingType(This,pRecordInfo) (This)->lpVtbl->IsMatchingType(This,pRecordInfo)
#define IRecordInfo_RecordCreate(This) (This)->lpVtbl->RecordCreate(This)
#define IRecordInfo_RecordCreateCopy(This,pvSource,ppvDest) (This)->lpVtbl->RecordCreateCopy(This,pvSource,ppvDest)
#define IRecordInfo_RecordDestroy(This,pvRecord) (This)->lpVtbl->RecordDestroy(This,pvRecord)
#endif
#endif
  HRESULT WINAPI IRecordInfo_RecordInit_Proxy(IRecordInfo *This,PVOID pvNew);
  void __RPC_STUB IRecordInfo_RecordInit_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_RecordClear_Proxy(IRecordInfo *This,PVOID pvExisting);
  void __RPC_STUB IRecordInfo_RecordClear_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_RecordCopy_Proxy(IRecordInfo *This,PVOID pvExisting,PVOID pvNew);
  void __RPC_STUB IRecordInfo_RecordCopy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetGuid_Proxy(IRecordInfo *This,GUID *pguid);
  void __RPC_STUB IRecordInfo_GetGuid_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetName_Proxy(IRecordInfo *This,BSTR *pbstrName);
  void __RPC_STUB IRecordInfo_GetName_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetSize_Proxy(IRecordInfo *This,ULONG *pcbSize);
  void __RPC_STUB IRecordInfo_GetSize_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetTypeInfo_Proxy(IRecordInfo *This,ITypeInfo **ppTypeInfo);
  void __RPC_STUB IRecordInfo_GetTypeInfo_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetField_Proxy(IRecordInfo *This,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
  void __RPC_STUB IRecordInfo_GetField_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetFieldNoCopy_Proxy(IRecordInfo *This,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField,PVOID *ppvDataCArray);
  void __RPC_STUB IRecordInfo_GetFieldNoCopy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_PutField_Proxy(IRecordInfo *This,ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
  void __RPC_STUB IRecordInfo_PutField_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_PutFieldNoCopy_Proxy(IRecordInfo *This,ULONG wFlags,PVOID pvData,LPCOLESTR szFieldName,VARIANT *pvarField);
  void __RPC_STUB IRecordInfo_PutFieldNoCopy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_GetFieldNames_Proxy(IRecordInfo *This,ULONG *pcNames,BSTR *rgBstrNames);
  void __RPC_STUB IRecordInfo_GetFieldNames_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  WINBOOL WINAPI IRecordInfo_IsMatchingType_Proxy(IRecordInfo *This,IRecordInfo *pRecordInfo);
  void __RPC_STUB IRecordInfo_IsMatchingType_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  PVOID WINAPI IRecordInfo_RecordCreate_Proxy(IRecordInfo *This);
  void __RPC_STUB IRecordInfo_RecordCreate_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_RecordCreateCopy_Proxy(IRecordInfo *This,PVOID pvSource,PVOID *ppvDest);
  void __RPC_STUB IRecordInfo_RecordCreateCopy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IRecordInfo_RecordDestroy_Proxy(IRecordInfo *This,PVOID pvRecord);
  void __RPC_STUB IRecordInfo_RecordDestroy_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IErrorLog_INTERFACE_DEFINED__
#define __IErrorLog_INTERFACE_DEFINED__
  typedef IErrorLog *LPERRORLOG;

  EXTERN_C const IID IID_IErrorLog;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IErrorLog : public IUnknown {
  public:
    virtual HRESULT WINAPI AddError(LPCOLESTR pszPropName,EXCEPINFO *pExcepInfo) = 0;
  };
#else
  typedef struct IErrorLogVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IErrorLog *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IErrorLog *This);
      ULONG (WINAPI *Release)(IErrorLog *This);
      HRESULT (WINAPI *AddError)(IErrorLog *This,LPCOLESTR pszPropName,EXCEPINFO *pExcepInfo);
    END_INTERFACE
  } IErrorLogVtbl;
  struct IErrorLog {
    CONST_VTBL struct IErrorLogVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IErrorLog_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IErrorLog_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IErrorLog_Release(This) (This)->lpVtbl->Release(This)
#define IErrorLog_AddError(This,pszPropName,pExcepInfo) (This)->lpVtbl->AddError(This,pszPropName,pExcepInfo)
#endif
#endif
  HRESULT WINAPI IErrorLog_AddError_Proxy(IErrorLog *This,LPCOLESTR pszPropName,EXCEPINFO *pExcepInfo);
  void __RPC_STUB IErrorLog_AddError_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

#ifndef __IPropertyBag_INTERFACE_DEFINED__
#define __IPropertyBag_INTERFACE_DEFINED__
  typedef IPropertyBag *LPPROPERTYBAG;

  EXTERN_C const IID IID_IPropertyBag;
#if defined(__cplusplus) && !defined(CINTERFACE)
  struct IPropertyBag : public IUnknown {
  public:
    virtual HRESULT WINAPI Read(LPCOLESTR pszPropName,VARIANT *pVar,IErrorLog *pErrorLog) = 0;
    virtual HRESULT WINAPI Write(LPCOLESTR pszPropName,VARIANT *pVar) = 0;
  };
#else
  typedef struct IPropertyBagVtbl {
    BEGIN_INTERFACE
      HRESULT (WINAPI *QueryInterface)(IPropertyBag *This,REFIID riid,void **ppvObject);
      ULONG (WINAPI *AddRef)(IPropertyBag *This);
      ULONG (WINAPI *Release)(IPropertyBag *This);
      HRESULT (WINAPI *Read)(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar,IErrorLog *pErrorLog);
      HRESULT (WINAPI *Write)(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar);
    END_INTERFACE
  } IPropertyBagVtbl;
  struct IPropertyBag {
    CONST_VTBL struct IPropertyBagVtbl *lpVtbl;
  };
#ifdef COBJMACROS
#define IPropertyBag_QueryInterface(This,riid,ppvObject) (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPropertyBag_AddRef(This) (This)->lpVtbl->AddRef(This)
#define IPropertyBag_Release(This) (This)->lpVtbl->Release(This)
#define IPropertyBag_Read(This,pszPropName,pVar,pErrorLog) (This)->lpVtbl->Read(This,pszPropName,pVar,pErrorLog)
#define IPropertyBag_Write(This,pszPropName,pVar) (This)->lpVtbl->Write(This,pszPropName,pVar)
#endif
#endif
  HRESULT WINAPI IPropertyBag_RemoteRead_Proxy(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar,IErrorLog *pErrorLog,DWORD varType,IUnknown *pUnkObj);
  void __RPC_STUB IPropertyBag_RemoteRead_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
  HRESULT WINAPI IPropertyBag_Write_Proxy(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar);
  void __RPC_STUB IPropertyBag_Write_Stub(IRpcStubBuffer *This,IRpcChannelBuffer *_pRpcChannelBuffer,PRPC_MESSAGE _pRpcMessage,DWORD *_pdwStubPhase);
#endif

  extern RPC_IF_HANDLE __MIDL_itf_oaidl_0114_v0_0_c_ifspec;
  extern RPC_IF_HANDLE __MIDL_itf_oaidl_0114_v0_0_s_ifspec;

  unsigned long __RPC_API BSTR_UserSize(unsigned long *,unsigned long,BSTR *);
  unsigned char *__RPC_API BSTR_UserMarshal(unsigned long *,unsigned char *,BSTR *);
  unsigned char *__RPC_API BSTR_UserUnmarshal(unsigned long *,unsigned char *,BSTR *);
  void __RPC_API BSTR_UserFree(unsigned long *,BSTR *);
  unsigned long __RPC_API CLEANLOCALSTORAGE_UserSize(unsigned long *,unsigned long,CLEANLOCALSTORAGE *);
  unsigned char *__RPC_API CLEANLOCALSTORAGE_UserMarshal(unsigned long *,unsigned char *,CLEANLOCALSTORAGE *);
  unsigned char *__RPC_API CLEANLOCALSTORAGE_UserUnmarshal(unsigned long *,unsigned char *,CLEANLOCALSTORAGE *);
  void __RPC_API CLEANLOCALSTORAGE_UserFree(unsigned long *,CLEANLOCALSTORAGE *);
  unsigned long __RPC_API VARIANT_UserSize(unsigned long *,unsigned long,VARIANT *);
  unsigned char *__RPC_API VARIANT_UserMarshal(unsigned long *,unsigned char *,VARIANT *);
  unsigned char *__RPC_API VARIANT_UserUnmarshal(unsigned long *,unsigned char *,VARIANT *);
  void __RPC_API VARIANT_UserFree(unsigned long *,VARIANT *);

  HRESULT WINAPI IDispatch_Invoke_Proxy(IDispatch *This,DISPID dispIdMember,REFIID riid,LCID lcid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr);
  HRESULT WINAPI IDispatch_Invoke_Stub(IDispatch *This,DISPID dispIdMember,REFIID riid,LCID lcid,DWORD dwFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *pArgErr,UINT cVarRef,UINT *rgVarRefIdx,VARIANTARG *rgVarRef);
  HRESULT WINAPI IEnumVARIANT_Next_Proxy(IEnumVARIANT *This,ULONG celt,VARIANT *rgVar,ULONG *pCeltFetched);
  HRESULT WINAPI IEnumVARIANT_Next_Stub(IEnumVARIANT *This,ULONG celt,VARIANT *rgVar,ULONG *pCeltFetched);
  HRESULT WINAPI ITypeComp_Bind_Proxy(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,WORD wFlags,ITypeInfo **ppTInfo,DESCKIND *pDescKind,BINDPTR *pBindPtr);
  HRESULT WINAPI ITypeComp_Bind_Stub(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,WORD wFlags,ITypeInfo **ppTInfo,DESCKIND *pDescKind,LPFUNCDESC *ppFuncDesc,LPVARDESC *ppVarDesc,ITypeComp **ppTypeComp,CLEANLOCALSTORAGE *pDummy);
  HRESULT WINAPI ITypeComp_BindType_Proxy(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,ITypeInfo **ppTInfo,ITypeComp **ppTComp);
  HRESULT WINAPI ITypeComp_BindType_Stub(ITypeComp *This,LPOLESTR szName,ULONG lHashVal,ITypeInfo **ppTInfo);
  HRESULT WINAPI ITypeInfo_GetTypeAttr_Proxy(ITypeInfo *This,TYPEATTR **ppTypeAttr);
  HRESULT WINAPI ITypeInfo_GetTypeAttr_Stub(ITypeInfo *This,LPTYPEATTR *ppTypeAttr,CLEANLOCALSTORAGE *pDummy);
  HRESULT WINAPI ITypeInfo_GetFuncDesc_Proxy(ITypeInfo *This,UINT index,FUNCDESC **ppFuncDesc);
  HRESULT WINAPI ITypeInfo_GetFuncDesc_Stub(ITypeInfo *This,UINT index,LPFUNCDESC *ppFuncDesc,CLEANLOCALSTORAGE *pDummy);
  HRESULT WINAPI ITypeInfo_GetVarDesc_Proxy(ITypeInfo *This,UINT index,VARDESC **ppVarDesc);
  HRESULT WINAPI ITypeInfo_GetVarDesc_Stub(ITypeInfo *This,UINT index,LPVARDESC *ppVarDesc,CLEANLOCALSTORAGE *pDummy);
  HRESULT WINAPI ITypeInfo_GetNames_Proxy(ITypeInfo *This,MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames);
  HRESULT WINAPI ITypeInfo_GetNames_Stub(ITypeInfo *This,MEMBERID memid,BSTR *rgBstrNames,UINT cMaxNames,UINT *pcNames);
  HRESULT WINAPI ITypeInfo_GetIDsOfNames_Proxy(ITypeInfo *This,LPOLESTR *rgszNames,UINT cNames,MEMBERID *pMemId);
  HRESULT WINAPI ITypeInfo_GetIDsOfNames_Stub(ITypeInfo *This);
  HRESULT WINAPI ITypeInfo_Invoke_Proxy(ITypeInfo *This,PVOID pvInstance,MEMBERID memid,WORD wFlags,DISPPARAMS *pDispParams,VARIANT *pVarResult,EXCEPINFO *pExcepInfo,UINT *puArgErr);
  HRESULT WINAPI ITypeInfo_Invoke_Stub(ITypeInfo *This);
  HRESULT WINAPI ITypeInfo_GetDocumentation_Proxy(ITypeInfo *This,MEMBERID memid,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  HRESULT WINAPI ITypeInfo_GetDocumentation_Stub(ITypeInfo *This,MEMBERID memid,DWORD refPtrFlags,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  HRESULT WINAPI ITypeInfo_GetDllEntry_Proxy(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal);
  HRESULT WINAPI ITypeInfo_GetDllEntry_Stub(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,DWORD refPtrFlags,BSTR *pBstrDllName,BSTR *pBstrName,WORD *pwOrdinal);
  HRESULT WINAPI ITypeInfo_AddressOfMember_Proxy(ITypeInfo *This,MEMBERID memid,INVOKEKIND invKind,PVOID *ppv);
  HRESULT WINAPI ITypeInfo_AddressOfMember_Stub(ITypeInfo *This);
  HRESULT WINAPI ITypeInfo_CreateInstance_Proxy(ITypeInfo *This,IUnknown *pUnkOuter,REFIID riid,PVOID *ppvObj);
  HRESULT WINAPI ITypeInfo_CreateInstance_Stub(ITypeInfo *This,REFIID riid,IUnknown **ppvObj);
  HRESULT WINAPI ITypeInfo_GetContainingTypeLib_Proxy(ITypeInfo *This,ITypeLib **ppTLib,UINT *pIndex);
  HRESULT WINAPI ITypeInfo_GetContainingTypeLib_Stub(ITypeInfo *This,ITypeLib **ppTLib,UINT *pIndex);
  void WINAPI ITypeInfo_ReleaseTypeAttr_Proxy(ITypeInfo *This,TYPEATTR *pTypeAttr);
  HRESULT WINAPI ITypeInfo_ReleaseTypeAttr_Stub(ITypeInfo *This);
  void WINAPI ITypeInfo_ReleaseFuncDesc_Proxy(ITypeInfo *This,FUNCDESC *pFuncDesc);
  HRESULT WINAPI ITypeInfo_ReleaseFuncDesc_Stub(ITypeInfo *This);
  void WINAPI ITypeInfo_ReleaseVarDesc_Proxy(ITypeInfo *This,VARDESC *pVarDesc);
  HRESULT WINAPI ITypeInfo_ReleaseVarDesc_Stub(ITypeInfo *This);
  HRESULT WINAPI ITypeInfo2_GetDocumentation2_Proxy(ITypeInfo2 *This,MEMBERID memid,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  HRESULT WINAPI ITypeInfo2_GetDocumentation2_Stub(ITypeInfo2 *This,MEMBERID memid,LCID lcid,DWORD refPtrFlags,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  UINT WINAPI ITypeLib_GetTypeInfoCount_Proxy(ITypeLib *This);
  HRESULT WINAPI ITypeLib_GetTypeInfoCount_Stub(ITypeLib *This,UINT *pcTInfo);
  HRESULT WINAPI ITypeLib_GetLibAttr_Proxy(ITypeLib *This,TLIBATTR **ppTLibAttr);
  HRESULT WINAPI ITypeLib_GetLibAttr_Stub(ITypeLib *This,LPTLIBATTR *ppTLibAttr,CLEANLOCALSTORAGE *pDummy);
  HRESULT WINAPI ITypeLib_GetDocumentation_Proxy(ITypeLib *This,INT index,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  HRESULT WINAPI ITypeLib_GetDocumentation_Stub(ITypeLib *This,INT index,DWORD refPtrFlags,BSTR *pBstrName,BSTR *pBstrDocString,DWORD *pdwHelpContext,BSTR *pBstrHelpFile);
  HRESULT WINAPI ITypeLib_IsName_Proxy(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName);
  HRESULT WINAPI ITypeLib_IsName_Stub(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,WINBOOL *pfName,BSTR *pBstrLibName);
  HRESULT WINAPI ITypeLib_FindName_Proxy(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound);
  HRESULT WINAPI ITypeLib_FindName_Stub(ITypeLib *This,LPOLESTR szNameBuf,ULONG lHashVal,ITypeInfo **ppTInfo,MEMBERID *rgMemId,USHORT *pcFound,BSTR *pBstrLibName);
  void WINAPI ITypeLib_ReleaseTLibAttr_Proxy(ITypeLib *This,TLIBATTR *pTLibAttr);
  HRESULT WINAPI ITypeLib_ReleaseTLibAttr_Stub(ITypeLib *This);
  HRESULT WINAPI ITypeLib2_GetLibStatistics_Proxy(ITypeLib2 *This,ULONG *pcUniqueNames,ULONG *pcchUniqueNames);
  HRESULT WINAPI ITypeLib2_GetLibStatistics_Stub(ITypeLib2 *This,ULONG *pcUniqueNames,ULONG *pcchUniqueNames);
  HRESULT WINAPI ITypeLib2_GetDocumentation2_Proxy(ITypeLib2 *This,INT index,LCID lcid,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  HRESULT WINAPI ITypeLib2_GetDocumentation2_Stub(ITypeLib2 *This,INT index,LCID lcid,DWORD refPtrFlags,BSTR *pbstrHelpString,DWORD *pdwHelpStringContext,BSTR *pbstrHelpStringDll);
  HRESULT WINAPI IPropertyBag_Read_Proxy(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar,IErrorLog *pErrorLog);
  HRESULT WINAPI IPropertyBag_Read_Stub(IPropertyBag *This,LPCOLESTR pszPropName,VARIANT *pVar,IErrorLog *pErrorLog,DWORD varType,IUnknown *pUnkObj);

#ifdef __cplusplus
}
#endif
#endif
