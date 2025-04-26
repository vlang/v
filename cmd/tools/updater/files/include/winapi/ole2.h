/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _OLE2_H_
#define _OLE2_H_

#include <pshpack8.h>

#ifndef WIN32
#define WIN32 100
#endif

#include <winerror.h>
#include <objbase.h>
#include <oleauto.h>

#define E_DRAW VIEW_E_DRAW

#define DATA_E_FORMATETC DV_E_FORMATETC

#define OLEIVERB_PRIMARY (0L)
#define OLEIVERB_SHOW (-1L)
#define OLEIVERB_OPEN (-2L)
#define OLEIVERB_HIDE (-3L)
#define OLEIVERB_UIACTIVATE (-4L)
#define OLEIVERB_INPLACEACTIVATE (-5L)
#define OLEIVERB_DISCARDUNDOSTATE (-6L)

#define EMBDHLP_INPROC_HANDLER 0x0000L
#define EMBDHLP_INPROC_SERVER 0x0001L
#define EMBDHLP_CREATENOW 0x00000000L
#define EMBDHLP_DELAYCREATE 0x00010000L

#define OLECREATE_LEAVERUNNING 0x00000001

#include <oleidl.h>

WINOLEAPI CreateDataAdviseHolder(LPDATAADVISEHOLDER *ppDAHolder);
WINOLEAPI_(DWORD) OleBuildVersion(VOID);
WINOLEAPI ReadClassStg(LPSTORAGE pStg,CLSID *pclsid);
WINOLEAPI WriteClassStg(LPSTORAGE pStg,REFCLSID rclsid);
WINOLEAPI ReadClassStm(LPSTREAM pStm,CLSID *pclsid);
WINOLEAPI WriteClassStm(LPSTREAM pStm,REFCLSID rclsid);
WINOLEAPI WriteFmtUserTypeStg (LPSTORAGE pstg,CLIPFORMAT cf,LPOLESTR lpszUserType);
WINOLEAPI ReadFmtUserTypeStg (LPSTORAGE pstg,CLIPFORMAT *pcf,LPOLESTR *lplpszUserType);
WINOLEAPI OleInitialize(LPVOID pvReserved);
WINOLEAPI_(void) OleUninitialize(void);
WINOLEAPI OleQueryLinkFromData(LPDATAOBJECT pSrcDataObject);
WINOLEAPI OleQueryCreateFromData(LPDATAOBJECT pSrcDataObject);
WINOLEAPI OleCreate(REFCLSID rclsid,REFIID riid,DWORD renderopt,LPFORMATETC pFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateEx(REFCLSID rclsid,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateFromData(LPDATAOBJECT pSrcDataObj,REFIID riid,DWORD renderopt,LPFORMATETC pFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateFromDataEx(LPDATAOBJECT pSrcDataObj,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLinkFromData(LPDATAOBJECT pSrcDataObj,REFIID riid,DWORD renderopt,LPFORMATETC pFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLinkFromDataEx(LPDATAOBJECT pSrcDataObj,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateStaticFromData(LPDATAOBJECT pSrcDataObj,REFIID iid,DWORD renderopt,LPFORMATETC pFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLink(LPMONIKER pmkLinkSrc,REFIID riid,DWORD renderopt,LPFORMATETC lpFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLinkEx(LPMONIKER pmkLinkSrc,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLinkToFile(LPCOLESTR lpszFileName,REFIID riid,DWORD renderopt,LPFORMATETC lpFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateLinkToFileEx(LPCOLESTR lpszFileName,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateFromFile(REFCLSID rclsid,LPCOLESTR lpszFileName,REFIID riid,DWORD renderopt,LPFORMATETC lpFormatEtc,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleCreateFromFileEx(REFCLSID rclsid,LPCOLESTR lpszFileName,REFIID riid,DWORD dwFlags,DWORD renderopt,ULONG cFormats,DWORD *rgAdvf,LPFORMATETC rgFormatEtc,IAdviseSink *lpAdviseSink,DWORD *rgdwConnection,LPOLECLIENTSITE pClientSite,LPSTORAGE pStg,LPVOID *ppvObj);
WINOLEAPI OleLoad(LPSTORAGE pStg,REFIID riid,LPOLECLIENTSITE pClientSite,LPVOID *ppvObj);
WINOLEAPI OleSave(LPPERSISTSTORAGE pPS,LPSTORAGE pStg,WINBOOL fSameAsLoad);
WINOLEAPI OleLoadFromStream(LPSTREAM pStm,REFIID iidInterface,LPVOID *ppvObj);
WINOLEAPI OleSaveToStream(LPPERSISTSTREAM pPStm,LPSTREAM pStm);
WINOLEAPI OleSetContainedObject(LPUNKNOWN pUnknown,WINBOOL fContained);
WINOLEAPI OleNoteObjectVisible(LPUNKNOWN pUnknown,WINBOOL fVisible);
WINOLEAPI RegisterDragDrop(HWND hwnd,LPDROPTARGET pDropTarget);
WINOLEAPI RevokeDragDrop(HWND hwnd);
WINOLEAPI DoDragDrop(LPDATAOBJECT pDataObj,LPDROPSOURCE pDropSource,DWORD dwOKEffects,LPDWORD pdwEffect);
WINOLEAPI OleSetClipboard(LPDATAOBJECT pDataObj);
WINOLEAPI OleGetClipboard(LPDATAOBJECT *ppDataObj);
WINOLEAPI OleFlushClipboard(void);
WINOLEAPI OleIsCurrentClipboard(LPDATAOBJECT pDataObj);
WINOLEAPI_(HOLEMENU) OleCreateMenuDescriptor (HMENU hmenuCombined,LPOLEMENUGROUPWIDTHS lpMenuWidths);
WINOLEAPI OleSetMenuDescriptor (HOLEMENU holemenu,HWND hwndFrame,HWND hwndActiveObject,LPOLEINPLACEFRAME lpFrame,LPOLEINPLACEACTIVEOBJECT lpActiveObj);
WINOLEAPI OleDestroyMenuDescriptor (HOLEMENU holemenu);
WINOLEAPI OleTranslateAccelerator (LPOLEINPLACEFRAME lpFrame,LPOLEINPLACEFRAMEINFO lpFrameInfo,LPMSG lpmsg);
WINOLEAPI_(HANDLE) OleDuplicateData (HANDLE hSrc,CLIPFORMAT cfFormat,UINT uiFlags);
WINOLEAPI OleDraw (LPUNKNOWN pUnknown,DWORD dwAspect,HDC hdcDraw,LPCRECT lprcBounds);
WINOLEAPI OleRun(LPUNKNOWN pUnknown);
WINOLEAPI_(WINBOOL) OleIsRunning(LPOLEOBJECT pObject);
WINOLEAPI OleLockRunning(LPUNKNOWN pUnknown,WINBOOL fLock,WINBOOL fLastUnlockCloses);
WINOLEAPI_(void) ReleaseStgMedium(LPSTGMEDIUM);
WINOLEAPI CreateOleAdviseHolder(LPOLEADVISEHOLDER *ppOAHolder);
WINOLEAPI OleCreateDefaultHandler(REFCLSID clsid,LPUNKNOWN pUnkOuter,REFIID riid,LPVOID *lplpObj);
WINOLEAPI OleCreateEmbeddingHelper(REFCLSID clsid,LPUNKNOWN pUnkOuter,DWORD flags,LPCLASSFACTORY pCF,REFIID riid,LPVOID *lplpObj);
WINOLEAPI_(WINBOOL) IsAccelerator(HACCEL hAccel,int cAccelEntries,LPMSG lpMsg,WORD *lpwCmd);
WINOLEAPI_(HGLOBAL) OleGetIconOfFile(LPOLESTR lpszPath,WINBOOL fUseFileAsLabel);
WINOLEAPI_(HGLOBAL) OleGetIconOfClass(REFCLSID rclsid,LPOLESTR lpszLabel,WINBOOL fUseTypeAsLabel);
WINOLEAPI_(HGLOBAL) OleMetafilePictFromIconAndLabel(HICON hIcon,LPOLESTR lpszLabel,LPOLESTR lpszSourceFile,UINT iIconIndex);
WINOLEAPI OleRegGetUserType (REFCLSID clsid,DWORD dwFormOfType,LPOLESTR *pszUserType);
WINOLEAPI OleRegGetMiscStatus (REFCLSID clsid,DWORD dwAspect,DWORD *pdwStatus);
WINOLEAPI OleRegEnumFormatEtc (REFCLSID clsid,DWORD dwDirection,LPENUMFORMATETC *ppenum);
WINOLEAPI OleRegEnumVerbs (REFCLSID clsid,LPENUMOLEVERB *ppenum);

typedef struct _OLESTREAM *LPOLESTREAM;

#ifndef _DEFINED_OLESTREAMVRBL
#define _DEFINED_OLESTREAMVRBL
typedef struct _OLESTREAMVTBL {
  DWORD (CALLBACK *Get)(LPOLESTREAM,void *,DWORD);
  DWORD (CALLBACK *Put)(LPOLESTREAM,const void *,DWORD);
} OLESTREAMVTBL;

typedef OLESTREAMVTBL *LPOLESTREAMVTBL;
#endif

#ifndef _DEFINED_OLESTREAM
#define _DEFINED_OLESTREAM
typedef struct _OLESTREAM {
  LPOLESTREAMVTBL lpstbl;
} OLESTREAM;
#endif

WINOLEAPI OleConvertOLESTREAMToIStorage(LPOLESTREAM lpolestream,LPSTORAGE pstg,const DVTARGETDEVICE *ptd);
WINOLEAPI OleConvertIStorageToOLESTREAM(LPSTORAGE pstg,LPOLESTREAM lpolestream);
WINOLEAPI GetHGlobalFromILockBytes (LPLOCKBYTES plkbyt,HGLOBAL *phglobal);
WINOLEAPI CreateILockBytesOnHGlobal (HGLOBAL hGlobal,WINBOOL fDeleteOnRelease,LPLOCKBYTES *pplkbyt);
WINOLEAPI GetHGlobalFromStream (LPSTREAM pstm,HGLOBAL *phglobal);
WINOLEAPI CreateStreamOnHGlobal (HGLOBAL hGlobal,WINBOOL fDeleteOnRelease,LPSTREAM *ppstm);
WINOLEAPI OleDoAutoConvert(LPSTORAGE pStg,LPCLSID pClsidNew);
WINOLEAPI OleGetAutoConvert(REFCLSID clsidOld,LPCLSID pClsidNew);
WINOLEAPI OleSetAutoConvert(REFCLSID clsidOld,REFCLSID clsidNew);
WINOLEAPI GetConvertStg(LPSTORAGE pStg);
WINOLEAPI SetConvertStg(LPSTORAGE pStg,WINBOOL fConvert);
WINOLEAPI OleConvertIStorageToOLESTREAMEx(LPSTORAGE pstg,CLIPFORMAT cfFormat,LONG lWidth,LONG lHeight,DWORD dwSize,LPSTGMEDIUM pmedium,LPOLESTREAM polestm);
WINOLEAPI OleConvertOLESTREAMToIStorageEx(LPOLESTREAM polestm,LPSTORAGE pstg,CLIPFORMAT *pcfFormat,LONG *plwWidth,LONG *plHeight,DWORD *pdwSize,LPSTGMEDIUM pmedium);

#ifndef RC_INVOKED
#include <poppack.h>
#endif
#endif
