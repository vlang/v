/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef VER_H
#define VER_H

#ifdef __cplusplus
extern "C" {
#endif

#define VS_FILE_INFO RT_VERSION
#define VS_VERSION_INFO 1
#define VS_USER_DEFINED 100

#define VS_FFI_SIGNATURE 0xFEEF04BDL
#define VS_FFI_STRUCVERSION 0x00010000L
#define VS_FFI_FILEFLAGSMASK 0x0000003FL

#define VS_FF_DEBUG 0x00000001L
#define VS_FF_PRERELEASE 0x00000002L
#define VS_FF_PATCHED 0x00000004L
#define VS_FF_PRIVATEBUILD 0x00000008L
#define VS_FF_INFOINFERRED 0x00000010L
#define VS_FF_SPECIALBUILD 0x00000020L

#define VOS_UNKNOWN 0x00000000L
#define VOS_DOS 0x00010000L
#define VOS_OS216 0x00020000L
#define VOS_OS232 0x00030000L
#define VOS_NT 0x00040000L
#define VOS_WINCE 0x00050000L

#define VOS__BASE 0x00000000L
#define VOS__WINDOWS16 0x00000001L
#define VOS__PM16 0x00000002L
#define VOS__PM32 0x00000003L
#define VOS__WINDOWS32 0x00000004L

#define VOS_DOS_WINDOWS16 0x00010001L
#define VOS_DOS_WINDOWS32 0x00010004L
#define VOS_OS216_PM16 0x00020002L
#define VOS_OS232_PM32 0x00030003L
#define VOS_NT_WINDOWS32 0x00040004L

#define VFT_UNKNOWN 0x00000000L
#define VFT_APP 0x00000001L
#define VFT_DLL 0x00000002L
#define VFT_DRV 0x00000003L
#define VFT_FONT 0x00000004L
#define VFT_VXD 0x00000005L
#define VFT_STATIC_LIB 0x00000007L

#define VFT2_UNKNOWN 0x00000000L
#define VFT2_DRV_PRINTER 0x00000001L
#define VFT2_DRV_KEYBOARD 0x00000002L
#define VFT2_DRV_LANGUAGE 0x00000003L
#define VFT2_DRV_DISPLAY 0x00000004L
#define VFT2_DRV_MOUSE 0x00000005L
#define VFT2_DRV_NETWORK 0x00000006L
#define VFT2_DRV_SYSTEM 0x00000007L
#define VFT2_DRV_INSTALLABLE 0x00000008L
#define VFT2_DRV_SOUND 0x00000009L
#define VFT2_DRV_COMM 0x0000000AL
#define VFT2_DRV_INPUTMETHOD 0x0000000BL
#define VFT2_DRV_VERSIONED_PRINTER 0x0000000CL

#define VFT2_FONT_RASTER 0x00000001L
#define VFT2_FONT_VECTOR 0x00000002L
#define VFT2_FONT_TRUETYPE 0x00000003L

#define VFFF_ISSHAREDFILE 0x0001

#define VFF_CURNEDEST 0x0001
#define VFF_FILEINUSE 0x0002
#define VFF_BUFFTOOSMALL 0x0004

#define VIFF_FORCEINSTALL 0x0001
#define VIFF_DONTDELETEOLD 0x0002

#define VIF_TEMPFILE 0x00000001L
#define VIF_MISMATCH 0x00000002L
#define VIF_SRCOLD 0x00000004L

#define VIF_DIFFLANG 0x00000008L
#define VIF_DIFFCODEPG 0x00000010L
#define VIF_DIFFTYPE 0x00000020L

#define VIF_WRITEPROT 0x00000040L
#define VIF_FILEINUSE 0x00000080L
#define VIF_OUTOFSPACE 0x00000100L
#define VIF_ACCESSVIOLATION 0x00000200L
#define VIF_SHARINGVIOLATION 0x00000400L
#define VIF_CANNOTCREATE 0x00000800L
#define VIF_CANNOTDELETE 0x00001000L
#define VIF_CANNOTRENAME 0x00002000L
#define VIF_CANNOTDELETECUR 0x00004000L
#define VIF_OUTOFMEMORY 0x00008000L

#define VIF_CANNOTREADSRC 0x00010000L
#define VIF_CANNOTREADDST 0x00020000L

#define VIF_BUFFTOOSMALL 0x00040000L
#define VIF_CANNOTLOADLZ32 0x00080000L
#define VIF_CANNOTLOADCABINET 0x00100000L

#ifndef RC_INVOKED

  typedef struct tagVS_FIXEDFILEINFO
  {
    DWORD dwSignature;
    DWORD dwStrucVersion;
    DWORD dwFileVersionMS;
    DWORD dwFileVersionLS;
    DWORD dwProductVersionMS;
    DWORD dwProductVersionLS;
    DWORD dwFileFlagsMask;
    DWORD dwFileFlags;
    DWORD dwFileOS;
    DWORD dwFileType;
    DWORD dwFileSubtype;
    DWORD dwFileDateMS;
    DWORD dwFileDateLS;
  } VS_FIXEDFILEINFO;

#ifdef UNICODE
#define VerFindFile VerFindFileW
#define VerInstallFile VerInstallFileW
#define GetFileVersionInfoSize GetFileVersionInfoSizeW
#define GetFileVersionInfo GetFileVersionInfoW
#define VerLanguageName VerLanguageNameW
#define VerQueryValue VerQueryValueW
#else
#define VerFindFile VerFindFileA
#define VerInstallFile VerInstallFileA
#define GetFileVersionInfoSize GetFileVersionInfoSizeA
#define GetFileVersionInfo GetFileVersionInfoA
#define VerLanguageName VerLanguageNameA
#define VerQueryValue VerQueryValueA
#endif

  DWORD WINAPI VerFindFileA(DWORD uFlags,LPSTR szFileName,LPSTR szWinDir,LPSTR szAppDir,LPSTR szCurDir,PUINT lpuCurDirLen,LPSTR szDestDir,PUINT lpuDestDirLen);
  DWORD WINAPI VerFindFileW(DWORD uFlags,LPWSTR szFileName,LPWSTR szWinDir,LPWSTR szAppDir,LPWSTR szCurDir,PUINT lpuCurDirLen,LPWSTR szDestDir,PUINT lpuDestDirLen);
  DWORD WINAPI VerInstallFileA(DWORD uFlags,LPSTR szSrcFileName,LPSTR szDestFileName,LPSTR szSrcDir,LPSTR szDestDir,LPSTR szCurDir,LPSTR szTmpFile,PUINT lpuTmpFileLen);
  DWORD WINAPI VerInstallFileW(DWORD uFlags,LPWSTR szSrcFileName,LPWSTR szDestFileName,LPWSTR szSrcDir,LPWSTR szDestDir,LPWSTR szCurDir,LPWSTR szTmpFile,PUINT lpuTmpFileLen);
  DWORD WINAPI GetFileVersionInfoSizeA(LPCSTR lptstrFilename,LPDWORD lpdwHandle);
  DWORD WINAPI GetFileVersionInfoSizeW(LPCWSTR lptstrFilename,LPDWORD lpdwHandle);
  WINBOOL WINAPI GetFileVersionInfoA(LPCSTR lptstrFilename,DWORD dwHandle,DWORD dwLen,LPVOID lpData);
  WINBOOL WINAPI GetFileVersionInfoW(LPCWSTR lptstrFilename,DWORD dwHandle,DWORD dwLen,LPVOID lpData);
  DWORD WINAPI VerLanguageNameA(DWORD wLang,LPSTR szLang,DWORD nSize);
  DWORD WINAPI VerLanguageNameW(DWORD wLang,LPWSTR szLang,DWORD nSize);
  WINBOOL WINAPI VerQueryValueA(const LPVOID pBlock,LPSTR lpSubBlock,LPVOID *lplpBuffer,PUINT puLen);
  WINBOOL WINAPI VerQueryValueW(const LPVOID pBlock,LPWSTR lpSubBlock,LPVOID *lplpBuffer,PUINT puLen);
#endif

#ifdef __cplusplus
}
#endif
#endif
