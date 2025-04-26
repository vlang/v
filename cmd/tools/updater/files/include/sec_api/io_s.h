/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_IO_S
#define _INC_IO_S

#include <io.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

  _CRTIMP errno_t __cdecl _access_s(const char *_Filename,int _AccessMode);
  _CRTIMP errno_t __cdecl _chsize_s(int _FileHandle,__int64 _Size);
  _CRTIMP errno_t __cdecl _mktemp_s(char *_TemplateName,size_t _Size);
  _CRTIMP errno_t __cdecl _umask_s(int _NewMode,int *_OldMode);

#ifndef _WIO_S_DEFINED
#define _WIO_S_DEFINED
  _CRTIMP errno_t __cdecl _waccess_s(const wchar_t *_Filename,int _AccessMode);
  _CRTIMP errno_t __cdecl _wmktemp_s(wchar_t *_TemplateName,size_t _SizeInWords);
#endif

#ifdef __cplusplus
}
#endif

#endif
#endif
