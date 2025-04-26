/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_SEARCH_S
#define _INC_SEARCH_S

#include <search.h>

#if defined(MINGW_HAS_SECURE_API)

#ifdef __cplusplus
extern "C" {
#endif

  _CRTIMP void *__cdecl _lfind_s(const void *_Key,const void *_Base,unsigned int *_NumOfElements,size_t _SizeOfElements,int (__cdecl *_PtFuncCompare)(void *,const void *,const void *),void *_Context);
  _CRTIMP void *__cdecl _lsearch_s(const void *_Key,void *_Base,unsigned int *_NumOfElements,size_t _SizeOfElements,int (__cdecl *_PtFuncCompare)(void *,const void *,const void *),void *_Context);

#ifdef __cplusplus
}
#endif

#endif
#endif
