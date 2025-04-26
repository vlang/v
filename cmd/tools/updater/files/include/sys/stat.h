/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_STAT
#define _INC_STAT

#ifndef _WIN32
#error Only Win32 target is supported!
#endif

#include <_mingw.h>
#include <io.h>

#pragma pack(push,_CRT_PACKING)

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _CRTIMP
#define _CRTIMP __declspec(dllimport)
#endif

#include <sys/types.h>

#ifndef __TINYC__ /* gr */
#ifdef _USE_32BIT_TIME_T
#ifdef _WIN64
#undef _USE_32BIT_TIME_T
#endif
#else
#if _INTEGRAL_MAX_BITS < 64
#define _USE_32BIT_TIME_T
#endif
#endif
#endif

#ifndef _TIME32_T_DEFINED
  typedef long __time32_t;
#define _TIME32_T_DEFINED
#endif

#ifndef _TIME64_T_DEFINED
#if _INTEGRAL_MAX_BITS >= 64
  typedef __int64 __time64_t;
#endif
#define _TIME64_T_DEFINED
#endif

#ifndef _TIME_T_DEFINED
#ifdef _USE_32BIT_TIME_T
  typedef __time32_t time_t;
#else
  typedef __time64_t time_t;
#endif
#define _TIME_T_DEFINED
#endif

#ifndef _WCHAR_T_DEFINED
  typedef unsigned short wchar_t;
#define _WCHAR_T_DEFINED
#endif

#ifndef _STAT_DEFINED

#ifdef _USE_32BIT_TIME_T
#ifndef _WIN64
#define _fstat32 _fstat
#define _stat32 _stat
#define _wstat32 _wstat
#else
#define _fstat _fstat32
#define _stat _stat32
#define _wstat _wstat32
#endif
#define _fstati64 _fstat32i64
#define _stati64 _stat32i64
#define _wstati64 _wstat32i64
#else
#define _fstat _fstat64i32
#define _fstati64 _fstat64
#define _stat _stat64
#define _stati64 _stat64
#define _wstat _wstat64
#define _wstati64 _wstat64
#endif

  struct _stat32 {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    _off_t st_size;
    __time32_t st_atime;
    __time32_t st_mtime;
    __time32_t st_ctime;
  };

#ifndef	NO_OLDNAMES
  struct stat {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    _off_t st_size;
    time_t st_atime;
    time_t st_mtime;
    time_t st_ctime;
  };
#endif

#if _INTEGRAL_MAX_BITS >= 64
  struct _stat32i64 {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    __int64 st_size;
    __time32_t st_atime;
    __time32_t st_mtime;
    __time32_t st_ctime;
  };

  struct _stat64i32 {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    _off_t st_size;
    __time64_t st_atime;
    __time64_t st_mtime;
    __time64_t st_ctime;
  };

  struct _stat64 {
    _dev_t st_dev;
    _ino_t st_ino;
    unsigned short st_mode;
    short st_nlink;
    short st_uid;
    short st_gid;
    _dev_t st_rdev;
    __int64 st_size;
    __time64_t st_atime;
    __time64_t st_mtime;
    __time64_t st_ctime;
  };
#endif

#define __stat64 _stat64

#define _STAT_DEFINED
#endif

#define _S_IFMT 0xF000
#define _S_IFDIR 0x4000
#define _S_IFCHR 0x2000
#define _S_IFIFO 0x1000
#define _S_IFREG 0x8000
#define _S_IREAD 0x0100
#define _S_IWRITE 0x0080
#define _S_IEXEC 0x0040

  _CRTIMP int __cdecl _fstat32(int _FileDes,struct _stat32 *_Stat);
  _CRTIMP int __cdecl _stat32(const char *_Name,struct _stat32 *_Stat);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP int __cdecl _fstat64(int _FileDes,struct _stat64 *_Stat);
  _CRTIMP int __cdecl _fstat32i64(int _FileDes,struct _stat32i64 *_Stat);
  int __cdecl _fstat64i32(int _FileDes,struct _stat64i32 *_Stat);
  __CRT_INLINE int __cdecl _fstat64i32(int _FileDes,struct _stat64i32 *_Stat)
  {
    struct _stat64 st;
    int ret=_fstat64(_FileDes,&st);
    _Stat->st_dev=st.st_dev;
    _Stat->st_ino=st.st_ino;
    _Stat->st_mode=st.st_mode;
    _Stat->st_nlink=st.st_nlink;
    _Stat->st_uid=st.st_uid;
    _Stat->st_gid=st.st_gid;
    _Stat->st_rdev=st.st_rdev;
    _Stat->st_size=(_off_t) st.st_size;
    _Stat->st_atime=st.st_atime;
    _Stat->st_mtime=st.st_mtime;
    _Stat->st_ctime=st.st_ctime;
    return ret;
  }
  _CRTIMP int __cdecl _stat64(const char *_Name,struct _stat64 *_Stat);
  _CRTIMP int __cdecl _stat32i64(const char *_Name,struct _stat32i64 *_Stat);
  int __cdecl _stat64i32(const char *_Name,struct _stat64i32 *_Stat);
  __CRT_INLINE int __cdecl _stat64i32(const char *_Name,struct _stat64i32 *_Stat)
  {
    struct _stat64 st;
    int ret=_stat64(_Name,&st);
    _Stat->st_dev=st.st_dev;
    _Stat->st_ino=st.st_ino;
    _Stat->st_mode=st.st_mode;
    _Stat->st_nlink=st.st_nlink;
    _Stat->st_uid=st.st_uid;
    _Stat->st_gid=st.st_gid;
    _Stat->st_rdev=st.st_rdev;
    _Stat->st_size=(_off_t) st.st_size;
    _Stat->st_atime=st.st_atime;
    _Stat->st_mtime=st.st_mtime;
    _Stat->st_ctime=st.st_ctime;
    return ret;
  }
#endif

#ifndef _WSTAT_DEFINED
#define _WSTAT_DEFINED
  _CRTIMP int __cdecl _wstat32(const wchar_t *_Name,struct _stat32 *_Stat);
#if _INTEGRAL_MAX_BITS >= 64
  _CRTIMP int __cdecl _wstat32i64(const wchar_t *_Name,struct _stat32i64 *_Stat);
  int __cdecl _wstat64i32(const wchar_t *_Name,struct _stat64i32 *_Stat);
  _CRTIMP int __cdecl _wstat64(const wchar_t *_Name,struct _stat64 *_Stat);
#endif
#endif

#ifndef	NO_OLDNAMES
#define	_S_IFBLK	0x3000	/* Block: Is this ever set under w32? */

#define S_IFMT _S_IFMT
#define S_IFDIR _S_IFDIR
#define S_IFCHR _S_IFCHR
#define S_IFREG _S_IFREG
#define S_IREAD _S_IREAD
#define S_IWRITE _S_IWRITE
#define S_IEXEC _S_IEXEC
#define	S_IFIFO		_S_IFIFO
#define	S_IFBLK		_S_IFBLK

#define	_S_IRWXU	(_S_IREAD | _S_IWRITE | _S_IEXEC)
#define	_S_IXUSR	_S_IEXEC
#define	_S_IWUSR	_S_IWRITE

#define	S_IRWXU		_S_IRWXU
#define	S_IXUSR		_S_IXUSR
#define	S_IWUSR		_S_IWUSR
#define	S_IRUSR		_S_IRUSR
#define	_S_IRUSR	_S_IREAD

#define	S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#define	S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)
#define	S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define	S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define	S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)

#endif

#if !defined (RC_INVOKED) && !defined (NO_OLDNAMES)
int __cdecl stat(const char *_Filename,struct stat *_Stat);
int __cdecl fstat(int _Desc,struct stat *_Stat);
int __cdecl wstat(const wchar_t *_Filename,struct stat *_Stat);
#ifdef _USE_32BIT_TIME_T
__CRT_INLINE int __cdecl fstat(int _Desc,struct stat *_Stat) {
  return _fstat32(_Desc,(struct _stat32 *)_Stat);
}
__CRT_INLINE int __cdecl stat(const char *_Filename,struct stat *_Stat) {
  return _stat32(_Filename,(struct _stat32 *)_Stat);
}
#else
__CRT_INLINE int __cdecl fstat(int _Desc,struct stat *_Stat) {
  return _fstat64i32(_Desc,(struct _stat64i32 *)_Stat);
}
__CRT_INLINE int __cdecl stat(const char *_Filename,struct stat *_Stat) {
  return _stat64i32(_Filename,(struct _stat64i32 *)_Stat);
}
#endif
#endif

#ifdef __cplusplus
}
#endif

#pragma pack(pop)
#endif
