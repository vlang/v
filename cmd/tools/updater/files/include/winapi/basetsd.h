/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _BASETSD_H_
#define _BASETSD_H_

#if (defined(__x86_64) || defined(__ia64__)) && !defined(RC_INVOKED)
typedef unsigned __int64 POINTER_64_INT;
#else
typedef unsigned long POINTER_64_INT;
#endif

#define POINTER_32
#define POINTER_64
#define FIRMWARE_PTR

#ifdef __cplusplus
extern "C" {
#endif

  typedef signed char INT8,*PINT8;
  typedef signed short INT16,*PINT16;
  typedef signed int INT32,*PINT32;
  typedef signed __int64 INT64,*PINT64;
  typedef unsigned char UINT8,*PUINT8;
  typedef unsigned short UINT16,*PUINT16;
  typedef unsigned int UINT32,*PUINT32;
  typedef unsigned __int64 UINT64,*PUINT64;
  typedef signed int LONG32,*PLONG32;
  typedef unsigned int ULONG32,*PULONG32;
  typedef unsigned int DWORD32,*PDWORD32;

#ifndef _W64
#define _W64
#endif

#ifdef _WIN64
  typedef __int64 INT_PTR,*PINT_PTR;
  typedef unsigned __int64 UINT_PTR,*PUINT_PTR;
  typedef __int64 LONG_PTR,*PLONG_PTR;
  typedef unsigned __int64 ULONG_PTR,*PULONG_PTR;
#define __int3264 __int64
#else
  typedef int INT_PTR,*PINT_PTR;
  typedef unsigned int UINT_PTR,*PUINT_PTR;
  typedef long LONG_PTR,*PLONG_PTR;
  typedef unsigned long ULONG_PTR,*PULONG_PTR;
#define __int3264 __int32
#endif

#ifdef _WIN64
#define ADDRESS_TAG_BIT 0x40000000000ULL
  typedef __int64 SHANDLE_PTR;
  typedef unsigned __int64 HANDLE_PTR;
  typedef unsigned int UHALF_PTR,*PUHALF_PTR;
  typedef int HALF_PTR,*PHALF_PTR;

  static __inline unsigned long HandleToULong(const void *h) { return((unsigned long) (ULONG_PTR) h); }
  static __inline long HandleToLong(const void *h) { return((long) (LONG_PTR) h); }
  static __inline void *ULongToHandle(const unsigned long h) { return((void *) (UINT_PTR) h); }
  static __inline void *LongToHandle(const long h) { return((void *) (INT_PTR) h); }
  static __inline unsigned long PtrToUlong(const void *p) { return((unsigned long) (ULONG_PTR) p); }
  static __inline unsigned int PtrToUint(const void *p) { return((unsigned int) (UINT_PTR) p); }
  static __inline unsigned short PtrToUshort(const void *p) { return((unsigned short) (unsigned long) (ULONG_PTR) p); }
  static __inline long PtrToLong(const void *p) { return((long) (LONG_PTR) p); }
  static __inline int PtrToInt(const void *p) { return((int) (INT_PTR) p); }
  static __inline short PtrToShort(const void *p) { return((short) (long) (LONG_PTR) p); }
  static __inline void *IntToPtr(const int i) { return((void *)(INT_PTR)i); }
  static __inline void *UIntToPtr(const unsigned int ui) { return((void *)(UINT_PTR)ui); }
  static __inline void *LongToPtr(const long l) { return((void *)(LONG_PTR)l); }
  static __inline void *ULongToPtr(const unsigned long ul) { return((void *)(ULONG_PTR)ul); }

#define PtrToPtr64(p) ((void *) p)
#define Ptr64ToPtr(p) ((void *) p)
#define HandleToHandle64(h) (PtrToPtr64(h))
#define Handle64ToHandle(h) (Ptr64ToPtr(h))

  static __inline void *Ptr32ToPtr(const void *p) { return (void *)p; }
  static __inline void *Handle32ToHandle(const void *h) { return((void *) h); }
  static __inline void *PtrToPtr32(const void *p) { return((void *) (ULONG_PTR) p); }

#define HandleToHandle32(h) (PtrToPtr32(h))
#else

#define ADDRESS_TAG_BIT 0x80000000UL

  typedef unsigned short UHALF_PTR,*PUHALF_PTR;
  typedef short HALF_PTR,*PHALF_PTR;
  typedef long SHANDLE_PTR;
  typedef unsigned long HANDLE_PTR;

#define HandleToULong(h) ((ULONG)(ULONG_PTR)(h))
#define HandleToLong(h) ((LONG)(LONG_PTR) (h))
#define ULongToHandle(ul) ((HANDLE)(ULONG_PTR) (ul))
#define LongToHandle(h) ((HANDLE)(LONG_PTR) (h))
#define PtrToUlong(p) ((ULONG)(ULONG_PTR) (p))
#define PtrToLong(p) ((LONG)(LONG_PTR) (p))
#define PtrToUint(p) ((UINT)(UINT_PTR) (p))
#define PtrToInt(p) ((INT)(INT_PTR) (p))
#define PtrToUshort(p) ((unsigned short)(ULONG_PTR)(p))
#define PtrToShort(p) ((short)(LONG_PTR)(p))
#define IntToPtr(i) ((VOID *)(INT_PTR)((int)i))
#define UIntToPtr(ui) ((VOID *)(UINT_PTR)((unsigned int)ui))
#define LongToPtr(l) ((VOID *)(LONG_PTR)((long)l))
#define ULongToPtr(ul) ((VOID *)(ULONG_PTR)((unsigned long)ul))

  static __inline void *PtrToPtr64(const void *p) { return((void *) (ULONG_PTR)p); }
  static __inline void *Ptr64ToPtr(const void *p) { return((void *) (ULONG_PTR) p); }
  static __inline void *HandleToHandle64(const void *h) { return((void *) h); }
  static __inline void *Handle64ToHandle(const void *h) { return((void *) (ULONG_PTR) h); }

#define Ptr32ToPtr(p) ((void *) p)
#define Handle32ToHandle(h) (Ptr32ToPtr(h))
#define PtrToPtr32(p) ((void *) p)
#define HandleToHandle32(h) (PtrToPtr32(h))
#endif

#define HandleToUlong(h) HandleToULong(h)
#define UlongToHandle(ul) ULongToHandle(ul)
#define UlongToPtr(ul) ULongToPtr(ul)
#define UintToPtr(ui) UIntToPtr(ui)

#define MAXUINT_PTR (~((UINT_PTR)0))
#define MAXINT_PTR ((INT_PTR)(MAXUINT_PTR >> 1))
#define MININT_PTR (~MAXINT_PTR)

#define MAXULONG_PTR (~((ULONG_PTR)0))
#define MAXLONG_PTR ((LONG_PTR)(MAXULONG_PTR >> 1))
#define MINLONG_PTR (~MAXLONG_PTR)

#define MAXUHALF_PTR ((UHALF_PTR)~0)
#define MAXHALF_PTR ((HALF_PTR)(MAXUHALF_PTR >> 1))
#define MINHALF_PTR (~MAXHALF_PTR)

  typedef ULONG_PTR SIZE_T,*PSIZE_T;
  typedef LONG_PTR SSIZE_T,*PSSIZE_T;
  typedef ULONG_PTR DWORD_PTR,*PDWORD_PTR;
  typedef __int64 LONG64,*PLONG64;
  typedef unsigned __int64 ULONG64,*PULONG64;
  typedef unsigned __int64 DWORD64,*PDWORD64;
  typedef ULONG_PTR KAFFINITY;
  typedef KAFFINITY *PKAFFINITY;

#ifdef __cplusplus
}
#endif
#endif
