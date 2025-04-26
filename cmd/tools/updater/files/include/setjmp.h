/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_SETJMP
#define _INC_SETJMP

#include <_mingw.h>

#pragma pack(push,_CRT_PACKING)

#ifdef __cplusplus
extern "C" {
#endif

#if (defined(_X86_) && !defined(__x86_64))

#define _JBLEN 16
#define _JBTYPE int

  typedef struct __JUMP_BUFFER {
    unsigned long Ebp;
    unsigned long Ebx;
    unsigned long Edi;
    unsigned long Esi;
    unsigned long Esp;
    unsigned long Eip;
    unsigned long Registration;
    unsigned long TryLevel;
    unsigned long Cookie;
    unsigned long UnwindFunc;
    unsigned long UnwindData[6];
  } _JUMP_BUFFER;
#elif defined(__ia64__)
  typedef _CRT_ALIGN(16) struct _SETJMP_FLOAT128 {
    __int64 LowPart;
    __int64 HighPart;
  } SETJMP_FLOAT128;

#define _JBLEN 33
  typedef SETJMP_FLOAT128 _JBTYPE;

  typedef struct __JUMP_BUFFER {

    unsigned long iAReserved[6];

    unsigned long Registration;
    unsigned long TryLevel;
    unsigned long Cookie;
    unsigned long UnwindFunc;

    unsigned long UnwindData[6];

    SETJMP_FLOAT128 FltS0;
    SETJMP_FLOAT128 FltS1;
    SETJMP_FLOAT128 FltS2;
    SETJMP_FLOAT128 FltS3;
    SETJMP_FLOAT128 FltS4;
    SETJMP_FLOAT128 FltS5;
    SETJMP_FLOAT128 FltS6;
    SETJMP_FLOAT128 FltS7;
    SETJMP_FLOAT128 FltS8;
    SETJMP_FLOAT128 FltS9;
    SETJMP_FLOAT128 FltS10;
    SETJMP_FLOAT128 FltS11;
    SETJMP_FLOAT128 FltS12;
    SETJMP_FLOAT128 FltS13;
    SETJMP_FLOAT128 FltS14;
    SETJMP_FLOAT128 FltS15;
    SETJMP_FLOAT128 FltS16;
    SETJMP_FLOAT128 FltS17;
    SETJMP_FLOAT128 FltS18;
    SETJMP_FLOAT128 FltS19;
    __int64 FPSR;
    __int64 StIIP;
    __int64 BrS0;
    __int64 BrS1;
    __int64 BrS2;
    __int64 BrS3;
    __int64 BrS4;
    __int64 IntS0;
    __int64 IntS1;
    __int64 IntS2;
    __int64 IntS3;
    __int64 RsBSP;
    __int64 RsPFS;
    __int64 ApUNAT;
    __int64 ApLC;
    __int64 IntSp;
    __int64 IntNats;
    __int64 Preds;

  } _JUMP_BUFFER;
#elif defined(__x86_64)
  typedef _CRT_ALIGN(16) struct _SETJMP_FLOAT128 {
    unsigned __int64 Part[2];
  } SETJMP_FLOAT128;

#define _JBLEN 16
  typedef SETJMP_FLOAT128 _JBTYPE;

  typedef struct _JUMP_BUFFER {
    unsigned __int64 Frame;
    unsigned __int64 Rbx;
    unsigned __int64 Rsp;
    unsigned __int64 Rbp;
    unsigned __int64 Rsi;
    unsigned __int64 Rdi;
    unsigned __int64 R12;
    unsigned __int64 R13;
    unsigned __int64 R14;
    unsigned __int64 R15;
    unsigned __int64 Rip;
    unsigned __int64 Spare;
    SETJMP_FLOAT128 Xmm6;
    SETJMP_FLOAT128 Xmm7;
    SETJMP_FLOAT128 Xmm8;
    SETJMP_FLOAT128 Xmm9;
    SETJMP_FLOAT128 Xmm10;
    SETJMP_FLOAT128 Xmm11;
    SETJMP_FLOAT128 Xmm12;
    SETJMP_FLOAT128 Xmm13;
    SETJMP_FLOAT128 Xmm14;
    SETJMP_FLOAT128 Xmm15;
  } _JUMP_BUFFER;
#endif
#ifndef _JMP_BUF_DEFINED
  typedef _JBTYPE jmp_buf[_JBLEN];
#define _JMP_BUF_DEFINED
#endif

  void * __cdecl __attribute__ ((__nothrow__)) mingw_getsp(void);

#ifdef USE_MINGW_SETJMP_TWO_ARGS
#ifndef _INC_SETJMPEX
#define setjmp(BUF) _setjmp((BUF),mingw_getsp())
  int __cdecl __attribute__ ((__nothrow__)) _setjmp(jmp_buf _Buf,void *_Ctx);
#else
#undef setjmp
#define setjmp(BUF) _setjmpex((BUF),mingw_getsp())
#define setjmpex(BUF) _setjmpex((BUF),mingw_getsp())
  int __cdecl __attribute__ ((__nothrow__)) _setjmpex(jmp_buf _Buf,void *_Ctx);
#endif
#else
#ifndef _INC_SETJMPEX
#define setjmp _setjmp
#endif
  int __cdecl __attribute__ ((__nothrow__)) setjmp(jmp_buf _Buf);
#endif

  __declspec(noreturn) __attribute__ ((__nothrow__)) void __cdecl ms_longjmp(jmp_buf _Buf,int _Value)/* throw(...)*/;
  __declspec(noreturn) __attribute__ ((__nothrow__)) void __cdecl longjmp(jmp_buf _Buf,int _Value);

#ifdef __cplusplus
}
#endif

#pragma pack(pop)
#endif
