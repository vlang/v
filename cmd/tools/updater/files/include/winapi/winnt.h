/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _WINNT_
#define _WINNT_

#ifdef __cplusplus
extern "C" {
#endif

#include <ctype.h>
#define ANYSIZE_ARRAY 1

//gr #include <specstrings.h>

#define RESTRICTED_POINTER

#ifndef __CRT_UNALIGNED
#define __CRT_UNALIGNED
#endif

#if defined(__ia64__) || defined(__x86_64)
#define UNALIGNED __CRT_UNALIGNED
#ifdef _WIN64
#define UNALIGNED64 __CRT_UNALIGNED
#else
#define UNALIGNED64
#endif
#else
#define UNALIGNED
#define UNALIGNED64
#endif

#if !defined(I_X86_) && !defined(_IA64_) && !defined(_AMD64_) && (defined(_X86_) && !defined(__x86_64))
#define I_X86_
#endif

#if !defined(I_X86_) && !defined(_IA64_) && !defined(_AMD64_) && defined(__x86_64)
#define _AMD64_
#endif

#if !defined(I_X86_) && !(defined(_X86_) && !defined(__x86_64)) && !defined(_AMD64_) && defined(__ia64__)
#if !defined(_IA64_)
#define _IA64_
#endif
#endif


#ifdef _WIN64
#define MAX_NATURAL_ALIGNMENT sizeof(ULONGLONG)
#define MEMORY_ALLOCATION_ALIGNMENT 16
#else
#define MAX_NATURAL_ALIGNMENT sizeof(DWORD)
#define MEMORY_ALLOCATION_ALIGNMENT 8
#endif

#ifdef __cplusplus
#define TYPE_ALIGNMENT(t) __alignof__ (t)
#else
#define TYPE_ALIGNMENT(t) FIELD_OFFSET(struct { char x; t test; },test)
#endif

#ifdef _WIN64
#ifdef _AMD64_
#define PROBE_ALIGNMENT(_s) TYPE_ALIGNMENT(DWORD)
#elif defined(_IA64_)
#define PROBE_ALIGNMENT(_s) (TYPE_ALIGNMENT(_s) > TYPE_ALIGNMENT(DWORD) ? TYPE_ALIGNMENT(_s) : TYPE_ALIGNMENT(DWORD))
#else
#error No Target Architecture
#endif
#define PROBE_ALIGNMENT32(_s) TYPE_ALIGNMENT(DWORD)
#else
#define PROBE_ALIGNMENT(_s) TYPE_ALIGNMENT(DWORD)
#endif

#define C_ASSERT(e) typedef char __C_ASSERT__[(e)?1:-1]

#include <basetsd.h>

#if defined(_X86_) || defined(__ia64__) || defined(__x86_64)
#define DECLSPEC_IMPORT __declspec(dllimport)
#else
#define DECLSPEC_IMPORT
#endif

#ifndef DECLSPEC_NORETURN
#define DECLSPEC_NORETURN __declspec(noreturn)
#endif

#ifndef DECLSPEC_ALIGN
#define DECLSPEC_ALIGN(x) __attribute__ ((aligned(x)))
#endif

#ifndef SYSTEM_CACHE_ALIGNMENT_SIZE
#if defined(_AMD64_) || defined(I_X86_)
#define SYSTEM_CACHE_ALIGNMENT_SIZE 64
#else
#define SYSTEM_CACHE_ALIGNMENT_SIZE 128
#endif
#endif

#ifndef DECLSPEC_CACHEALIGN
#define DECLSPEC_CACHEALIGN DECLSPEC_ALIGN(SYSTEM_CACHE_ALIGNMENT_SIZE)
#endif

#ifndef DECLSPEC_UUID
#define DECLSPEC_UUID(x)
#endif

#ifndef DECLSPEC_NOVTABLE
#define DECLSPEC_NOVTABLE
#endif

#ifndef DECLSPEC_SELECTANY
#define DECLSPEC_SELECTANY __declspec(selectany)
#endif

#ifndef NOP_FUNCTION
#define NOP_FUNCTION (void)0
#endif

#ifndef DECLSPEC_NOINLINE
#define DECLSPEC_NOINLINE
#endif

#ifndef FORCEINLINE
#define FORCEINLINE static __inline__
#endif

#ifndef DECLSPEC_DEPRECATED
#define DECLSPEC_DEPRECATED __declspec(deprecated)
#define DEPRECATE_SUPPORTED
#endif

#define DECLSPEC_DEPRECATED_DDK
#define PRAGMA_DEPRECATED_DDK 0

  typedef void *PVOID;
  typedef void *PVOID64;

#define NTAPI __stdcall
#define NTSYSAPI DECLSPEC_IMPORT
#define NTSYSCALLAPI DECLSPEC_IMPORT

#ifndef VOID
#define VOID void
  typedef char CHAR;
  typedef short SHORT;
  typedef long LONG;
#endif

  typedef wchar_t WCHAR;
  typedef WCHAR *PWCHAR,*LPWCH,*PWCH;
  typedef CONST WCHAR *LPCWCH,*PCWCH;
  typedef WCHAR *NWPSTR,*LPWSTR,*PWSTR;
  typedef PWSTR *PZPWSTR;
  typedef CONST PWSTR *PCZPWSTR;
  typedef WCHAR UNALIGNED *LPUWSTR,*PUWSTR;
  typedef CONST WCHAR *LPCWSTR,*PCWSTR;
  typedef PCWSTR *PZPCWSTR;
  typedef CONST WCHAR UNALIGNED *LPCUWSTR,*PCUWSTR;
  typedef CHAR *PCHAR,*LPCH,*PCH;
  typedef CONST CHAR *LPCCH,*PCCH;
  typedef CHAR *NPSTR,*LPSTR,*PSTR;
  typedef PSTR *PZPSTR;
  typedef CONST PSTR *PCZPSTR;
  typedef CONST CHAR *LPCSTR,*PCSTR;
  typedef PCSTR *PZPCSTR;

#ifdef UNICODE
#ifndef _TCHAR_DEFINED
#define _TCHAR_DEFINED
  typedef WCHAR TCHAR,*PTCHAR;
  typedef WCHAR TBYTE ,*PTBYTE;
#endif

  typedef LPWSTR LPTCH,PTCH;
  typedef LPWSTR PTSTR,LPTSTR;
  typedef LPCWSTR PCTSTR,LPCTSTR;
  typedef LPUWSTR PUTSTR,LPUTSTR;
  typedef LPCUWSTR PCUTSTR,LPCUTSTR;
  typedef LPWSTR LP;
#define __TEXT(quote) L##quote
#else
#ifndef _TCHAR_DEFINED
#define _TCHAR_DEFINED
  typedef char TCHAR,*PTCHAR;
  typedef unsigned char TBYTE ,*PTBYTE;
#endif

  typedef LPSTR LPTCH,PTCH;
  typedef LPSTR PTSTR,LPTSTR,PUTSTR,LPUTSTR;
  typedef LPCSTR PCTSTR,LPCTSTR,PCUTSTR,LPCUTSTR;
#define __TEXT(quote) quote
#endif

#define TEXT(quote) __TEXT(quote)

  typedef SHORT *PSHORT;
  typedef LONG *PLONG;

  typedef void *HANDLE;
#define DECLARE_HANDLE(name) struct name##__ { int unused; }; typedef struct name##__ *name
  typedef HANDLE *PHANDLE;

  typedef BYTE FCHAR;
  typedef WORD FSHORT;
  typedef DWORD FLONG;

#ifndef _HRESULT_DEFINED
#define _HRESULT_DEFINED
  typedef LONG HRESULT;
#endif

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C extern
#endif

#define STDMETHODCALLTYPE WINAPI
#define STDMETHODVCALLTYPE __cdecl
#define STDAPICALLTYPE WINAPI
#define STDAPIVCALLTYPE __cdecl
#define STDAPI EXTERN_C HRESULT WINAPI
#define STDAPI_(type) EXTERN_C type WINAPI
#define STDMETHODIMP HRESULT WINAPI
#define STDMETHODIMP_(type) type WINAPI
#define STDAPIV EXTERN_C HRESULT STDAPIVCALLTYPE
#define STDAPIV_(type) EXTERN_C type STDAPIVCALLTYPE
#define STDMETHODIMPV HRESULT STDMETHODVCALLTYPE
#define STDMETHODIMPV_(type) type STDMETHODVCALLTYPE

  typedef char CCHAR;
#ifndef _LCID_DEFINED
#define _LCID_DEFINED
typedef DWORD LCID;
#endif
  typedef PDWORD PLCID;
#ifndef _LANGID_DEFINED
#define _LANGID_DEFINED
  typedef WORD LANGID;
#endif
#define APPLICATION_ERROR_MASK 0x20000000
#define ERROR_SEVERITY_SUCCESS 0x00000000
#define ERROR_SEVERITY_INFORMATIONAL 0x40000000
#define ERROR_SEVERITY_WARNING 0x80000000
#define ERROR_SEVERITY_ERROR 0xC0000000

#ifdef __ia64__
  __declspec(align(16))
#endif
    typedef struct _FLOAT128 {
      __int64 LowPart;
      __int64 HighPart;
  } FLOAT128;

  typedef FLOAT128 *PFLOAT128;

#define _ULONGLONG_
#if((!(defined(_X86_) && !defined(__x86_64)) || (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 64)))
  typedef __int64 LONGLONG;
  typedef unsigned __int64 ULONGLONG;

#define MAXLONGLONG (0x7fffffffffffffff)
#else

  typedef double LONGLONG;
  typedef double ULONGLONG;
#endif

  typedef LONGLONG *PLONGLONG;
  typedef ULONGLONG *PULONGLONG;

  typedef LONGLONG USN;

  typedef union _LARGE_INTEGER {
    struct {
      DWORD LowPart;
      LONG HighPart;
    };
    struct {
      DWORD LowPart;
      LONG HighPart;
    } u;
    LONGLONG QuadPart;
  } LARGE_INTEGER;

  typedef LARGE_INTEGER *PLARGE_INTEGER;

  typedef union _ULARGE_INTEGER {
    struct {
      DWORD LowPart;
      DWORD HighPart;
    };
    struct {
      DWORD LowPart;
      DWORD HighPart;
    } u;
    ULONGLONG QuadPart;
  } ULARGE_INTEGER;

  typedef ULARGE_INTEGER *PULARGE_INTEGER;

  typedef struct _LUID {
    DWORD LowPart;
    LONG HighPart;
  } LUID,*PLUID;

#define _DWORDLONG_
  typedef ULONGLONG DWORDLONG;
  typedef DWORDLONG *PDWORDLONG;

#ifdef RC_INVOKED
#define Int32x32To64(a,b) ((LONGLONG)((LONG)(a)) *(LONGLONG)((LONG)(b)))
#define UInt32x32To64(a,b) ((ULONGLONG)((DWORD)(a)) *(ULONGLONG)((DWORD)(b)))
#define Int64ShrlMod32(a,b) ((ULONGLONG)(a) >> (b))
#elif (defined(_X86_) && !defined(__x86_64))
#define Int32x32To64(a,b) (LONGLONG)((LONGLONG)(LONG)(a) *(LONG)(b))
#define UInt32x32To64(a,b) (ULONGLONG)((ULONGLONG)(DWORD)(a) *(DWORD)(b))
#define Int64ShrlMod32(a,b) ((DWORDLONG)(a)>>(b))
#elif defined(__ia64__) || defined(__x86_64)
#define Int32x32To64(a,b) ((LONGLONG)((LONG)(a)) *(LONGLONG)((LONG)(b)))
#define UInt32x32To64(a,b) ((ULONGLONG)((DWORD)(a)) *(ULONGLONG)((DWORD)(b)))
#define Int64ShrlMod32(a,b) ((ULONGLONG)(a) >> (b))
#else
#error Must define a target architecture.
#endif

#define Int64ShraMod32(a,b) ((LONGLONG)(a) >> (b))
#define Int64ShllMod32(a,b) ((ULONGLONG)(a) << (b))

#ifdef __cplusplus
  extern "C" {
#endif

#ifdef __x86_64

#define RotateLeft8 _rotl8
#define RotateLeft16 _rotl16
#define RotateRight8 _rotr8
#define RotateRight16 _rotr16

    unsigned char __cdecl _rotl8(unsigned char Value,unsigned char Shift);
    unsigned short __cdecl _rotl16(unsigned short Value,unsigned char Shift);
    unsigned char __cdecl _rotr8(unsigned char Value,unsigned char Shift);
    unsigned short __cdecl _rotr16(unsigned short Value,unsigned char Shift);
#endif

#define RotateLeft32 _rotl
#define RotateLeft64 _rotl64
#define RotateRight32 _rotr
#define RotateRight64 _rotr64

    unsigned int __cdecl _rotl(unsigned int Value,int Shift);
    unsigned __int64 __cdecl _rotl64(unsigned __int64 Value,int Shift);
    unsigned int __cdecl _rotr(unsigned int Value,int Shift);
    unsigned __int64 __cdecl _rotr64(unsigned __int64 Value,int Shift);
#ifdef __cplusplus
  }
#endif

#define ANSI_NULL ((CHAR)0)
#define UNICODE_NULL ((WCHAR)0)
#define UNICODE_STRING_MAX_BYTES ((WORD) 65534)
#define UNICODE_STRING_MAX_CHARS (32767)

#ifndef _BOOLEAN_
#define _BOOLEAN_
  typedef BYTE BOOLEAN;
#endif
  typedef BOOLEAN *PBOOLEAN;

  typedef struct _LIST_ENTRY {
    struct _LIST_ENTRY *Flink;
    struct _LIST_ENTRY *Blink;
  } LIST_ENTRY,*PLIST_ENTRY,*RESTRICTED_POINTER PRLIST_ENTRY;

  typedef struct _SINGLE_LIST_ENTRY {
    struct _SINGLE_LIST_ENTRY *Next;
  } SINGLE_LIST_ENTRY,*PSINGLE_LIST_ENTRY;

  typedef struct LIST_ENTRY32 {
    DWORD Flink;
    DWORD Blink;
  } LIST_ENTRY32;
  typedef LIST_ENTRY32 *PLIST_ENTRY32;

  typedef struct LIST_ENTRY64 {
    ULONGLONG Flink;
    ULONGLONG Blink;
  } LIST_ENTRY64;
  typedef LIST_ENTRY64 *PLIST_ENTRY64;

#include <guiddef.h>

#ifndef __OBJECTID_DEFINED
#define __OBJECTID_DEFINED
  typedef struct _OBJECTID {
    GUID Lineage;
    DWORD Uniquifier;
  } OBJECTID;
#endif

#define MINCHAR 0x80
#define MAXCHAR 0x7f
#define MINSHORT 0x8000
#define MAXSHORT 0x7fff
#define MINLONG 0x80000000
#define MAXLONG 0x7fffffff
#define MAXBYTE 0xff
#define MAXWORD 0xffff
#define MAXDWORD 0xffffffff

#define FIELD_OFFSET(type,field) ((LONG)(LONG_PTR)&(((type *)0)->field))
#define RTL_FIELD_SIZE(type,field) (sizeof(((type *)0)->field))
#define RTL_SIZEOF_THROUGH_FIELD(type,field) (FIELD_OFFSET(type,field) + RTL_FIELD_SIZE(type,field))
#define RTL_CONTAINS_FIELD(Struct,Size,Field) ((((PCHAR)(&(Struct)->Field)) + sizeof((Struct)->Field)) <= (((PCHAR)(Struct))+(Size)))
#define RTL_NUMBER_OF_V1(A) (sizeof(A)/sizeof((A)[0]))
#define RTL_NUMBER_OF_V2(A) RTL_NUMBER_OF_V1(A)

#ifdef ENABLE_RTL_NUMBER_OF_V2
#define RTL_NUMBER_OF(A) RTL_NUMBER_OF_V2(A)
#else
#define RTL_NUMBER_OF(A) RTL_NUMBER_OF_V1(A)
#endif

#define ARRAYSIZE(A) RTL_NUMBER_OF_V2(A)
#define _ARRAYSIZE(A) RTL_NUMBER_OF_V1(A)

#define RTL_FIELD_TYPE(type,field) (((type*)0)->field)
#define RTL_NUMBER_OF_FIELD(type,field) (RTL_NUMBER_OF(RTL_FIELD_TYPE(type,field)))
#define RTL_PADDING_BETWEEN_FIELDS(T,F1,F2) ((FIELD_OFFSET(T,F2) > FIELD_OFFSET(T,F1)) ? (FIELD_OFFSET(T,F2) - FIELD_OFFSET(T,F1) - RTL_FIELD_SIZE(T,F1)) : (FIELD_OFFSET(T,F1) - FIELD_OFFSET(T,F2) - RTL_FIELD_SIZE(T,F2)))

#ifdef __cplusplus
#define RTL_CONST_CAST(type) const_cast<type>
#else
#define RTL_CONST_CAST(type) (type)
#endif

#define RTL_BITS_OF(sizeOfArg) (sizeof(sizeOfArg) *8)
#define RTL_BITS_OF_FIELD(type,field) (RTL_BITS_OF(RTL_FIELD_TYPE(type,field)))
#define CONTAINING_RECORD(address,type,field) ((type *)((PCHAR)(address) - (ULONG_PTR)(&((type *)0)->field)))

#define VER_SERVER_NT 0x80000000
#define VER_WORKSTATION_NT 0x40000000
#define VER_SUITE_SMALLBUSINESS 0x00000001
#define VER_SUITE_ENTERPRISE 0x00000002
#define VER_SUITE_BACKOFFICE 0x00000004
#define VER_SUITE_COMMUNICATIONS 0x00000008
#define VER_SUITE_TERMINAL 0x00000010
#define VER_SUITE_SMALLBUSINESS_RESTRICTED 0x00000020
#define VER_SUITE_EMBEDDEDNT 0x00000040
#define VER_SUITE_DATACENTER 0x00000080
#define VER_SUITE_SINGLEUSERTS 0x00000100
#define VER_SUITE_PERSONAL 0x00000200
#define VER_SUITE_BLADE 0x00000400
#define VER_SUITE_EMBEDDED_RESTRICTED 0x00000800
#define VER_SUITE_SECURITY_APPLIANCE 0x00001000
#define VER_SUITE_STORAGE_SERVER 0x00002000
#define VER_SUITE_COMPUTE_SERVER 0x00004000

#define PRODUCT_UNDEFINED                       0x0

#define PRODUCT_ULTIMATE                        0x1
#define PRODUCT_HOME_BASIC                      0x2
#define PRODUCT_HOME_PREMIUM                    0x3
#define PRODUCT_ENTERPRISE                      0x4
#define PRODUCT_HOME_BASIC_N                    0x5
#define PRODUCT_BUSINESS                        0x6
#define PRODUCT_STANDARD_SERVER                 0x7
#define PRODUCT_DATACENTER_SERVER               0x8
#define PRODUCT_SMALLBUSINESS_SERVER            0x9
#define PRODUCT_ENTERPRISE_SERVER               0xa
#define PRODUCT_STARTER                         0xb
#define PRODUCT_DATACENTER_SERVER_CORE          0xc
#define PRODUCT_STANDARD_SERVER_CORE            0xd
#define PRODUCT_ENTERPRISE_SERVER_CORE          0xe
#define PRODUCT_ENTERPRISE_SERVER_IA64          0xf
#define PRODUCT_BUSINESS_N                      0x10
#define PRODUCT_WEB_SERVER                      0x11
#define PRODUCT_CLUSTER_SERVER                  0x12
#define PRODUCT_HOME_SERVER                     0x13
#define PRODUCT_STORAGE_EXPRESS_SERVER          0x14
#define PRODUCT_STORAGE_STANDARD_SERVER         0x15
#define PRODUCT_STORAGE_WORKGROUP_SERVER        0x16
#define PRODUCT_STORAGE_ENTERPRISE_SERVER       0x17
#define PRODUCT_SERVER_FOR_SMALLBUSINESS        0x18
#define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM    0x19

#define PRODUCT_UNLICENSED                      0xabcdabcd

#define LANG_NEUTRAL 0x00
#define LANG_INVARIANT 0x7f

#define LANG_AFRIKAANS 0x36
#define LANG_ALBANIAN 0x1c
#define LANG_ALSATIAN 0x84
#define LANG_AMHARIC 0x5e
#define LANG_ARABIC 0x01
#define LANG_ARMENIAN 0x2b
#define LANG_ASSAMESE 0x4d
#define LANG_AZERI 0x2c
#define LANG_BASHKIR 0x6d
#define LANG_BASQUE 0x2d
#define LANG_BELARUSIAN 0x23
#define LANG_BENGALI 0x45
#define LANG_BRETON 0x7e
#define LANG_BOSNIAN 0x1a
#define LANG_BOSNIAN_NEUTRAL 0x781a
#define LANG_BULGARIAN 0x02
#define LANG_CATALAN 0x03
#define LANG_CHINESE 0x04
#define LANG_CHINESE_SIMPLIFIED 0x04
#define LANG_CHINESE_TRADITIONAL 0x7c04
#define LANG_CORSICAN 0x83
#define LANG_CROATIAN 0x1a
#define LANG_CZECH 0x05
#define LANG_DANISH 0x06
#define LANG_DARI 0x8c
#define LANG_DIVEHI 0x65
#define LANG_DUTCH 0x13
#define LANG_ENGLISH 0x09
#define LANG_ESTONIAN 0x25
#define LANG_FAEROESE 0x38
#define LANG_FARSI 0x29
#define LANG_FILIPINO 0x64
#define LANG_FINNISH 0x0b
#define LANG_FRENCH 0x0c
#define LANG_FRISIAN 0x62
#define LANG_GALICIAN 0x56
#define LANG_GEORGIAN 0x37
#define LANG_GERMAN 0x07
#define LANG_GREEK 0x08
#define LANG_GREENLANDIC 0x6f
#define LANG_GUJARATI 0x47
#define LANG_HAUSA 0x68
#define LANG_HEBREW 0x0d
#define LANG_HINDI 0x39
#define LANG_HUNGARIAN 0x0e
#define LANG_ICELANDIC 0x0f
#define LANG_IGBO 0x70
#define LANG_INDONESIAN 0x21
#define LANG_INUKTITUT 0x5d
#define LANG_IRISH 0x3c
#define LANG_ITALIAN 0x10
#define LANG_JAPANESE 0x11
#define LANG_KANNADA 0x4b
#define LANG_KASHMIRI 0x60
#define LANG_KAZAK 0x3f
#define LANG_KHMER 0x53
#define LANG_KICHE 0x86
#define LANG_KINYARWANDA 0x87
#define LANG_KONKANI 0x57
#define LANG_KOREAN 0x12
#define LANG_KYRGYZ 0x40
#define LANG_LAO 0x54
#define LANG_LATVIAN 0x26
#define LANG_LITHUANIAN 0x27
#define LANG_LOWER_SORBIAN 0x2e
#define LANG_LUXEMBOURGISH 0x6e
#define LANG_MACEDONIAN 0x2f
#define LANG_MALAY 0x3e
#define LANG_MALAYALAM 0x4c
#define LANG_MALTESE 0x3a
#define LANG_MANIPURI 0x58
#define LANG_MAORI 0x81
#define LANG_MAPUDUNGUN 0x7a
#define LANG_MARATHI 0x4e
#define LANG_MOHAWK 0x7c
#define LANG_MONGOLIAN 0x50
#define LANG_NEPALI 0x61
#define LANG_NORWEGIAN 0x14
#define LANG_OCCITAN 0x82
#define LANG_ORIYA 0x48
#define LANG_PASHTO 0x63
#define LANG_PERSIAN 0x29
#define LANG_POLISH 0x15
#define LANG_PORTUGUESE 0x16
#define LANG_PUNJABI 0x46
#define LANG_QUECHUA 0x6b
#define LANG_ROMANIAN 0x18
#define LANG_RUSSIAN 0x19
#define LANG_SAMI 0x3b
#define LANG_ROMANSH 0x17
#define LANG_SANSKRIT 0x4f
#define LANG_SERBIAN 0x1a
#define LANG_SERBIAN_NEUTRAL 0x7c1a
#define LANG_SINDHI 0x59
#define LANG_SINHALESE 0x5b
#define LANG_SLOVAK 0x1b
#define LANG_SLOVENIAN 0x24
#define LANG_SOTHO 0x6c
#define LANG_SPANISH 0x0a
#define LANG_SWAHILI 0x41
#define LANG_SWEDISH 0x1d
#define LANG_SYRIAC 0x5a
#define LANG_TAJIK 0x28
#define LANG_TAMAZIGHT 0x5f
#define LANG_TAMIL 0x49
#define LANG_TATAR 0x44
#define LANG_TELUGU 0x4a
#define LANG_THAI 0x1e
#define LANG_TIBETAN 0x51
#define LANG_TIGRIGNA 0x73
#define LANG_TSWANA 0x32
#define LANG_TURKISH 0x1f
#define LANG_TURKMEN 0x42
#define LANG_UIGHUR 0x80
#define LANG_UKRAINIAN 0x22
#define LANG_UPPER_SORBIAN 0x2e
#define LANG_URDU 0x20
#define LANG_UZBEK 0x43
#define LANG_VIETNAMESE 0x2a
#define LANG_WELSH 0x52
#define LANG_WOLOF 0x88
#define LANG_XHOSA 0x34
#define LANG_YAKUT 0x85
#define LANG_YI 0x78
#define LANG_YORUBA 0x6a
#define LANG_ZULU 0x35

#define SUBLANG_NEUTRAL 0x0
#define SUBLANG_DEFAULT 0x1
#define SUBLANG_SYS_DEFAULT 0x2
#define SUBLANG_CUSTOM_DEFAULT 0x3
#define SUBLANG_CUSTOM_UNSPECIFIED 0x4
#define SUBLANG_UI_CUSTOM_DEFAULT 0x5

#define SUBLANG_ARABIC_SAUDI_ARABIA 0x01
#define SUBLANG_ARABIC_IRAQ 0x02
#define SUBLANG_ARABIC_EGYPT 0x03
#define SUBLANG_ARABIC_LIBYA 0x04
#define SUBLANG_ARABIC_ALGERIA 0x05
#define SUBLANG_ARABIC_MOROCCO 0x06
#define SUBLANG_ARABIC_TUNISIA 0x07
#define SUBLANG_ARABIC_OMAN 0x08
#define SUBLANG_ARABIC_YEMEN 0x09
#define SUBLANG_ARABIC_SYRIA 0x0a
#define SUBLANG_ARABIC_JORDAN 0x0b
#define SUBLANG_ARABIC_LEBANON 0x0c
#define SUBLANG_ARABIC_KUWAIT 0x0d
#define SUBLANG_ARABIC_UAE 0x0e
#define SUBLANG_ARABIC_BAHRAIN 0x0f
#define SUBLANG_ARABIC_QATAR 0x10
#define SUBLANG_AZERI_LATIN 0x01
#define SUBLANG_AZERI_CYRILLIC 0x02
#define SUBLANG_CHINESE_TRADITIONAL 0x01
#define SUBLANG_CHINESE_SIMPLIFIED 0x02
#define SUBLANG_CHINESE_HONGKONG 0x03
#define SUBLANG_CHINESE_SINGAPORE 0x04
#define SUBLANG_CHINESE_MACAU 0x05
#define SUBLANG_DUTCH 0x01
#define SUBLANG_DUTCH_BELGIAN 0x02
#define SUBLANG_ENGLISH_US 0x01
#define SUBLANG_ENGLISH_UK 0x02
#define SUBLANG_ENGLISH_AUS 0x03
#define SUBLANG_ENGLISH_CAN 0x04
#define SUBLANG_ENGLISH_NZ 0x05
#define SUBLANG_ENGLISH_EIRE 0x06
#define SUBLANG_ENGLISH_SOUTH_AFRICA 0x07
#define SUBLANG_ENGLISH_JAMAICA 0x08
#define SUBLANG_ENGLISH_CARIBBEAN 0x09
#define SUBLANG_ENGLISH_BELIZE 0x0a
#define SUBLANG_ENGLISH_TRINIDAD 0x0b
#define SUBLANG_ENGLISH_ZIMBABWE 0x0c
#define SUBLANG_ENGLISH_PHILIPPINES 0x0d
#define SUBLANG_FRENCH 0x01
#define SUBLANG_FRENCH_BELGIAN 0x02
#define SUBLANG_FRENCH_CANADIAN 0x03
#define SUBLANG_FRENCH_SWISS 0x04
#define SUBLANG_FRENCH_LUXEMBOURG 0x05
#define SUBLANG_FRENCH_MONACO 0x06
#define SUBLANG_GERMAN 0x01
#define SUBLANG_GERMAN_SWISS 0x02
#define SUBLANG_GERMAN_AUSTRIAN 0x03
#define SUBLANG_GERMAN_LUXEMBOURG 0x04
#define SUBLANG_GERMAN_LIECHTENSTEIN 0x05
#define SUBLANG_ITALIAN 0x01
#define SUBLANG_ITALIAN_SWISS 0x02
#define SUBLANG_KASHMIRI_SASIA 0x02
#define SUBLANG_KASHMIRI_INDIA 0x02
#define SUBLANG_KOREAN 0x01
#define SUBLANG_LITHUANIAN 0x01
#define SUBLANG_MALAY_MALAYSIA 0x01
#define SUBLANG_MALAY_BRUNEI_DARUSSALAM 0x02
#define SUBLANG_NEPALI_INDIA 0x02
#define SUBLANG_NORWEGIAN_BOKMAL 0x01
#define SUBLANG_NORWEGIAN_NYNORSK 0x02
#define SUBLANG_PORTUGUESE 0x02
#define SUBLANG_PORTUGUESE_BRAZILIAN 0x01
#define SUBLANG_SERBIAN_LATIN 0x02
#define SUBLANG_SERBIAN_CYRILLIC 0x03
#define SUBLANG_SPANISH 0x01
#define SUBLANG_SPANISH_MEXICAN 0x02
#define SUBLANG_SPANISH_MODERN 0x03
#define SUBLANG_SPANISH_GUATEMALA 0x04
#define SUBLANG_SPANISH_COSTA_RICA 0x05
#define SUBLANG_SPANISH_PANAMA 0x06
#define SUBLANG_SPANISH_DOMINICAN_REPUBLIC 0x07
#define SUBLANG_SPANISH_VENEZUELA 0x08
#define SUBLANG_SPANISH_COLOMBIA 0x09
#define SUBLANG_SPANISH_PERU 0x0a
#define SUBLANG_SPANISH_ARGENTINA 0x0b
#define SUBLANG_SPANISH_ECUADOR 0x0c
#define SUBLANG_SPANISH_CHILE 0x0d
#define SUBLANG_SPANISH_URUGUAY 0x0e
#define SUBLANG_SPANISH_PARAGUAY 0x0f
#define SUBLANG_SPANISH_BOLIVIA 0x10
#define SUBLANG_SPANISH_EL_SALVADOR 0x11
#define SUBLANG_SPANISH_HONDURAS 0x12
#define SUBLANG_SPANISH_NICARAGUA 0x13
#define SUBLANG_SPANISH_PUERTO_RICO 0x14
#define SUBLANG_SWEDISH 0x01
#define SUBLANG_SWEDISH_FINLAND 0x02
#define SUBLANG_URDU_PAKISTAN 0x01
#define SUBLANG_URDU_INDIA 0x02
#define SUBLANG_UZBEK_LATIN 0x01
#define SUBLANG_UZBEK_CYRILLIC 0x02

#define SORT_DEFAULT 0x0
#define SORT_INVARIANT_MATH 0x1

#define SORT_JAPANESE_XJIS 0x0
#define SORT_JAPANESE_UNICODE 0x1
#define SORT_JAPANESE_RADICALSTROKE 0x4

#define SORT_CHINESE_BIG5 0x0
#define SORT_CHINESE_PRCP 0x0
#define SORT_CHINESE_UNICODE 0x1
#define SORT_CHINESE_PRC 0x2
#define SORT_CHINESE_BOPOMOFO 0x3

#define SORT_KOREAN_KSC 0x0
#define SORT_KOREAN_UNICODE 0x1

#define SORT_GERMAN_PHONE_BOOK 0x1

#define SORT_HUNGARIAN_DEFAULT 0x0
#define SORT_HUNGARIAN_TECHNICAL 0x1

#define SORT_GEORGIAN_TRADITIONAL 0x0
#define SORT_GEORGIAN_MODERN 0x1

#define MAKELANGID(p,s) ((((WORD)(s)) << 10) | (WORD)(p))
#define PRIMARYLANGID(lgid) ((WORD)(lgid) & 0x3ff)
#define SUBLANGID(lgid) ((WORD)(lgid) >> 10)

#define NLS_VALID_LOCALE_MASK 0x000fffff

#define MAKELCID(lgid,srtid) ((DWORD)((((DWORD)((WORD)(srtid))) << 16) | ((DWORD)((WORD)(lgid)))))
#define MAKESORTLCID(lgid,srtid,ver) ((DWORD)((MAKELCID(lgid,srtid)) | (((DWORD)((WORD)(ver))) << 20)))
#define LANGIDFROMLCID(lcid) ((WORD)(lcid))
#define SORTIDFROMLCID(lcid) ((WORD)((((DWORD)(lcid)) >> 16) & 0xf))
#define SORTVERSIONFROMLCID(lcid) ((WORD)((((DWORD)(lcid)) >> 20) & 0xf))

#define LOCALE_NAME_MAX_LENGTH 85
#define LANG_SYSTEM_DEFAULT (MAKELANGID(LANG_NEUTRAL,SUBLANG_SYS_DEFAULT))
#define LANG_USER_DEFAULT (MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT))

#define LOCALE_SYSTEM_DEFAULT (MAKELCID(LANG_SYSTEM_DEFAULT,SORT_DEFAULT))
#define LOCALE_USER_DEFAULT (MAKELCID(LANG_USER_DEFAULT,SORT_DEFAULT))

#define LOCALE_NEUTRAL (MAKELCID(MAKELANGID(LANG_NEUTRAL,SUBLANG_NEUTRAL),SORT_DEFAULT))

#define LOCALE_CUSTOM_DEFAULT (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_CUSTOM_DEFAULT), SORT_DEFAULT))
#define LOCALE_CUSTOM_UNSPECIFIED (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_CUSTOM_UNSPECIFIED), SORT_DEFAULT))
#define LOCALE_CUSTOM_UI_DEFAULT (MAKELCID(MAKELANGID(LANG_NEUTRAL, SUBLANG_UI_CUSTOM_DEFAULT), SORT_DEFAULT))

#define LOCALE_INVARIANT (MAKELCID(MAKELANGID(LANG_INVARIANT,SUBLANG_NEUTRAL),SORT_DEFAULT))

#define UNREFERENCED_PARAMETER(P) (P)
#define DBG_UNREFERENCED_PARAMETER(P) (P)
#define DBG_UNREFERENCED_LOCAL_VARIABLE(V) (V)

#define DEFAULT_UNREACHABLE

#ifndef WIN32_NO_STATUS
#define STATUS_WAIT_0 ((DWORD)0x00000000L)
#define STATUS_ABANDONED_WAIT_0 ((DWORD)0x00000080L)
#define STATUS_USER_APC ((DWORD)0x000000C0L)
#define STATUS_TIMEOUT ((DWORD)0x00000102L)
#define STATUS_PENDING ((DWORD)0x00000103L)
#define DBG_EXCEPTION_HANDLED ((DWORD)0x00010001L)
#define DBG_CONTINUE ((DWORD)0x00010002L)
#define STATUS_SEGMENT_NOTIFICATION ((DWORD)0x40000005L)
#define DBG_TERMINATE_THREAD ((DWORD)0x40010003L)
#define DBG_TERMINATE_PROCESS ((DWORD)0x40010004L)
#define DBG_CONTROL_C ((DWORD)0x40010005L)
#define DBG_CONTROL_BREAK ((DWORD)0x40010008L)
#define DBG_COMMAND_EXCEPTION ((DWORD)0x40010009L)
#define STATUS_GUARD_PAGE_VIOLATION ((DWORD)0x80000001L)
#define STATUS_DATATYPE_MISALIGNMENT ((DWORD)0x80000002L)
#define STATUS_BREAKPOINT ((DWORD)0x80000003L)
#define STATUS_SINGLE_STEP ((DWORD)0x80000004L)
#define DBG_EXCEPTION_NOT_HANDLED ((DWORD)0x80010001L)
#define STATUS_ACCESS_VIOLATION ((DWORD)0xC0000005L)
#define STATUS_IN_PAGE_ERROR ((DWORD)0xC0000006L)
#define STATUS_INVALID_HANDLE ((DWORD)0xC0000008L)
#define STATUS_NO_MEMORY ((DWORD)0xC0000017L)
#define STATUS_ILLEGAL_INSTRUCTION ((DWORD)0xC000001DL)
#define STATUS_NONCONTINUABLE_EXCEPTION ((DWORD)0xC0000025L)
#define STATUS_INVALID_DISPOSITION ((DWORD)0xC0000026L)
#define STATUS_ARRAY_BOUNDS_EXCEEDED ((DWORD)0xC000008CL)
#define STATUS_FLOAT_DENORMAL_OPERAND ((DWORD)0xC000008DL)
#define STATUS_FLOAT_DIVIDE_BY_ZERO ((DWORD)0xC000008EL)
#define STATUS_FLOAT_INEXACT_RESULT ((DWORD)0xC000008FL)
#define STATUS_FLOAT_INVALID_OPERATION ((DWORD)0xC0000090L)
#define STATUS_FLOAT_OVERFLOW ((DWORD)0xC0000091L)
#define STATUS_FLOAT_STACK_CHECK ((DWORD)0xC0000092L)
#define STATUS_FLOAT_UNDERFLOW ((DWORD)0xC0000093L)
#define STATUS_INTEGER_DIVIDE_BY_ZERO ((DWORD)0xC0000094L)
#define STATUS_INTEGER_OVERFLOW ((DWORD)0xC0000095L)
#define STATUS_PRIVILEGED_INSTRUCTION ((DWORD)0xC0000096L)
#define STATUS_STACK_OVERFLOW ((DWORD)0xC00000FDL)
#define STATUS_CONTROL_C_EXIT ((DWORD)0xC000013AL)
#define STATUS_FLOAT_MULTIPLE_FAULTS ((DWORD)0xC00002B4L)
#define STATUS_FLOAT_MULTIPLE_TRAPS ((DWORD)0xC00002B5L)
#define STATUS_REG_NAT_CONSUMPTION ((DWORD)0xC00002C9L)
#define STATUS_SXS_EARLY_DEACTIVATION ((DWORD)0xC015000FL)
#define STATUS_SXS_INVALID_DEACTIVATION ((DWORD)0xC0150010L)
#endif

#define MAXIMUM_WAIT_OBJECTS 64
#define MAXIMUM_SUSPEND_COUNT MAXCHAR

  typedef ULONG_PTR KSPIN_LOCK;
  typedef KSPIN_LOCK *PKSPIN_LOCK;

#ifdef _AMD64_

#if defined(__x86_64) && !defined(RC_INVOKED)

#ifdef __cplusplus
  extern "C" {
#endif

#define BitTest _bittest
#define BitTestAndComplement _bittestandcomplement
#define BitTestAndSet _bittestandset
#define BitTestAndReset _bittestandreset
#define InterlockedBitTestAndSet _interlockedbittestandset
#define InterlockedBitTestAndReset _interlockedbittestandreset
#define BitTest64 _bittest64
#define BitTestAndComplement64 _bittestandcomplement64
#define BitTestAndSet64 _bittestandset64
#define BitTestAndReset64 _bittestandreset64
#define InterlockedBitTestAndSet64 _interlockedbittestandset64
#define InterlockedBitTestAndReset64 _interlockedbittestandreset64

    __CRT_INLINE BOOLEAN _bittest(LONG const *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandcomplement(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btcl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN InterlockedBitTestAndComplement(LONG *Base,LONG Bit) {
      int old = 0;
      __asm__ __volatile__("lock ; btcl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Bit));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btsl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandreset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("btrl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btsl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandreset(LONG *Base,LONG Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btrl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittest64(LONG64 const *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandcomplement64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btcq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btsq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _bittestandreset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("btrq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btsq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
    __CRT_INLINE BOOLEAN _interlockedbittestandreset64(LONG64 *Base,LONG64 Offset) {
      int old = 0;
      __asm__ __volatile__("lock ; btrq %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long long *) Base))
	:"Ir" (Offset));
      return (BOOLEAN) (old!=0);
    }
#define BitScanForward _BitScanForward
#define BitScanReverse _BitScanReverse
#define BitScanForward64 _BitScanForward64
#define BitScanReverse64 _BitScanReverse64

    __CRT_INLINE BOOLEAN _BitScanForward(DWORD *Index,DWORD Mask) {
      __asm__ __volatile__("bsfl %1,%0" : "=r" (Mask),"=m" ((*(volatile long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanReverse(DWORD *Index,DWORD Mask) {
      __asm__ __volatile__("bsrl %1,%0" : "=r" (Mask),"=m" ((*(volatile long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanForward64(DWORD *Index,DWORD64 Mask) {
      __asm__ __volatile__("bsfq %1,%0" : "=r" (Mask),"=m" ((*(volatile long long *)Index)));
      return Mask!=0;
    }
    __CRT_INLINE BOOLEAN _BitScanReverse64(DWORD *Index,DWORD64 Mask) {
      __asm__ __volatile__("bsrq %1,%0" : "=r" (Mask),"=m" ((*(volatile long long *)Index)));
      return Mask!=0;
    }

#define InterlockedIncrement16 _InterlockedIncrement16
#define InterlockedDecrement16 _InterlockedDecrement16
#define InterlockedCompareExchange16 _InterlockedCompareExchange16

#define InterlockedAnd _InterlockedAnd
#define InterlockedOr _InterlockedOr
#define InterlockedXor _InterlockedXor
#define InterlockedIncrement _InterlockedIncrement
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedDecrement _InterlockedDecrement
#define InterlockedDecrementAcquire InterlockedDecrement
#define InterlockedDecrementRelease InterlockedDecrement
#define InterlockedAdd _InterlockedAdd
#define InterlockedExchange _InterlockedExchange
#define InterlockedExchangeAdd _InterlockedExchangeAdd
#define InterlockedCompareExchange _InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire InterlockedCompareExchange
#define InterlockedCompareExchangeRelease InterlockedCompareExchange

#define InterlockedAnd64 _InterlockedAnd64
#define InterlockedAndAffinity InterlockedAnd64
#define InterlockedOr64 _InterlockedOr64
#define InterlockedOrAffinity InterlockedOr64
#define InterlockedXor64 _InterlockedXor64
#define InterlockedIncrement64 _InterlockedIncrement64
#define InterlockedDecrement64 _InterlockedDecrement64
#define InterlockedAdd64 _InterlockedAdd64
#define InterlockedExchange64 _InterlockedExchange64
#define InterlockedExchangeAcquire64 InterlockedExchange64
#define InterlockedExchangeAdd64 _InterlockedExchangeAdd64
#define InterlockedCompareExchange64 _InterlockedCompareExchange64
#define InterlockedCompareExchangeAcquire64 InterlockedCompareExchange64
#define InterlockedCompareExchangeRelease64 InterlockedCompareExchange64

#define InterlockedExchangePointer _InterlockedExchangePointer
#define InterlockedCompareExchangePointer _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerAcquire _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerRelease _InterlockedCompareExchangePointer

#define InterlockedExchangeAddSizeT(a,b) InterlockedExchangeAdd64((LONG64 *)a,b)
#define InterlockedIncrementSizeT(a) InterlockedIncrement64((LONG64 *)a)
#define InterlockedDecrementSizeT(a) InterlockedDecrement64((LONG64 *)a)

    __CRT_INLINE SHORT InterlockedIncrement16(SHORT volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addw $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE SHORT InterlockedDecrement16(SHORT volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subw $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE SHORT InterlockedCompareExchange16(SHORT volatile *Destination,SHORT ExChange,SHORT Comperand) {
      SHORT prev;
      __asm__ __volatile__("lock ; cmpxchgw %w1,%2"
	:"=a"(prev)
	:"q"(ExChange), "m"(*Destination), "0"(Comperand)
	: "memory");
      return prev;
    }
    __CRT_INLINE LONG InterlockedAnd(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; andl %0,%1"
	: :"r"(Value),"m"(*Destination)
	: "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedOr(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; orl %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedXor(LONG volatile *Destination,LONG Value) {
      __asm__ __volatile__("lock ; xorl %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    //		$$$$
    __CRT_INLINE LONG64 InterlockedAnd64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; andq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG64 InterlockedOr64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; orq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG64 InterlockedXor64(LONG64 volatile *Destination,LONG64 Value) {
      __asm__ __volatile__("lock ; xorq %0,%1"
	: : "r"(Value),"m"(*Destination) : "memory");
      return *Destination;
    }
    __CRT_INLINE LONG InterlockedIncrement(LONG volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addl $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG InterlockedDecrement(LONG volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subl $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG InterlockedExchange(LONG volatile *Target,LONG Value) {
      __asm__ __volatile("lock ; xchgl %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }
    LONG InterlockedExchangeAdd(LONG volatile *Addend,LONG Value);

#ifndef _X86AMD64_
    __CRT_INLINE LONG InterlockedAdd(LONG volatile *Addend,LONG Value) { return InterlockedExchangeAdd(Addend,Value) + Value; }
#endif
    __CRT_INLINE LONG InterlockedCompareExchange(LONG volatile *Destination,LONG ExChange,LONG Comperand) {
      LONG prev;
      __asm__ __volatile__("lock ; cmpxchgl %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE LONG64 InterlockedIncrement64(LONG64 volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; addq $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG64 InterlockedDecrement64(LONG64 volatile *Addend) {
      unsigned char c;
      unsigned char s;
      __asm__ __volatile__(
	"lock ; subq $1,%0; sete %1 ; sets %2"
	:"=m" (*Addend), "=qm" (c), "=qm" (s)
	:"m" (*Addend) : "memory");
      return (c != 0 ? 0 : (s != 0 ? -1 : 1));
    }
    __CRT_INLINE LONG64 InterlockedExchange64(LONG64 volatile *Target,LONG64 Value) {
      __asm__ __volatile("lock ; xchgq %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }
    LONG64 InterlockedExchangeAdd64(LONG64 volatile *Addend,LONG64 Value);

#ifndef _X86AMD64_
    __CRT_INLINE LONG64 InterlockedAdd64(LONG64 volatile *Addend,LONG64 Value) { return InterlockedExchangeAdd64(Addend,Value) + Value; }
#endif

    __CRT_INLINE LONG64 InterlockedCompareExchange64(LONG64 volatile *Destination,LONG64 ExChange,LONG64 Comperand) {
      LONG64 prev;
      __asm__ __volatile__("lock ; cmpxchgq %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE PVOID InterlockedCompareExchangePointer(PVOID volatile *Destination,PVOID ExChange,PVOID Comperand) {
      PVOID prev;
      __asm__ __volatile__("lock ; cmpxchgq %1,%2" : "=a" (prev) : "q" (ExChange),"m" (*Destination), "0" (Comperand) : "memory");
      return prev;
    }
    __CRT_INLINE PVOID InterlockedExchangePointer(PVOID volatile *Target,PVOID Value) {
      __asm__ __volatile("lock ; xchgq %0,%1"
	: "=r"(Value)
	: "m"(*Target),"0"(Value)
	: "memory");
      return Value;
    }

#define CacheLineFlush(Address) _mm_clflush(Address)

    VOID _ReadWriteBarrier(VOID);

#define FastFence __faststorefence
#define LoadFence _mm_lfence
#define MemoryFence _mm_mfence
#define StoreFence _mm_sfence

    VOID __faststorefence(VOID);
    VOID _m_prefetchw(volatile CONST VOID *Source);

//!__TINYC__: #include <intrin.h>

#define YieldProcessor _mm_pause
#define MemoryBarrier __faststorefence
#define PreFetchCacheLine(l,a) _mm_prefetch((CHAR CONST *) a,l)
#define PrefetchForWrite(p) _m_prefetchw(p)
#define ReadForWriteAccess(p) (_m_prefetchw(p),*(p))

#define PF_TEMPORAL_LEVEL_1 _MM_HINT_T0
#define PF_TEMPORAL_LEVEL_2 _MM_HINT_T1
#define PF_TEMPORAL_LEVEL_3 _MM_HINT_T2
#define PF_NON_TEMPORAL_LEVEL_ALL _MM_HINT_NTA

#define ReadMxCsr _mm_getcsr
#define WriteMxCsr _mm_setcsr

    VOID __int2c(VOID);

#define DbgRaiseAssertionFailure() __int2c()
#define GetCallersEflags() __getcallerseflags()

    unsigned __int32 __getcallerseflags(VOID);

#define GetSegmentLimit __segmentlimit

    DWORD __segmentlimit(DWORD Selector);

#define ReadTimeStampCounter() __rdtsc()

    DWORD64 __rdtsc(VOID);
    VOID __movsb(PBYTE Destination,BYTE const *Source,SIZE_T Count);
    VOID __movsw(PWORD Destination,WORD const *Source,SIZE_T Count);
    VOID __movsd(PDWORD Destination,DWORD const *Source,SIZE_T Count);
    VOID __movsq(PDWORD64 Destination,DWORD64 const *Source,SIZE_T Count);
    VOID __stosb(PBYTE Destination,BYTE Value,SIZE_T Count);
    VOID __stosw(PWORD Destination,WORD Value,SIZE_T Count);
    VOID __stosd(PDWORD Destination,DWORD Value,SIZE_T Count);
    VOID __stosq(PDWORD64 Destination,DWORD64 Value,SIZE_T Count);

#define MultiplyHigh __mulh
#define UnsignedMultiplyHigh __umulh

    LONGLONG MultiplyHigh(LONGLONG Multiplier,LONGLONG Multiplicand);
    ULONGLONG UnsignedMultiplyHigh(ULONGLONG Multiplier,ULONGLONG Multiplicand);

#define ShiftLeft128 __shiftleft128
#define ShiftRight128 __shiftright128

    DWORD64 ShiftLeft128(DWORD64 LowPart,DWORD64 HighPart,BYTE Shift);
    DWORD64 ShiftRight128(DWORD64 LowPart,DWORD64 HighPart,BYTE Shift);

#define Multiply128 _mul128

    LONG64 Multiply128(LONG64 Multiplier,LONG64 Multiplicand,LONG64 *HighProduct);

#define UnsignedMultiply128 _umul128

    DWORD64 UnsignedMultiply128(DWORD64 Multiplier,DWORD64 Multiplicand,DWORD64 *HighProduct);

    __CRT_INLINE LONG64 MultiplyExtract128(LONG64 Multiplier,LONG64 Multiplicand,BYTE Shift) {
      LONG64 extractedProduct;
      LONG64 highProduct;
      LONG64 lowProduct;
      lowProduct = Multiply128(Multiplier,Multiplicand,&highProduct);
      extractedProduct = (LONG64)ShiftRight128((LONG64)lowProduct,(LONG64)highProduct,Shift);
      return extractedProduct;
    }

    __CRT_INLINE DWORD64 UnsignedMultiplyExtract128(DWORD64 Multiplier,DWORD64 Multiplicand,BYTE Shift) {
      DWORD64 extractedProduct;
      DWORD64 highProduct;
      DWORD64 lowProduct;
      lowProduct = UnsignedMultiply128(Multiplier,Multiplicand,&highProduct);
      extractedProduct = ShiftRight128(lowProduct,highProduct,Shift);
      return extractedProduct;
    }

    __CRT_INLINE BYTE __readgsbyte(DWORD Offset) {
      BYTE ret;
      __asm__ volatile ("movb	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE WORD __readgsword(DWORD Offset) {
      WORD ret;
      __asm__ volatile ("movw	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE DWORD __readgsdword(DWORD Offset) {
      DWORD ret;
      __asm__ volatile ("movl	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return ret;
    }
    __CRT_INLINE DWORD64 __readgsqword(DWORD Offset) {
      void *ret;
      __asm__ volatile ("movq	%%gs:%1,%0"
	: "=r" (ret) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
      return (DWORD64) ret;
    }
    __CRT_INLINE VOID __writegsbyte(DWORD Offset,BYTE Data) {
      __asm__ volatile ("movb	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsword(DWORD Offset,WORD Data) {
      __asm__ volatile ("movw	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsdword(DWORD Offset,DWORD Data) {
      __asm__ volatile ("movl	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }
    __CRT_INLINE VOID __writegsqword(DWORD Offset,DWORD64 Data) {
      __asm__ volatile ("movq	%0,%%gs:%1"
	: "=r" (Data) ,"=m" ((*(volatile long *) (DWORD64) Offset)));
    }

#ifdef __cplusplus
  }
#endif
#endif

#define EXCEPTION_READ_FAULT 0
#define EXCEPTION_WRITE_FAULT 1
#define EXCEPTION_EXECUTE_FAULT 8

#if !defined(RC_INVOKED)

#define CONTEXT_AMD64 0x100000

#define CONTEXT_CONTROL (CONTEXT_AMD64 | 0x1L)
#define CONTEXT_INTEGER (CONTEXT_AMD64 | 0x2L)
#define CONTEXT_SEGMENTS (CONTEXT_AMD64 | 0x4L)
#define CONTEXT_FLOATING_POINT (CONTEXT_AMD64 | 0x8L)
#define CONTEXT_DEBUG_REGISTERS (CONTEXT_AMD64 | 0x10L)

#define CONTEXT_FULL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_FLOATING_POINT)
#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS | CONTEXT_FLOATING_POINT | CONTEXT_DEBUG_REGISTERS)

#define CONTEXT_EXCEPTION_ACTIVE 0x8000000
#define CONTEXT_SERVICE_ACTIVE 0x10000000
#define CONTEXT_EXCEPTION_REQUEST 0x40000000
#define CONTEXT_EXCEPTION_REPORTING 0x80000000
#endif

#define INITIAL_MXCSR 0x1f80
#define INITIAL_FPCSR 0x027f

  typedef struct DECLSPEC_ALIGN(16) _M128A {
    ULONGLONG Low;
    LONGLONG High;
  } M128A,*PM128A;

  typedef struct _XMM_SAVE_AREA32 {
    WORD ControlWord;
    WORD StatusWord;
    BYTE TagWord;
    BYTE Reserved1;
    WORD ErrorOpcode;
    DWORD ErrorOffset;
    WORD ErrorSelector;
    WORD Reserved2;
    DWORD DataOffset;
    WORD DataSelector;
    WORD Reserved3;
    DWORD MxCsr;
    DWORD MxCsr_Mask;
    M128A FloatRegisters[8];
    M128A XmmRegisters[16];
    BYTE Reserved4[96];
  } XMM_SAVE_AREA32,*PXMM_SAVE_AREA32;

#define LEGACY_SAVE_AREA_LENGTH sizeof(XMM_SAVE_AREA32)

  typedef struct DECLSPEC_ALIGN(16) _CONTEXT {
    DWORD64 P1Home;
    DWORD64 P2Home;
    DWORD64 P3Home;
    DWORD64 P4Home;
    DWORD64 P5Home;
    DWORD64 P6Home;
    DWORD ContextFlags;
    DWORD MxCsr;
    WORD SegCs;
    WORD SegDs;
    WORD SegEs;
    WORD SegFs;
    WORD SegGs;
    WORD SegSs;
    DWORD EFlags;
    DWORD64 Dr0;
    DWORD64 Dr1;
    DWORD64 Dr2;
    DWORD64 Dr3;
    DWORD64 Dr6;
    DWORD64 Dr7;
    DWORD64 Rax;
    DWORD64 Rcx;
    DWORD64 Rdx;
    DWORD64 Rbx;
    DWORD64 Rsp;
    DWORD64 Rbp;
    DWORD64 Rsi;
    DWORD64 Rdi;
    DWORD64 R8;
    DWORD64 R9;
    DWORD64 R10;
    DWORD64 R11;
    DWORD64 R12;
    DWORD64 R13;
    DWORD64 R14;
    DWORD64 R15;
    DWORD64 Rip;
    union {
      XMM_SAVE_AREA32 FltSave;
      XMM_SAVE_AREA32 FloatSave;
      struct {
	M128A Header[2];
	M128A Legacy[8];
	M128A Xmm0;
	M128A Xmm1;
	M128A Xmm2;
	M128A Xmm3;
	M128A Xmm4;
	M128A Xmm5;
	M128A Xmm6;
	M128A Xmm7;
	M128A Xmm8;
	M128A Xmm9;
	M128A Xmm10;
	M128A Xmm11;
	M128A Xmm12;
	M128A Xmm13;
	M128A Xmm14;
	M128A Xmm15;
      };
    };
    M128A VectorRegister[26];
    DWORD64 VectorControl;
    DWORD64 DebugControl;
    DWORD64 LastBranchToRip;
    DWORD64 LastBranchFromRip;
    DWORD64 LastExceptionToRip;
    DWORD64 LastExceptionFromRip;
  } CONTEXT,*PCONTEXT;

#define RUNTIME_FUNCTION_INDIRECT 0x1

  typedef struct _RUNTIME_FUNCTION {
    DWORD BeginAddress;
    DWORD EndAddress;
    DWORD UnwindData;
  } RUNTIME_FUNCTION,*PRUNTIME_FUNCTION;

  typedef PRUNTIME_FUNCTION (*PGET_RUNTIME_FUNCTION_CALLBACK)(DWORD64 ControlPc,PVOID Context);
  typedef DWORD (*POUT_OF_PROCESS_FUNCTION_TABLE_CALLBACK)(HANDLE Process,PVOID TableAddress,PDWORD Entries,PRUNTIME_FUNCTION *Functions);

#define OUT_OF_PROCESS_FUNCTION_TABLE_CALLBACK_EXPORT_NAME "OutOfProcessFunctionTableCallback"

  NTSYSAPI VOID __cdecl RtlRestoreContext (PCONTEXT ContextRecord,struct _EXCEPTION_RECORD *ExceptionRecord);
  NTSYSAPI BOOLEAN __cdecl RtlAddFunctionTable(PRUNTIME_FUNCTION FunctionTable,DWORD EntryCount,DWORD64 BaseAddress);
  NTSYSAPI BOOLEAN __cdecl RtlInstallFunctionTableCallback(DWORD64 TableIdentifier,DWORD64 BaseAddress,DWORD Length,PGET_RUNTIME_FUNCTION_CALLBACK Callback,PVOID Context,PCWSTR OutOfProcessCallbackDll);
  NTSYSAPI BOOLEAN __cdecl RtlDeleteFunctionTable(PRUNTIME_FUNCTION FunctionTable);
#endif

#ifdef I_X86_
#if(defined(_X86_) && !defined(__x86_64)) && !defined(RC_INVOKED)
#ifdef __cplusplus
  extern "C" {
#endif

    __CRT_INLINE BOOLEAN InterlockedBitTestAndSet(LONG *Base,LONG Bit) {
      int old = 0;
      __asm__ __volatile__("lock ; btsl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Bit));
      return (BOOLEAN) (old!=0);
    }

    __CRT_INLINE BOOLEAN InterlockedBitTestAndReset(LONG *Base,LONG Bit) {
      int old = 0;
      __asm__ __volatile__("lock ; btrl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Bit));
      return (BOOLEAN) (old!=0);
    }

    __CRT_INLINE BOOLEAN InterlockedBitTestAndComplement(LONG *Base,LONG Bit) {
      int old = 0;
      __asm__ __volatile__("lock ; btcl %2,%1\n\tsbbl %0,%0 "
	:"=r" (old),"=m" ((*(volatile long *) Base))
	:"Ir" (Bit));
      return (BOOLEAN) (old!=0);
    }

#ifdef _PREFIX_
    BYTE __readfsbyte(DWORD Offset);
    WORD __readfsword(DWORD Offset);
    DWORD __readfsdword(DWORD Offset);
    VOID __writefsbyte(DWORD Offset,BYTE Data);
    VOID __writefsword(DWORD Offset,WORD Data);
    VOID __writefsdword(DWORD Offset,DWORD Data);
#endif

#ifdef __cplusplus
  }
#endif
#endif

#if(defined(_X86_) && !defined(__x86_64))
  __CRT_INLINE VOID MemoryBarrier(VOID) {
    LONG Barrier;
    __asm__ __volatile__("xchgl %%eax,%0 "
      :"=r" (Barrier));
  }
#define YieldProcessor() __asm__ __volatile__("rep nop ");

#define PreFetchCacheLine(l,a)
#define ReadForWriteAccess(p) (*(p))

#define PF_TEMPORAL_LEVEL_1
#define PF_NON_TEMPORAL_LEVEL_ALL

  __CRT_INLINE VOID DbgRaiseAssertionFailure(void) {
    __asm__ __volatile__("int $0x2c ");
  }
  PVOID GetCurrentFiber(void);
  __CRT_INLINE PVOID GetCurrentFiber(void)
  {
    void *ret;
    __asm__ volatile ("movl	%%fs:0x10,%0"
	: "=r" (ret));
    return ret;
  }
  PVOID GetFiberData(void);
  __CRT_INLINE PVOID GetFiberData(void)
  {
    void *ret;
    __asm__ volatile ("movl	%%fs:0x10,%0\n"
	"movl	(%0),%0"
	: "=r" (ret));
    return ret;
  }
#endif

#define EXCEPTION_READ_FAULT 0
#define EXCEPTION_WRITE_FAULT 1
#define EXCEPTION_EXECUTE_FAULT 8

#define SIZE_OF_80387_REGISTERS 80

#if !defined(RC_INVOKED)

#define CONTEXT_i386 0x00010000
#define CONTEXT_i486 0x00010000

#define CONTEXT_CONTROL (CONTEXT_i386 | 0x00000001L)
#define CONTEXT_INTEGER (CONTEXT_i386 | 0x00000002L)
#define CONTEXT_SEGMENTS (CONTEXT_i386 | 0x00000004L)
#define CONTEXT_FLOATING_POINT (CONTEXT_i386 | 0x00000008L)
#define CONTEXT_DEBUG_REGISTERS (CONTEXT_i386 | 0x00000010L)
#define CONTEXT_EXTENDED_REGISTERS (CONTEXT_i386 | 0x00000020L)

#define CONTEXT_FULL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS)

#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS | CONTEXT_FLOATING_POINT | CONTEXT_DEBUG_REGISTERS | CONTEXT_EXTENDED_REGISTERS)
#endif

#define MAXIMUM_SUPPORTED_EXTENSION 512

    typedef struct _FLOATING_SAVE_AREA {
      DWORD ControlWord;
      DWORD StatusWord;
      DWORD TagWord;
      DWORD ErrorOffset;
      DWORD ErrorSelector;
      DWORD DataOffset;
      DWORD DataSelector;
      BYTE RegisterArea[SIZE_OF_80387_REGISTERS];
      DWORD Cr0NpxState;
    } FLOATING_SAVE_AREA;

    typedef FLOATING_SAVE_AREA *PFLOATING_SAVE_AREA;

    typedef struct _CONTEXT {
      DWORD ContextFlags;
      DWORD Dr0;
      DWORD Dr1;
      DWORD Dr2;
      DWORD Dr3;
      DWORD Dr6;
      DWORD Dr7;
      FLOATING_SAVE_AREA FloatSave;
      DWORD SegGs;
      DWORD SegFs;
      DWORD SegEs;
      DWORD SegDs;

      DWORD Edi;
      DWORD Esi;
      DWORD Ebx;
      DWORD Edx;
      DWORD Ecx;
      DWORD Eax;
      DWORD Ebp;
      DWORD Eip;
      DWORD SegCs;
      DWORD EFlags;
      DWORD Esp;
      DWORD SegSs;
      BYTE ExtendedRegisters[MAXIMUM_SUPPORTED_EXTENSION];
    } CONTEXT;

    typedef CONTEXT *PCONTEXT;
#endif

#ifndef _LDT_ENTRY_DEFINED
#define _LDT_ENTRY_DEFINED

    typedef struct _LDT_ENTRY {
      WORD LimitLow;
      WORD BaseLow;
      union {
	struct {
	  BYTE BaseMid;
	  BYTE Flags1;
	  BYTE Flags2;
	  BYTE BaseHi;
	} Bytes;
	struct {
	  DWORD BaseMid : 8;
	  DWORD Type : 5;
	  DWORD Dpl : 2;
	  DWORD Pres : 1;
	  DWORD LimitHi : 4;
	  DWORD Sys : 1;
	  DWORD Reserved_0 : 1;
	  DWORD Default_Big : 1;
	  DWORD Granularity : 1;
	  DWORD BaseHi : 8;
	} Bits;
      } HighWord;
    } LDT_ENTRY,*PLDT_ENTRY;
#endif

#if defined(__ia64__) && !defined(RC_INVOKED)

#ifdef __cplusplus
    extern "C" {
#endif

      BOOLEAN BitScanForward64(DWORD *Index,DWORD64 Mask);
      BOOLEAN BitScanReverse64(DWORD *Index,DWORD64 Mask);

#ifdef __cplusplus
    }
#endif
#endif

#if !defined(GENUTIL) && !defined(_GENIA64_) && defined(_IA64_)

    void *_cdecl _rdteb(void);
#ifdef __ia64__

#define NtCurrentTeb() ((struct _TEB *)_rdteb())
#define GetCurrentFiber() (((PNT_TIB)NtCurrentTeb())->FiberData)
#define GetFiberData() (*(PVOID *)(GetCurrentFiber()))

#ifdef __cplusplus
    extern "C" {
#endif

      void __break(int);
      void __yield(void);
      void __mf(void);
      void __lfetch(int Level,VOID CONST *Address);
      void __lfetchfault(int Level,VOID CONST *Address);
      void __lfetch_excl(int Level,VOID CONST *Address);
      void __lfetchfault_excl(int Level,VOID CONST *Address);

#define MD_LFHINT_NONE 0x00
#define MD_LFHINT_NT1 0x01
#define MD_LFHINT_NT2 0x02
#define MD_LFHINT_NTA 0x03

#ifdef __cplusplus
    }
#endif

#define YieldProcessor __yield
#define MemoryBarrier __mf
#define PreFetchCacheLine __lfetch
#define ReadForWriteAccess(p) (*(p))
#define DbgRaiseAssertionFailure() __break(ASSERT_BREAKPOINT)

#define PF_TEMPORAL_LEVEL_1 MD_LFHINT_NONE
#define PF_NON_TEMPORAL_LEVEL_ALL MD_LFHINT_NTA

#define UnsignedMultiplyHigh __UMULH

    ULONGLONG UnsignedMultiplyHigh(ULONGLONG Multiplier,ULONGLONG Multiplicand);
#else
    struct _TEB *NtCurrentTeb(void);
#endif
#endif

#ifdef _IA64_

#define EXCEPTION_READ_FAULT 0
#define EXCEPTION_WRITE_FAULT 1
#define EXCEPTION_EXECUTE_FAULT 2

#if !defined(RC_INVOKED)

#define CONTEXT_IA64 0x00080000

#define CONTEXT_CONTROL (CONTEXT_IA64 | 0x00000001L)
#define CONTEXT_LOWER_FLOATING_POINT (CONTEXT_IA64 | 0x00000002L)
#define CONTEXT_HIGHER_FLOATING_POINT (CONTEXT_IA64 | 0x00000004L)
#define CONTEXT_INTEGER (CONTEXT_IA64 | 0x00000008L)
#define CONTEXT_DEBUG (CONTEXT_IA64 | 0x00000010L)
#define CONTEXT_IA32_CONTROL (CONTEXT_IA64 | 0x00000020L)

#define CONTEXT_FLOATING_POINT (CONTEXT_LOWER_FLOATING_POINT | CONTEXT_HIGHER_FLOATING_POINT)
#define CONTEXT_FULL (CONTEXT_CONTROL | CONTEXT_FLOATING_POINT | CONTEXT_INTEGER | CONTEXT_IA32_CONTROL)
#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_FLOATING_POINT | CONTEXT_INTEGER | CONTEXT_DEBUG | CONTEXT_IA32_CONTROL)

#define CONTEXT_EXCEPTION_ACTIVE 0x8000000
#define CONTEXT_SERVICE_ACTIVE 0x10000000
#define CONTEXT_EXCEPTION_REQUEST 0x40000000
#define CONTEXT_EXCEPTION_REPORTING 0x80000000
#endif

    typedef struct _CONTEXT {
      DWORD ContextFlags;
      DWORD Fill1[3];
      ULONGLONG DbI0;
      ULONGLONG DbI1;
      ULONGLONG DbI2;
      ULONGLONG DbI3;
      ULONGLONG DbI4;
      ULONGLONG DbI5;
      ULONGLONG DbI6;
      ULONGLONG DbI7;
      ULONGLONG DbD0;
      ULONGLONG DbD1;
      ULONGLONG DbD2;
      ULONGLONG DbD3;
      ULONGLONG DbD4;
      ULONGLONG DbD5;
      ULONGLONG DbD6;
      ULONGLONG DbD7;
      FLOAT128 FltS0;
      FLOAT128 FltS1;
      FLOAT128 FltS2;
      FLOAT128 FltS3;
      FLOAT128 FltT0;
      FLOAT128 FltT1;
      FLOAT128 FltT2;
      FLOAT128 FltT3;
      FLOAT128 FltT4;
      FLOAT128 FltT5;
      FLOAT128 FltT6;
      FLOAT128 FltT7;
      FLOAT128 FltT8;
      FLOAT128 FltT9;
      FLOAT128 FltS4;
      FLOAT128 FltS5;
      FLOAT128 FltS6;
      FLOAT128 FltS7;
      FLOAT128 FltS8;
      FLOAT128 FltS9;
      FLOAT128 FltS10;
      FLOAT128 FltS11;
      FLOAT128 FltS12;
      FLOAT128 FltS13;
      FLOAT128 FltS14;
      FLOAT128 FltS15;
      FLOAT128 FltS16;
      FLOAT128 FltS17;
      FLOAT128 FltS18;
      FLOAT128 FltS19;
      FLOAT128 FltF32;
      FLOAT128 FltF33;
      FLOAT128 FltF34;
      FLOAT128 FltF35;
      FLOAT128 FltF36;
      FLOAT128 FltF37;
      FLOAT128 FltF38;
      FLOAT128 FltF39;
      FLOAT128 FltF40;
      FLOAT128 FltF41;
      FLOAT128 FltF42;
      FLOAT128 FltF43;
      FLOAT128 FltF44;
      FLOAT128 FltF45;
      FLOAT128 FltF46;
      FLOAT128 FltF47;
      FLOAT128 FltF48;
      FLOAT128 FltF49;
      FLOAT128 FltF50;
      FLOAT128 FltF51;
      FLOAT128 FltF52;
      FLOAT128 FltF53;
      FLOAT128 FltF54;
      FLOAT128 FltF55;
      FLOAT128 FltF56;
      FLOAT128 FltF57;
      FLOAT128 FltF58;
      FLOAT128 FltF59;
      FLOAT128 FltF60;
      FLOAT128 FltF61;
      FLOAT128 FltF62;
      FLOAT128 FltF63;
      FLOAT128 FltF64;
      FLOAT128 FltF65;
      FLOAT128 FltF66;
      FLOAT128 FltF67;
      FLOAT128 FltF68;
      FLOAT128 FltF69;
      FLOAT128 FltF70;
      FLOAT128 FltF71;
      FLOAT128 FltF72;
      FLOAT128 FltF73;
      FLOAT128 FltF74;
      FLOAT128 FltF75;
      FLOAT128 FltF76;
      FLOAT128 FltF77;
      FLOAT128 FltF78;
      FLOAT128 FltF79;
      FLOAT128 FltF80;
      FLOAT128 FltF81;
      FLOAT128 FltF82;
      FLOAT128 FltF83;
      FLOAT128 FltF84;
      FLOAT128 FltF85;
      FLOAT128 FltF86;
      FLOAT128 FltF87;
      FLOAT128 FltF88;
      FLOAT128 FltF89;
      FLOAT128 FltF90;
      FLOAT128 FltF91;
      FLOAT128 FltF92;
      FLOAT128 FltF93;
      FLOAT128 FltF94;
      FLOAT128 FltF95;
      FLOAT128 FltF96;
      FLOAT128 FltF97;
      FLOAT128 FltF98;
      FLOAT128 FltF99;
      FLOAT128 FltF100;
      FLOAT128 FltF101;
      FLOAT128 FltF102;
      FLOAT128 FltF103;
      FLOAT128 FltF104;
      FLOAT128 FltF105;
      FLOAT128 FltF106;
      FLOAT128 FltF107;
      FLOAT128 FltF108;
      FLOAT128 FltF109;
      FLOAT128 FltF110;
      FLOAT128 FltF111;
      FLOAT128 FltF112;
      FLOAT128 FltF113;
      FLOAT128 FltF114;
      FLOAT128 FltF115;
      FLOAT128 FltF116;
      FLOAT128 FltF117;
      FLOAT128 FltF118;
      FLOAT128 FltF119;
      FLOAT128 FltF120;
      FLOAT128 FltF121;
      FLOAT128 FltF122;
      FLOAT128 FltF123;
      FLOAT128 FltF124;
      FLOAT128 FltF125;
      FLOAT128 FltF126;
      FLOAT128 FltF127;
      ULONGLONG StFPSR;
      ULONGLONG IntGp;
      ULONGLONG IntT0;
      ULONGLONG IntT1;
      ULONGLONG IntS0;
      ULONGLONG IntS1;
      ULONGLONG IntS2;
      ULONGLONG IntS3;
      ULONGLONG IntV0;
      ULONGLONG IntT2;
      ULONGLONG IntT3;
      ULONGLONG IntT4;
      ULONGLONG IntSp;
      ULONGLONG IntTeb;
      ULONGLONG IntT5;
      ULONGLONG IntT6;
      ULONGLONG IntT7;
      ULONGLONG IntT8;
      ULONGLONG IntT9;
      ULONGLONG IntT10;
      ULONGLONG IntT11;
      ULONGLONG IntT12;
      ULONGLONG IntT13;
      ULONGLONG IntT14;
      ULONGLONG IntT15;
      ULONGLONG IntT16;
      ULONGLONG IntT17;
      ULONGLONG IntT18;
      ULONGLONG IntT19;
      ULONGLONG IntT20;
      ULONGLONG IntT21;
      ULONGLONG IntT22;
      ULONGLONG IntNats;
      ULONGLONG Preds;
      ULONGLONG BrRp;
      ULONGLONG BrS0;
      ULONGLONG BrS1;
      ULONGLONG BrS2;
      ULONGLONG BrS3;
      ULONGLONG BrS4;
      ULONGLONG BrT0;
      ULONGLONG BrT1;
      ULONGLONG ApUNAT;
      ULONGLONG ApLC;
      ULONGLONG ApEC;
      ULONGLONG ApCCV;
      ULONGLONG ApDCR;
      ULONGLONG RsPFS;
      ULONGLONG RsBSP;
      ULONGLONG RsBSPSTORE;
      ULONGLONG RsRSC;
      ULONGLONG RsRNAT;
      ULONGLONG StIPSR;
      ULONGLONG StIIP;
      ULONGLONG StIFS;
      ULONGLONG StFCR;
      ULONGLONG Eflag;
      ULONGLONG SegCSD;
      ULONGLONG SegSSD;
      ULONGLONG Cflag;
      ULONGLONG StFSR;
      ULONGLONG StFIR;
      ULONGLONG StFDR;
      ULONGLONG UNUSEDPACK;
    } CONTEXT,*PCONTEXT;

    typedef struct _PLABEL_DESCRIPTOR {
      ULONGLONG EntryPoint;
      ULONGLONG GlobalPointer;
    } PLABEL_DESCRIPTOR,*PPLABEL_DESCRIPTOR;

    typedef struct _RUNTIME_FUNCTION {
      DWORD BeginAddress;
      DWORD EndAddress;
      DWORD UnwindInfoAddress;
    } RUNTIME_FUNCTION,*PRUNTIME_FUNCTION;

    typedef PRUNTIME_FUNCTION (*PGET_RUNTIME_FUNCTION_CALLBACK)(DWORD64 ControlPc,PVOID Context);
    typedef DWORD (*POUT_OF_PROCESS_FUNCTION_TABLE_CALLBACK)(HANDLE Process,PVOID TableAddress,PDWORD Entries,PRUNTIME_FUNCTION *Functions);

#define OUT_OF_PROCESS_FUNCTION_TABLE_CALLBACK_EXPORT_NAME "OutOfProcessFunctionTableCallback"

    BOOLEAN RtlAddFunctionTable(PRUNTIME_FUNCTION FunctionTable,DWORD EntryCount,ULONGLONG BaseAddress,ULONGLONG TargetGp);
    BOOLEAN RtlInstallFunctionTableCallback(DWORD64 TableIdentifier,DWORD64 BaseAddress,DWORD Length,DWORD64 TargetGp,PGET_RUNTIME_FUNCTION_CALLBACK Callback,PVOID Context,PCWSTR OutOfProcessCallbackDll);
    BOOLEAN RtlDeleteFunctionTable(PRUNTIME_FUNCTION FunctionTable);
    VOID RtlRestoreContext (PCONTEXT ContextRecord,struct _EXCEPTION_RECORD *ExceptionRecord);
    VOID __jump_unwind(ULONGLONG TargetMsFrame,ULONGLONG TargetBsFrame,ULONGLONG TargetPc);
#endif

#define EXCEPTION_NONCONTINUABLE 0x1
#define EXCEPTION_MAXIMUM_PARAMETERS 15

    typedef struct _EXCEPTION_RECORD {
      DWORD ExceptionCode;
      DWORD ExceptionFlags;
      struct _EXCEPTION_RECORD *ExceptionRecord;
      PVOID ExceptionAddress;
      DWORD NumberParameters;
      ULONG_PTR ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
    } EXCEPTION_RECORD;

    typedef EXCEPTION_RECORD *PEXCEPTION_RECORD;

    typedef struct _EXCEPTION_RECORD32 {
      DWORD ExceptionCode;
      DWORD ExceptionFlags;
      DWORD ExceptionRecord;
      DWORD ExceptionAddress;
      DWORD NumberParameters;
      DWORD ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
    } EXCEPTION_RECORD32,*PEXCEPTION_RECORD32;

    typedef struct _EXCEPTION_RECORD64 {
      DWORD ExceptionCode;
      DWORD ExceptionFlags;
      DWORD64 ExceptionRecord;
      DWORD64 ExceptionAddress;
      DWORD NumberParameters;
      DWORD __unusedAlignment;
      DWORD64 ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
    } EXCEPTION_RECORD64,*PEXCEPTION_RECORD64;

    typedef struct _EXCEPTION_POINTERS {
      PEXCEPTION_RECORD ExceptionRecord;
      PCONTEXT ContextRecord;
    } EXCEPTION_POINTERS,*PEXCEPTION_POINTERS;

#ifdef __x86_64

    typedef EXCEPTION_DISPOSITION NTAPI EXCEPTION_ROUTINE (struct _EXCEPTION_RECORD *ExceptionRecord, PVOID EstablisherFrame, struct _CONTEXT *ContextRecord, PVOID DispatcherContext);
#ifndef __PEXCEPTION_ROUTINE_DEFINED
#define __PEXCEPTION_ROUTINE_DEFINED
    typedef EXCEPTION_ROUTINE *PEXCEPTION_ROUTINE;
#endif

    /* http://msdn.microsoft.com/en-us/library/ms680597(VS.85).aspx */

#define UNWIND_HISTORY_TABLE_SIZE 12

  typedef struct _UNWIND_HISTORY_TABLE_ENTRY {
    ULONG64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
  } UNWIND_HISTORY_TABLE_ENTRY, *PUNWIND_HISTORY_TABLE_ENTRY;

#define UNWIND_HISTORY_TABLE_NONE    0
#define UNWIND_HISTORY_TABLE_GLOBAL  1
#define UNWIND_HISTORY_TABLE_LOCAL   2

  typedef struct _UNWIND_HISTORY_TABLE {
    ULONG Count;
    UCHAR Search;
    ULONG64 LowAddress;
    ULONG64 HighAddress;
    UNWIND_HISTORY_TABLE_ENTRY Entry[UNWIND_HISTORY_TABLE_SIZE];
  } UNWIND_HISTORY_TABLE, *PUNWIND_HISTORY_TABLE;

  /* http://msdn.microsoft.com/en-us/library/b6sf5kbd(VS.80).aspx */

  struct _DISPATCHER_CONTEXT;
  typedef struct _DISPATCHER_CONTEXT DISPATCHER_CONTEXT;
  typedef struct _DISPATCHER_CONTEXT *PDISPATCHER_CONTEXT;

  struct _DISPATCHER_CONTEXT {
    ULONG64 ControlPc;
    ULONG64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG64 EstablisherFrame;
    ULONG64 TargetIp;
    PCONTEXT ContextRecord;
    PEXCEPTION_ROUTINE LanguageHandler;
    PVOID HandlerData;
    /* http://www.nynaeve.net/?p=99 */
    PUNWIND_HISTORY_TABLE HistoryTable;
    ULONG ScopeIndex;
    ULONG Fill0;
  };

  /* http://msdn.microsoft.com/en-us/library/ms680617(VS.85).aspx */

  typedef struct _KNONVOLATILE_CONTEXT_POINTERS
  {
    PM128A FloatingContext[16];
    PULONG64 IntegerContext[16];
  } KNONVOLATILE_CONTEXT_POINTERS, *PKNONVOLATILE_CONTEXT_POINTERS;
#endif /* defined(__x86_64) */

    typedef PVOID PACCESS_TOKEN;
    typedef PVOID PSECURITY_DESCRIPTOR;
    typedef PVOID PSID;

    typedef DWORD ACCESS_MASK;
    typedef ACCESS_MASK *PACCESS_MASK;

#define DELETE (0x00010000L)
#define READ_CONTROL (0x00020000L)
#define WRITE_DAC (0x00040000L)
#define WRITE_OWNER (0x00080000L)
#define SYNCHRONIZE (0x00100000L)

#define STANDARD_RIGHTS_REQUIRED (0x000F0000L)
#define STANDARD_RIGHTS_READ (READ_CONTROL)
#define STANDARD_RIGHTS_WRITE (READ_CONTROL)
#define STANDARD_RIGHTS_EXECUTE (READ_CONTROL)
#define STANDARD_RIGHTS_ALL (0x001F0000L)

#define SPECIFIC_RIGHTS_ALL (0x0000FFFFL)

#define ACCESS_SYSTEM_SECURITY (0x01000000L)

#define MAXIMUM_ALLOWED (0x02000000L)

#define GENERIC_READ (0x80000000L)
#define GENERIC_WRITE (0x40000000L)
#define GENERIC_EXECUTE (0x20000000L)
#define GENERIC_ALL (0x10000000L)

    typedef struct _GENERIC_MAPPING {
      ACCESS_MASK GenericRead;
      ACCESS_MASK GenericWrite;
      ACCESS_MASK GenericExecute;
      ACCESS_MASK GenericAll;
    } GENERIC_MAPPING;
    typedef GENERIC_MAPPING *PGENERIC_MAPPING;

#include <pshpack4.h>

    typedef struct _LUID_AND_ATTRIBUTES {
      LUID Luid;
      DWORD Attributes;
    } LUID_AND_ATTRIBUTES,*PLUID_AND_ATTRIBUTES;
    typedef LUID_AND_ATTRIBUTES LUID_AND_ATTRIBUTES_ARRAY[ANYSIZE_ARRAY];
    typedef LUID_AND_ATTRIBUTES_ARRAY *PLUID_AND_ATTRIBUTES_ARRAY;

#include <poppack.h>

#ifndef SID_IDENTIFIER_AUTHORITY_DEFINED
#define SID_IDENTIFIER_AUTHORITY_DEFINED
    typedef struct _SID_IDENTIFIER_AUTHORITY {
      BYTE Value[6];
    } SID_IDENTIFIER_AUTHORITY,*PSID_IDENTIFIER_AUTHORITY;
#endif

#ifndef SID_DEFINED
#define SID_DEFINED
    typedef struct _SID {
      BYTE Revision;
      BYTE SubAuthorityCount;
      SID_IDENTIFIER_AUTHORITY IdentifierAuthority;
      DWORD SubAuthority[ANYSIZE_ARRAY];
    } SID,*PISID;
#endif

#define SID_REVISION (1)
#define SID_MAX_SUB_AUTHORITIES (15)
#define SID_RECOMMENDED_SUB_AUTHORITIES (1)

#define SECURITY_MAX_SID_SIZE (sizeof(SID) - sizeof(DWORD) + (SID_MAX_SUB_AUTHORITIES *sizeof(DWORD)))

    typedef enum _SID_NAME_USE {
      SidTypeUser = 1,SidTypeGroup,SidTypeDomain,SidTypeAlias,SidTypeWellKnownGroup,SidTypeDeletedAccount,SidTypeInvalid,SidTypeUnknown,SidTypeComputer
    } SID_NAME_USE,*PSID_NAME_USE;

    typedef struct _SID_AND_ATTRIBUTES {
      PSID Sid;
      DWORD Attributes;
    } SID_AND_ATTRIBUTES,*PSID_AND_ATTRIBUTES;

    typedef SID_AND_ATTRIBUTES SID_AND_ATTRIBUTES_ARRAY[ANYSIZE_ARRAY];
    typedef SID_AND_ATTRIBUTES_ARRAY *PSID_AND_ATTRIBUTES_ARRAY;

#define SECURITY_NULL_SID_AUTHORITY {0,0,0,0,0,0}
#define SECURITY_WORLD_SID_AUTHORITY {0,0,0,0,0,1}
#define SECURITY_LOCAL_SID_AUTHORITY {0,0,0,0,0,2}
#define SECURITY_CREATOR_SID_AUTHORITY {0,0,0,0,0,3}
#define SECURITY_NON_UNIQUE_AUTHORITY {0,0,0,0,0,4}
#define SECURITY_RESOURCE_MANAGER_AUTHORITY {0,0,0,0,0,9}

#define SECURITY_NULL_RID (0x00000000L)
#define SECURITY_WORLD_RID (0x00000000L)
#define SECURITY_LOCAL_RID (0x00000000L)

#define SECURITY_CREATOR_OWNER_RID (0x00000000L)
#define SECURITY_CREATOR_GROUP_RID (0x00000001L)

#define SECURITY_CREATOR_OWNER_SERVER_RID (0x00000002L)
#define SECURITY_CREATOR_GROUP_SERVER_RID (0x00000003L)

#define SECURITY_NT_AUTHORITY {0,0,0,0,0,5}

#define SECURITY_DIALUP_RID (0x00000001L)
#define SECURITY_NETWORK_RID (0x00000002L)
#define SECURITY_BATCH_RID (0x00000003L)
#define SECURITY_INTERACTIVE_RID (0x00000004L)
#define SECURITY_LOGON_IDS_RID (0x00000005L)
#define SECURITY_LOGON_IDS_RID_COUNT (3L)
#define SECURITY_SERVICE_RID (0x00000006L)
#define SECURITY_ANONYMOUS_LOGON_RID (0x00000007L)
#define SECURITY_PROXY_RID (0x00000008L)
#define SECURITY_ENTERPRISE_CONTROLLERS_RID (0x00000009L)
#define SECURITY_SERVER_LOGON_RID SECURITY_ENTERPRISE_CONTROLLERS_RID
#define SECURITY_PRINCIPAL_SELF_RID (0x0000000AL)
#define SECURITY_AUTHENTICATED_USER_RID (0x0000000BL)
#define SECURITY_RESTRICTED_CODE_RID (0x0000000CL)
#define SECURITY_TERMINAL_SERVER_RID (0x0000000DL)
#define SECURITY_REMOTE_LOGON_RID (0x0000000EL)
#define SECURITY_THIS_ORGANIZATION_RID (0x0000000FL)
#define SECURITY_IUSER_RID (0x00000011L)

#define SECURITY_LOCAL_SYSTEM_RID (0x00000012L)
#define SECURITY_LOCAL_SERVICE_RID (0x00000013L)
#define SECURITY_NETWORK_SERVICE_RID (0x00000014L)

#define SECURITY_NT_NON_UNIQUE (0x00000015L)
#define SECURITY_NT_NON_UNIQUE_SUB_AUTH_COUNT (3L)

#define SECURITY_ENTERPRISE_READONLY_CONTROLLERS_RID (0x00000016L)

#define SECURITY_BUILTIN_DOMAIN_RID (0x00000020L)
#define SECURITY_WRITE_RESTRICTED_CODE_RID (0x00000021L)

#define SECURITY_PACKAGE_BASE_RID (0x00000040L)
#define SECURITY_PACKAGE_RID_COUNT (2L)
#define SECURITY_PACKAGE_NTLM_RID (0x0000000AL)
#define SECURITY_PACKAGE_SCHANNEL_RID (0x0000000EL)
#define SECURITY_PACKAGE_DIGEST_RID (0x00000015L)

#define SECURITY_SERVICE_ID_BASE_RID (0x00000050L)
#define SECURITY_SERVICE_ID_RID_COUNT (6L)

#define SECURITY_RESERVED_ID_BASE_RID (0x00000051L)

#define SECURITY_MAX_ALWAYS_FILTERED (0x000003E7L)
#define SECURITY_MIN_NEVER_FILTERED (0x000003E8L)

#define SECURITY_OTHER_ORGANIZATION_RID (0x000003E8L)

#define FOREST_USER_RID_MAX (0x000001F3L)

#define DOMAIN_USER_RID_ADMIN (0x000001F4L)
#define DOMAIN_USER_RID_GUEST (0x000001F5L)
#define DOMAIN_USER_RID_KRBTGT (0x000001F6L)

#define DOMAIN_USER_RID_MAX (0x000003E7L)

#define DOMAIN_GROUP_RID_ADMINS (0x00000200L)
#define DOMAIN_GROUP_RID_USERS (0x00000201L)
#define DOMAIN_GROUP_RID_GUESTS (0x00000202L)
#define DOMAIN_GROUP_RID_COMPUTERS (0x00000203L)
#define DOMAIN_GROUP_RID_CONTROLLERS (0x00000204L)
#define DOMAIN_GROUP_RID_CERT_ADMINS (0x00000205L)
#define DOMAIN_GROUP_RID_SCHEMA_ADMINS (0x00000206L)
#define DOMAIN_GROUP_RID_ENTERPRISE_ADMINS (0x00000207L)
#define DOMAIN_GROUP_RID_POLICY_ADMINS (0x00000208L)
#define DOMAIN_GROUP_RID_READONLY_CONTROLLERS (0x00000209L)

#define DOMAIN_ALIAS_RID_ADMINS (0x00000220L)
#define DOMAIN_ALIAS_RID_USERS (0x00000221L)
#define DOMAIN_ALIAS_RID_GUESTS (0x00000222L)
#define DOMAIN_ALIAS_RID_POWER_USERS (0x00000223L)

#define DOMAIN_ALIAS_RID_ACCOUNT_OPS (0x00000224L)
#define DOMAIN_ALIAS_RID_SYSTEM_OPS (0x00000225L)
#define DOMAIN_ALIAS_RID_PRINT_OPS (0x00000226L)
#define DOMAIN_ALIAS_RID_BACKUP_OPS (0x00000227L)

#define DOMAIN_ALIAS_RID_REPLICATOR (0x00000228L)
#define DOMAIN_ALIAS_RID_RAS_SERVERS (0x00000229L)
#define DOMAIN_ALIAS_RID_PREW2KCOMPACCESS (0x0000022AL)
#define DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS (0x0000022BL)
#define DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS (0x0000022CL)
#define DOMAIN_ALIAS_RID_INCOMING_FOREST_TRUST_BUILDERS (0x0000022DL)

#define DOMAIN_ALIAS_RID_MONITORING_USERS (0x0000022EL)
#define DOMAIN_ALIAS_RID_LOGGING_USERS (0x0000022FL)
#define DOMAIN_ALIAS_RID_AUTHORIZATIONACCESS (0x00000230L)
#define DOMAIN_ALIAS_RID_TS_LICENSE_SERVERS (0x00000231L)
#define DOMAIN_ALIAS_RID_DCOM_USERS (0x00000232L)

#define DOMAIN_ALIAS_RID_IUSERS (0x00000238L)
#define DOMAIN_ALIAS_RID_CRYPTO_OPERATORS (0x00000239L)
#define DOMAIN_ALIAS_RID_CACHEABLE_PRINCIPALS_GROUP (0x0000023BL)
#define DOMAIN_ALIAS_RID_NON_CACHEABLE_PRINCIPALS_GROUP (0x0000023CL)
#define DOMAIN_ALIAS_RID_EVENT_LOG_READERS_GROUP (0x0000023DL)

#define SECURITY_MANDATORY_LABEL_AUTHORITY {0,0,0,0,0,16}
#define SECURITY_MANDATORY_UNTRUSTED_RID (0x00000000L)
#define SECURITY_MANDATORY_LOW_RID (0x00001000L)
#define SECURITY_MANDATORY_MEDIUM_RID (0x00002000L)
#define SECURITY_MANDATORY_HIGH_RID (0x00003000L)
#define SECURITY_MANDATORY_SYSTEM_RID (0x00004000L)
#define SECURITY_MANDATORY_PROTECTED_PROCESS_RID (0x00005000L)

#define SECURITY_MANDATORY_MAXIMUM_USER_RID SECURITY_MANDATORY_SYSTEM_RID

#define MANDATORY_LEVEL_TO_MANDATORY_RID(IL) (IL * 0x1000)

    typedef enum {
      WinNullSid = 0,WinWorldSid = 1,WinLocalSid = 2,WinCreatorOwnerSid = 3,WinCreatorGroupSid = 4,WinCreatorOwnerServerSid = 5,WinCreatorGroupServerSid = 6,WinNtAuthoritySid = 7,WinDialupSid = 8,WinNetworkSid = 9,WinBatchSid = 10,WinInteractiveSid = 11,WinServiceSid = 12,WinAnonymousSid = 13,WinProxySid = 14,WinEnterpriseControllersSid = 15,WinSelfSid = 16,WinAuthenticatedUserSid = 17,WinRestrictedCodeSid = 18,WinTerminalServerSid = 19,WinRemoteLogonIdSid = 20,WinLogonIdsSid = 21,WinLocalSystemSid = 22,WinLocalServiceSid = 23,WinNetworkServiceSid = 24,WinBuiltinDomainSid = 25,WinBuiltinAdministratorsSid = 26,WinBuiltinUsersSid = 27,WinBuiltinGuestsSid = 28,WinBuiltinPowerUsersSid = 29,WinBuiltinAccountOperatorsSid = 30,WinBuiltinSystemOperatorsSid = 31,WinBuiltinPrintOperatorsSid = 32,WinBuiltinBackupOperatorsSid = 33,WinBuiltinReplicatorSid = 34,WinBuiltinPreWindows2000CompatibleAccessSid = 35,WinBuiltinRemoteDesktopUsersSid = 36,WinBuiltinNetworkConfigurationOperatorsSid = 37,WinAccountAdministratorSid = 38,WinAccountGuestSid = 39,WinAccountKrbtgtSid = 40,WinAccountDomainAdminsSid = 41,WinAccountDomainUsersSid = 42,WinAccountDomainGuestsSid = 43,WinAccountComputersSid = 44,WinAccountControllersSid = 45,WinAccountCertAdminsSid = 46,WinAccountSchemaAdminsSid = 47,WinAccountEnterpriseAdminsSid = 48,WinAccountPolicyAdminsSid = 49,WinAccountRasAndIasServersSid = 50,WinNTLMAuthenticationSid = 51,WinDigestAuthenticationSid = 52,WinSChannelAuthenticationSid = 53,WinThisOrganizationSid = 54,WinOtherOrganizationSid = 55,WinBuiltinIncomingForestTrustBuildersSid = 56,WinBuiltinPerfMonitoringUsersSid = 57,WinBuiltinPerfLoggingUsersSid = 58,WinBuiltinAuthorizationAccessSid = 59,WinBuiltinTerminalServerLicenseServersSid = 60,WinBuiltinDCOMUsersSid = 61
    } WELL_KNOWN_SID_TYPE;

#define SYSTEM_LUID { 0x3E7,0x0 }
#define ANONYMOUS_LOGON_LUID { 0x3e6,0x0 }
#define LOCALSERVICE_LUID { 0x3e5,0x0 }
#define NETWORKSERVICE_LUID { 0x3e4,0x0 }
#define IUSER_LUID { 0x3e3, 0x0 }

#define SE_GROUP_MANDATORY (0x00000001L)
#define SE_GROUP_ENABLED_BY_DEFAULT (0x00000002L)
#define SE_GROUP_ENABLED (0x00000004L)
#define SE_GROUP_OWNER (0x00000008L)
#define SE_GROUP_USE_FOR_DENY_ONLY (0x00000010L)
#define SE_GROUP_INTEGRITY (0x00000020L)
#define SE_GROUP_INTEGRITY_ENABLED (0x00000040L)
#define SE_GROUP_LOGON_ID (0xC0000000L)
#define SE_GROUP_RESOURCE (0x20000000L)

#define ACL_REVISION (2)
#define ACL_REVISION_DS (4)

#define ACL_REVISION1 (1)
#define MIN_ACL_REVISION ACL_REVISION2
#define ACL_REVISION2 (2)
#define ACL_REVISION3 (3)
#define ACL_REVISION4 (4)
#define MAX_ACL_REVISION ACL_REVISION4

    typedef struct _ACL {
      BYTE AclRevision;
      BYTE Sbz1;
      WORD AclSize;
      WORD AceCount;
      WORD Sbz2;
    } ACL;
    typedef ACL *PACL;

    typedef struct _ACE_HEADER {
      BYTE AceType;
      BYTE AceFlags;
      WORD AceSize;
    } ACE_HEADER;
    typedef ACE_HEADER *PACE_HEADER;

#define ACCESS_MIN_MS_ACE_TYPE (0x0)
#define ACCESS_ALLOWED_ACE_TYPE (0x0)
#define ACCESS_DENIED_ACE_TYPE (0x1)
#define SYSTEM_AUDIT_ACE_TYPE (0x2)
#define SYSTEM_ALARM_ACE_TYPE (0x3)
#define ACCESS_MAX_MS_V2_ACE_TYPE (0x3)

#define ACCESS_ALLOWED_COMPOUND_ACE_TYPE (0x4)
#define ACCESS_MAX_MS_V3_ACE_TYPE (0x4)

#define ACCESS_MIN_MS_OBJECT_ACE_TYPE (0x5)
#define ACCESS_ALLOWED_OBJECT_ACE_TYPE (0x5)
#define ACCESS_DENIED_OBJECT_ACE_TYPE (0x6)
#define SYSTEM_AUDIT_OBJECT_ACE_TYPE (0x7)
#define SYSTEM_ALARM_OBJECT_ACE_TYPE (0x8)
#define ACCESS_MAX_MS_OBJECT_ACE_TYPE (0x8)

#define ACCESS_MAX_MS_V4_ACE_TYPE (0x8)
#define ACCESS_MAX_MS_ACE_TYPE (0x8)

#define ACCESS_ALLOWED_CALLBACK_ACE_TYPE (0x9)
#define ACCESS_DENIED_CALLBACK_ACE_TYPE (0xA)
#define ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE (0xB)
#define ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE (0xC)
#define SYSTEM_AUDIT_CALLBACK_ACE_TYPE (0xD)
#define SYSTEM_ALARM_CALLBACK_ACE_TYPE (0xE)
#define SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE (0xF)
#define SYSTEM_ALARM_CALLBACK_OBJECT_ACE_TYPE (0x10)

#define SYSTEM_MANDATORY_LABEL_ACE_TYPE (0x11)
#define ACCESS_MAX_MS_V5_ACE_TYPE (0x11)

#define OBJECT_INHERIT_ACE (0x1)
#define CONTAINER_INHERIT_ACE (0x2)
#define NO_PROPAGATE_INHERIT_ACE (0x4)
#define INHERIT_ONLY_ACE (0x8)
#define INHERITED_ACE (0x10)
#define VALID_INHERIT_FLAGS (0x1F)

#define SUCCESSFUL_ACCESS_ACE_FLAG (0x40)
#define FAILED_ACCESS_ACE_FLAG (0x80)

    typedef struct _ACCESS_ALLOWED_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;
    } ACCESS_ALLOWED_ACE;

    typedef ACCESS_ALLOWED_ACE *PACCESS_ALLOWED_ACE;

    typedef struct _ACCESS_DENIED_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;
    } ACCESS_DENIED_ACE;
    typedef ACCESS_DENIED_ACE *PACCESS_DENIED_ACE;

    typedef struct _SYSTEM_AUDIT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;
    } SYSTEM_AUDIT_ACE;
    typedef SYSTEM_AUDIT_ACE *PSYSTEM_AUDIT_ACE;

    typedef struct _SYSTEM_ALARM_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;
    } SYSTEM_ALARM_ACE;
    typedef SYSTEM_ALARM_ACE *PSYSTEM_ALARM_ACE;

    typedef struct _ACCESS_ALLOWED_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;
    } ACCESS_ALLOWED_OBJECT_ACE,*PACCESS_ALLOWED_OBJECT_ACE;

    typedef struct _ACCESS_DENIED_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;
    } ACCESS_DENIED_OBJECT_ACE,*PACCESS_DENIED_OBJECT_ACE;

    typedef struct _SYSTEM_AUDIT_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;
    } SYSTEM_AUDIT_OBJECT_ACE,*PSYSTEM_AUDIT_OBJECT_ACE;

    typedef struct _SYSTEM_ALARM_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;
    } SYSTEM_ALARM_OBJECT_ACE,*PSYSTEM_ALARM_OBJECT_ACE;

    typedef struct _ACCESS_ALLOWED_CALLBACK_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;

    } ACCESS_ALLOWED_CALLBACK_ACE,*PACCESS_ALLOWED_CALLBACK_ACE;

    typedef struct _ACCESS_DENIED_CALLBACK_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;

    } ACCESS_DENIED_CALLBACK_ACE,*PACCESS_DENIED_CALLBACK_ACE;

    typedef struct _SYSTEM_AUDIT_CALLBACK_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;

    } SYSTEM_AUDIT_CALLBACK_ACE,*PSYSTEM_AUDIT_CALLBACK_ACE;

    typedef struct _SYSTEM_ALARM_CALLBACK_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD SidStart;

    } SYSTEM_ALARM_CALLBACK_ACE,*PSYSTEM_ALARM_CALLBACK_ACE;

    typedef struct _ACCESS_ALLOWED_CALLBACK_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;

    } ACCESS_ALLOWED_CALLBACK_OBJECT_ACE,*PACCESS_ALLOWED_CALLBACK_OBJECT_ACE;

    typedef struct _ACCESS_DENIED_CALLBACK_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;

    } ACCESS_DENIED_CALLBACK_OBJECT_ACE,*PACCESS_DENIED_CALLBACK_OBJECT_ACE;

    typedef struct _SYSTEM_AUDIT_CALLBACK_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;

    } SYSTEM_AUDIT_CALLBACK_OBJECT_ACE,*PSYSTEM_AUDIT_CALLBACK_OBJECT_ACE;

    typedef struct _SYSTEM_ALARM_CALLBACK_OBJECT_ACE {
      ACE_HEADER Header;
      ACCESS_MASK Mask;
      DWORD Flags;
      GUID ObjectType;
      GUID InheritedObjectType;
      DWORD SidStart;

    } SYSTEM_ALARM_CALLBACK_OBJECT_ACE,*PSYSTEM_ALARM_CALLBACK_OBJECT_ACE;

#define ACE_OBJECT_TYPE_PRESENT 0x1
#define ACE_INHERITED_OBJECT_TYPE_PRESENT 0x2

    typedef enum _ACL_INFORMATION_CLASS {
      AclRevisionInformation = 1,AclSizeInformation
    } ACL_INFORMATION_CLASS;

    typedef struct _ACL_REVISION_INFORMATION {
      DWORD AclRevision;
    } ACL_REVISION_INFORMATION;
    typedef ACL_REVISION_INFORMATION *PACL_REVISION_INFORMATION;

    typedef struct _ACL_SIZE_INFORMATION {
      DWORD AceCount;
      DWORD AclBytesInUse;
      DWORD AclBytesFree;
    } ACL_SIZE_INFORMATION;
    typedef ACL_SIZE_INFORMATION *PACL_SIZE_INFORMATION;

#define SECURITY_DESCRIPTOR_REVISION (1)
#define SECURITY_DESCRIPTOR_REVISION1 (1)

#define SECURITY_DESCRIPTOR_MIN_LENGTH (sizeof(SECURITY_DESCRIPTOR))

    typedef WORD SECURITY_DESCRIPTOR_CONTROL,*PSECURITY_DESCRIPTOR_CONTROL;

#define SE_OWNER_DEFAULTED (0x0001)
#define SE_GROUP_DEFAULTED (0x0002)
#define SE_DACL_PRESENT (0x0004)
#define SE_DACL_DEFAULTED (0x0008)
#define SE_SACL_PRESENT (0x0010)
#define SE_SACL_DEFAULTED (0x0020)
#define SE_DACL_AUTO_INHERIT_REQ (0x0100)
#define SE_SACL_AUTO_INHERIT_REQ (0x0200)
#define SE_DACL_AUTO_INHERITED (0x0400)
#define SE_SACL_AUTO_INHERITED (0x0800)
#define SE_DACL_PROTECTED (0x1000)
#define SE_SACL_PROTECTED (0x2000)
#define SE_RM_CONTROL_VALID (0x4000)
#define SE_SELF_RELATIVE (0x8000)

    typedef struct _SECURITY_DESCRIPTOR_RELATIVE {
      BYTE Revision;
      BYTE Sbz1;
      SECURITY_DESCRIPTOR_CONTROL Control;
      DWORD Owner;
      DWORD Group;
      DWORD Sacl;
      DWORD Dacl;
    } SECURITY_DESCRIPTOR_RELATIVE,*PISECURITY_DESCRIPTOR_RELATIVE;

    typedef struct _SECURITY_DESCRIPTOR {
      BYTE Revision;
      BYTE Sbz1;
      SECURITY_DESCRIPTOR_CONTROL Control;
      PSID Owner;
      PSID Group;
      PACL Sacl;
      PACL Dacl;

    } SECURITY_DESCRIPTOR,*PISECURITY_DESCRIPTOR;

    typedef struct _OBJECT_TYPE_LIST {
      WORD Level;
      WORD Sbz;
      GUID *ObjectType;
    } OBJECT_TYPE_LIST,*POBJECT_TYPE_LIST;

#define ACCESS_OBJECT_GUID 0
#define ACCESS_PROPERTY_SET_GUID 1
#define ACCESS_PROPERTY_GUID 2

#define ACCESS_MAX_LEVEL 4

    typedef enum _AUDIT_EVENT_TYPE {
      AuditEventObjectAccess,AuditEventDirectoryServiceAccess
    } AUDIT_EVENT_TYPE,*PAUDIT_EVENT_TYPE;

#define AUDIT_ALLOW_NO_PRIVILEGE 0x1

#define ACCESS_DS_SOURCE_A "DS"
#define ACCESS_DS_SOURCE_W L"DS"
#define ACCESS_DS_OBJECT_TYPE_NAME_A "Directory Service Object"
#define ACCESS_DS_OBJECT_TYPE_NAME_W L"Directory Service Object"

#define SE_PRIVILEGE_ENABLED_BY_DEFAULT (0x00000001L)
#define SE_PRIVILEGE_ENABLED (0x00000002L)
#define SE_PRIVILEGE_REMOVED (0X00000004L)
#define SE_PRIVILEGE_USED_FOR_ACCESS (0x80000000L)

#define PRIVILEGE_SET_ALL_NECESSARY (1)

    typedef struct _PRIVILEGE_SET {
      DWORD PrivilegeCount;
      DWORD Control;
      LUID_AND_ATTRIBUTES Privilege[ANYSIZE_ARRAY];
    } PRIVILEGE_SET,*PPRIVILEGE_SET;

#define SE_CREATE_TOKEN_NAME TEXT("SeCreateTokenPrivilege")
#define SE_ASSIGNPRIMARYTOKEN_NAME TEXT("SeAssignPrimaryTokenPrivilege")
#define SE_LOCK_MEMORY_NAME TEXT("SeLockMemoryPrivilege")
#define SE_INCREASE_QUOTA_NAME TEXT("SeIncreaseQuotaPrivilege")
#define SE_UNSOLICITED_INPUT_NAME TEXT("SeUnsolicitedInputPrivilege")
#define SE_MACHINE_ACCOUNT_NAME TEXT("SeMachineAccountPrivilege")
#define SE_TCB_NAME TEXT("SeTcbPrivilege")
#define SE_SECURITY_NAME TEXT("SeSecurityPrivilege")
#define SE_TAKE_OWNERSHIP_NAME TEXT("SeTakeOwnershipPrivilege")
#define SE_LOAD_DRIVER_NAME TEXT("SeLoadDriverPrivilege")
#define SE_SYSTEM_PROFILE_NAME TEXT("SeSystemProfilePrivilege")
#define SE_SYSTEMTIME_NAME TEXT("SeSystemtimePrivilege")
#define SE_PROF_SINGLE_PROCESS_NAME TEXT("SeProfileSingleProcessPrivilege")
#define SE_INC_BASE_PRIORITY_NAME TEXT("SeIncreaseBasePriorityPrivilege")
#define SE_CREATE_PAGEFILE_NAME TEXT("SeCreatePagefilePrivilege")
#define SE_CREATE_PERMANENT_NAME TEXT("SeCreatePermanentPrivilege")
#define SE_BACKUP_NAME TEXT("SeBackupPrivilege")
#define SE_RESTORE_NAME TEXT("SeRestorePrivilege")
#define SE_SHUTDOWN_NAME TEXT("SeShutdownPrivilege")
#define SE_DEBUG_NAME TEXT("SeDebugPrivilege")
#define SE_AUDIT_NAME TEXT("SeAuditPrivilege")
#define SE_SYSTEM_ENVIRONMENT_NAME TEXT("SeSystemEnvironmentPrivilege")
#define SE_CHANGE_NOTIFY_NAME TEXT("SeChangeNotifyPrivilege")
#define SE_REMOTE_SHUTDOWN_NAME TEXT("SeRemoteShutdownPrivilege")
#define SE_UNDOCK_NAME TEXT("SeUndockPrivilege")
#define SE_SYNC_AGENT_NAME TEXT("SeSyncAgentPrivilege")
#define SE_ENABLE_DELEGATION_NAME TEXT("SeEnableDelegationPrivilege")
#define SE_MANAGE_VOLUME_NAME TEXT("SeManageVolumePrivilege")
#define SE_IMPERSONATE_NAME TEXT("SeImpersonatePrivilege")
#define SE_CREATE_GLOBAL_NAME TEXT("SeCreateGlobalPrivilege")

    typedef enum _SECURITY_IMPERSONATION_LEVEL {
      SecurityAnonymous,SecurityIdentification,SecurityImpersonation,SecurityDelegation
    } SECURITY_IMPERSONATION_LEVEL,*PSECURITY_IMPERSONATION_LEVEL;

#define SECURITY_MAX_IMPERSONATION_LEVEL SecurityDelegation
#define SECURITY_MIN_IMPERSONATION_LEVEL SecurityAnonymous
#define DEFAULT_IMPERSONATION_LEVEL SecurityImpersonation
#define VALID_IMPERSONATION_LEVEL(L) (((L) >= SECURITY_MIN_IMPERSONATION_LEVEL) && ((L) <= SECURITY_MAX_IMPERSONATION_LEVEL))

#define TOKEN_ASSIGN_PRIMARY (0x0001)
#define TOKEN_DUPLICATE (0x0002)
#define TOKEN_IMPERSONATE (0x0004)
#define TOKEN_QUERY (0x0008)
#define TOKEN_QUERY_SOURCE (0x0010)
#define TOKEN_ADJUST_PRIVILEGES (0x0020)
#define TOKEN_ADJUST_GROUPS (0x0040)
#define TOKEN_ADJUST_DEFAULT (0x0080)
#define TOKEN_ADJUST_SESSIONID (0x0100)

#define TOKEN_ALL_ACCESS_P (STANDARD_RIGHTS_REQUIRED | TOKEN_ASSIGN_PRIMARY | TOKEN_DUPLICATE | TOKEN_IMPERSONATE | TOKEN_QUERY | TOKEN_QUERY_SOURCE | TOKEN_ADJUST_PRIVILEGES | TOKEN_ADJUST_GROUPS | TOKEN_ADJUST_DEFAULT)
#define TOKEN_ALL_ACCESS (TOKEN_ALL_ACCESS_P | TOKEN_ADJUST_SESSIONID)
#define TOKEN_READ (STANDARD_RIGHTS_READ | TOKEN_QUERY)

#define TOKEN_WRITE (STANDARD_RIGHTS_WRITE | TOKEN_ADJUST_PRIVILEGES | TOKEN_ADJUST_GROUPS | TOKEN_ADJUST_DEFAULT)

#define TOKEN_EXECUTE (STANDARD_RIGHTS_EXECUTE)

    typedef enum _TOKEN_TYPE {
      TokenPrimary = 1,TokenImpersonation
    } TOKEN_TYPE;
    typedef TOKEN_TYPE *PTOKEN_TYPE;

    typedef enum _TOKEN_INFORMATION_CLASS {
      TokenUser = 1,TokenGroups,TokenPrivileges,TokenOwner,TokenPrimaryGroup,TokenDefaultDacl,TokenSource,TokenType,TokenImpersonationLevel,
      TokenStatistics,TokenRestrictedSids,TokenSessionId,TokenGroupsAndPrivileges,TokenSessionReference,TokenSandBoxInert,TokenAuditPolicy,
      TokenOrigin,MaxTokenInfoClass
    } TOKEN_INFORMATION_CLASS,*PTOKEN_INFORMATION_CLASS;

    typedef struct _TOKEN_USER {
      SID_AND_ATTRIBUTES User;
    } TOKEN_USER,*PTOKEN_USER;

    typedef struct _TOKEN_GROUPS {
      DWORD GroupCount;
      SID_AND_ATTRIBUTES Groups[ANYSIZE_ARRAY];
    } TOKEN_GROUPS,*PTOKEN_GROUPS;

    typedef struct _TOKEN_PRIVILEGES {
      DWORD PrivilegeCount;
      LUID_AND_ATTRIBUTES Privileges[ANYSIZE_ARRAY];
    } TOKEN_PRIVILEGES,*PTOKEN_PRIVILEGES;

    typedef struct _TOKEN_OWNER {
      PSID Owner;
    } TOKEN_OWNER,*PTOKEN_OWNER;

    typedef struct _TOKEN_PRIMARY_GROUP {
      PSID PrimaryGroup;
    } TOKEN_PRIMARY_GROUP,*PTOKEN_PRIMARY_GROUP;

    typedef struct _TOKEN_DEFAULT_DACL {
      PACL DefaultDacl;
    } TOKEN_DEFAULT_DACL,*PTOKEN_DEFAULT_DACL;

    typedef struct _TOKEN_GROUPS_AND_PRIVILEGES {
      DWORD SidCount;
      DWORD SidLength;
      PSID_AND_ATTRIBUTES Sids;
      DWORD RestrictedSidCount;
      DWORD RestrictedSidLength;
      PSID_AND_ATTRIBUTES RestrictedSids;
      DWORD PrivilegeCount;
      DWORD PrivilegeLength;
      PLUID_AND_ATTRIBUTES Privileges;
      LUID AuthenticationId;
    } TOKEN_GROUPS_AND_PRIVILEGES,*PTOKEN_GROUPS_AND_PRIVILEGES;

#define TOKEN_AUDIT_SUCCESS_INCLUDE 0x1
#define TOKEN_AUDIT_SUCCESS_EXCLUDE 0x2
#define TOKEN_AUDIT_FAILURE_INCLUDE 0x4
#define TOKEN_AUDIT_FAILURE_EXCLUDE 0x8

#define VALID_AUDIT_POLICY_BITS (TOKEN_AUDIT_SUCCESS_INCLUDE | TOKEN_AUDIT_SUCCESS_EXCLUDE | TOKEN_AUDIT_FAILURE_INCLUDE | TOKEN_AUDIT_FAILURE_EXCLUDE)
#define VALID_TOKEN_AUDIT_POLICY_ELEMENT(P) ((((P).PolicyMask & ~VALID_AUDIT_POLICY_BITS)==0) && ((P).Category <= AuditEventMaxType))

    typedef struct _TOKEN_AUDIT_POLICY_ELEMENT {
      DWORD Category;
      DWORD PolicyMask;
    } TOKEN_AUDIT_POLICY_ELEMENT,*PTOKEN_AUDIT_POLICY_ELEMENT;

    typedef struct _TOKEN_AUDIT_POLICY {
      DWORD PolicyCount;
      TOKEN_AUDIT_POLICY_ELEMENT Policy[ANYSIZE_ARRAY];
    } TOKEN_AUDIT_POLICY,*PTOKEN_AUDIT_POLICY;

#define PER_USER_AUDITING_POLICY_SIZE(p) (sizeof(TOKEN_AUDIT_POLICY) + (((p)->PolicyCount > ANYSIZE_ARRAY) ? (sizeof(TOKEN_AUDIT_POLICY_ELEMENT) *((p)->PolicyCount - ANYSIZE_ARRAY)) : 0))
#define PER_USER_AUDITING_POLICY_SIZE_BY_COUNT(C) (sizeof(TOKEN_AUDIT_POLICY) + (((C) > ANYSIZE_ARRAY) ? (sizeof(TOKEN_AUDIT_POLICY_ELEMENT) *((C) - ANYSIZE_ARRAY)) : 0))

#define TOKEN_SOURCE_LENGTH 8

    typedef struct _TOKEN_SOURCE {
      CHAR SourceName[TOKEN_SOURCE_LENGTH];
      LUID SourceIdentifier;
    } TOKEN_SOURCE,*PTOKEN_SOURCE;

    typedef struct _TOKEN_STATISTICS {
      LUID TokenId;
      LUID AuthenticationId;
      LARGE_INTEGER ExpirationTime;
      TOKEN_TYPE TokenType;
      SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;
      DWORD DynamicCharged;
      DWORD DynamicAvailable;
      DWORD GroupCount;
      DWORD PrivilegeCount;
      LUID ModifiedId;
    } TOKEN_STATISTICS,*PTOKEN_STATISTICS;

    typedef struct _TOKEN_CONTROL {
      LUID TokenId;
      LUID AuthenticationId;
      LUID ModifiedId;
      TOKEN_SOURCE TokenSource;
    } TOKEN_CONTROL,*PTOKEN_CONTROL;

    typedef struct _TOKEN_ORIGIN {
      LUID OriginatingLogonSession;
    } TOKEN_ORIGIN,*PTOKEN_ORIGIN;

#define SECURITY_DYNAMIC_TRACKING (TRUE)
#define SECURITY_STATIC_TRACKING (FALSE)

    typedef BOOLEAN SECURITY_CONTEXT_TRACKING_MODE,*PSECURITY_CONTEXT_TRACKING_MODE;

    typedef struct _SECURITY_QUALITY_OF_SERVICE {
      DWORD Length;
      SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;
      SECURITY_CONTEXT_TRACKING_MODE ContextTrackingMode;
      BOOLEAN EffectiveOnly;
    } SECURITY_QUALITY_OF_SERVICE,*PSECURITY_QUALITY_OF_SERVICE;

    typedef struct _SE_IMPERSONATION_STATE {
      PACCESS_TOKEN Token;
      BOOLEAN CopyOnOpen;
      BOOLEAN EffectiveOnly;
      SECURITY_IMPERSONATION_LEVEL Level;
    } SE_IMPERSONATION_STATE,*PSE_IMPERSONATION_STATE;

#define DISABLE_MAX_PRIVILEGE 0x1
#define SANDBOX_INERT 0x2

    typedef DWORD SECURITY_INFORMATION,*PSECURITY_INFORMATION;

#define OWNER_SECURITY_INFORMATION (0x00000001L)
#define GROUP_SECURITY_INFORMATION (0x00000002L)
#define DACL_SECURITY_INFORMATION (0x00000004L)
#define SACL_SECURITY_INFORMATION (0x00000008L)

#define PROTECTED_DACL_SECURITY_INFORMATION (0x80000000L)
#define PROTECTED_SACL_SECURITY_INFORMATION (0x40000000L)
#define UNPROTECTED_DACL_SECURITY_INFORMATION (0x20000000L)
#define UNPROTECTED_SACL_SECURITY_INFORMATION (0x10000000L)

#define PROCESS_TERMINATE (0x0001)
#define PROCESS_CREATE_THREAD (0x0002)
#define PROCESS_SET_SESSIONID (0x0004)
#define PROCESS_VM_OPERATION (0x0008)
#define PROCESS_VM_READ (0x0010)
#define PROCESS_VM_WRITE (0x0020)
#define PROCESS_DUP_HANDLE (0x0040)
#define PROCESS_CREATE_PROCESS (0x0080)
#define PROCESS_SET_QUOTA (0x0100)
#define PROCESS_SET_INFORMATION (0x0200)
#define PROCESS_QUERY_INFORMATION (0x0400)
#define PROCESS_SUSPEND_RESUME (0x0800)
#define PROCESS_QUERY_LIMITED_INFORMATION (0x1000)
#define PROCESS_SET_LIMITED_INFORMATION (0x2000)
#define PROCESS_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0xFFF)

#ifdef _WIN64
#define MAXIMUM_PROCESSORS 64
#else
#define MAXIMUM_PROCESSORS 32
#endif

#define THREAD_TERMINATE (0x0001)
#define THREAD_SUSPEND_RESUME (0x0002)
#define THREAD_GET_CONTEXT (0x0008)
#define THREAD_SET_CONTEXT (0x0010)
#define THREAD_SET_INFORMATION (0x0020)
#define THREAD_QUERY_INFORMATION (0x0040)
#define THREAD_SET_THREAD_TOKEN (0x0080)
#define THREAD_IMPERSONATE (0x0100)
#define THREAD_DIRECT_IMPERSONATION (0x0200)

#define THREAD_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x3FF)

#define JOB_OBJECT_ASSIGN_PROCESS (0x0001)
#define JOB_OBJECT_SET_ATTRIBUTES (0x0002)
#define JOB_OBJECT_QUERY (0x0004)
#define JOB_OBJECT_TERMINATE (0x0008)
#define JOB_OBJECT_SET_SECURITY_ATTRIBUTES (0x0010)
#define JOB_OBJECT_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x1F)

    typedef struct _JOB_SET_ARRAY {
      HANDLE JobHandle;
      DWORD MemberLevel;
      DWORD Flags;
    } JOB_SET_ARRAY,*PJOB_SET_ARRAY;

#define FLS_MAXIMUM_AVAILABLE 128
#define TLS_MINIMUM_AVAILABLE 64

#ifndef _NT_TIB_DEFINED
#define _NT_TIB_DEFINED
    typedef struct _NT_TIB {
      struct _EXCEPTION_REGISTRATION_RECORD *ExceptionList;
      PVOID StackBase;
      PVOID StackLimit;
      PVOID SubSystemTib;
      union {
	PVOID FiberData;
	DWORD Version;
      };
      PVOID ArbitraryUserPointer;
      struct _NT_TIB *Self;
    } NT_TIB;
    typedef NT_TIB *PNT_TIB;
#endif

    typedef struct _NT_TIB32 {
      DWORD ExceptionList;
      DWORD StackBase;
      DWORD StackLimit;
      DWORD SubSystemTib;
      union {
	DWORD FiberData;
	DWORD Version;
      };
      DWORD ArbitraryUserPointer;
      DWORD Self;
    } NT_TIB32,*PNT_TIB32;

    typedef struct _NT_TIB64 {
      DWORD64 ExceptionList;
      DWORD64 StackBase;
      DWORD64 StackLimit;
      DWORD64 SubSystemTib;
      union {
	DWORD64 FiberData;
	DWORD Version;
      };
      DWORD64 ArbitraryUserPointer;
      DWORD64 Self;
    } NT_TIB64,*PNT_TIB64;

#if !defined(I_X86_) && !defined(_IA64_) && !defined(_AMD64_)
#define WX86
#endif

#define THREAD_BASE_PRIORITY_LOWRT 15
#define THREAD_BASE_PRIORITY_MAX 2
#define THREAD_BASE_PRIORITY_MIN (-2)
#define THREAD_BASE_PRIORITY_IDLE (-15)

    typedef struct _QUOTA_LIMITS {
      SIZE_T PagedPoolLimit;
      SIZE_T NonPagedPoolLimit;
      SIZE_T MinimumWorkingSetSize;
      SIZE_T MaximumWorkingSetSize;
      SIZE_T PagefileLimit;
      LARGE_INTEGER TimeLimit;
    } QUOTA_LIMITS,*PQUOTA_LIMITS;

#define QUOTA_LIMITS_HARDWS_MIN_ENABLE 0x00000001
#define QUOTA_LIMITS_HARDWS_MIN_DISABLE 0x00000002
#define QUOTA_LIMITS_HARDWS_MAX_ENABLE 0x00000004
#define QUOTA_LIMITS_HARDWS_MAX_DISABLE 0x00000008

    typedef struct _QUOTA_LIMITS_EX {
      SIZE_T PagedPoolLimit;
      SIZE_T NonPagedPoolLimit;
      SIZE_T MinimumWorkingSetSize;
      SIZE_T MaximumWorkingSetSize;
      SIZE_T PagefileLimit;
      LARGE_INTEGER TimeLimit;
      SIZE_T Reserved1;
      SIZE_T Reserved2;
      SIZE_T Reserved3;
      SIZE_T Reserved4;
      DWORD Flags;
      DWORD Reserved5;
    } QUOTA_LIMITS_EX,*PQUOTA_LIMITS_EX;

    typedef struct _IO_COUNTERS {
      ULONGLONG ReadOperationCount;
      ULONGLONG WriteOperationCount;
      ULONGLONG OtherOperationCount;
      ULONGLONG ReadTransferCount;
      ULONGLONG WriteTransferCount;
      ULONGLONG OtherTransferCount;
    } IO_COUNTERS;
    typedef IO_COUNTERS *PIO_COUNTERS;

    typedef struct _JOBOBJECT_BASIC_ACCOUNTING_INFORMATION {
      LARGE_INTEGER TotalUserTime;
      LARGE_INTEGER TotalKernelTime;
      LARGE_INTEGER ThisPeriodTotalUserTime;
      LARGE_INTEGER ThisPeriodTotalKernelTime;
      DWORD TotalPageFaultCount;
      DWORD TotalProcesses;
      DWORD ActiveProcesses;
      DWORD TotalTerminatedProcesses;
    } JOBOBJECT_BASIC_ACCOUNTING_INFORMATION,*PJOBOBJECT_BASIC_ACCOUNTING_INFORMATION;

    typedef struct _JOBOBJECT_BASIC_LIMIT_INFORMATION {
      LARGE_INTEGER PerProcessUserTimeLimit;
      LARGE_INTEGER PerJobUserTimeLimit;
      DWORD LimitFlags;
      SIZE_T MinimumWorkingSetSize;
      SIZE_T MaximumWorkingSetSize;
      DWORD ActiveProcessLimit;
      ULONG_PTR Affinity;
      DWORD PriorityClass;
      DWORD SchedulingClass;
    } JOBOBJECT_BASIC_LIMIT_INFORMATION,*PJOBOBJECT_BASIC_LIMIT_INFORMATION;

    typedef struct _JOBOBJECT_EXTENDED_LIMIT_INFORMATION {
      JOBOBJECT_BASIC_LIMIT_INFORMATION BasicLimitInformation;
      IO_COUNTERS IoInfo;
      SIZE_T ProcessMemoryLimit;
      SIZE_T JobMemoryLimit;
      SIZE_T PeakProcessMemoryUsed;
      SIZE_T PeakJobMemoryUsed;
    } JOBOBJECT_EXTENDED_LIMIT_INFORMATION,*PJOBOBJECT_EXTENDED_LIMIT_INFORMATION;

    typedef struct _JOBOBJECT_BASIC_PROCESS_ID_LIST {
      DWORD NumberOfAssignedProcesses;
      DWORD NumberOfProcessIdsInList;
      ULONG_PTR ProcessIdList[1];
    } JOBOBJECT_BASIC_PROCESS_ID_LIST,*PJOBOBJECT_BASIC_PROCESS_ID_LIST;

    typedef struct _JOBOBJECT_BASIC_UI_RESTRICTIONS {
      DWORD UIRestrictionsClass;
    } JOBOBJECT_BASIC_UI_RESTRICTIONS,*PJOBOBJECT_BASIC_UI_RESTRICTIONS;

    typedef struct _JOBOBJECT_SECURITY_LIMIT_INFORMATION {
      DWORD SecurityLimitFlags;
      HANDLE JobToken;
      PTOKEN_GROUPS SidsToDisable;
      PTOKEN_PRIVILEGES PrivilegesToDelete;
      PTOKEN_GROUPS RestrictedSids;
    } JOBOBJECT_SECURITY_LIMIT_INFORMATION,*PJOBOBJECT_SECURITY_LIMIT_INFORMATION;

    typedef struct _JOBOBJECT_END_OF_JOB_TIME_INFORMATION {
      DWORD EndOfJobTimeAction;
    } JOBOBJECT_END_OF_JOB_TIME_INFORMATION,*PJOBOBJECT_END_OF_JOB_TIME_INFORMATION;

    typedef struct _JOBOBJECT_ASSOCIATE_COMPLETION_PORT {
      PVOID CompletionKey;
      HANDLE CompletionPort;
    } JOBOBJECT_ASSOCIATE_COMPLETION_PORT,*PJOBOBJECT_ASSOCIATE_COMPLETION_PORT;

    typedef struct _JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION {
      JOBOBJECT_BASIC_ACCOUNTING_INFORMATION BasicInfo;
      IO_COUNTERS IoInfo;
    } JOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION,*PJOBOBJECT_BASIC_AND_IO_ACCOUNTING_INFORMATION;

    typedef struct _JOBOBJECT_JOBSET_INFORMATION {
      DWORD MemberLevel;
    } JOBOBJECT_JOBSET_INFORMATION,*PJOBOBJECT_JOBSET_INFORMATION;

#define JOB_OBJECT_TERMINATE_AT_END_OF_JOB 0
#define JOB_OBJECT_POST_AT_END_OF_JOB 1

#define JOB_OBJECT_MSG_END_OF_JOB_TIME 1
#define JOB_OBJECT_MSG_END_OF_PROCESS_TIME 2
#define JOB_OBJECT_MSG_ACTIVE_PROCESS_LIMIT 3
#define JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO 4
#define JOB_OBJECT_MSG_NEW_PROCESS 6
#define JOB_OBJECT_MSG_EXIT_PROCESS 7
#define JOB_OBJECT_MSG_ABNORMAL_EXIT_PROCESS 8
#define JOB_OBJECT_MSG_PROCESS_MEMORY_LIMIT 9
#define JOB_OBJECT_MSG_JOB_MEMORY_LIMIT 10

#define JOB_OBJECT_LIMIT_WORKINGSET 0x00000001
#define JOB_OBJECT_LIMIT_PROCESS_TIME 0x00000002
#define JOB_OBJECT_LIMIT_JOB_TIME 0x00000004
#define JOB_OBJECT_LIMIT_ACTIVE_PROCESS 0x00000008
#define JOB_OBJECT_LIMIT_AFFINITY 0x00000010
#define JOB_OBJECT_LIMIT_PRIORITY_CLASS 0x00000020
#define JOB_OBJECT_LIMIT_PRESERVE_JOB_TIME 0x00000040
#define JOB_OBJECT_LIMIT_SCHEDULING_CLASS 0x00000080

#define JOB_OBJECT_LIMIT_PROCESS_MEMORY 0x00000100
#define JOB_OBJECT_LIMIT_JOB_MEMORY 0x00000200
#define JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION 0x00000400
#define JOB_OBJECT_LIMIT_BREAKAWAY_OK 0x00000800
#define JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK 0x00001000
#define JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE 0x00002000

#define JOB_OBJECT_LIMIT_RESERVED2 0x00004000
#define JOB_OBJECT_LIMIT_RESERVED3 0x00008000
#define JOB_OBJECT_LIMIT_RESERVED4 0x00010000
#define JOB_OBJECT_LIMIT_RESERVED5 0x00020000
#define JOB_OBJECT_LIMIT_RESERVED6 0x00040000

#define JOB_OBJECT_LIMIT_VALID_FLAGS 0x0007ffff

#define JOB_OBJECT_BASIC_LIMIT_VALID_FLAGS 0x000000ff
#define JOB_OBJECT_EXTENDED_LIMIT_VALID_FLAGS 0x00003fff
#define JOB_OBJECT_RESERVED_LIMIT_VALID_FLAGS 0x0007ffff

#define JOB_OBJECT_UILIMIT_NONE 0x00000000

#define JOB_OBJECT_UILIMIT_HANDLES 0x00000001
#define JOB_OBJECT_UILIMIT_READCLIPBOARD 0x00000002
#define JOB_OBJECT_UILIMIT_WRITECLIPBOARD 0x00000004
#define JOB_OBJECT_UILIMIT_SYSTEMPARAMETERS 0x00000008
#define JOB_OBJECT_UILIMIT_DISPLAYSETTINGS 0x00000010
#define JOB_OBJECT_UILIMIT_GLOBALATOMS 0x00000020
#define JOB_OBJECT_UILIMIT_DESKTOP 0x00000040
#define JOB_OBJECT_UILIMIT_EXITWINDOWS 0x00000080

#define JOB_OBJECT_UILIMIT_ALL 0x000000FF

#define JOB_OBJECT_UI_VALID_FLAGS 0x000000FF

#define JOB_OBJECT_SECURITY_NO_ADMIN 0x00000001
#define JOB_OBJECT_SECURITY_RESTRICTED_TOKEN 0x00000002
#define JOB_OBJECT_SECURITY_ONLY_TOKEN 0x00000004
#define JOB_OBJECT_SECURITY_FILTER_TOKENS 0x00000008

#define JOB_OBJECT_SECURITY_VALID_FLAGS 0x0000000f

    typedef enum _JOBOBJECTINFOCLASS {
      JobObjectBasicAccountingInformation = 1,JobObjectBasicLimitInformation,JobObjectBasicProcessIdList,JobObjectBasicUIRestrictions,
      JobObjectSecurityLimitInformation,JobObjectEndOfJobTimeInformation,JobObjectAssociateCompletionPortInformation,
      JobObjectBasicAndIoAccountingInformation,JobObjectExtendedLimitInformation,JobObjectJobSetInformation,MaxJobObjectInfoClass
    } JOBOBJECTINFOCLASS;

#define EVENT_MODIFY_STATE 0x0002
#define EVENT_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|0x3)

#define MUTANT_QUERY_STATE 0x0001

#define MUTANT_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE| MUTANT_QUERY_STATE)
#define SEMAPHORE_MODIFY_STATE 0x0002
#define SEMAPHORE_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|0x3)

#define TIMER_QUERY_STATE 0x0001
#define TIMER_MODIFY_STATE 0x0002

#define TIMER_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE| TIMER_QUERY_STATE|TIMER_MODIFY_STATE)

#define TIME_ZONE_ID_UNKNOWN 0
#define TIME_ZONE_ID_STANDARD 1
#define TIME_ZONE_ID_DAYLIGHT 2

    typedef enum _LOGICAL_PROCESSOR_RELATIONSHIP {
      RelationProcessorCore,RelationNumaNode,RelationCache
    } LOGICAL_PROCESSOR_RELATIONSHIP;

#define LTP_PC_SMT 0x1

    typedef enum _PROCESSOR_CACHE_TYPE {
      CacheUnified,CacheInstruction,CacheData,CacheTrace
    } PROCESSOR_CACHE_TYPE;

#define CACHE_FULLY_ASSOCIATIVE 0xFF

    typedef struct _CACHE_DESCRIPTOR {
      BYTE Level;
      BYTE Associativity;
      WORD LineSize;
      DWORD Size;
      PROCESSOR_CACHE_TYPE Type;
    } CACHE_DESCRIPTOR,*PCACHE_DESCRIPTOR;

    typedef struct _SYSTEM_LOGICAL_PROCESSOR_INFORMATION {
      ULONG_PTR ProcessorMask;
      LOGICAL_PROCESSOR_RELATIONSHIP Relationship;
      union {
	struct {
	  BYTE Flags;
	} ProcessorCore;
	struct {
	  DWORD NodeNumber;
	} NumaNode;
	CACHE_DESCRIPTOR Cache;
	ULONGLONG Reserved[2];
      };
    } SYSTEM_LOGICAL_PROCESSOR_INFORMATION,*PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;

#define PROCESSOR_INTEL_386 386
#define PROCESSOR_INTEL_486 486
#define PROCESSOR_INTEL_PENTIUM 586
#define PROCESSOR_INTEL_IA64 2200
#define PROCESSOR_AMD_X8664 8664
#define PROCESSOR_MIPS_R4000 4000
#define PROCESSOR_ALPHA_21064 21064
#define PROCESSOR_PPC_601 601
#define PROCESSOR_PPC_603 603
#define PROCESSOR_PPC_604 604
#define PROCESSOR_PPC_620 620
#define PROCESSOR_HITACHI_SH3 10003
#define PROCESSOR_HITACHI_SH3E 10004
#define PROCESSOR_HITACHI_SH4 10005
#define PROCESSOR_MOTOROLA_821 821
#define PROCESSOR_SHx_SH3 103
#define PROCESSOR_SHx_SH4 104
#define PROCESSOR_STRONGARM 2577
#define PROCESSOR_ARM720 1824
#define PROCESSOR_ARM820 2080
#define PROCESSOR_ARM920 2336
#define PROCESSOR_ARM_7TDMI 70001
#define PROCESSOR_OPTIL 0x494f

#define PROCESSOR_ARCHITECTURE_INTEL 0
#define PROCESSOR_ARCHITECTURE_MIPS 1
#define PROCESSOR_ARCHITECTURE_ALPHA 2
#define PROCESSOR_ARCHITECTURE_PPC 3
#define PROCESSOR_ARCHITECTURE_SHX 4
#define PROCESSOR_ARCHITECTURE_ARM 5
#define PROCESSOR_ARCHITECTURE_IA64 6
#define PROCESSOR_ARCHITECTURE_ALPHA64 7
#define PROCESSOR_ARCHITECTURE_MSIL 8
#define PROCESSOR_ARCHITECTURE_AMD64 9
#define PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 10

#define PROCESSOR_ARCHITECTURE_UNKNOWN 0xFFFF

#define PF_FLOATING_POINT_PRECISION_ERRATA 0
#define PF_FLOATING_POINT_EMULATED 1
#define PF_COMPARE_EXCHANGE_DOUBLE 2
#define PF_MMX_INSTRUCTIONS_AVAILABLE 3
#define PF_PPC_MOVEMEM_64BIT_OK 4
#define PF_ALPHA_BYTE_INSTRUCTIONS 5
#define PF_XMMI_INSTRUCTIONS_AVAILABLE 6
#define PF_3DNOW_INSTRUCTIONS_AVAILABLE 7
#define PF_RDTSC_INSTRUCTION_AVAILABLE 8
#define PF_PAE_ENABLED 9
#define PF_XMMI64_INSTRUCTIONS_AVAILABLE 10
#define PF_SSE_DAZ_MODE_AVAILABLE 11
#define PF_NX_ENABLED 12

    typedef struct _MEMORY_BASIC_INFORMATION {
      PVOID BaseAddress;
      PVOID AllocationBase;
      DWORD AllocationProtect;
      SIZE_T RegionSize;
      DWORD State;
      DWORD Protect;
      DWORD Type;
    } MEMORY_BASIC_INFORMATION,*PMEMORY_BASIC_INFORMATION;

    typedef struct _MEMORY_BASIC_INFORMATION32 {
      DWORD BaseAddress;
      DWORD AllocationBase;
      DWORD AllocationProtect;
      DWORD RegionSize;
      DWORD State;
      DWORD Protect;
      DWORD Type;
    } MEMORY_BASIC_INFORMATION32,*PMEMORY_BASIC_INFORMATION32;

    typedef struct DECLSPEC_ALIGN(16) _MEMORY_BASIC_INFORMATION64 {
      ULONGLONG BaseAddress;
      ULONGLONG AllocationBase;
      DWORD AllocationProtect;
      DWORD __alignment1;
      ULONGLONG RegionSize;
      DWORD State;
      DWORD Protect;
      DWORD Type;
      DWORD __alignment2;
    } MEMORY_BASIC_INFORMATION64,*PMEMORY_BASIC_INFORMATION64;

#define SECTION_QUERY 0x0001
#define SECTION_MAP_WRITE 0x0002
#define SECTION_MAP_READ 0x0004
#define SECTION_MAP_EXECUTE 0x0008
#define SECTION_EXTEND_SIZE 0x0010
#define SECTION_MAP_EXECUTE_EXPLICIT 0x0020

#define SECTION_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SECTION_QUERY| SECTION_MAP_WRITE | SECTION_MAP_READ | SECTION_MAP_EXECUTE | SECTION_EXTEND_SIZE)
#define PAGE_NOACCESS 0x01
#define PAGE_READONLY 0x02
#define PAGE_READWRITE 0x04
#define PAGE_WRITECOPY 0x08
#define PAGE_EXECUTE 0x10
#define PAGE_EXECUTE_READ 0x20
#define PAGE_EXECUTE_READWRITE 0x40
#define PAGE_EXECUTE_WRITECOPY 0x80
#define PAGE_GUARD 0x100
#define PAGE_NOCACHE 0x200
#define PAGE_WRITECOMBINE 0x400
#define MEM_COMMIT 0x1000
#define MEM_RESERVE 0x2000
#define MEM_DECOMMIT 0x4000
#define MEM_RELEASE 0x8000
#define MEM_FREE 0x10000
#define MEM_PRIVATE 0x20000
#define MEM_MAPPED 0x40000
#define MEM_RESET 0x80000
#define MEM_TOP_DOWN 0x100000
#define MEM_WRITE_WATCH 0x200000
#define MEM_PHYSICAL 0x400000
#define MEM_LARGE_PAGES 0x20000000
#define MEM_4MB_PAGES 0x80000000
#define SEC_FILE 0x800000
#define SEC_IMAGE 0x1000000
#define SEC_RESERVE 0x4000000
#define SEC_COMMIT 0x8000000
#define SEC_NOCACHE 0x10000000
#define SEC_LARGE_PAGES 0x80000000
#define MEM_IMAGE SEC_IMAGE
#define WRITE_WATCH_FLAG_RESET 0x01

#define FILE_READ_DATA (0x0001)
#define FILE_LIST_DIRECTORY (0x0001)

#define FILE_WRITE_DATA (0x0002)
#define FILE_ADD_FILE (0x0002)

#define FILE_APPEND_DATA (0x0004)
#define FILE_ADD_SUBDIRECTORY (0x0004)
#define FILE_CREATE_PIPE_INSTANCE (0x0004)

#define FILE_READ_EA (0x0008)

#define FILE_WRITE_EA (0x0010)

#define FILE_EXECUTE (0x0020)
#define FILE_TRAVERSE (0x0020)

#define FILE_DELETE_CHILD (0x0040)

#define FILE_READ_ATTRIBUTES (0x0080)

#define FILE_WRITE_ATTRIBUTES (0x0100)

#define FILE_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x1FF)
#define FILE_GENERIC_READ (STANDARD_RIGHTS_READ | FILE_READ_DATA | FILE_READ_ATTRIBUTES | FILE_READ_EA | SYNCHRONIZE)
#define FILE_GENERIC_WRITE (STANDARD_RIGHTS_WRITE | FILE_WRITE_DATA | FILE_WRITE_ATTRIBUTES | FILE_WRITE_EA | FILE_APPEND_DATA | SYNCHRONIZE)
#define FILE_GENERIC_EXECUTE (STANDARD_RIGHTS_EXECUTE | FILE_READ_ATTRIBUTES | FILE_EXECUTE | SYNCHRONIZE)

#define FILE_SHARE_READ 0x00000001
#define FILE_SHARE_WRITE 0x00000002
#define FILE_SHARE_DELETE 0x00000004
#define FILE_ATTRIBUTE_READONLY 0x00000001
#define FILE_ATTRIBUTE_HIDDEN 0x00000002
#define FILE_ATTRIBUTE_SYSTEM 0x00000004
#define FILE_ATTRIBUTE_DIRECTORY 0x00000010
#define FILE_ATTRIBUTE_ARCHIVE 0x00000020
#define FILE_ATTRIBUTE_DEVICE 0x00000040
#define FILE_ATTRIBUTE_NORMAL 0x00000080
#define FILE_ATTRIBUTE_TEMPORARY 0x00000100
#define FILE_ATTRIBUTE_SPARSE_FILE 0x00000200
#define FILE_ATTRIBUTE_REPARSE_POINT 0x00000400
#define FILE_ATTRIBUTE_COMPRESSED 0x00000800
#define FILE_ATTRIBUTE_OFFLINE 0x00001000
#define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED 0x00002000
#define FILE_ATTRIBUTE_ENCRYPTED 0x00004000
#define FILE_NOTIFY_CHANGE_FILE_NAME 0x00000001
#define FILE_NOTIFY_CHANGE_DIR_NAME 0x00000002
#define FILE_NOTIFY_CHANGE_ATTRIBUTES 0x00000004
#define FILE_NOTIFY_CHANGE_SIZE 0x00000008
#define FILE_NOTIFY_CHANGE_LAST_WRITE 0x00000010
#define FILE_NOTIFY_CHANGE_LAST_ACCESS 0x00000020
#define FILE_NOTIFY_CHANGE_CREATION 0x00000040
#define FILE_NOTIFY_CHANGE_SECURITY 0x00000100
#define FILE_ACTION_ADDED 0x00000001
#define FILE_ACTION_REMOVED 0x00000002
#define FILE_ACTION_MODIFIED 0x00000003
#define FILE_ACTION_RENAMED_OLD_NAME 0x00000004
#define FILE_ACTION_RENAMED_NEW_NAME 0x00000005
#define MAILSLOT_NO_MESSAGE ((DWORD)-1)
#define MAILSLOT_WAIT_FOREVER ((DWORD)-1)
#define FILE_CASE_SENSITIVE_SEARCH 0x00000001
#define FILE_CASE_PRESERVED_NAMES 0x00000002
#define FILE_UNICODE_ON_DISK 0x00000004
#define FILE_PERSISTENT_ACLS 0x00000008
#define FILE_FILE_COMPRESSION 0x00000010
#define FILE_VOLUME_QUOTAS 0x00000020
#define FILE_SUPPORTS_SPARSE_FILES 0x00000040
#define FILE_SUPPORTS_REPARSE_POINTS 0x00000080
#define FILE_SUPPORTS_REMOTE_STORAGE 0x00000100
#define FILE_VOLUME_IS_COMPRESSED 0x00008000
#define FILE_SUPPORTS_OBJECT_IDS 0x00010000
#define FILE_SUPPORTS_ENCRYPTION 0x00020000
#define FILE_NAMED_STREAMS 0x00040000
#define FILE_READ_ONLY_VOLUME 0x00080000

    typedef struct _FILE_NOTIFY_INFORMATION {
      DWORD NextEntryOffset;
      DWORD Action;
      DWORD FileNameLength;
      WCHAR FileName[1];
    } FILE_NOTIFY_INFORMATION,*PFILE_NOTIFY_INFORMATION;

    typedef union _FILE_SEGMENT_ELEMENT {
      PVOID64 Buffer;
      ULONGLONG Alignment;
    }FILE_SEGMENT_ELEMENT,*PFILE_SEGMENT_ELEMENT;

    typedef struct _REPARSE_GUID_DATA_BUFFER {
      DWORD ReparseTag;
      WORD ReparseDataLength;
      WORD Reserved;
      GUID ReparseGuid;
      struct {
	BYTE DataBuffer[1];
      } GenericReparseBuffer;
    } REPARSE_GUID_DATA_BUFFER,*PREPARSE_GUID_DATA_BUFFER;

#define REPARSE_GUID_DATA_BUFFER_HEADER_SIZE FIELD_OFFSET(REPARSE_GUID_DATA_BUFFER,GenericReparseBuffer)

#define MAXIMUM_REPARSE_DATA_BUFFER_SIZE (16 *1024)

#define IO_REPARSE_TAG_RESERVED_ZERO (0)
#define IO_REPARSE_TAG_RESERVED_ONE (1)

#define IO_REPARSE_TAG_RESERVED_RANGE IO_REPARSE_TAG_RESERVED_ONE

#define IsReparseTagMicrosoft(_tag) (((_tag) & 0x80000000))
#define IsReparseTagNameSurrogate(_tag) (((_tag) & 0x20000000))

#define IO_REPARSE_TAG_MOUNT_POINT (0xA0000003L)
#define IO_REPARSE_TAG_HSM (0xC0000004L)
#define IO_REPARSE_TAG_SIS (0x80000007L)
#define IO_REPARSE_TAG_DFS (0x8000000AL)
#define IO_REPARSE_TAG_FILTER_MANAGER (0x8000000BL)
#define IO_COMPLETION_MODIFY_STATE 0x0002
#define IO_COMPLETION_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|0x3)
#define DUPLICATE_CLOSE_SOURCE 0x00000001
#define DUPLICATE_SAME_ACCESS 0x00000002

    typedef enum _SYSTEM_POWER_STATE {
      PowerSystemUnspecified = 0,PowerSystemWorking = 1,PowerSystemSleeping1 = 2,PowerSystemSleeping2 = 3,PowerSystemSleeping3 = 4,PowerSystemHibernate = 5,PowerSystemShutdown = 6,PowerSystemMaximum = 7
    } SYSTEM_POWER_STATE,*PSYSTEM_POWER_STATE;

#define POWER_SYSTEM_MAXIMUM 7

    typedef enum {
      PowerActionNone = 0,PowerActionReserved,PowerActionSleep,PowerActionHibernate,PowerActionShutdown,PowerActionShutdownReset,PowerActionShutdownOff,PowerActionWarmEject
    } POWER_ACTION,*PPOWER_ACTION;

    typedef enum _DEVICE_POWER_STATE {
      PowerDeviceUnspecified = 0,PowerDeviceD0,PowerDeviceD1,PowerDeviceD2,PowerDeviceD3,PowerDeviceMaximum
    } DEVICE_POWER_STATE,*PDEVICE_POWER_STATE;

#define ES_SYSTEM_REQUIRED ((DWORD)0x00000001)
#define ES_DISPLAY_REQUIRED ((DWORD)0x00000002)
#define ES_USER_PRESENT ((DWORD)0x00000004)
#define ES_CONTINUOUS ((DWORD)0x80000000)

    typedef DWORD EXECUTION_STATE;

    typedef enum {
      LT_DONT_CARE,LT_LOWEST_LATENCY
    } LATENCY_TIME;

#define PDCAP_D0_SUPPORTED 0x00000001
#define PDCAP_D1_SUPPORTED 0x00000002
#define PDCAP_D2_SUPPORTED 0x00000004
#define PDCAP_D3_SUPPORTED 0x00000008
#define PDCAP_WAKE_FROM_D0_SUPPORTED 0x00000010
#define PDCAP_WAKE_FROM_D1_SUPPORTED 0x00000020
#define PDCAP_WAKE_FROM_D2_SUPPORTED 0x00000040
#define PDCAP_WAKE_FROM_D3_SUPPORTED 0x00000080
#define PDCAP_WARM_EJECT_SUPPORTED 0x00000100

    typedef struct CM_Power_Data_s {
      DWORD PD_Size;
      DEVICE_POWER_STATE PD_MostRecentPowerState;
      DWORD PD_Capabilities;
      DWORD PD_D1Latency;
      DWORD PD_D2Latency;
      DWORD PD_D3Latency;
      DEVICE_POWER_STATE PD_PowerStateMapping[POWER_SYSTEM_MAXIMUM];
      SYSTEM_POWER_STATE PD_DeepestSystemWake;
    } CM_POWER_DATA,*PCM_POWER_DATA;

    typedef enum {
      SystemPowerPolicyAc,SystemPowerPolicyDc,VerifySystemPolicyAc,VerifySystemPolicyDc,SystemPowerCapabilities,SystemBatteryState,SystemPowerStateHandler,ProcessorStateHandler,SystemPowerPolicyCurrent,AdministratorPowerPolicy,SystemReserveHiberFile,ProcessorInformation,SystemPowerInformation,ProcessorStateHandler2,LastWakeTime,LastSleepTime,SystemExecutionState,SystemPowerStateNotifyHandler,ProcessorPowerPolicyAc,ProcessorPowerPolicyDc,VerifyProcessorPowerPolicyAc,VerifyProcessorPowerPolicyDc,ProcessorPowerPolicyCurrent,SystemPowerStateLogging,SystemPowerLoggingEntry
    } POWER_INFORMATION_LEVEL;

    typedef struct {
      DWORD Granularity;
      DWORD Capacity;
    } BATTERY_REPORTING_SCALE,*PBATTERY_REPORTING_SCALE;

    typedef struct {
      POWER_ACTION Action;
      DWORD Flags;
      DWORD EventCode;
    } POWER_ACTION_POLICY,*PPOWER_ACTION_POLICY;

#define POWER_ACTION_QUERY_ALLOWED 0x00000001
#define POWER_ACTION_UI_ALLOWED 0x00000002
#define POWER_ACTION_OVERRIDE_APPS 0x00000004
#define POWER_ACTION_LIGHTEST_FIRST 0x10000000
#define POWER_ACTION_LOCK_CONSOLE 0x20000000
#define POWER_ACTION_DISABLE_WAKES 0x40000000
#define POWER_ACTION_CRITICAL 0x80000000

#define POWER_LEVEL_USER_NOTIFY_TEXT 0x00000001
#define POWER_LEVEL_USER_NOTIFY_SOUND 0x00000002
#define POWER_LEVEL_USER_NOTIFY_EXEC 0x00000004
#define POWER_USER_NOTIFY_BUTTON 0x00000008
#define POWER_USER_NOTIFY_SHUTDOWN 0x00000010
#define POWER_FORCE_TRIGGER_RESET 0x80000000

    typedef struct {
      BOOLEAN Enable;
      BYTE Spare[3];
      DWORD BatteryLevel;
      POWER_ACTION_POLICY PowerPolicy;
      SYSTEM_POWER_STATE MinSystemState;
    } SYSTEM_POWER_LEVEL,*PSYSTEM_POWER_LEVEL;

#define NUM_DISCHARGE_POLICIES 4
#define DISCHARGE_POLICY_CRITICAL 0
#define DISCHARGE_POLICY_LOW 1

#define PO_THROTTLE_NONE 0
#define PO_THROTTLE_CONSTANT 1
#define PO_THROTTLE_DEGRADE 2
#define PO_THROTTLE_ADAPTIVE 3
#define PO_THROTTLE_MAXIMUM 4

    typedef struct _SYSTEM_POWER_POLICY {
      DWORD Revision;
      POWER_ACTION_POLICY PowerButton;
      POWER_ACTION_POLICY SleepButton;
      POWER_ACTION_POLICY LidClose;
      SYSTEM_POWER_STATE LidOpenWake;
      DWORD Reserved;
      POWER_ACTION_POLICY Idle;
      DWORD IdleTimeout;
      BYTE IdleSensitivity;
      BYTE DynamicThrottle;
      BYTE Spare2[2];
      SYSTEM_POWER_STATE MinSleep;
      SYSTEM_POWER_STATE MaxSleep;
      SYSTEM_POWER_STATE ReducedLatencySleep;
      DWORD WinLogonFlags;
      DWORD Spare3;
      DWORD DozeS4Timeout;
      DWORD BroadcastCapacityResolution;
      SYSTEM_POWER_LEVEL DischargePolicy[NUM_DISCHARGE_POLICIES];
      DWORD VideoTimeout;
      BOOLEAN VideoDimDisplay;
      DWORD VideoReserved[3];
      DWORD SpindownTimeout;
      BOOLEAN OptimizeForPower;
      BYTE FanThrottleTolerance;
      BYTE ForcedThrottle;
      BYTE MinThrottle;
      POWER_ACTION_POLICY OverThrottled;
    } SYSTEM_POWER_POLICY,*PSYSTEM_POWER_POLICY;

    typedef struct _PROCESSOR_POWER_POLICY_INFO {
      DWORD TimeCheck;
      DWORD DemoteLimit;
      DWORD PromoteLimit;
      BYTE DemotePercent;
      BYTE PromotePercent;
      BYTE Spare[2];
      DWORD AllowDemotion:1;
      DWORD AllowPromotion:1;
      DWORD Reserved:30;
    } PROCESSOR_POWER_POLICY_INFO,*PPROCESSOR_POWER_POLICY_INFO;

    typedef struct _PROCESSOR_POWER_POLICY {
      DWORD Revision;
      BYTE DynamicThrottle;
      BYTE Spare[3];
      DWORD DisableCStates:1;
      DWORD Reserved:31;
      DWORD PolicyCount;
      PROCESSOR_POWER_POLICY_INFO Policy[3];
    } PROCESSOR_POWER_POLICY,*PPROCESSOR_POWER_POLICY;

    typedef struct _ADMINISTRATOR_POWER_POLICY {
      SYSTEM_POWER_STATE MinSleep;
      SYSTEM_POWER_STATE MaxSleep;
      DWORD MinVideoTimeout;
      DWORD MaxVideoTimeout;
      DWORD MinSpindownTimeout;
      DWORD MaxSpindownTimeout;
    } ADMINISTRATOR_POWER_POLICY,*PADMINISTRATOR_POWER_POLICY;

    typedef struct {
      BOOLEAN PowerButtonPresent;
      BOOLEAN SleepButtonPresent;
      BOOLEAN LidPresent;
      BOOLEAN SystemS1;
      BOOLEAN SystemS2;
      BOOLEAN SystemS3;
      BOOLEAN SystemS4;
      BOOLEAN SystemS5;
      BOOLEAN HiberFilePresent;
      BOOLEAN FullWake;
      BOOLEAN VideoDimPresent;
      BOOLEAN ApmPresent;
      BOOLEAN UpsPresent;
      BOOLEAN ThermalControl;
      BOOLEAN ProcessorThrottle;
      BYTE ProcessorMinThrottle;
      BYTE ProcessorMaxThrottle;
      BYTE spare2[4];
      BOOLEAN DiskSpinDown;
      BYTE spare3[8];
      BOOLEAN SystemBatteriesPresent;
      BOOLEAN BatteriesAreShortTerm;
      BATTERY_REPORTING_SCALE BatteryScale[3];
      SYSTEM_POWER_STATE AcOnLineWake;
      SYSTEM_POWER_STATE SoftLidWake;
      SYSTEM_POWER_STATE RtcWake;
      SYSTEM_POWER_STATE MinDeviceWakeState;
      SYSTEM_POWER_STATE DefaultLowLatencyWake;
    } SYSTEM_POWER_CAPABILITIES,*PSYSTEM_POWER_CAPABILITIES;

    typedef struct {
      BOOLEAN AcOnLine;
      BOOLEAN BatteryPresent;
      BOOLEAN Charging;
      BOOLEAN Discharging;
      BOOLEAN Spare1[4];
      DWORD MaxCapacity;
      DWORD RemainingCapacity;
      DWORD Rate;
      DWORD EstimatedTime;
      DWORD DefaultAlert1;
      DWORD DefaultAlert2;
    } SYSTEM_BATTERY_STATE,*PSYSTEM_BATTERY_STATE;

#include "pshpack4.h"

#define IMAGE_DOS_SIGNATURE 0x5A4D
#define IMAGE_OS2_SIGNATURE 0x454E
#define IMAGE_OS2_SIGNATURE_LE 0x454C
#define IMAGE_VXD_SIGNATURE 0x454C
#define IMAGE_NT_SIGNATURE 0x00004550

#include "pshpack2.h"

    typedef struct _IMAGE_DOS_HEADER {
      WORD e_magic;
      WORD e_cblp;
      WORD e_cp;
      WORD e_crlc;
      WORD e_cparhdr;
      WORD e_minalloc;
      WORD e_maxalloc;
      WORD e_ss;
      WORD e_sp;
      WORD e_csum;
      WORD e_ip;
      WORD e_cs;
      WORD e_lfarlc;
      WORD e_ovno;
      WORD e_res[4];
      WORD e_oemid;
      WORD e_oeminfo;
      WORD e_res2[10];
      LONG e_lfanew;
    } IMAGE_DOS_HEADER,*PIMAGE_DOS_HEADER;

    typedef struct _IMAGE_OS2_HEADER {
      WORD ne_magic;
      CHAR ne_ver;
      CHAR ne_rev;
      WORD ne_enttab;
      WORD ne_cbenttab;
      LONG ne_crc;
      WORD ne_flags;
      WORD ne_autodata;
      WORD ne_heap;
      WORD ne_stack;
      LONG ne_csip;
      LONG ne_sssp;
      WORD ne_cseg;
      WORD ne_cmod;
      WORD ne_cbnrestab;
      WORD ne_segtab;
      WORD ne_rsrctab;
      WORD ne_restab;
      WORD ne_modtab;
      WORD ne_imptab;
      LONG ne_nrestab;
      WORD ne_cmovent;
      WORD ne_align;
      WORD ne_cres;
      BYTE ne_exetyp;
      BYTE ne_flagsothers;
      WORD ne_pretthunks;
      WORD ne_psegrefbytes;
      WORD ne_swaparea;
      WORD ne_expver;
    } IMAGE_OS2_HEADER,*PIMAGE_OS2_HEADER;

    typedef struct _IMAGE_VXD_HEADER {
      WORD e32_magic;
      BYTE e32_border;
      BYTE e32_worder;
      DWORD e32_level;
      WORD e32_cpu;
      WORD e32_os;
      DWORD e32_ver;
      DWORD e32_mflags;
      DWORD e32_mpages;
      DWORD e32_startobj;
      DWORD e32_eip;
      DWORD e32_stackobj;
      DWORD e32_esp;
      DWORD e32_pagesize;
      DWORD e32_lastpagesize;
      DWORD e32_fixupsize;
      DWORD e32_fixupsum;
      DWORD e32_ldrsize;
      DWORD e32_ldrsum;
      DWORD e32_objtab;
      DWORD e32_objcnt;
      DWORD e32_objmap;
      DWORD e32_itermap;
      DWORD e32_rsrctab;
      DWORD e32_rsrccnt;
      DWORD e32_restab;
      DWORD e32_enttab;
      DWORD e32_dirtab;
      DWORD e32_dircnt;
      DWORD e32_fpagetab;
      DWORD e32_frectab;
      DWORD e32_impmod;
      DWORD e32_impmodcnt;
      DWORD e32_impproc;
      DWORD e32_pagesum;
      DWORD e32_datapage;
      DWORD e32_preload;
      DWORD e32_nrestab;
      DWORD e32_cbnrestab;
      DWORD e32_nressum;
      DWORD e32_autodata;
      DWORD e32_debuginfo;
      DWORD e32_debuglen;
      DWORD e32_instpreload;
      DWORD e32_instdemand;
      DWORD e32_heapsize;
      BYTE e32_res3[12];
      DWORD e32_winresoff;
      DWORD e32_winreslen;
      WORD e32_devid;
      WORD e32_ddkver;
    } IMAGE_VXD_HEADER,*PIMAGE_VXD_HEADER;

#include "poppack.h"

    typedef struct _IMAGE_FILE_HEADER {
      WORD Machine;
      WORD NumberOfSections;
      DWORD TimeDateStamp;
      DWORD PointerToSymbolTable;
      DWORD NumberOfSymbols;
      WORD SizeOfOptionalHeader;
      WORD Characteristics;
    } IMAGE_FILE_HEADER,*PIMAGE_FILE_HEADER;

#define IMAGE_SIZEOF_FILE_HEADER 20

#define IMAGE_FILE_RELOCS_STRIPPED 0x0001
#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_LINE_NUMS_STRIPPED 0x0004
#define IMAGE_FILE_LOCAL_SYMS_STRIPPED 0x0008
#define IMAGE_FILE_AGGRESIVE_WS_TRIM 0x0010
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020
#define IMAGE_FILE_BYTES_REVERSED_LO 0x0080
#define IMAGE_FILE_32BIT_MACHINE 0x0100
#define IMAGE_FILE_DEBUG_STRIPPED 0x0200
#define IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0x0400
#define IMAGE_FILE_NET_RUN_FROM_SWAP 0x0800
#define IMAGE_FILE_SYSTEM 0x1000
#define IMAGE_FILE_DLL 0x2000
#define IMAGE_FILE_UP_SYSTEM_ONLY 0x4000
#define IMAGE_FILE_BYTES_REVERSED_HI 0x8000

#define IMAGE_FILE_MACHINE_UNKNOWN 0
#define IMAGE_FILE_MACHINE_I386 0x014c
#define IMAGE_FILE_MACHINE_R3000 0x0162
#define IMAGE_FILE_MACHINE_R4000 0x0166
#define IMAGE_FILE_MACHINE_R10000 0x0168
#define IMAGE_FILE_MACHINE_WCEMIPSV2 0x0169
#define IMAGE_FILE_MACHINE_ALPHA 0x0184
#define IMAGE_FILE_MACHINE_SH3 0x01a2
#define IMAGE_FILE_MACHINE_SH3DSP 0x01a3
#define IMAGE_FILE_MACHINE_SH3E 0x01a4
#define IMAGE_FILE_MACHINE_SH4 0x01a6
#define IMAGE_FILE_MACHINE_SH5 0x01a8
#define IMAGE_FILE_MACHINE_ARM 0x01c0
#define IMAGE_FILE_MACHINE_THUMB 0x01c2
#define IMAGE_FILE_MACHINE_AM33 0x01d3
#define IMAGE_FILE_MACHINE_POWERPC 0x01F0
#define IMAGE_FILE_MACHINE_POWERPCFP 0x01f1
#define IMAGE_FILE_MACHINE_IA64 0x0200
#define IMAGE_FILE_MACHINE_MIPS16 0x0266
#define IMAGE_FILE_MACHINE_ALPHA64 0x0284
#define IMAGE_FILE_MACHINE_MIPSFPU 0x0366
#define IMAGE_FILE_MACHINE_MIPSFPU16 0x0466
#define IMAGE_FILE_MACHINE_AXP64 IMAGE_FILE_MACHINE_ALPHA64
#define IMAGE_FILE_MACHINE_TRICORE 0x0520
#define IMAGE_FILE_MACHINE_CEF 0x0CEF
#define IMAGE_FILE_MACHINE_EBC 0x0EBC
#define IMAGE_FILE_MACHINE_AMD64 0x8664
#define IMAGE_FILE_MACHINE_M32R 0x9041
#define IMAGE_FILE_MACHINE_CEE 0xC0EE

    typedef struct _IMAGE_DATA_DIRECTORY {
      DWORD VirtualAddress;
      DWORD Size;
    } IMAGE_DATA_DIRECTORY,*PIMAGE_DATA_DIRECTORY;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16

    typedef struct _IMAGE_OPTIONAL_HEADER {

      WORD Magic;
      BYTE MajorLinkerVersion;
      BYTE MinorLinkerVersion;
      DWORD SizeOfCode;
      DWORD SizeOfInitializedData;
      DWORD SizeOfUninitializedData;
      DWORD AddressOfEntryPoint;
      DWORD BaseOfCode;
      DWORD BaseOfData;
      DWORD ImageBase;
      DWORD SectionAlignment;
      DWORD FileAlignment;
      WORD MajorOperatingSystemVersion;
      WORD MinorOperatingSystemVersion;
      WORD MajorImageVersion;
      WORD MinorImageVersion;
      WORD MajorSubsystemVersion;
      WORD MinorSubsystemVersion;
      DWORD Win32VersionValue;
      DWORD SizeOfImage;
      DWORD SizeOfHeaders;
      DWORD CheckSum;
      WORD Subsystem;
      WORD DllCharacteristics;
      DWORD SizeOfStackReserve;
      DWORD SizeOfStackCommit;
      DWORD SizeOfHeapReserve;
      DWORD SizeOfHeapCommit;
      DWORD LoaderFlags;
      DWORD NumberOfRvaAndSizes;
      IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
    } IMAGE_OPTIONAL_HEADER32,*PIMAGE_OPTIONAL_HEADER32;

    typedef struct _IMAGE_ROM_OPTIONAL_HEADER {
      WORD Magic;
      BYTE MajorLinkerVersion;
      BYTE MinorLinkerVersion;
      DWORD SizeOfCode;
      DWORD SizeOfInitializedData;
      DWORD SizeOfUninitializedData;
      DWORD AddressOfEntryPoint;
      DWORD BaseOfCode;
      DWORD BaseOfData;
      DWORD BaseOfBss;
      DWORD GprMask;
      DWORD CprMask[4];
      DWORD GpValue;
    } IMAGE_ROM_OPTIONAL_HEADER,*PIMAGE_ROM_OPTIONAL_HEADER;

    typedef struct _IMAGE_OPTIONAL_HEADER64 {
      WORD Magic;
      BYTE MajorLinkerVersion;
      BYTE MinorLinkerVersion;
      DWORD SizeOfCode;
      DWORD SizeOfInitializedData;
      DWORD SizeOfUninitializedData;
      DWORD AddressOfEntryPoint;
      DWORD BaseOfCode;
      ULONGLONG ImageBase;
      DWORD SectionAlignment;
      DWORD FileAlignment;
      WORD MajorOperatingSystemVersion;
      WORD MinorOperatingSystemVersion;
      WORD MajorImageVersion;
      WORD MinorImageVersion;
      WORD MajorSubsystemVersion;
      WORD MinorSubsystemVersion;
      DWORD Win32VersionValue;
      DWORD SizeOfImage;
      DWORD SizeOfHeaders;
      DWORD CheckSum;
      WORD Subsystem;
      WORD DllCharacteristics;
      ULONGLONG SizeOfStackReserve;
      ULONGLONG SizeOfStackCommit;
      ULONGLONG SizeOfHeapReserve;
      ULONGLONG SizeOfHeapCommit;
      DWORD LoaderFlags;
      DWORD NumberOfRvaAndSizes;
      IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
    } IMAGE_OPTIONAL_HEADER64,*PIMAGE_OPTIONAL_HEADER64;

#define IMAGE_SIZEOF_ROM_OPTIONAL_HEADER 56
#define IMAGE_SIZEOF_STD_OPTIONAL_HEADER 28
#define IMAGE_SIZEOF_NT_OPTIONAL32_HEADER 224
#define IMAGE_SIZEOF_NT_OPTIONAL64_HEADER 240

#define IMAGE_NT_OPTIONAL_HDR32_MAGIC 0x10b
#define IMAGE_NT_OPTIONAL_HDR64_MAGIC 0x20b
#define IMAGE_ROM_OPTIONAL_HDR_MAGIC 0x107

#ifdef _WIN64
    typedef IMAGE_OPTIONAL_HEADER64 IMAGE_OPTIONAL_HEADER;
    typedef PIMAGE_OPTIONAL_HEADER64 PIMAGE_OPTIONAL_HEADER;
#define IMAGE_SIZEOF_NT_OPTIONAL_HEADER IMAGE_SIZEOF_NT_OPTIONAL64_HEADER
#define IMAGE_NT_OPTIONAL_HDR_MAGIC IMAGE_NT_OPTIONAL_HDR64_MAGIC
#else
    typedef IMAGE_OPTIONAL_HEADER32 IMAGE_OPTIONAL_HEADER;
    typedef PIMAGE_OPTIONAL_HEADER32 PIMAGE_OPTIONAL_HEADER;
#define IMAGE_SIZEOF_NT_OPTIONAL_HEADER IMAGE_SIZEOF_NT_OPTIONAL32_HEADER
#define IMAGE_NT_OPTIONAL_HDR_MAGIC IMAGE_NT_OPTIONAL_HDR32_MAGIC
#endif

    typedef struct _IMAGE_NT_HEADERS64 {
      DWORD Signature;
      IMAGE_FILE_HEADER FileHeader;
      IMAGE_OPTIONAL_HEADER64 OptionalHeader;
    } IMAGE_NT_HEADERS64,*PIMAGE_NT_HEADERS64;

    typedef struct _IMAGE_NT_HEADERS {
      DWORD Signature;
      IMAGE_FILE_HEADER FileHeader;
      IMAGE_OPTIONAL_HEADER32 OptionalHeader;
    } IMAGE_NT_HEADERS32,*PIMAGE_NT_HEADERS32;

    typedef struct _IMAGE_ROM_HEADERS {
      IMAGE_FILE_HEADER FileHeader;
      IMAGE_ROM_OPTIONAL_HEADER OptionalHeader;
    } IMAGE_ROM_HEADERS,*PIMAGE_ROM_HEADERS;

#ifdef _WIN64
    typedef IMAGE_NT_HEADERS64 IMAGE_NT_HEADERS;
    typedef PIMAGE_NT_HEADERS64 PIMAGE_NT_HEADERS;
#else
    typedef IMAGE_NT_HEADERS32 IMAGE_NT_HEADERS;
    typedef PIMAGE_NT_HEADERS32 PIMAGE_NT_HEADERS;
#endif

#define IMAGE_FIRST_SECTION(ntheader) ((PIMAGE_SECTION_HEADER) ((ULONG_PTR)ntheader + FIELD_OFFSET(IMAGE_NT_HEADERS,OptionalHeader) + ((PIMAGE_NT_HEADERS)(ntheader))->FileHeader.SizeOfOptionalHeader))

#define IMAGE_SUBSYSTEM_UNKNOWN 0
#define IMAGE_SUBSYSTEM_NATIVE 1
#define IMAGE_SUBSYSTEM_WINDOWS_GUI 2
#define IMAGE_SUBSYSTEM_WINDOWS_CUI 3
#define IMAGE_SUBSYSTEM_OS2_CUI 5
#define IMAGE_SUBSYSTEM_POSIX_CUI 7
#define IMAGE_SUBSYSTEM_NATIVE_WINDOWS 8
#define IMAGE_SUBSYSTEM_WINDOWS_CE_GUI 9
#define IMAGE_SUBSYSTEM_EFI_APPLICATION 10
#define IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER 11
#define IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER 12
#define IMAGE_SUBSYSTEM_EFI_ROM 13
#define IMAGE_SUBSYSTEM_XBOX 14

#define IMAGE_DLLCHARACTERISTICS_NO_ISOLATION 0x0200
#define IMAGE_DLLCHARACTERISTICS_NO_SEH 0x0400
#define IMAGE_DLLCHARACTERISTICS_NO_BIND 0x0800
#define IMAGE_DLLCHARACTERISTICS_WDM_DRIVER 0x2000
#define IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE 0x8000

#define IMAGE_DIRECTORY_ENTRY_EXPORT 0
#define IMAGE_DIRECTORY_ENTRY_IMPORT 1
#define IMAGE_DIRECTORY_ENTRY_RESOURCE 2
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION 3
#define IMAGE_DIRECTORY_ENTRY_SECURITY 4
#define IMAGE_DIRECTORY_ENTRY_BASERELOC 5
#define IMAGE_DIRECTORY_ENTRY_DEBUG 6

#define IMAGE_DIRECTORY_ENTRY_ARCHITECTURE 7
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR 8
#define IMAGE_DIRECTORY_ENTRY_TLS 9
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG 10
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT 11
#define IMAGE_DIRECTORY_ENTRY_IAT 12
#define IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT 13
#define IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14

    typedef struct ANON_OBJECT_HEADER {
      WORD Sig1;
      WORD Sig2;
      WORD Version;
      WORD Machine;
      DWORD TimeDateStamp;
      CLSID ClassID;
      DWORD SizeOfData;
    } ANON_OBJECT_HEADER;

#define IMAGE_SIZEOF_SHORT_NAME 8

    typedef struct _IMAGE_SECTION_HEADER {
      BYTE Name[IMAGE_SIZEOF_SHORT_NAME];
      union {
	DWORD PhysicalAddress;
	DWORD VirtualSize;
      } Misc;
      DWORD VirtualAddress;
      DWORD SizeOfRawData;
      DWORD PointerToRawData;
      DWORD PointerToRelocations;
      DWORD PointerToLinenumbers;
      WORD NumberOfRelocations;
      WORD NumberOfLinenumbers;
      DWORD Characteristics;
    } IMAGE_SECTION_HEADER,*PIMAGE_SECTION_HEADER;

#define IMAGE_SIZEOF_SECTION_HEADER 40

#define IMAGE_SCN_TYPE_NO_PAD 0x00000008

#define IMAGE_SCN_CNT_CODE 0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x00000080
#define IMAGE_SCN_LNK_OTHER 0x00000100
#define IMAGE_SCN_LNK_INFO 0x00000200
#define IMAGE_SCN_LNK_REMOVE 0x00000800
#define IMAGE_SCN_LNK_COMDAT 0x00001000
#define IMAGE_SCN_NO_DEFER_SPEC_EXC 0x00004000
#define IMAGE_SCN_GPREL 0x00008000
#define IMAGE_SCN_MEM_FARDATA 0x00008000
#define IMAGE_SCN_MEM_PURGEABLE 0x00020000
#define IMAGE_SCN_MEM_16BIT 0x00020000
#define IMAGE_SCN_MEM_LOCKED 0x00040000
#define IMAGE_SCN_MEM_PRELOAD 0x00080000

#define IMAGE_SCN_ALIGN_1BYTES 0x00100000
#define IMAGE_SCN_ALIGN_2BYTES 0x00200000
#define IMAGE_SCN_ALIGN_4BYTES 0x00300000
#define IMAGE_SCN_ALIGN_8BYTES 0x00400000
#define IMAGE_SCN_ALIGN_16BYTES 0x00500000
#define IMAGE_SCN_ALIGN_32BYTES 0x00600000
#define IMAGE_SCN_ALIGN_64BYTES 0x00700000
#define IMAGE_SCN_ALIGN_128BYTES 0x00800000
#define IMAGE_SCN_ALIGN_256BYTES 0x00900000
#define IMAGE_SCN_ALIGN_512BYTES 0x00A00000
#define IMAGE_SCN_ALIGN_1024BYTES 0x00B00000
#define IMAGE_SCN_ALIGN_2048BYTES 0x00C00000
#define IMAGE_SCN_ALIGN_4096BYTES 0x00D00000
#define IMAGE_SCN_ALIGN_8192BYTES 0x00E00000

#define IMAGE_SCN_ALIGN_MASK 0x00F00000

#define IMAGE_SCN_LNK_NRELOC_OVFL 0x01000000
#define IMAGE_SCN_MEM_DISCARDABLE 0x02000000
#define IMAGE_SCN_MEM_NOT_CACHED 0x04000000
#define IMAGE_SCN_MEM_NOT_PAGED 0x08000000
#define IMAGE_SCN_MEM_SHARED 0x10000000
#define IMAGE_SCN_MEM_EXECUTE 0x20000000
#define IMAGE_SCN_MEM_READ 0x40000000
#define IMAGE_SCN_MEM_WRITE 0x80000000

#define IMAGE_SCN_SCALE_INDEX 0x00000001

#include "pshpack2.h"

    typedef struct _IMAGE_SYMBOL {
      union {
	BYTE ShortName[8];
	struct {
	  DWORD Short;
	  DWORD Long;
	} Name;
	DWORD LongName[2];
      } N;
      DWORD Value;
      SHORT SectionNumber;
      WORD Type;
      BYTE StorageClass;
      BYTE NumberOfAuxSymbols;
    } IMAGE_SYMBOL;
    typedef IMAGE_SYMBOL UNALIGNED *PIMAGE_SYMBOL;

#define IMAGE_SIZEOF_SYMBOL 18

#define IMAGE_SYM_UNDEFINED (SHORT)0
#define IMAGE_SYM_ABSOLUTE (SHORT)-1
#define IMAGE_SYM_DEBUG (SHORT)-2
#define IMAGE_SYM_SECTION_MAX 0xFEFF

#define IMAGE_SYM_TYPE_NULL 0x0000
#define IMAGE_SYM_TYPE_VOID 0x0001
#define IMAGE_SYM_TYPE_CHAR 0x0002
#define IMAGE_SYM_TYPE_SHORT 0x0003
#define IMAGE_SYM_TYPE_INT 0x0004
#define IMAGE_SYM_TYPE_LONG 0x0005
#define IMAGE_SYM_TYPE_FLOAT 0x0006
#define IMAGE_SYM_TYPE_DOUBLE 0x0007
#define IMAGE_SYM_TYPE_STRUCT 0x0008
#define IMAGE_SYM_TYPE_UNION 0x0009
#define IMAGE_SYM_TYPE_ENUM 0x000A
#define IMAGE_SYM_TYPE_MOE 0x000B
#define IMAGE_SYM_TYPE_BYTE 0x000C
#define IMAGE_SYM_TYPE_WORD 0x000D
#define IMAGE_SYM_TYPE_UINT 0x000E
#define IMAGE_SYM_TYPE_DWORD 0x000F
#define IMAGE_SYM_TYPE_PCODE 0x8000

#define IMAGE_SYM_DTYPE_NULL 0
#define IMAGE_SYM_DTYPE_POINTER 1
#define IMAGE_SYM_DTYPE_FUNCTION 2
#define IMAGE_SYM_DTYPE_ARRAY 3

#define IMAGE_SYM_CLASS_END_OF_FUNCTION (BYTE)-1
#define IMAGE_SYM_CLASS_NULL 0x0000
#define IMAGE_SYM_CLASS_AUTOMATIC 0x0001
#define IMAGE_SYM_CLASS_EXTERNAL 0x0002
#define IMAGE_SYM_CLASS_STATIC 0x0003
#define IMAGE_SYM_CLASS_REGISTER 0x0004
#define IMAGE_SYM_CLASS_EXTERNAL_DEF 0x0005
#define IMAGE_SYM_CLASS_LABEL 0x0006
#define IMAGE_SYM_CLASS_UNDEFINED_LABEL 0x0007
#define IMAGE_SYM_CLASS_MEMBER_OF_STRUCT 0x0008
#define IMAGE_SYM_CLASS_ARGUMENT 0x0009
#define IMAGE_SYM_CLASS_STRUCT_TAG 0x000A
#define IMAGE_SYM_CLASS_MEMBER_OF_UNION 0x000B
#define IMAGE_SYM_CLASS_UNION_TAG 0x000C
#define IMAGE_SYM_CLASS_TYPE_DEFINITION 0x000D
#define IMAGE_SYM_CLASS_UNDEFINED_STATIC 0x000E
#define IMAGE_SYM_CLASS_ENUM_TAG 0x000F
#define IMAGE_SYM_CLASS_MEMBER_OF_ENUM 0x0010
#define IMAGE_SYM_CLASS_REGISTER_PARAM 0x0011
#define IMAGE_SYM_CLASS_BIT_FIELD 0x0012
#define IMAGE_SYM_CLASS_FAR_EXTERNAL 0x0044
#define IMAGE_SYM_CLASS_BLOCK 0x0064
#define IMAGE_SYM_CLASS_FUNCTION 0x0065
#define IMAGE_SYM_CLASS_END_OF_STRUCT 0x0066
#define IMAGE_SYM_CLASS_FILE 0x0067
#define IMAGE_SYM_CLASS_SECTION 0x0068
#define IMAGE_SYM_CLASS_WEAK_EXTERNAL 0x0069
#define IMAGE_SYM_CLASS_CLR_TOKEN 0x006B

#define N_BTMASK 0x000F
#define N_TMASK 0x0030
#define N_TMASK1 0x00C0
#define N_TMASK2 0x00F0
#define N_BTSHFT 4
#define N_TSHIFT 2

#define BTYPE(x) ((x) & N_BTMASK)

#ifndef ISPTR
#define ISPTR(x) (((x) & N_TMASK)==(IMAGE_SYM_DTYPE_POINTER << N_BTSHFT))
#endif

#ifndef ISFCN
#define ISFCN(x) (((x) & N_TMASK)==(IMAGE_SYM_DTYPE_FUNCTION << N_BTSHFT))
#endif

#ifndef ISARY
#define ISARY(x) (((x) & N_TMASK)==(IMAGE_SYM_DTYPE_ARRAY << N_BTSHFT))
#endif

#ifndef ISTAG
#define ISTAG(x) ((x)==IMAGE_SYM_CLASS_STRUCT_TAG || (x)==IMAGE_SYM_CLASS_UNION_TAG || (x)==IMAGE_SYM_CLASS_ENUM_TAG)
#endif

#ifndef INCREF
#define INCREF(x) ((((x)&~N_BTMASK)<<N_TSHIFT)|(IMAGE_SYM_DTYPE_POINTER<<N_BTSHFT)|((x)&N_BTMASK))
#endif
#ifndef DECREF
#define DECREF(x) ((((x)>>N_TSHIFT)&~N_BTMASK)|((x)&N_BTMASK))
#endif

    typedef union _IMAGE_AUX_SYMBOL {
      struct {
	DWORD TagIndex;
	union {
	  struct {
	    WORD Linenumber;
	    WORD Size;
	  } LnSz;
	  DWORD TotalSize;
	} Misc;
	union {
	  struct {
	    DWORD PointerToLinenumber;
	    DWORD PointerToNextFunction;
	  } Function;
	  struct {
	    WORD Dimension[4];
	  } Array;
	} FcnAry;
	WORD TvIndex;
      } Sym;
      struct {
	BYTE Name[IMAGE_SIZEOF_SYMBOL];
      } File;
      struct {
	DWORD Length;
	WORD NumberOfRelocations;
	WORD NumberOfLinenumbers;
	DWORD CheckSum;
	SHORT Number;
	BYTE Selection;
      } Section;
    } IMAGE_AUX_SYMBOL;
    typedef IMAGE_AUX_SYMBOL UNALIGNED *PIMAGE_AUX_SYMBOL;

#define IMAGE_SIZEOF_AUX_SYMBOL 18

    typedef enum IMAGE_AUX_SYMBOL_TYPE {
      IMAGE_AUX_SYMBOL_TYPE_TOKEN_DEF = 1
    } IMAGE_AUX_SYMBOL_TYPE;

#include <pshpack2.h>

    typedef struct IMAGE_AUX_SYMBOL_TOKEN_DEF {
      BYTE bAuxType;
      BYTE bReserved;
      DWORD SymbolTableIndex;
      BYTE rgbReserved[12];
    } IMAGE_AUX_SYMBOL_TOKEN_DEF;

    typedef IMAGE_AUX_SYMBOL_TOKEN_DEF UNALIGNED *PIMAGE_AUX_SYMBOL_TOKEN_DEF;

#include <poppack.h>

#define IMAGE_COMDAT_SELECT_NODUPLICATES 1
#define IMAGE_COMDAT_SELECT_ANY 2
#define IMAGE_COMDAT_SELECT_SAME_SIZE 3
#define IMAGE_COMDAT_SELECT_EXACT_MATCH 4
#define IMAGE_COMDAT_SELECT_ASSOCIATIVE 5
#define IMAGE_COMDAT_SELECT_LARGEST 6
#define IMAGE_COMDAT_SELECT_NEWEST 7

#define IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY 1
#define IMAGE_WEAK_EXTERN_SEARCH_LIBRARY 2
#define IMAGE_WEAK_EXTERN_SEARCH_ALIAS 3

    typedef struct _IMAGE_RELOCATION {
      union {
	DWORD VirtualAddress;
	DWORD RelocCount;
      };
      DWORD SymbolTableIndex;
      WORD Type;
    } IMAGE_RELOCATION;
    typedef IMAGE_RELOCATION UNALIGNED *PIMAGE_RELOCATION;

#define IMAGE_SIZEOF_RELOCATION 10

#define IMAGE_REL_I386_ABSOLUTE 0x0000
#define IMAGE_REL_I386_DIR16 0x0001
#define IMAGE_REL_I386_REL16 0x0002
#define IMAGE_REL_I386_DIR32 0x0006
#define IMAGE_REL_I386_DIR32NB 0x0007
#define IMAGE_REL_I386_SEG12 0x0009
#define IMAGE_REL_I386_SECTION 0x000A
#define IMAGE_REL_I386_SECREL 0x000B
#define IMAGE_REL_I386_TOKEN 0x000C
#define IMAGE_REL_I386_SECREL7 0x000D
#define IMAGE_REL_I386_REL32 0x0014

#define IMAGE_REL_MIPS_ABSOLUTE 0x0000
#define IMAGE_REL_MIPS_REFHALF 0x0001
#define IMAGE_REL_MIPS_REFWORD 0x0002
#define IMAGE_REL_MIPS_JMPADDR 0x0003
#define IMAGE_REL_MIPS_REFHI 0x0004
#define IMAGE_REL_MIPS_REFLO 0x0005
#define IMAGE_REL_MIPS_GPREL 0x0006
#define IMAGE_REL_MIPS_LITERAL 0x0007
#define IMAGE_REL_MIPS_SECTION 0x000A
#define IMAGE_REL_MIPS_SECREL 0x000B
#define IMAGE_REL_MIPS_SECRELLO 0x000C
#define IMAGE_REL_MIPS_SECRELHI 0x000D
#define IMAGE_REL_MIPS_TOKEN 0x000E
#define IMAGE_REL_MIPS_JMPADDR16 0x0010
#define IMAGE_REL_MIPS_REFWORDNB 0x0022
#define IMAGE_REL_MIPS_PAIR 0x0025

#define IMAGE_REL_ALPHA_ABSOLUTE 0x0000
#define IMAGE_REL_ALPHA_REFLONG 0x0001
#define IMAGE_REL_ALPHA_REFQUAD 0x0002
#define IMAGE_REL_ALPHA_GPREL32 0x0003
#define IMAGE_REL_ALPHA_LITERAL 0x0004
#define IMAGE_REL_ALPHA_LITUSE 0x0005
#define IMAGE_REL_ALPHA_GPDISP 0x0006
#define IMAGE_REL_ALPHA_BRADDR 0x0007
#define IMAGE_REL_ALPHA_HINT 0x0008
#define IMAGE_REL_ALPHA_INLINE_REFLONG 0x0009
#define IMAGE_REL_ALPHA_REFHI 0x000A
#define IMAGE_REL_ALPHA_REFLO 0x000B
#define IMAGE_REL_ALPHA_PAIR 0x000C
#define IMAGE_REL_ALPHA_MATCH 0x000D
#define IMAGE_REL_ALPHA_SECTION 0x000E
#define IMAGE_REL_ALPHA_SECREL 0x000F
#define IMAGE_REL_ALPHA_REFLONGNB 0x0010
#define IMAGE_REL_ALPHA_SECRELLO 0x0011
#define IMAGE_REL_ALPHA_SECRELHI 0x0012
#define IMAGE_REL_ALPHA_REFQ3 0x0013
#define IMAGE_REL_ALPHA_REFQ2 0x0014
#define IMAGE_REL_ALPHA_REFQ1 0x0015
#define IMAGE_REL_ALPHA_GPRELLO 0x0016
#define IMAGE_REL_ALPHA_GPRELHI 0x0017

#define IMAGE_REL_PPC_ABSOLUTE 0x0000
#define IMAGE_REL_PPC_ADDR64 0x0001
#define IMAGE_REL_PPC_ADDR32 0x0002
#define IMAGE_REL_PPC_ADDR24 0x0003
#define IMAGE_REL_PPC_ADDR16 0x0004
#define IMAGE_REL_PPC_ADDR14 0x0005
#define IMAGE_REL_PPC_REL24 0x0006
#define IMAGE_REL_PPC_REL14 0x0007
#define IMAGE_REL_PPC_TOCREL16 0x0008
#define IMAGE_REL_PPC_TOCREL14 0x0009
#define IMAGE_REL_PPC_ADDR32NB 0x000A
#define IMAGE_REL_PPC_SECREL 0x000B
#define IMAGE_REL_PPC_SECTION 0x000C
#define IMAGE_REL_PPC_IFGLUE 0x000D
#define IMAGE_REL_PPC_IMGLUE 0x000E
#define IMAGE_REL_PPC_SECREL16 0x000F
#define IMAGE_REL_PPC_REFHI 0x0010
#define IMAGE_REL_PPC_REFLO 0x0011
#define IMAGE_REL_PPC_PAIR 0x0012
#define IMAGE_REL_PPC_SECRELLO 0x0013
#define IMAGE_REL_PPC_SECRELHI 0x0014
#define IMAGE_REL_PPC_GPREL 0x0015
#define IMAGE_REL_PPC_TOKEN 0x0016
#define IMAGE_REL_PPC_TYPEMASK 0x00FF
#define IMAGE_REL_PPC_NEG 0x0100
#define IMAGE_REL_PPC_BRTAKEN 0x0200
#define IMAGE_REL_PPC_BRNTAKEN 0x0400
#define IMAGE_REL_PPC_TOCDEFN 0x0800

#define IMAGE_REL_SH3_ABSOLUTE 0x0000
#define IMAGE_REL_SH3_DIRECT16 0x0001
#define IMAGE_REL_SH3_DIRECT32 0x0002
#define IMAGE_REL_SH3_DIRECT8 0x0003
#define IMAGE_REL_SH3_DIRECT8_WORD 0x0004
#define IMAGE_REL_SH3_DIRECT8_LONG 0x0005
#define IMAGE_REL_SH3_DIRECT4 0x0006
#define IMAGE_REL_SH3_DIRECT4_WORD 0x0007
#define IMAGE_REL_SH3_DIRECT4_LONG 0x0008
#define IMAGE_REL_SH3_PCREL8_WORD 0x0009
#define IMAGE_REL_SH3_PCREL8_LONG 0x000A
#define IMAGE_REL_SH3_PCREL12_WORD 0x000B
#define IMAGE_REL_SH3_STARTOF_SECTION 0x000C
#define IMAGE_REL_SH3_SIZEOF_SECTION 0x000D
#define IMAGE_REL_SH3_SECTION 0x000E
#define IMAGE_REL_SH3_SECREL 0x000F
#define IMAGE_REL_SH3_DIRECT32_NB 0x0010
#define IMAGE_REL_SH3_GPREL4_LONG 0x0011
#define IMAGE_REL_SH3_TOKEN 0x0012

#define IMAGE_REL_SHM_PCRELPT 0x0013
#define IMAGE_REL_SHM_REFLO 0x0014
#define IMAGE_REL_SHM_REFHALF 0x0015
#define IMAGE_REL_SHM_RELLO 0x0016
#define IMAGE_REL_SHM_RELHALF 0x0017
#define IMAGE_REL_SHM_PAIR 0x0018

#define IMAGE_REL_SH_NOMODE 0x8000

#define IMAGE_REL_ARM_ABSOLUTE 0x0000
#define IMAGE_REL_ARM_ADDR32 0x0001
#define IMAGE_REL_ARM_ADDR32NB 0x0002
#define IMAGE_REL_ARM_BRANCH24 0x0003
#define IMAGE_REL_ARM_BRANCH11 0x0004
#define IMAGE_REL_ARM_TOKEN 0x0005
#define IMAGE_REL_ARM_GPREL12 0x0006
#define IMAGE_REL_ARM_GPREL7 0x0007
#define IMAGE_REL_ARM_BLX24 0x0008
#define IMAGE_REL_ARM_BLX11 0x0009
#define IMAGE_REL_ARM_SECTION 0x000E
#define IMAGE_REL_ARM_SECREL 0x000F

#define IMAGE_REL_AM_ABSOLUTE 0x0000
#define IMAGE_REL_AM_ADDR32 0x0001
#define IMAGE_REL_AM_ADDR32NB 0x0002
#define IMAGE_REL_AM_CALL32 0x0003
#define IMAGE_REL_AM_FUNCINFO 0x0004
#define IMAGE_REL_AM_REL32_1 0x0005
#define IMAGE_REL_AM_REL32_2 0x0006
#define IMAGE_REL_AM_SECREL 0x0007
#define IMAGE_REL_AM_SECTION 0x0008
#define IMAGE_REL_AM_TOKEN 0x0009

#define IMAGE_REL_AMD64_ABSOLUTE 0x0000
#define IMAGE_REL_AMD64_ADDR64 0x0001
#define IMAGE_REL_AMD64_ADDR32 0x0002
#define IMAGE_REL_AMD64_ADDR32NB 0x0003
#define IMAGE_REL_AMD64_REL32 0x0004
#define IMAGE_REL_AMD64_REL32_1 0x0005
#define IMAGE_REL_AMD64_REL32_2 0x0006
#define IMAGE_REL_AMD64_REL32_3 0x0007
#define IMAGE_REL_AMD64_REL32_4 0x0008
#define IMAGE_REL_AMD64_REL32_5 0x0009
#define IMAGE_REL_AMD64_SECTION 0x000A
#define IMAGE_REL_AMD64_SECREL 0x000B
#define IMAGE_REL_AMD64_SECREL7 0x000C
#define IMAGE_REL_AMD64_TOKEN 0x000D
#define IMAGE_REL_AMD64_SREL32 0x000E
#define IMAGE_REL_AMD64_PAIR 0x000F
#define IMAGE_REL_AMD64_SSPAN32 0x0010

#define IMAGE_REL_IA64_ABSOLUTE 0x0000
#define IMAGE_REL_IA64_IMM14 0x0001
#define IMAGE_REL_IA64_IMM22 0x0002
#define IMAGE_REL_IA64_IMM64 0x0003
#define IMAGE_REL_IA64_DIR32 0x0004
#define IMAGE_REL_IA64_DIR64 0x0005
#define IMAGE_REL_IA64_PCREL21B 0x0006
#define IMAGE_REL_IA64_PCREL21M 0x0007
#define IMAGE_REL_IA64_PCREL21F 0x0008
#define IMAGE_REL_IA64_GPREL22 0x0009
#define IMAGE_REL_IA64_LTOFF22 0x000A
#define IMAGE_REL_IA64_SECTION 0x000B
#define IMAGE_REL_IA64_SECREL22 0x000C
#define IMAGE_REL_IA64_SECREL64I 0x000D
#define IMAGE_REL_IA64_SECREL32 0x000E

#define IMAGE_REL_IA64_DIR32NB 0x0010
#define IMAGE_REL_IA64_SREL14 0x0011
#define IMAGE_REL_IA64_SREL22 0x0012
#define IMAGE_REL_IA64_SREL32 0x0013
#define IMAGE_REL_IA64_UREL32 0x0014
#define IMAGE_REL_IA64_PCREL60X 0x0015
#define IMAGE_REL_IA64_PCREL60B 0x0016
#define IMAGE_REL_IA64_PCREL60F 0x0017
#define IMAGE_REL_IA64_PCREL60I 0x0018
#define IMAGE_REL_IA64_PCREL60M 0x0019
#define IMAGE_REL_IA64_IMMGPREL64 0x001A
#define IMAGE_REL_IA64_TOKEN 0x001B
#define IMAGE_REL_IA64_GPREL32 0x001C
#define IMAGE_REL_IA64_ADDEND 0x001F

#define IMAGE_REL_CEF_ABSOLUTE 0x0000
#define IMAGE_REL_CEF_ADDR32 0x0001
#define IMAGE_REL_CEF_ADDR64 0x0002
#define IMAGE_REL_CEF_ADDR32NB 0x0003
#define IMAGE_REL_CEF_SECTION 0x0004
#define IMAGE_REL_CEF_SECREL 0x0005
#define IMAGE_REL_CEF_TOKEN 0x0006

#define IMAGE_REL_CEE_ABSOLUTE 0x0000
#define IMAGE_REL_CEE_ADDR32 0x0001
#define IMAGE_REL_CEE_ADDR64 0x0002
#define IMAGE_REL_CEE_ADDR32NB 0x0003
#define IMAGE_REL_CEE_SECTION 0x0004
#define IMAGE_REL_CEE_SECREL 0x0005
#define IMAGE_REL_CEE_TOKEN 0x0006

#define IMAGE_REL_M32R_ABSOLUTE 0x0000
#define IMAGE_REL_M32R_ADDR32 0x0001
#define IMAGE_REL_M32R_ADDR32NB 0x0002
#define IMAGE_REL_M32R_ADDR24 0x0003
#define IMAGE_REL_M32R_GPREL16 0x0004
#define IMAGE_REL_M32R_PCREL24 0x0005
#define IMAGE_REL_M32R_PCREL16 0x0006
#define IMAGE_REL_M32R_PCREL8 0x0007
#define IMAGE_REL_M32R_REFHALF 0x0008
#define IMAGE_REL_M32R_REFHI 0x0009
#define IMAGE_REL_M32R_REFLO 0x000A
#define IMAGE_REL_M32R_PAIR 0x000B
#define IMAGE_REL_M32R_SECTION 0x000C
#define IMAGE_REL_M32R_SECREL32 0x000D
#define IMAGE_REL_M32R_TOKEN 0x000E

#define EXT_IMM64(Value,Address,Size,InstPos,ValPos) Value |= (((ULONGLONG)((*(Address) >> InstPos) & (((ULONGLONG)1 << Size) - 1))) << ValPos)
#define INS_IMM64(Value,Address,Size,InstPos,ValPos) *(PDWORD)Address = (*(PDWORD)Address & ~(((1 << Size) - 1) << InstPos)) | ((DWORD)((((ULONGLONG)Value >> ValPos) & (((ULONGLONG)1 << Size) - 1))) << InstPos)

#define EMARCH_ENC_I17_IMM7B_INST_WORD_X 3
#define EMARCH_ENC_I17_IMM7B_SIZE_X 7
#define EMARCH_ENC_I17_IMM7B_INST_WORD_POS_X 4
#define EMARCH_ENC_I17_IMM7B_VAL_POS_X 0

#define EMARCH_ENC_I17_IMM9D_INST_WORD_X 3
#define EMARCH_ENC_I17_IMM9D_SIZE_X 9
#define EMARCH_ENC_I17_IMM9D_INST_WORD_POS_X 18
#define EMARCH_ENC_I17_IMM9D_VAL_POS_X 7

#define EMARCH_ENC_I17_IMM5C_INST_WORD_X 3
#define EMARCH_ENC_I17_IMM5C_SIZE_X 5
#define EMARCH_ENC_I17_IMM5C_INST_WORD_POS_X 13
#define EMARCH_ENC_I17_IMM5C_VAL_POS_X 16

#define EMARCH_ENC_I17_IC_INST_WORD_X 3
#define EMARCH_ENC_I17_IC_SIZE_X 1
#define EMARCH_ENC_I17_IC_INST_WORD_POS_X 12
#define EMARCH_ENC_I17_IC_VAL_POS_X 21

#define EMARCH_ENC_I17_IMM41a_INST_WORD_X 1
#define EMARCH_ENC_I17_IMM41a_SIZE_X 10
#define EMARCH_ENC_I17_IMM41a_INST_WORD_POS_X 14
#define EMARCH_ENC_I17_IMM41a_VAL_POS_X 22

#define EMARCH_ENC_I17_IMM41b_INST_WORD_X 1
#define EMARCH_ENC_I17_IMM41b_SIZE_X 8
#define EMARCH_ENC_I17_IMM41b_INST_WORD_POS_X 24
#define EMARCH_ENC_I17_IMM41b_VAL_POS_X 32

#define EMARCH_ENC_I17_IMM41c_INST_WORD_X 2
#define EMARCH_ENC_I17_IMM41c_SIZE_X 23
#define EMARCH_ENC_I17_IMM41c_INST_WORD_POS_X 0
#define EMARCH_ENC_I17_IMM41c_VAL_POS_X 40

#define EMARCH_ENC_I17_SIGN_INST_WORD_X 3
#define EMARCH_ENC_I17_SIGN_SIZE_X 1
#define EMARCH_ENC_I17_SIGN_INST_WORD_POS_X 27
#define EMARCH_ENC_I17_SIGN_VAL_POS_X 63

#define X3_OPCODE_INST_WORD_X 3
#define X3_OPCODE_SIZE_X 4
#define X3_OPCODE_INST_WORD_POS_X 28
#define X3_OPCODE_SIGN_VAL_POS_X 0

#define X3_I_INST_WORD_X 3
#define X3_I_SIZE_X 1
#define X3_I_INST_WORD_POS_X 27
#define X3_I_SIGN_VAL_POS_X 59

#define X3_D_WH_INST_WORD_X 3
#define X3_D_WH_SIZE_X 3
#define X3_D_WH_INST_WORD_POS_X 24
#define X3_D_WH_SIGN_VAL_POS_X 0

#define X3_IMM20_INST_WORD_X 3
#define X3_IMM20_SIZE_X 20
#define X3_IMM20_INST_WORD_POS_X 4
#define X3_IMM20_SIGN_VAL_POS_X 0

#define X3_IMM39_1_INST_WORD_X 2
#define X3_IMM39_1_SIZE_X 23
#define X3_IMM39_1_INST_WORD_POS_X 0
#define X3_IMM39_1_SIGN_VAL_POS_X 36

#define X3_IMM39_2_INST_WORD_X 1
#define X3_IMM39_2_SIZE_X 16
#define X3_IMM39_2_INST_WORD_POS_X 16
#define X3_IMM39_2_SIGN_VAL_POS_X 20

#define X3_P_INST_WORD_X 3
#define X3_P_SIZE_X 4
#define X3_P_INST_WORD_POS_X 0
#define X3_P_SIGN_VAL_POS_X 0

#define X3_TMPLT_INST_WORD_X 0
#define X3_TMPLT_SIZE_X 4
#define X3_TMPLT_INST_WORD_POS_X 0
#define X3_TMPLT_SIGN_VAL_POS_X 0

#define X3_BTYPE_QP_INST_WORD_X 2
#define X3_BTYPE_QP_SIZE_X 9
#define X3_BTYPE_QP_INST_WORD_POS_X 23
#define X3_BTYPE_QP_INST_VAL_POS_X 0

#define X3_EMPTY_INST_WORD_X 1
#define X3_EMPTY_SIZE_X 2
#define X3_EMPTY_INST_WORD_POS_X 14
#define X3_EMPTY_INST_VAL_POS_X 0

    typedef struct _IMAGE_LINENUMBER {
      union {
	DWORD SymbolTableIndex;
	DWORD VirtualAddress;
      } Type;
      WORD Linenumber;
    } IMAGE_LINENUMBER;
    typedef IMAGE_LINENUMBER UNALIGNED *PIMAGE_LINENUMBER;

#define IMAGE_SIZEOF_LINENUMBER 6

#include "poppack.h"

    typedef struct _IMAGE_BASE_RELOCATION {
      DWORD VirtualAddress;
      DWORD SizeOfBlock;

    } IMAGE_BASE_RELOCATION;
    typedef IMAGE_BASE_RELOCATION UNALIGNED *PIMAGE_BASE_RELOCATION;

#define IMAGE_SIZEOF_BASE_RELOCATION 8

#define IMAGE_REL_BASED_ABSOLUTE 0
#define IMAGE_REL_BASED_HIGH 1
#define IMAGE_REL_BASED_LOW 2
#define IMAGE_REL_BASED_HIGHLOW 3
#define IMAGE_REL_BASED_HIGHADJ 4
#define IMAGE_REL_BASED_MIPS_JMPADDR 5
#define IMAGE_REL_BASED_MIPS_JMPADDR16 9
#define IMAGE_REL_BASED_IA64_IMM64 9
#define IMAGE_REL_BASED_DIR64 10

#define IMAGE_ARCHIVE_START_SIZE 8
#define IMAGE_ARCHIVE_START "!<arch>\n"
#define IMAGE_ARCHIVE_END "`\n"
#define IMAGE_ARCHIVE_PAD "\n"
#define IMAGE_ARCHIVE_LINKER_MEMBER "/               "
#define IMAGE_ARCHIVE_LONGNAMES_MEMBER "//              "

    typedef struct _IMAGE_ARCHIVE_MEMBER_HEADER {
      BYTE Name[16];
      BYTE Date[12];
      BYTE UserID[6];
      BYTE GroupID[6];
      BYTE Mode[8];
      BYTE Size[10];
      BYTE EndHeader[2];
    } IMAGE_ARCHIVE_MEMBER_HEADER,*PIMAGE_ARCHIVE_MEMBER_HEADER;

#define IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR 60

    typedef struct _IMAGE_EXPORT_DIRECTORY {
      DWORD Characteristics;
      DWORD TimeDateStamp;
      WORD MajorVersion;
      WORD MinorVersion;
      DWORD Name;
      DWORD Base;
      DWORD NumberOfFunctions;
      DWORD NumberOfNames;
      DWORD AddressOfFunctions;
      DWORD AddressOfNames;
      DWORD AddressOfNameOrdinals;
    } IMAGE_EXPORT_DIRECTORY,*PIMAGE_EXPORT_DIRECTORY;

    typedef struct _IMAGE_IMPORT_BY_NAME {
      WORD Hint;
      BYTE Name[1];
    } IMAGE_IMPORT_BY_NAME,*PIMAGE_IMPORT_BY_NAME;

#include "pshpack8.h"

    typedef struct _IMAGE_THUNK_DATA64 {
      union {
	ULONGLONG ForwarderString;
	ULONGLONG Function;
	ULONGLONG Ordinal;
	ULONGLONG AddressOfData;
      } u1;
    } IMAGE_THUNK_DATA64;
    typedef IMAGE_THUNK_DATA64 *PIMAGE_THUNK_DATA64;

#include "poppack.h"

    typedef struct _IMAGE_THUNK_DATA32 {
      union {
	DWORD ForwarderString;
	DWORD Function;
	DWORD Ordinal;
	DWORD AddressOfData;
      } u1;
    } IMAGE_THUNK_DATA32;
    typedef IMAGE_THUNK_DATA32 *PIMAGE_THUNK_DATA32;

#define IMAGE_ORDINAL_FLAG64 0x8000000000000000ull
#define IMAGE_ORDINAL_FLAG32 0x80000000
#define IMAGE_ORDINAL64(Ordinal) (Ordinal & 0xffffull)
#define IMAGE_ORDINAL32(Ordinal) (Ordinal & 0xffff)
#define IMAGE_SNAP_BY_ORDINAL64(Ordinal) ((Ordinal & IMAGE_ORDINAL_FLAG64)!=0)
#define IMAGE_SNAP_BY_ORDINAL32(Ordinal) ((Ordinal & IMAGE_ORDINAL_FLAG32)!=0)

    typedef VOID
      (NTAPI *PIMAGE_TLS_CALLBACK)(PVOID DllHandle,DWORD Reason,PVOID Reserved);

    typedef struct _IMAGE_TLS_DIRECTORY64 {
      ULONGLONG StartAddressOfRawData;
      ULONGLONG EndAddressOfRawData;
      ULONGLONG AddressOfIndex;
      ULONGLONG AddressOfCallBacks;
      DWORD SizeOfZeroFill;
      DWORD Characteristics;
    } IMAGE_TLS_DIRECTORY64;
    typedef IMAGE_TLS_DIRECTORY64 *PIMAGE_TLS_DIRECTORY64;

    typedef struct _IMAGE_TLS_DIRECTORY32 {
      DWORD StartAddressOfRawData;
      DWORD EndAddressOfRawData;
      DWORD AddressOfIndex;
      DWORD AddressOfCallBacks;
      DWORD SizeOfZeroFill;
      DWORD Characteristics;
    } IMAGE_TLS_DIRECTORY32;
    typedef IMAGE_TLS_DIRECTORY32 *PIMAGE_TLS_DIRECTORY32;

#ifdef _WIN64
#define IMAGE_ORDINAL_FLAG IMAGE_ORDINAL_FLAG64
#define IMAGE_ORDINAL(Ordinal) IMAGE_ORDINAL64(Ordinal)
    typedef IMAGE_THUNK_DATA64 IMAGE_THUNK_DATA;
    typedef PIMAGE_THUNK_DATA64 PIMAGE_THUNK_DATA;
#define IMAGE_SNAP_BY_ORDINAL(Ordinal) IMAGE_SNAP_BY_ORDINAL64(Ordinal)
    typedef IMAGE_TLS_DIRECTORY64 IMAGE_TLS_DIRECTORY;
    typedef PIMAGE_TLS_DIRECTORY64 PIMAGE_TLS_DIRECTORY;
#else
#define IMAGE_ORDINAL_FLAG IMAGE_ORDINAL_FLAG32
#define IMAGE_ORDINAL(Ordinal) IMAGE_ORDINAL32(Ordinal)
    typedef IMAGE_THUNK_DATA32 IMAGE_THUNK_DATA;
    typedef PIMAGE_THUNK_DATA32 PIMAGE_THUNK_DATA;
#define IMAGE_SNAP_BY_ORDINAL(Ordinal) IMAGE_SNAP_BY_ORDINAL32(Ordinal)
    typedef IMAGE_TLS_DIRECTORY32 IMAGE_TLS_DIRECTORY;
    typedef PIMAGE_TLS_DIRECTORY32 PIMAGE_TLS_DIRECTORY;
#endif

    typedef struct _IMAGE_IMPORT_DESCRIPTOR {
      union {
	DWORD Characteristics;
	DWORD OriginalFirstThunk;
      };
      DWORD TimeDateStamp;

      DWORD ForwarderChain;
      DWORD Name;
      DWORD FirstThunk;
    } IMAGE_IMPORT_DESCRIPTOR;
    typedef IMAGE_IMPORT_DESCRIPTOR UNALIGNED *PIMAGE_IMPORT_DESCRIPTOR;

    typedef struct _IMAGE_BOUND_IMPORT_DESCRIPTOR {
      DWORD TimeDateStamp;
      WORD OffsetModuleName;
      WORD NumberOfModuleForwarderRefs;
    } IMAGE_BOUND_IMPORT_DESCRIPTOR,*PIMAGE_BOUND_IMPORT_DESCRIPTOR;

    typedef struct _IMAGE_BOUND_FORWARDER_REF {
      DWORD TimeDateStamp;
      WORD OffsetModuleName;
      WORD Reserved;
    } IMAGE_BOUND_FORWARDER_REF,*PIMAGE_BOUND_FORWARDER_REF;

    typedef struct _IMAGE_RESOURCE_DIRECTORY {
      DWORD Characteristics;
      DWORD TimeDateStamp;
      WORD MajorVersion;
      WORD MinorVersion;
      WORD NumberOfNamedEntries;
      WORD NumberOfIdEntries;
    } IMAGE_RESOURCE_DIRECTORY,*PIMAGE_RESOURCE_DIRECTORY;

#define IMAGE_RESOURCE_NAME_IS_STRING 0x80000000
#define IMAGE_RESOURCE_DATA_IS_DIRECTORY 0x80000000

    typedef struct _IMAGE_RESOURCE_DIRECTORY_ENTRY {
      union {
	struct {
	  DWORD NameOffset:31;
	  DWORD NameIsString:1;
	};
	DWORD Name;
	WORD Id;
      };
      union {
	DWORD OffsetToData;
	struct {
	  DWORD OffsetToDirectory:31;
	  DWORD DataIsDirectory:1;
	};
      };
    } IMAGE_RESOURCE_DIRECTORY_ENTRY,*PIMAGE_RESOURCE_DIRECTORY_ENTRY;

    typedef struct _IMAGE_RESOURCE_DIRECTORY_STRING {
      WORD Length;
      CHAR NameString[1];
    } IMAGE_RESOURCE_DIRECTORY_STRING,*PIMAGE_RESOURCE_DIRECTORY_STRING;

    typedef struct _IMAGE_RESOURCE_DIR_STRING_U {
      WORD Length;
      WCHAR NameString[1];
    } IMAGE_RESOURCE_DIR_STRING_U,*PIMAGE_RESOURCE_DIR_STRING_U;

    typedef struct _IMAGE_RESOURCE_DATA_ENTRY {
      DWORD OffsetToData;
      DWORD Size;
      DWORD CodePage;
      DWORD Reserved;
    } IMAGE_RESOURCE_DATA_ENTRY,*PIMAGE_RESOURCE_DATA_ENTRY;

    typedef struct {
      DWORD Size;
      DWORD TimeDateStamp;
      WORD MajorVersion;
      WORD MinorVersion;
      DWORD GlobalFlagsClear;
      DWORD GlobalFlagsSet;
      DWORD CriticalSectionDefaultTimeout;
      DWORD DeCommitFreeBlockThreshold;
      DWORD DeCommitTotalFreeThreshold;
      DWORD LockPrefixTable;
      DWORD MaximumAllocationSize;
      DWORD VirtualMemoryThreshold;
      DWORD ProcessHeapFlags;
      DWORD ProcessAffinityMask;
      WORD CSDVersion;
      WORD Reserved1;
      DWORD EditList;
      DWORD SecurityCookie;
      DWORD SEHandlerTable;
      DWORD SEHandlerCount;
    } IMAGE_LOAD_CONFIG_DIRECTORY32,*PIMAGE_LOAD_CONFIG_DIRECTORY32;

    typedef struct {
      DWORD Size;
      DWORD TimeDateStamp;
      WORD MajorVersion;
      WORD MinorVersion;
      DWORD GlobalFlagsClear;
      DWORD GlobalFlagsSet;
      DWORD CriticalSectionDefaultTimeout;
      ULONGLONG DeCommitFreeBlockThreshold;
      ULONGLONG DeCommitTotalFreeThreshold;
      ULONGLONG LockPrefixTable;
      ULONGLONG MaximumAllocationSize;
      ULONGLONG VirtualMemoryThreshold;
      ULONGLONG ProcessAffinityMask;
      DWORD ProcessHeapFlags;
      WORD CSDVersion;
      WORD Reserved1;
      ULONGLONG EditList;
      ULONGLONG SecurityCookie;
      ULONGLONG SEHandlerTable;
      ULONGLONG SEHandlerCount;
    } IMAGE_LOAD_CONFIG_DIRECTORY64,*PIMAGE_LOAD_CONFIG_DIRECTORY64;

#ifdef _WIN64
    typedef IMAGE_LOAD_CONFIG_DIRECTORY64 IMAGE_LOAD_CONFIG_DIRECTORY;
    typedef PIMAGE_LOAD_CONFIG_DIRECTORY64 PIMAGE_LOAD_CONFIG_DIRECTORY;
#else
    typedef IMAGE_LOAD_CONFIG_DIRECTORY32 IMAGE_LOAD_CONFIG_DIRECTORY;
    typedef PIMAGE_LOAD_CONFIG_DIRECTORY32 PIMAGE_LOAD_CONFIG_DIRECTORY;
#endif

    typedef struct _IMAGE_CE_RUNTIME_FUNCTION_ENTRY {
      DWORD FuncStart;
      DWORD PrologLen : 8;
      DWORD FuncLen : 22;
      DWORD ThirtyTwoBit : 1;
      DWORD ExceptionFlag : 1;
    } IMAGE_CE_RUNTIME_FUNCTION_ENTRY,*PIMAGE_CE_RUNTIME_FUNCTION_ENTRY;

    typedef struct _IMAGE_ALPHA64_RUNTIME_FUNCTION_ENTRY {
      ULONGLONG BeginAddress;
      ULONGLONG EndAddress;
      ULONGLONG ExceptionHandler;
      ULONGLONG HandlerData;
      ULONGLONG PrologEndAddress;
    } IMAGE_ALPHA64_RUNTIME_FUNCTION_ENTRY,*PIMAGE_ALPHA64_RUNTIME_FUNCTION_ENTRY;

    typedef struct _IMAGE_ALPHA_RUNTIME_FUNCTION_ENTRY {
      DWORD BeginAddress;
      DWORD EndAddress;
      DWORD ExceptionHandler;
      DWORD HandlerData;
      DWORD PrologEndAddress;
    } IMAGE_ALPHA_RUNTIME_FUNCTION_ENTRY,*PIMAGE_ALPHA_RUNTIME_FUNCTION_ENTRY;

    typedef struct _IMAGE_RUNTIME_FUNCTION_ENTRY {
      DWORD BeginAddress;
      DWORD EndAddress;
      DWORD UnwindInfoAddress;
    } _IMAGE_RUNTIME_FUNCTION_ENTRY,*_PIMAGE_RUNTIME_FUNCTION_ENTRY;

    typedef _IMAGE_RUNTIME_FUNCTION_ENTRY IMAGE_IA64_RUNTIME_FUNCTION_ENTRY;
    typedef _PIMAGE_RUNTIME_FUNCTION_ENTRY PIMAGE_IA64_RUNTIME_FUNCTION_ENTRY;

    typedef _IMAGE_RUNTIME_FUNCTION_ENTRY IMAGE_RUNTIME_FUNCTION_ENTRY;
    typedef _PIMAGE_RUNTIME_FUNCTION_ENTRY PIMAGE_RUNTIME_FUNCTION_ENTRY;

    typedef struct _IMAGE_DEBUG_DIRECTORY {
      DWORD Characteristics;
      DWORD TimeDateStamp;
      WORD MajorVersion;
      WORD MinorVersion;
      DWORD Type;
      DWORD SizeOfData;
      DWORD AddressOfRawData;
      DWORD PointerToRawData;
    } IMAGE_DEBUG_DIRECTORY,*PIMAGE_DEBUG_DIRECTORY;

#define IMAGE_DEBUG_TYPE_UNKNOWN 0
#define IMAGE_DEBUG_TYPE_COFF 1
#define IMAGE_DEBUG_TYPE_CODEVIEW 2
#define IMAGE_DEBUG_TYPE_FPO 3
#define IMAGE_DEBUG_TYPE_MISC 4
#define IMAGE_DEBUG_TYPE_EXCEPTION 5
#define IMAGE_DEBUG_TYPE_FIXUP 6
#define IMAGE_DEBUG_TYPE_OMAP_TO_SRC 7
#define IMAGE_DEBUG_TYPE_OMAP_FROM_SRC 8
#define IMAGE_DEBUG_TYPE_BORLAND 9
#define IMAGE_DEBUG_TYPE_RESERVED10 10
#define IMAGE_DEBUG_TYPE_CLSID 11

    typedef struct _IMAGE_COFF_SYMBOLS_HEADER {
      DWORD NumberOfSymbols;
      DWORD LvaToFirstSymbol;
      DWORD NumberOfLinenumbers;
      DWORD LvaToFirstLinenumber;
      DWORD RvaToFirstByteOfCode;
      DWORD RvaToLastByteOfCode;
      DWORD RvaToFirstByteOfData;
      DWORD RvaToLastByteOfData;
    } IMAGE_COFF_SYMBOLS_HEADER,*PIMAGE_COFF_SYMBOLS_HEADER;

#define FRAME_FPO 0
#define FRAME_TRAP 1
#define FRAME_TSS 2
#define FRAME_NONFPO 3

    typedef struct _FPO_DATA {
      DWORD ulOffStart;
      DWORD cbProcSize;
      DWORD cdwLocals;
      WORD cdwParams;
      WORD cbProlog : 8;
      WORD cbRegs : 3;
      WORD fHasSEH : 1;
      WORD fUseBP : 1;
      WORD reserved : 1;
      WORD cbFrame : 2;
    } FPO_DATA,*PFPO_DATA;
#define SIZEOF_RFPO_DATA 16

#define IMAGE_DEBUG_MISC_EXENAME 1

    typedef struct _IMAGE_DEBUG_MISC {
      DWORD DataType;
      DWORD Length;
      BOOLEAN Unicode;
      BYTE Reserved[3];
      BYTE Data[1];
    } IMAGE_DEBUG_MISC,*PIMAGE_DEBUG_MISC;

    typedef struct _IMAGE_FUNCTION_ENTRY {
      DWORD StartingAddress;
      DWORD EndingAddress;
      DWORD EndOfPrologue;
    } IMAGE_FUNCTION_ENTRY,*PIMAGE_FUNCTION_ENTRY;

    typedef struct _IMAGE_FUNCTION_ENTRY64 {
      ULONGLONG StartingAddress;
      ULONGLONG EndingAddress;
      union {
	ULONGLONG EndOfPrologue;
	ULONGLONG UnwindInfoAddress;
      };
    } IMAGE_FUNCTION_ENTRY64,*PIMAGE_FUNCTION_ENTRY64;

    typedef struct _IMAGE_SEPARATE_DEBUG_HEADER {
      WORD Signature;
      WORD Flags;
      WORD Machine;
      WORD Characteristics;
      DWORD TimeDateStamp;
      DWORD CheckSum;
      DWORD ImageBase;
      DWORD SizeOfImage;
      DWORD NumberOfSections;
      DWORD ExportedNamesSize;
      DWORD DebugDirectorySize;
      DWORD SectionAlignment;
      DWORD Reserved[2];
    } IMAGE_SEPARATE_DEBUG_HEADER,*PIMAGE_SEPARATE_DEBUG_HEADER;

    typedef struct _NON_PAGED_DEBUG_INFO {
      WORD Signature;
      WORD Flags;
      DWORD Size;
      WORD Machine;
      WORD Characteristics;
      DWORD TimeDateStamp;
      DWORD CheckSum;
      DWORD SizeOfImage;
      ULONGLONG ImageBase;

    } NON_PAGED_DEBUG_INFO,*PNON_PAGED_DEBUG_INFO;

#define IMAGE_SEPARATE_DEBUG_SIGNATURE 0x4944
#define NON_PAGED_DEBUG_SIGNATURE 0x494E

#define IMAGE_SEPARATE_DEBUG_FLAGS_MASK 0x8000
#define IMAGE_SEPARATE_DEBUG_MISMATCH 0x8000

    typedef struct _ImageArchitectureHeader {
      unsigned int AmaskValue: 1;
      int Adummy1 :7;
      unsigned int AmaskShift: 8;
      int Adummy2 :16;
      DWORD FirstEntryRVA;
    } IMAGE_ARCHITECTURE_HEADER,*PIMAGE_ARCHITECTURE_HEADER;

    typedef struct _ImageArchitectureEntry {
      DWORD FixupInstRVA;
      DWORD NewInst;
    } IMAGE_ARCHITECTURE_ENTRY,*PIMAGE_ARCHITECTURE_ENTRY;

#include "poppack.h"

#define IMPORT_OBJECT_HDR_SIG2 0xffff

    typedef struct IMPORT_OBJECT_HEADER {
      WORD Sig1;
      WORD Sig2;
      WORD Version;
      WORD Machine;
      DWORD TimeDateStamp;
      DWORD SizeOfData;
      union {
	WORD Ordinal;
	WORD Hint;
      };
      WORD Type : 2;
      WORD NameType : 3;
      WORD Reserved : 11;
    } IMPORT_OBJECT_HEADER;

    typedef enum IMPORT_OBJECT_TYPE {
      IMPORT_OBJECT_CODE = 0,IMPORT_OBJECT_DATA = 1,IMPORT_OBJECT_CONST = 2
    } IMPORT_OBJECT_TYPE;

    typedef enum IMPORT_OBJECT_NAME_TYPE {
      IMPORT_OBJECT_ORDINAL = 0,IMPORT_OBJECT_NAME = 1,IMPORT_OBJECT_NAME_NO_PREFIX = 2,IMPORT_OBJECT_NAME_UNDECORATE = 3
    } IMPORT_OBJECT_NAME_TYPE;

#ifndef __IMAGE_COR20_HEADER_DEFINED__
#define __IMAGE_COR20_HEADER_DEFINED__
    typedef enum ReplacesCorHdrNumericDefines {
      COMIMAGE_FLAGS_ILONLY =0x00000001,COMIMAGE_FLAGS_32BITREQUIRED =0x00000002,COMIMAGE_FLAGS_IL_LIBRARY =0x00000004,
      COMIMAGE_FLAGS_STRONGNAMESIGNED =0x00000008,COMIMAGE_FLAGS_TRACKDEBUGDATA =0x00010000,COR_VERSION_MAJOR_V2 =2,
      COR_VERSION_MAJOR =COR_VERSION_MAJOR_V2,COR_VERSION_MINOR =0,COR_DELETED_NAME_LENGTH =8,COR_VTABLEGAP_NAME_LENGTH =8,
      NATIVE_TYPE_MAX_CB =1,COR_ILMETHOD_SECT_SMALL_MAX_DATASIZE=0xFF,IMAGE_COR_MIH_METHODRVA =0x01,IMAGE_COR_MIH_EHRVA =0x02,
      IMAGE_COR_MIH_BASICBLOCK =0x08,COR_VTABLE_32BIT =0x01,COR_VTABLE_64BIT =0x02,COR_VTABLE_FROM_UNMANAGED =0x04,
      COR_VTABLE_CALL_MOST_DERIVED =0x10,IMAGE_COR_EATJ_THUNK_SIZE =32,MAX_CLASS_NAME =1024,MAX_PACKAGE_NAME =1024
    } ReplacesCorHdrNumericDefines;

    typedef struct IMAGE_COR20_HEADER {
      DWORD cb;
      WORD MajorRuntimeVersion;
      WORD MinorRuntimeVersion;
      IMAGE_DATA_DIRECTORY MetaData;
      DWORD Flags;
      DWORD EntryPointToken;
      IMAGE_DATA_DIRECTORY Resources;
      IMAGE_DATA_DIRECTORY StrongNameSignature;
      IMAGE_DATA_DIRECTORY CodeManagerTable;
      IMAGE_DATA_DIRECTORY VTableFixups;
      IMAGE_DATA_DIRECTORY ExportAddressTableJumps;
      IMAGE_DATA_DIRECTORY ManagedNativeHeader;
    } IMAGE_COR20_HEADER,*PIMAGE_COR20_HEADER;
#endif

#if defined (__x86_64)
    NTSYSAPI PRUNTIME_FUNCTION NTAPI RtlLookupFunctionEntry (DWORD64 ControlPc, PDWORD64 ImageBase, PUNWIND_HISTORY_TABLE HistoryTable);
    NTSYSAPI VOID NTAPI RtlUnwindEx (PVOID TargetFrame, PVOID TargetIp, PEXCEPTION_RECORD ExceptionRecord, PVOID ReturnValue, PCONTEXT ContextRecord, PUNWIND_HISTORY_TABLE HistoryTable);
#endif

#include <string.h>

#ifndef _SLIST_HEADER_
#define _SLIST_HEADER_

#ifdef _WIN64
    typedef struct _SLIST_ENTRY *PSLIST_ENTRY;
    typedef struct DECLSPEC_ALIGN(16) _SLIST_ENTRY {
      PSLIST_ENTRY Next;
    } SLIST_ENTRY;
#else

#define SLIST_ENTRY SINGLE_LIST_ENTRY
#define _SLIST_ENTRY _SINGLE_LIST_ENTRY
#define PSLIST_ENTRY PSINGLE_LIST_ENTRY
#endif

#if defined(_WIN64)

    typedef struct DECLSPEC_ALIGN(16) _SLIST_HEADER {
      ULONGLONG Alignment;
      ULONGLONG Region;
    } SLIST_HEADER;

    typedef struct _SLIST_HEADER *PSLIST_HEADER;
#else

    typedef union _SLIST_HEADER {
      ULONGLONG Alignment;
      struct {
	SLIST_ENTRY Next;
	WORD Depth;
	WORD Sequence;
      };
    } SLIST_HEADER,*PSLIST_HEADER;
#endif
#endif

    NTSYSAPI VOID NTAPI RtlInitializeSListHead(PSLIST_HEADER ListHead);
    NTSYSAPI PSLIST_ENTRY NTAPI RtlFirstEntrySList(const SLIST_HEADER *ListHead);
    NTSYSAPI PSLIST_ENTRY NTAPI RtlInterlockedPopEntrySList(PSLIST_HEADER ListHead);
    NTSYSAPI PSLIST_ENTRY NTAPI RtlInterlockedPushEntrySList(PSLIST_HEADER ListHead,PSLIST_ENTRY ListEntry);
    NTSYSAPI PSLIST_ENTRY NTAPI RtlInterlockedFlushSList(PSLIST_HEADER ListHead);
    NTSYSAPI WORD NTAPI RtlQueryDepthSList(PSLIST_HEADER ListHead);

#define HEAP_NO_SERIALIZE 0x00000001
#define HEAP_GROWABLE 0x00000002
#define HEAP_GENERATE_EXCEPTIONS 0x00000004
#define HEAP_ZERO_MEMORY 0x00000008
#define HEAP_REALLOC_IN_PLACE_ONLY 0x00000010
#define HEAP_TAIL_CHECKING_ENABLED 0x00000020
#define HEAP_FREE_CHECKING_ENABLED 0x00000040
#define HEAP_DISABLE_COALESCE_ON_FREE 0x00000080
#define HEAP_CREATE_ALIGN_16 0x00010000
#define HEAP_CREATE_ENABLE_TRACING 0x00020000
#define HEAP_CREATE_ENABLE_EXECUTE 0x00040000
#define HEAP_MAXIMUM_TAG 0x0FFF
#define HEAP_PSEUDO_TAG_FLAG 0x8000
#define HEAP_TAG_SHIFT 18
#define HEAP_MAKE_TAG_FLAGS(b,o) ((DWORD)((b) + ((o) << 18)))

    NTSYSAPI VOID NTAPI RtlCaptureContext(PCONTEXT ContextRecord);

#define IS_TEXT_UNICODE_ASCII16 0x0001
#define IS_TEXT_UNICODE_REVERSE_ASCII16 0x0010

#define IS_TEXT_UNICODE_STATISTICS 0x0002
#define IS_TEXT_UNICODE_REVERSE_STATISTICS 0x0020

#define IS_TEXT_UNICODE_CONTROLS 0x0004
#define IS_TEXT_UNICODE_REVERSE_CONTROLS 0x0040

#define IS_TEXT_UNICODE_SIGNATURE 0x0008
#define IS_TEXT_UNICODE_REVERSE_SIGNATURE 0x0080

#define IS_TEXT_UNICODE_ILLEGAL_CHARS 0x0100
#define IS_TEXT_UNICODE_ODD_LENGTH 0x0200
#define IS_TEXT_UNICODE_DBCS_LEADBYTE 0x0400
#define IS_TEXT_UNICODE_NULL_BYTES 0x1000

#define IS_TEXT_UNICODE_UNICODE_MASK 0x000F
#define IS_TEXT_UNICODE_REVERSE_MASK 0x00F0
#define IS_TEXT_UNICODE_NOT_UNICODE_MASK 0x0F00
#define IS_TEXT_UNICODE_NOT_ASCII_MASK 0xF000

#define COMPRESSION_FORMAT_NONE (0x0000)
#define COMPRESSION_FORMAT_DEFAULT (0x0001)
#define COMPRESSION_FORMAT_LZNT1 (0x0002)
#define COMPRESSION_ENGINE_STANDARD (0x0000)
#define COMPRESSION_ENGINE_MAXIMUM (0x0100)
#define COMPRESSION_ENGINE_HIBER (0x0200)

#if _DBG_MEMCPY_INLINE_ && !defined(_MEMCPY_INLINE_) && !defined(_CRTBLD)
#define _MEMCPY_INLINE_
    __CRT_INLINE PVOID __cdecl memcpy_inline(void *dst,const void *src,size_t size) {
      if(((char *)dst > (char *)src) && ((char *)dst < ((char *)src + size))) {
	__debugbreak();
      }
      return memcpy(dst,src,size);
    }
#define memcpy memcpy_inline
#endif

    NTSYSAPI SIZE_T NTAPI RtlCompareMemory(const VOID *Source1,const VOID *Source2,SIZE_T Length);

#define RtlEqualMemory(Destination,Source,Length) (!memcmp((Destination),(Source),(Length)))
#define RtlMoveMemory(Destination,Source,Length) memmove((Destination),(Source),(Length))
#define RtlCopyMemory(Destination,Source,Length) memcpy((Destination),(Source),(Length))
#define RtlFillMemory(Destination,Length,Fill) memset((Destination),(Fill),(Length))
#define RtlZeroMemory(Destination,Length) memset((Destination),0,(Length))

    __CRT_INLINE PVOID RtlSecureZeroMemory(PVOID ptr,SIZE_T cnt) {
      volatile char *vptr =(volatile char *)ptr;
#ifdef __x86_64
      __stosb((PBYTE)((DWORD64)vptr),0,cnt);
#else
      while(cnt) {
	*vptr = 0;
	vptr++;
	cnt--;
      }
#endif
      return ptr;
    }

    typedef struct _MESSAGE_RESOURCE_ENTRY {
      WORD Length;
      WORD Flags;
      BYTE Text[1];
    } MESSAGE_RESOURCE_ENTRY,*PMESSAGE_RESOURCE_ENTRY;

#define MESSAGE_RESOURCE_UNICODE 0x0001

    typedef struct _MESSAGE_RESOURCE_BLOCK {
      DWORD LowId;
      DWORD HighId;
      DWORD OffsetToEntries;
    } MESSAGE_RESOURCE_BLOCK,*PMESSAGE_RESOURCE_BLOCK;

    typedef struct _MESSAGE_RESOURCE_DATA {
      DWORD NumberOfBlocks;
      MESSAGE_RESOURCE_BLOCK Blocks[1];
    } MESSAGE_RESOURCE_DATA,*PMESSAGE_RESOURCE_DATA;

    typedef struct _OSVERSIONINFOA {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      CHAR szCSDVersion[128];
    } OSVERSIONINFOA,*POSVERSIONINFOA,*LPOSVERSIONINFOA;

    typedef struct _OSVERSIONINFOW {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      WCHAR szCSDVersion[128];
    } OSVERSIONINFOW,*POSVERSIONINFOW,*LPOSVERSIONINFOW,RTL_OSVERSIONINFOW,*PRTL_OSVERSIONINFOW;

#ifdef UNICODE
    typedef OSVERSIONINFOW OSVERSIONINFO;
    typedef POSVERSIONINFOW POSVERSIONINFO;
    typedef LPOSVERSIONINFOW LPOSVERSIONINFO;
#else
    typedef OSVERSIONINFOA OSVERSIONINFO;
    typedef POSVERSIONINFOA POSVERSIONINFO;
    typedef LPOSVERSIONINFOA LPOSVERSIONINFO;
#endif

    typedef struct _OSVERSIONINFOEXA {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      CHAR szCSDVersion[128];
      WORD wServicePackMajor;
      WORD wServicePackMinor;
      WORD wSuiteMask;
      BYTE wProductType;
      BYTE wReserved;
    } OSVERSIONINFOEXA,*POSVERSIONINFOEXA,*LPOSVERSIONINFOEXA;

    typedef struct _OSVERSIONINFOEXW {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      WCHAR szCSDVersion[128];
      WORD wServicePackMajor;
      WORD wServicePackMinor;
      WORD wSuiteMask;
      BYTE wProductType;
      BYTE wReserved;
    } OSVERSIONINFOEXW,*POSVERSIONINFOEXW,*LPOSVERSIONINFOEXW,RTL_OSVERSIONINFOEXW,*PRTL_OSVERSIONINFOEXW;
#ifdef UNICODE
    typedef OSVERSIONINFOEXW OSVERSIONINFOEX;
    typedef POSVERSIONINFOEXW POSVERSIONINFOEX;
    typedef LPOSVERSIONINFOEXW LPOSVERSIONINFOEX;
#else
    typedef OSVERSIONINFOEXA OSVERSIONINFOEX;
    typedef POSVERSIONINFOEXA POSVERSIONINFOEX;
    typedef LPOSVERSIONINFOEXA LPOSVERSIONINFOEX;
#endif

#define VER_EQUAL 1
#define VER_GREATER 2
#define VER_GREATER_EQUAL 3
#define VER_LESS 4
#define VER_LESS_EQUAL 5
#define VER_AND 6
#define VER_OR 7

#define VER_CONDITION_MASK 7
#define VER_NUM_BITS_PER_CONDITION_MASK 3

#define VER_MINORVERSION 0x0000001
#define VER_MAJORVERSION 0x0000002
#define VER_BUILDNUMBER 0x0000004
#define VER_PLATFORMID 0x0000008
#define VER_SERVICEPACKMINOR 0x0000010
#define VER_SERVICEPACKMAJOR 0x0000020
#define VER_SUITENAME 0x0000040
#define VER_PRODUCT_TYPE 0x0000080

#define VER_NT_WORKSTATION 0x0000001
#define VER_NT_DOMAIN_CONTROLLER 0x0000002
#define VER_NT_SERVER 0x0000003

#define VER_PLATFORM_WIN32s 0
#define VER_PLATFORM_WIN32_WINDOWS 1
#define VER_PLATFORM_WIN32_NT 2

#define VER_SET_CONDITION(_m_,_t_,_c_) ((_m_)=VerSetConditionMask((_m_),(_t_),(_c_)))

    NTSYSAPI ULONGLONG NTAPI VerSetConditionMask(ULONGLONG ConditionMask,DWORD TypeMask,BYTE Condition);

    typedef struct _RTL_CRITICAL_SECTION_DEBUG {
      WORD Type;
      WORD CreatorBackTraceIndex;
      struct _RTL_CRITICAL_SECTION *CriticalSection;
      LIST_ENTRY ProcessLocksList;
      DWORD EntryCount;
      DWORD ContentionCount;
      DWORD Spare[2];
    } RTL_CRITICAL_SECTION_DEBUG,*PRTL_CRITICAL_SECTION_DEBUG,RTL_RESOURCE_DEBUG,*PRTL_RESOURCE_DEBUG;

#define RTL_CRITSECT_TYPE 0
#define RTL_RESOURCE_TYPE 1

    typedef struct _RTL_CRITICAL_SECTION {
      PRTL_CRITICAL_SECTION_DEBUG DebugInfo;
      LONG LockCount;
      LONG RecursionCount;
      HANDLE OwningThread;
      HANDLE LockSemaphore;
      ULONG_PTR SpinCount;
    } RTL_CRITICAL_SECTION,*PRTL_CRITICAL_SECTION;

    typedef VOID (NTAPI *RTL_VERIFIER_DLL_LOAD_CALLBACK) (PWSTR DllName,PVOID DllBase,SIZE_T DllSize,PVOID Reserved);
    typedef VOID (NTAPI *RTL_VERIFIER_DLL_UNLOAD_CALLBACK) (PWSTR DllName,PVOID DllBase,SIZE_T DllSize,PVOID Reserved);
    typedef VOID (NTAPI *RTL_VERIFIER_NTDLLHEAPFREE_CALLBACK)(PVOID AllocationBase,SIZE_T AllocationSize);

    typedef struct _RTL_VERIFIER_THUNK_DESCRIPTOR {
      PCHAR ThunkName;
      PVOID ThunkOldAddress;
      PVOID ThunkNewAddress;
    } RTL_VERIFIER_THUNK_DESCRIPTOR,*PRTL_VERIFIER_THUNK_DESCRIPTOR;

    typedef struct _RTL_VERIFIER_DLL_DESCRIPTOR {
      PWCHAR DllName;
      DWORD DllFlags;
      PVOID DllAddress;
      PRTL_VERIFIER_THUNK_DESCRIPTOR DllThunks;
    } RTL_VERIFIER_DLL_DESCRIPTOR,*PRTL_VERIFIER_DLL_DESCRIPTOR;

    typedef struct _RTL_VERIFIER_PROVIDER_DESCRIPTOR {
      DWORD Length;
      PRTL_VERIFIER_DLL_DESCRIPTOR ProviderDlls;
      RTL_VERIFIER_DLL_LOAD_CALLBACK ProviderDllLoadCallback;
      RTL_VERIFIER_DLL_UNLOAD_CALLBACK ProviderDllUnloadCallback;
      PWSTR VerifierImage;
      DWORD VerifierFlags;
      DWORD VerifierDebug;
      PVOID RtlpGetStackTraceAddress;
      PVOID RtlpDebugPageHeapCreate;
      PVOID RtlpDebugPageHeapDestroy;
      RTL_VERIFIER_NTDLLHEAPFREE_CALLBACK ProviderNtdllHeapFreeCallback;
    } RTL_VERIFIER_PROVIDER_DESCRIPTOR,*PRTL_VERIFIER_PROVIDER_DESCRIPTOR;

#define RTL_VRF_FLG_FULL_PAGE_HEAP 0x00000001
#define RTL_VRF_FLG_RESERVED_DONOTUSE 0x00000002
#define RTL_VRF_FLG_HANDLE_CHECKS 0x00000004
#define RTL_VRF_FLG_STACK_CHECKS 0x00000008
#define RTL_VRF_FLG_APPCOMPAT_CHECKS 0x00000010
#define RTL_VRF_FLG_TLS_CHECKS 0x00000020
#define RTL_VRF_FLG_DIRTY_STACKS 0x00000040
#define RTL_VRF_FLG_RPC_CHECKS 0x00000080
#define RTL_VRF_FLG_COM_CHECKS 0x00000100
#define RTL_VRF_FLG_DANGEROUS_APIS 0x00000200
#define RTL_VRF_FLG_RACE_CHECKS 0x00000400
#define RTL_VRF_FLG_DEADLOCK_CHECKS 0x00000800
#define RTL_VRF_FLG_FIRST_CHANCE_EXCEPTION_CHECKS 0x00001000
#define RTL_VRF_FLG_VIRTUAL_MEM_CHECKS 0x00002000
#define RTL_VRF_FLG_ENABLE_LOGGING 0x00004000
#define RTL_VRF_FLG_FAST_FILL_HEAP 0x00008000
#define RTL_VRF_FLG_VIRTUAL_SPACE_TRACKING 0x00010000
#define RTL_VRF_FLG_ENABLED_SYSTEM_WIDE 0x00020000
#define RTL_VRF_FLG_MISCELLANEOUS_CHECKS 0x00020000
#define RTL_VRF_FLG_LOCK_CHECKS 0x00040000

#define APPLICATION_VERIFIER_INTERNAL_ERROR 0x80000000
#define APPLICATION_VERIFIER_INTERNAL_WARNING 0x40000000
#define APPLICATION_VERIFIER_NO_BREAK 0x20000000
#define APPLICATION_VERIFIER_CONTINUABLE_BREAK 0x10000000

#define APPLICATION_VERIFIER_UNKNOWN_ERROR 0x0001
#define APPLICATION_VERIFIER_ACCESS_VIOLATION 0x0002
#define APPLICATION_VERIFIER_UNSYNCHRONIZED_ACCESS 0x0003
#define APPLICATION_VERIFIER_EXTREME_SIZE_REQUEST 0x0004
#define APPLICATION_VERIFIER_BAD_HEAP_HANDLE 0x0005
#define APPLICATION_VERIFIER_SWITCHED_HEAP_HANDLE 0x0006
#define APPLICATION_VERIFIER_DOUBLE_FREE 0x0007
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK 0x0008
#define APPLICATION_VERIFIER_DESTROY_PROCESS_HEAP 0x0009
#define APPLICATION_VERIFIER_UNEXPECTED_EXCEPTION 0x000A
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_EXCEPTION_RAISED_FOR_HEADER 0x000B
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_EXCEPTION_RAISED_FOR_PROBING 0x000C
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_HEADER 0x000D
#define APPLICATION_VERIFIER_CORRUPTED_FREED_HEAP_BLOCK 0x000E
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_SUFFIX 0x000F
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_START_STAMP 0x0010
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_END_STAMP 0x0011
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_BLOCK_PREFIX 0x0012
#define APPLICATION_VERIFIER_FIRST_CHANCE_ACCESS_VIOLATION 0x0013
#define APPLICATION_VERIFIER_CORRUPTED_HEAP_LIST 0x0014

#define APPLICATION_VERIFIER_TERMINATE_THREAD_CALL 0x0100
#define APPLICATION_VERIFIER_STACK_OVERFLOW 0x0101
#define APPLICATION_VERIFIER_INVALID_EXIT_PROCESS_CALL 0x0102

#define APPLICATION_VERIFIER_EXIT_THREAD_OWNS_LOCK 0x0200
#define APPLICATION_VERIFIER_LOCK_IN_UNLOADED_DLL 0x0201
#define APPLICATION_VERIFIER_LOCK_IN_FREED_HEAP 0x0202
#define APPLICATION_VERIFIER_LOCK_DOUBLE_INITIALIZE 0x0203
#define APPLICATION_VERIFIER_LOCK_IN_FREED_MEMORY 0x0204
#define APPLICATION_VERIFIER_LOCK_CORRUPTED 0x0205
#define APPLICATION_VERIFIER_LOCK_INVALID_OWNER 0x0206
#define APPLICATION_VERIFIER_LOCK_INVALID_RECURSION_COUNT 0x0207
#define APPLICATION_VERIFIER_LOCK_INVALID_LOCK_COUNT 0x0208
#define APPLICATION_VERIFIER_LOCK_OVER_RELEASED 0x0209
#define APPLICATION_VERIFIER_LOCK_NOT_INITIALIZED 0x0210
#define APPLICATION_VERIFIER_LOCK_ALREADY_INITIALIZED 0x0211
#define APPLICATION_VERIFIER_LOCK_IN_FREED_VMEM 0x0212
#define APPLICATION_VERIFIER_LOCK_IN_UNMAPPED_MEM 0x0213
#define APPLICATION_VERIFIER_THREAD_NOT_LOCK_OWNER 0x0214

#define APPLICATION_VERIFIER_INVALID_HANDLE 0x0300
#define APPLICATION_VERIFIER_INVALID_TLS_VALUE 0x0301
#define APPLICATION_VERIFIER_INCORRECT_WAIT_CALL 0x0302
#define APPLICATION_VERIFIER_NULL_HANDLE 0x0303
#define APPLICATION_VERIFIER_WAIT_IN_DLLMAIN 0x0304

#define APPLICATION_VERIFIER_COM_ERROR 0x0400
#define APPLICATION_VERIFIER_COM_API_IN_DLLMAIN 0x0401
#define APPLICATION_VERIFIER_COM_UNHANDLED_EXCEPTION 0x0402
#define APPLICATION_VERIFIER_COM_UNBALANCED_COINIT 0x0403
#define APPLICATION_VERIFIER_COM_UNBALANCED_OLEINIT 0x0404
#define APPLICATION_VERIFIER_COM_UNBALANCED_SWC 0x0405
#define APPLICATION_VERIFIER_COM_NULL_DACL 0x0406
#define APPLICATION_VERIFIER_COM_UNSAFE_IMPERSONATION 0x0407
#define APPLICATION_VERIFIER_COM_SMUGGLED_WRAPPER 0x0408
#define APPLICATION_VERIFIER_COM_SMUGGLED_PROXY 0x0409
#define APPLICATION_VERIFIER_COM_CF_SUCCESS_WITH_NULL 0x040A
#define APPLICATION_VERIFIER_COM_GCO_SUCCESS_WITH_NULL 0x040B
#define APPLICATION_VERIFIER_COM_OBJECT_IN_FREED_MEMORY 0x040C
#define APPLICATION_VERIFIER_COM_OBJECT_IN_UNLOADED_DLL 0x040D
#define APPLICATION_VERIFIER_COM_VTBL_IN_FREED_MEMORY 0x040E
#define APPLICATION_VERIFIER_COM_VTBL_IN_UNLOADED_DLL 0x040F
#define APPLICATION_VERIFIER_COM_HOLDING_LOCKS_ON_CALL 0x0410

#define APPLICATION_VERIFIER_RPC_ERROR 0x0500

#define APPLICATION_VERIFIER_INVALID_FREEMEM 0x0600
#define APPLICATION_VERIFIER_INVALID_ALLOCMEM 0x0601
#define APPLICATION_VERIFIER_INVALID_MAPVIEW 0x0602
#define APPLICATION_VERIFIER_PROBE_INVALID_ADDRESS 0x0603
#define APPLICATION_VERIFIER_PROBE_FREE_MEM 0x0604
#define APPLICATION_VERIFIER_PROBE_GUARD_PAGE 0x0605
#define APPLICATION_VERIFIER_PROBE_NULL 0x0606
#define APPLICATION_VERIFIER_PROBE_INVALID_START_OR_SIZE 0x0607
#define APPLICATION_VERIFIER_SIZE_HEAP_UNEXPECTED_EXCEPTION 0x0618

#define VERIFIER_STOP(Code,Msg,P1,S1,P2,S2,P3,S3,P4,S4) { RtlApplicationVerifierStop ((Code),(Msg),(ULONG_PTR)(P1),(S1),(ULONG_PTR)(P2),(S2),(ULONG_PTR)(P3),(S3),(ULONG_PTR)(P4),(S4)); }

    VOID NTAPI RtlApplicationVerifierStop(ULONG_PTR Code,PSTR Message,ULONG_PTR Param1,PSTR Description1,ULONG_PTR Param2,PSTR Description2,ULONG_PTR Param3,PSTR Description3,ULONG_PTR Param4,PSTR Description4);

    typedef LONG (NTAPI *PVECTORED_EXCEPTION_HANDLER)(struct _EXCEPTION_POINTERS *ExceptionInfo);
#define SEF_DACL_AUTO_INHERIT 0x01
#define SEF_SACL_AUTO_INHERIT 0x02
#define SEF_DEFAULT_DESCRIPTOR_FOR_OBJECT 0x04
#define SEF_AVOID_PRIVILEGE_CHECK 0x08
#define SEF_AVOID_OWNER_CHECK 0x10
#define SEF_DEFAULT_OWNER_FROM_PARENT 0x20
#define SEF_DEFAULT_GROUP_FROM_PARENT 0x40

    typedef enum _HEAP_INFORMATION_CLASS {
      HeapCompatibilityInformation
    } HEAP_INFORMATION_CLASS;

    NTSYSAPI DWORD NTAPI RtlSetHeapInformation(PVOID HeapHandle,HEAP_INFORMATION_CLASS HeapInformationClass,PVOID HeapInformation,SIZE_T HeapInformationLength);
    NTSYSAPI DWORD NTAPI RtlQueryHeapInformation(PVOID HeapHandle,HEAP_INFORMATION_CLASS HeapInformationClass,PVOID HeapInformation,SIZE_T HeapInformationLength,PSIZE_T ReturnLength);
    DWORD NTAPI RtlMultipleAllocateHeap(PVOID HeapHandle,DWORD Flags,SIZE_T Size,DWORD Count,PVOID *Array);
    DWORD NTAPI RtlMultipleFreeHeap(PVOID HeapHandle,DWORD Flags,DWORD Count,PVOID *Array);

#define WT_EXECUTEDEFAULT 0x00000000
#define WT_EXECUTEINIOTHREAD 0x00000001
#define WT_EXECUTEINUITHREAD 0x00000002
#define WT_EXECUTEINWAITTHREAD 0x00000004
#define WT_EXECUTEONLYONCE 0x00000008
#define WT_EXECUTEINTIMERTHREAD 0x00000020
#define WT_EXECUTELONGFUNCTION 0x00000010
#define WT_EXECUTEINPERSISTENTIOTHREAD 0x00000040
#define WT_EXECUTEINPERSISTENTTHREAD 0x00000080
#define WT_TRANSFER_IMPERSONATION 0x00000100
#define WT_SET_MAX_THREADPOOL_THREADS(Flags,Limit) ((Flags) |= (Limit)<<16)
    typedef VOID (NTAPI *WAITORTIMERCALLBACKFUNC)(PVOID,BOOLEAN);
    typedef VOID (NTAPI *WORKERCALLBACKFUNC)(PVOID);
    typedef VOID (NTAPI *APC_CALLBACK_FUNCTION)(DWORD ,PVOID,PVOID);
    typedef
      VOID
      (NTAPI *PFLS_CALLBACK_FUNCTION)(PVOID lpFlsData);
#define WT_EXECUTEINLONGTHREAD 0x00000010
#define WT_EXECUTEDELETEWAIT 0x00000008

    typedef enum _ACTIVATION_CONTEXT_INFO_CLASS {
      ActivationContextBasicInformation = 1,ActivationContextDetailedInformation = 2,AssemblyDetailedInformationInActivationContext = 3,FileInformationInAssemblyOfAssemblyInActivationContext = 4,MaxActivationContextInfoClass,AssemblyDetailedInformationInActivationContxt = 3,FileInformationInAssemblyOfAssemblyInActivationContxt = 4
    } ACTIVATION_CONTEXT_INFO_CLASS;

#define ACTIVATIONCONTEXTINFOCLASS ACTIVATION_CONTEXT_INFO_CLASS

    typedef struct _ACTIVATION_CONTEXT_QUERY_INDEX {
      DWORD ulAssemblyIndex;
      DWORD ulFileIndexInAssembly;
    } ACTIVATION_CONTEXT_QUERY_INDEX,*PACTIVATION_CONTEXT_QUERY_INDEX;

    typedef const struct _ACTIVATION_CONTEXT_QUERY_INDEX *PCACTIVATION_CONTEXT_QUERY_INDEX;

#define ACTIVATION_CONTEXT_PATH_TYPE_NONE (1)
#define ACTIVATION_CONTEXT_PATH_TYPE_WIN32_FILE (2)
#define ACTIVATION_CONTEXT_PATH_TYPE_URL (3)
#define ACTIVATION_CONTEXT_PATH_TYPE_ASSEMBLYREF (4)

    typedef struct _ASSEMBLY_FILE_DETAILED_INFORMATION {
      DWORD ulFlags;
      DWORD ulFilenameLength;
      DWORD ulPathLength;

      PCWSTR lpFileName;
      PCWSTR lpFilePath;
    } ASSEMBLY_FILE_DETAILED_INFORMATION,*PASSEMBLY_FILE_DETAILED_INFORMATION;
    typedef const ASSEMBLY_FILE_DETAILED_INFORMATION *PCASSEMBLY_FILE_DETAILED_INFORMATION;

#define _ASSEMBLY_DLL_REDIRECTION_DETAILED_INFORMATION _ASSEMBLY_FILE_DETAILED_INFORMATION
#define ASSEMBLY_DLL_REDIRECTION_DETAILED_INFORMATION ASSEMBLY_FILE_DETAILED_INFORMATION
#define PASSEMBLY_DLL_REDIRECTION_DETAILED_INFORMATION PASSEMBLY_FILE_DETAILED_INFORMATION
#define PCASSEMBLY_DLL_REDIRECTION_DETAILED_INFORMATION PCASSEMBLY_FILE_DETAILED_INFORMATION

    typedef struct _ACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION {
      DWORD ulFlags;
      DWORD ulEncodedAssemblyIdentityLength;
      DWORD ulManifestPathType;
      DWORD ulManifestPathLength;
      LARGE_INTEGER liManifestLastWriteTime;
      DWORD ulPolicyPathType;
      DWORD ulPolicyPathLength;
      LARGE_INTEGER liPolicyLastWriteTime;
      DWORD ulMetadataSatelliteRosterIndex;
      DWORD ulManifestVersionMajor;
      DWORD ulManifestVersionMinor;
      DWORD ulPolicyVersionMajor;
      DWORD ulPolicyVersionMinor;
      DWORD ulAssemblyDirectoryNameLength;
      PCWSTR lpAssemblyEncodedAssemblyIdentity;
      PCWSTR lpAssemblyManifestPath;
      PCWSTR lpAssemblyPolicyPath;
      PCWSTR lpAssemblyDirectoryName;
      DWORD ulFileCount;
    } ACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION,*PACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION;

    typedef const struct _ACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION *PCACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION;

    typedef struct _ACTIVATION_CONTEXT_DETAILED_INFORMATION {
      DWORD dwFlags;
      DWORD ulFormatVersion;
      DWORD ulAssemblyCount;
      DWORD ulRootManifestPathType;
      DWORD ulRootManifestPathChars;
      DWORD ulRootConfigurationPathType;
      DWORD ulRootConfigurationPathChars;
      DWORD ulAppDirPathType;
      DWORD ulAppDirPathChars;
      PCWSTR lpRootManifestPath;
      PCWSTR lpRootConfigurationPath;
      PCWSTR lpAppDirPath;
    } ACTIVATION_CONTEXT_DETAILED_INFORMATION,*PACTIVATION_CONTEXT_DETAILED_INFORMATION;

    typedef const struct _ACTIVATION_CONTEXT_DETAILED_INFORMATION *PCACTIVATION_CONTEXT_DETAILED_INFORMATION;

#define DLL_PROCESS_ATTACH 1
#define DLL_THREAD_ATTACH 2
#define DLL_THREAD_DETACH 3
#define DLL_PROCESS_DETACH 0
#define DLL_PROCESS_VERIFIER 4

#define EVENTLOG_SEQUENTIAL_READ 0x0001
#define EVENTLOG_SEEK_READ 0x0002
#define EVENTLOG_FORWARDS_READ 0x0004
#define EVENTLOG_BACKWARDS_READ 0x0008

#define EVENTLOG_SUCCESS 0x0000
#define EVENTLOG_ERROR_TYPE 0x0001
#define EVENTLOG_WARNING_TYPE 0x0002
#define EVENTLOG_INFORMATION_TYPE 0x0004
#define EVENTLOG_AUDIT_SUCCESS 0x0008
#define EVENTLOG_AUDIT_FAILURE 0x0010

#define EVENTLOG_START_PAIRED_EVENT 0x0001
#define EVENTLOG_END_PAIRED_EVENT 0x0002
#define EVENTLOG_END_ALL_PAIRED_EVENTS 0x0004
#define EVENTLOG_PAIRED_EVENT_ACTIVE 0x0008
#define EVENTLOG_PAIRED_EVENT_INACTIVE 0x0010

    typedef struct _EVENTLOGRECORD {
      DWORD Length;
      DWORD Reserved;
      DWORD RecordNumber;
      DWORD TimeGenerated;
      DWORD TimeWritten;
      DWORD EventID;
      WORD EventType;
      WORD NumStrings;
      WORD EventCategory;
      WORD ReservedFlags;
      DWORD ClosingRecordNumber;
      DWORD StringOffset;
      DWORD UserSidLength;
      DWORD UserSidOffset;
      DWORD DataLength;
      DWORD DataOffset;
    } EVENTLOGRECORD,*PEVENTLOGRECORD;

#define MAXLOGICALLOGNAMESIZE 256

    typedef struct _EVENTSFORLOGFILE{
      DWORD ulSize;
      WCHAR szLogicalLogFile[MAXLOGICALLOGNAMESIZE];
      DWORD ulNumRecords;
      EVENTLOGRECORD pEventLogRecords[];
    } EVENTSFORLOGFILE,*PEVENTSFORLOGFILE;

    typedef struct _PACKEDEVENTINFO{
      DWORD ulSize;
      DWORD ulNumEventsForLogFile;
      DWORD ulOffsets[];
    } PACKEDEVENTINFO,*PPACKEDEVENTINFO;

#define KEY_QUERY_VALUE (0x0001)
#define KEY_SET_VALUE (0x0002)
#define KEY_CREATE_SUB_KEY (0x0004)
#define KEY_ENUMERATE_SUB_KEYS (0x0008)
#define KEY_NOTIFY (0x0010)
#define KEY_CREATE_LINK (0x0020)
#define KEY_WOW64_32KEY (0x0200)
#define KEY_WOW64_64KEY (0x0100)
#define KEY_WOW64_RES (0x0300)

#define KEY_READ ((STANDARD_RIGHTS_READ | KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS | KEY_NOTIFY) & (~SYNCHRONIZE))
#define KEY_WRITE ((STANDARD_RIGHTS_WRITE | KEY_SET_VALUE | KEY_CREATE_SUB_KEY) & (~SYNCHRONIZE))
#define KEY_EXECUTE ((KEY_READ) & (~SYNCHRONIZE))
#define KEY_ALL_ACCESS ((STANDARD_RIGHTS_ALL | KEY_QUERY_VALUE | KEY_SET_VALUE | KEY_CREATE_SUB_KEY | KEY_ENUMERATE_SUB_KEYS | KEY_NOTIFY | KEY_CREATE_LINK) & (~SYNCHRONIZE))
#define REG_OPTION_RESERVED (0x00000000L)

#define REG_OPTION_NON_VOLATILE (0x00000000L)
#define REG_OPTION_VOLATILE (0x00000001L)
#define REG_OPTION_CREATE_LINK (0x00000002L)
#define REG_OPTION_BACKUP_RESTORE (0x00000004L)
#define REG_OPTION_OPEN_LINK (0x00000008L)
#define REG_LEGAL_OPTION (REG_OPTION_RESERVED | REG_OPTION_NON_VOLATILE | REG_OPTION_VOLATILE | REG_OPTION_CREATE_LINK | REG_OPTION_BACKUP_RESTORE | REG_OPTION_OPEN_LINK)
#define REG_CREATED_NEW_KEY (0x00000001L)
#define REG_OPENED_EXISTING_KEY (0x00000002L)
#define REG_STANDARD_FORMAT 1
#define REG_LATEST_FORMAT 2
#define REG_NO_COMPRESSION 4
#define REG_WHOLE_HIVE_VOLATILE (0x00000001L)
#define REG_REFRESH_HIVE (0x00000002L)
#define REG_NO_LAZY_FLUSH (0x00000004L)
#define REG_FORCE_RESTORE (0x00000008L)
#define REG_FORCE_UNLOAD 1

#define REG_NOTIFY_CHANGE_NAME (0x00000001L)
#define REG_NOTIFY_CHANGE_ATTRIBUTES (0x00000002L)
#define REG_NOTIFY_CHANGE_LAST_SET (0x00000004L)
#define REG_NOTIFY_CHANGE_SECURITY (0x00000008L)

#define REG_LEGAL_CHANGE_FILTER (REG_NOTIFY_CHANGE_NAME | REG_NOTIFY_CHANGE_ATTRIBUTES | REG_NOTIFY_CHANGE_LAST_SET | REG_NOTIFY_CHANGE_SECURITY)

#define REG_NONE (0)
#define REG_SZ (1)
#define REG_EXPAND_SZ (2)

#define REG_BINARY (3)
#define REG_DWORD (4)
#define REG_DWORD_LITTLE_ENDIAN (4)
#define REG_DWORD_BIG_ENDIAN (5)
#define REG_LINK (6)
#define REG_MULTI_SZ (7)
#define REG_RESOURCE_LIST (8)
#define REG_FULL_RESOURCE_DESCRIPTOR (9)
#define REG_RESOURCE_REQUIREMENTS_LIST (10)
#define REG_QWORD (11)
#define REG_QWORD_LITTLE_ENDIAN (11)

#define SERVICE_KERNEL_DRIVER 0x00000001
#define SERVICE_FILE_SYSTEM_DRIVER 0x00000002
#define SERVICE_ADAPTER 0x00000004
#define SERVICE_RECOGNIZER_DRIVER 0x00000008

#define SERVICE_DRIVER (SERVICE_KERNEL_DRIVER | SERVICE_FILE_SYSTEM_DRIVER | SERVICE_RECOGNIZER_DRIVER)

#define SERVICE_WIN32_OWN_PROCESS 0x00000010
#define SERVICE_WIN32_SHARE_PROCESS 0x00000020
#define SERVICE_WIN32 (SERVICE_WIN32_OWN_PROCESS | SERVICE_WIN32_SHARE_PROCESS)

#define SERVICE_INTERACTIVE_PROCESS 0x00000100

#define SERVICE_TYPE_ALL (SERVICE_WIN32 | SERVICE_ADAPTER | SERVICE_DRIVER | SERVICE_INTERACTIVE_PROCESS)

#define SERVICE_BOOT_START 0x00000000
#define SERVICE_SYSTEM_START 0x00000001
#define SERVICE_AUTO_START 0x00000002
#define SERVICE_DEMAND_START 0x00000003
#define SERVICE_DISABLED 0x00000004

#define SERVICE_ERROR_IGNORE 0x00000000
#define SERVICE_ERROR_NORMAL 0x00000001
#define SERVICE_ERROR_SEVERE 0x00000002
#define SERVICE_ERROR_CRITICAL 0x00000003

    typedef enum _CM_SERVICE_NODE_TYPE {
      DriverType = SERVICE_KERNEL_DRIVER,FileSystemType = SERVICE_FILE_SYSTEM_DRIVER,Win32ServiceOwnProcess = SERVICE_WIN32_OWN_PROCESS,
      Win32ServiceShareProcess = SERVICE_WIN32_SHARE_PROCESS,AdapterType = SERVICE_ADAPTER,RecognizerType = SERVICE_RECOGNIZER_DRIVER
    } SERVICE_NODE_TYPE;

    typedef enum _CM_SERVICE_LOAD_TYPE {
      BootLoad = SERVICE_BOOT_START,SystemLoad = SERVICE_SYSTEM_START,AutoLoad = SERVICE_AUTO_START,DemandLoad = SERVICE_DEMAND_START,
      DisableLoad = SERVICE_DISABLED
    } SERVICE_LOAD_TYPE;

    typedef enum _CM_ERROR_CONTROL_TYPE {
      IgnoreError = SERVICE_ERROR_IGNORE,NormalError = SERVICE_ERROR_NORMAL,SevereError = SERVICE_ERROR_SEVERE,CriticalError = SERVICE_ERROR_CRITICAL
    } SERVICE_ERROR_TYPE;

#define TAPE_ERASE_SHORT 0L
#define TAPE_ERASE_LONG 1L

    typedef struct _TAPE_ERASE {
      DWORD Type;
      BOOLEAN Immediate;
    } TAPE_ERASE,*PTAPE_ERASE;

#define TAPE_LOAD 0L
#define TAPE_UNLOAD 1L
#define TAPE_TENSION 2L
#define TAPE_LOCK 3L
#define TAPE_UNLOCK 4L
#define TAPE_FORMAT 5L

    typedef struct _TAPE_PREPARE {
      DWORD Operation;
      BOOLEAN Immediate;
    } TAPE_PREPARE,*PTAPE_PREPARE;

#define TAPE_SETMARKS 0L
#define TAPE_FILEMARKS 1L
#define TAPE_SHORT_FILEMARKS 2L
#define TAPE_LONG_FILEMARKS 3L

    typedef struct _TAPE_WRITE_MARKS {
      DWORD Type;
      DWORD Count;
      BOOLEAN Immediate;
    } TAPE_WRITE_MARKS,*PTAPE_WRITE_MARKS;

#define TAPE_ABSOLUTE_POSITION 0L
#define TAPE_LOGICAL_POSITION 1L
#define TAPE_PSEUDO_LOGICAL_POSITION 2L

    typedef struct _TAPE_GET_POSITION {
      DWORD Type;
      DWORD Partition;
      LARGE_INTEGER Offset;
    } TAPE_GET_POSITION,*PTAPE_GET_POSITION;

#define TAPE_REWIND 0L
#define TAPE_ABSOLUTE_BLOCK 1L
#define TAPE_LOGICAL_BLOCK 2L
#define TAPE_PSEUDO_LOGICAL_BLOCK 3L
#define TAPE_SPACE_END_OF_DATA 4L
#define TAPE_SPACE_RELATIVE_BLOCKS 5L
#define TAPE_SPACE_FILEMARKS 6L
#define TAPE_SPACE_SEQUENTIAL_FMKS 7L
#define TAPE_SPACE_SETMARKS 8L
#define TAPE_SPACE_SEQUENTIAL_SMKS 9L

    typedef struct _TAPE_SET_POSITION {
      DWORD Method;
      DWORD Partition;
      LARGE_INTEGER Offset;
      BOOLEAN Immediate;
    } TAPE_SET_POSITION,*PTAPE_SET_POSITION;

#define TAPE_DRIVE_FIXED 0x00000001
#define TAPE_DRIVE_SELECT 0x00000002
#define TAPE_DRIVE_INITIATOR 0x00000004

#define TAPE_DRIVE_ERASE_SHORT 0x00000010
#define TAPE_DRIVE_ERASE_LONG 0x00000020
#define TAPE_DRIVE_ERASE_BOP_ONLY 0x00000040
#define TAPE_DRIVE_ERASE_IMMEDIATE 0x00000080

#define TAPE_DRIVE_TAPE_CAPACITY 0x00000100
#define TAPE_DRIVE_TAPE_REMAINING 0x00000200
#define TAPE_DRIVE_FIXED_BLOCK 0x00000400
#define TAPE_DRIVE_VARIABLE_BLOCK 0x00000800

#define TAPE_DRIVE_WRITE_PROTECT 0x00001000
#define TAPE_DRIVE_EOT_WZ_SIZE 0x00002000

#define TAPE_DRIVE_ECC 0x00010000
#define TAPE_DRIVE_COMPRESSION 0x00020000
#define TAPE_DRIVE_PADDING 0x00040000
#define TAPE_DRIVE_REPORT_SMKS 0x00080000

#define TAPE_DRIVE_GET_ABSOLUTE_BLK 0x00100000
#define TAPE_DRIVE_GET_LOGICAL_BLK 0x00200000
#define TAPE_DRIVE_SET_EOT_WZ_SIZE 0x00400000

#define TAPE_DRIVE_EJECT_MEDIA 0x01000000
#define TAPE_DRIVE_CLEAN_REQUESTS 0x02000000
#define TAPE_DRIVE_SET_CMP_BOP_ONLY 0x04000000

#define TAPE_DRIVE_RESERVED_BIT 0x80000000

#define TAPE_DRIVE_LOAD_UNLOAD 0x80000001
#define TAPE_DRIVE_TENSION 0x80000002
#define TAPE_DRIVE_LOCK_UNLOCK 0x80000004
#define TAPE_DRIVE_REWIND_IMMEDIATE 0x80000008

#define TAPE_DRIVE_SET_BLOCK_SIZE 0x80000010
#define TAPE_DRIVE_LOAD_UNLD_IMMED 0x80000020
#define TAPE_DRIVE_TENSION_IMMED 0x80000040
#define TAPE_DRIVE_LOCK_UNLK_IMMED 0x80000080

#define TAPE_DRIVE_SET_ECC 0x80000100
#define TAPE_DRIVE_SET_COMPRESSION 0x80000200
#define TAPE_DRIVE_SET_PADDING 0x80000400
#define TAPE_DRIVE_SET_REPORT_SMKS 0x80000800

#define TAPE_DRIVE_ABSOLUTE_BLK 0x80001000
#define TAPE_DRIVE_ABS_BLK_IMMED 0x80002000
#define TAPE_DRIVE_LOGICAL_BLK 0x80004000
#define TAPE_DRIVE_LOG_BLK_IMMED 0x80008000

#define TAPE_DRIVE_END_OF_DATA 0x80010000
#define TAPE_DRIVE_RELATIVE_BLKS 0x80020000
#define TAPE_DRIVE_FILEMARKS 0x80040000
#define TAPE_DRIVE_SEQUENTIAL_FMKS 0x80080000

#define TAPE_DRIVE_SETMARKS 0x80100000
#define TAPE_DRIVE_SEQUENTIAL_SMKS 0x80200000
#define TAPE_DRIVE_REVERSE_POSITION 0x80400000
#define TAPE_DRIVE_SPACE_IMMEDIATE 0x80800000

#define TAPE_DRIVE_WRITE_SETMARKS 0x81000000
#define TAPE_DRIVE_WRITE_FILEMARKS 0x82000000
#define TAPE_DRIVE_WRITE_SHORT_FMKS 0x84000000
#define TAPE_DRIVE_WRITE_LONG_FMKS 0x88000000

#define TAPE_DRIVE_WRITE_MARK_IMMED 0x90000000
#define TAPE_DRIVE_FORMAT 0xA0000000
#define TAPE_DRIVE_FORMAT_IMMEDIATE 0xC0000000
#define TAPE_DRIVE_HIGH_FEATURES 0x80000000

    typedef struct _TAPE_GET_DRIVE_PARAMETERS {
      BOOLEAN ECC;
      BOOLEAN Compression;
      BOOLEAN DataPadding;
      BOOLEAN ReportSetmarks;
      DWORD DefaultBlockSize;
      DWORD MaximumBlockSize;
      DWORD MinimumBlockSize;
      DWORD MaximumPartitionCount;
      DWORD FeaturesLow;
      DWORD FeaturesHigh;
      DWORD EOTWarningZoneSize;
    } TAPE_GET_DRIVE_PARAMETERS,*PTAPE_GET_DRIVE_PARAMETERS;

    typedef struct _TAPE_SET_DRIVE_PARAMETERS {
      BOOLEAN ECC;
      BOOLEAN Compression;
      BOOLEAN DataPadding;
      BOOLEAN ReportSetmarks;
      DWORD EOTWarningZoneSize;
    } TAPE_SET_DRIVE_PARAMETERS,*PTAPE_SET_DRIVE_PARAMETERS;

    typedef struct _TAPE_GET_MEDIA_PARAMETERS {
      LARGE_INTEGER Capacity;
      LARGE_INTEGER Remaining;
      DWORD BlockSize;
      DWORD PartitionCount;
      BOOLEAN WriteProtected;
    } TAPE_GET_MEDIA_PARAMETERS,*PTAPE_GET_MEDIA_PARAMETERS;

    typedef struct _TAPE_SET_MEDIA_PARAMETERS {
      DWORD BlockSize;
    } TAPE_SET_MEDIA_PARAMETERS,*PTAPE_SET_MEDIA_PARAMETERS;

#define TAPE_FIXED_PARTITIONS 0L
#define TAPE_SELECT_PARTITIONS 1L
#define TAPE_INITIATOR_PARTITIONS 2L

    typedef struct _TAPE_CREATE_PARTITION {
      DWORD Method;
      DWORD Count;
      DWORD Size;
    } TAPE_CREATE_PARTITION,*PTAPE_CREATE_PARTITION;

#define TAPE_QUERY_DRIVE_PARAMETERS 0L
#define TAPE_QUERY_MEDIA_CAPACITY 1L
#define TAPE_CHECK_FOR_DRIVE_PROBLEM 2L
#define TAPE_QUERY_IO_ERROR_DATA 3L
#define TAPE_QUERY_DEVICE_ERROR_DATA 4L

    typedef struct _TAPE_WMI_OPERATIONS {
      DWORD Method;
      DWORD DataBufferSize;
      PVOID DataBuffer;
    } TAPE_WMI_OPERATIONS,*PTAPE_WMI_OPERATIONS;

    typedef enum _TAPE_DRIVE_PROBLEM_TYPE {
      TapeDriveProblemNone,TapeDriveReadWriteWarning,TapeDriveReadWriteError,TapeDriveReadWarning,TapeDriveWriteWarning,TapeDriveReadError,TapeDriveWriteError,TapeDriveHardwareError,TapeDriveUnsupportedMedia,TapeDriveScsiConnectionError,TapeDriveTimetoClean,TapeDriveCleanDriveNow,TapeDriveMediaLifeExpired,TapeDriveSnappedTape
    } TAPE_DRIVE_PROBLEM_TYPE;

#if defined(__x86_64)
    __CRT_INLINE struct _TEB *NtCurrentTeb(VOID) { return (struct _TEB *)__readgsqword(FIELD_OFFSET(NT_TIB,Self)); }
    __CRT_INLINE PVOID GetCurrentFiber(VOID) { return(PVOID)__readgsqword(FIELD_OFFSET(NT_TIB,FiberData)); }
    __CRT_INLINE PVOID GetFiberData(VOID) {
      return *(PVOID *)GetCurrentFiber();
    }
#endif

#if(defined(_X86_) && !defined(__x86_64))
#define PcTeb 0x18
    __CRT_INLINE struct _TEB *NtCurrentTeb(void) {
      struct _TEB *ret;
      __asm__ volatile ("movl	%%fs:0x18,%0"
	: "=r" (ret));
      return ret;
    }
#endif

#define ACTIVATION_CONTEXT_SECTION_ASSEMBLY_INFORMATION (1)
#define ACTIVATION_CONTEXT_SECTION_DLL_REDIRECTION (2)
#define ACTIVATION_CONTEXT_SECTION_WINDOW_CLASS_REDIRECTION (3)
#define ACTIVATION_CONTEXT_SECTION_COM_SERVER_REDIRECTION (4)
#define ACTIVATION_CONTEXT_SECTION_COM_INTERFACE_REDIRECTION (5)
#define ACTIVATION_CONTEXT_SECTION_COM_TYPE_LIBRARY_REDIRECTION (6)
#define ACTIVATION_CONTEXT_SECTION_COM_PROGID_REDIRECTION (7)
#define ACTIVATION_CONTEXT_SECTION_GLOBAL_OBJECT_RENAME_TABLE (8)
#define ACTIVATION_CONTEXT_SECTION_CLR_SURROGATES (9)
#define ACTIVATION_CONTEXT_SECTION_APPLICATION_SETTINGS (10)

#ifdef __cplusplus
  }
#endif
#endif
