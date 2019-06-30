module os

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types

type BOOL bool
type BOOLEAN BOOL

// A byte (8 bits)
type BYTE u8

// An 8-bit Windows (ANSI) character.
type CHAR u8

// An 16-bit Windows character.
type WCHAR u16

// A 32-bit signed integer. The range is -2147483648 through 2147483647 decimal.
type INT i32

// A signed integer type for pointer precision.
type INT_PTR INT*

// A pointer to an INT (INT32).
type PINT INT*

// A 8-bit signed integer.
type INT8 i8

// // A pointer to an INT8.
type PINT8 INT8*

// A 16-bit signed integer.
type INT16 i16

// // A pointer to an INT16.
type PINT16 INT16*

// A 32-bit signed integer.
type INT32 i32 

// A pointer to an INT32.
type PINT32 INT32*

// A 64-bit signed integer.
type INT64 i64

// A pointer to an INT64.
type PINT64 INT64*

// An unsigned INT. The range is 0 through 4294967295 decimal.
type UINT u32

// An unsigned INT_PTR.
type UINT_PTR UINT*

// An unsigned INT8.
type UINT8 u8

// An unsigned INT16.
type UINT16 u16

// An unsigned INT32.
type UINT32 u32 

// An unsigned INT64.
type UINT64 u64

// A 16-bit unsigned integer. The range is 0 through 65535 decimal.
type WORD u16

// A pointer to an WORD.
type LPWORD WORD*

// A 32-bit unsigned integer. The range is 0 through 4294967295 decimal.
type DWORD u32

// A pointer to an DWORD.
type DWORD_PTR DWORD*
type LPDWORD DWORD*

// A 64-bit unsigned integer. The range is 0 through 18446744073709551615 decimal.
type DWORDLONG u64 
type PDWORDLONG DWORDLONG*

// A 32-bit unsigned integer.
type DWORD32 u32
type PDWORD32 DWORD32*

// A 64-bit unsigned integer.
type DWORD64 u64
type PDWORD64 DWORD64*

// A floating-point variable.
type FLOAT f32
type FLOAT64 f64
type PFLOAT FLOAT*

// A 32-bit signed integer. The range is 2147483648 through 2147483647 decimal.
type LONG i32
type LONG_PTR LONG*
type LPLONG LONG*

// A 64-bit signed integer. The range is 9223372036854775808 through 9223372036854775807 decimal.
type LONGLONG i64
type LONG32 i32
type LONG64 i64 

// A pointer to a BOOL.
type LPBOOL BOOL*

// A pointer to a BYTE.
type LPBYTE BYTE*

// An 8-bit Windows (ANSI) character. For more information, see Character Sets Used By Fonts.
type LPCSTR CHAR*

// A 16-bit Unicode character. For more information, see Character Sets Used By Fonts.
type LPWSTR WCHAR*

// A pointer to an CHAR.
type PCHAR CHAR*

// A pointer to an BYTE.
type PBYTE BYTE*

//
// Descriptor
//

// A handle to an object.
type HANDLE C.HANDLE // voidptr?

// A pointer to a HANDLE.
type PHANDLE *HANDLE

// A handle to a registry key.
type HKEY HANDLE

// A pointer to a HKEY.
type PHKEY *HKEY
