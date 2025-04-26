/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _WINBASE_
#define _WINBASE_

#define WINADVAPI DECLSPEC_IMPORT
#define WINBASEAPI DECLSPEC_IMPORT
#define ZAWPROXYAPI DECLSPEC_IMPORT

#include <fileapi.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DefineHandleTable(w) ((w),TRUE)
#define LimitEmsPages(dw)
#define SetSwapAreaSize(w) (w)
#define LockSegment(w) GlobalFix((HANDLE)(w))
#define UnlockSegment(w) GlobalUnfix((HANDLE)(w))
#define GetCurrentTime() GetTickCount()

#define Yield()

#define INVALID_HANDLE_VALUE ((HANDLE)(LONG_PTR)-1)
#define INVALID_FILE_SIZE ((DWORD)0xffffffff)
#define INVALID_SET_FILE_POINTER ((DWORD)-1)
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)

#define FILE_BEGIN 0
#define FILE_CURRENT 1
#define FILE_END 2

#define TIME_ZONE_ID_INVALID ((DWORD)0xffffffff)

#define WAIT_FAILED ((DWORD)0xffffffff)
#define WAIT_OBJECT_0 ((STATUS_WAIT_0) + 0)
#define WAIT_ABANDONED ((STATUS_ABANDONED_WAIT_0) + 0)
#define WAIT_ABANDONED_0 ((STATUS_ABANDONED_WAIT_0) + 0)
#define WAIT_IO_COMPLETION STATUS_USER_APC
#define STILL_ACTIVE STATUS_PENDING
#define EXCEPTION_ACCESS_VIOLATION STATUS_ACCESS_VIOLATION
#define EXCEPTION_DATATYPE_MISALIGNMENT STATUS_DATATYPE_MISALIGNMENT
#define EXCEPTION_BREAKPOINT STATUS_BREAKPOINT
#define EXCEPTION_SINGLE_STEP STATUS_SINGLE_STEP
#define EXCEPTION_ARRAY_BOUNDS_EXCEEDED STATUS_ARRAY_BOUNDS_EXCEEDED
#define EXCEPTION_FLT_DENORMAL_OPERAND STATUS_FLOAT_DENORMAL_OPERAND
#define EXCEPTION_FLT_DIVIDE_BY_ZERO STATUS_FLOAT_DIVIDE_BY_ZERO
#define EXCEPTION_FLT_INEXACT_RESULT STATUS_FLOAT_INEXACT_RESULT
#define EXCEPTION_FLT_INVALID_OPERATION STATUS_FLOAT_INVALID_OPERATION
#define EXCEPTION_FLT_OVERFLOW STATUS_FLOAT_OVERFLOW
#define EXCEPTION_FLT_STACK_CHECK STATUS_FLOAT_STACK_CHECK
#define EXCEPTION_FLT_UNDERFLOW STATUS_FLOAT_UNDERFLOW
#define EXCEPTION_INT_DIVIDE_BY_ZERO STATUS_INTEGER_DIVIDE_BY_ZERO
#define EXCEPTION_INT_OVERFLOW STATUS_INTEGER_OVERFLOW
#define EXCEPTION_PRIV_INSTRUCTION STATUS_PRIVILEGED_INSTRUCTION
#define EXCEPTION_IN_PAGE_ERROR STATUS_IN_PAGE_ERROR
#define EXCEPTION_ILLEGAL_INSTRUCTION STATUS_ILLEGAL_INSTRUCTION
#define EXCEPTION_NONCONTINUABLE_EXCEPTION STATUS_NONCONTINUABLE_EXCEPTION
#define EXCEPTION_STACK_OVERFLOW STATUS_STACK_OVERFLOW
#define EXCEPTION_INVALID_DISPOSITION STATUS_INVALID_DISPOSITION
#define EXCEPTION_GUARD_PAGE STATUS_GUARD_PAGE_VIOLATION
#define EXCEPTION_INVALID_HANDLE STATUS_INVALID_HANDLE
#define EXCEPTION_POSSIBLE_DEADLOCK STATUS_POSSIBLE_DEADLOCK
#define CONTROL_C_EXIT STATUS_CONTROL_C_EXIT
#define MoveMemory RtlMoveMemory
#define CopyMemory RtlCopyMemory
#define FillMemory RtlFillMemory
#define ZeroMemory RtlZeroMemory
#define SecureZeroMemory RtlSecureZeroMemory

#define FILE_FLAG_WRITE_THROUGH 0x80000000
#define FILE_FLAG_OVERLAPPED 0x40000000
#define FILE_FLAG_NO_BUFFERING 0x20000000
#define FILE_FLAG_RANDOM_ACCESS 0x10000000
#define FILE_FLAG_SEQUENTIAL_SCAN 0x8000000
#define FILE_FLAG_DELETE_ON_CLOSE 0x4000000
#define FILE_FLAG_BACKUP_SEMANTICS 0x2000000
#define FILE_FLAG_POSIX_SEMANTICS 0x1000000
#define FILE_FLAG_OPEN_REPARSE_POINT 0x200000
#define FILE_FLAG_OPEN_NO_RECALL 0x100000
#define FILE_FLAG_FIRST_PIPE_INSTANCE 0x80000

#define CREATE_NEW 1
#define CREATE_ALWAYS 2
#define OPEN_EXISTING 3
#define OPEN_ALWAYS 4
#define TRUNCATE_EXISTING 5

#define PROGRESS_CONTINUE 0
#define PROGRESS_CANCEL 1
#define PROGRESS_STOP 2
#define PROGRESS_QUIET 3

#define CALLBACK_CHUNK_FINISHED 0x0
#define CALLBACK_STREAM_SWITCH 0x1

#define COPY_FILE_FAIL_IF_EXISTS 0x1
#define COPY_FILE_RESTARTABLE 0x2
#define COPY_FILE_OPEN_SOURCE_FOR_WRITE 0x4
#define COPY_FILE_ALLOW_DECRYPTED_DESTINATION 0x8

#define REPLACEFILE_WRITE_THROUGH 0x1
#define REPLACEFILE_IGNORE_MERGE_ERRORS 0x2

#define PIPE_ACCESS_INBOUND 0x1
#define PIPE_ACCESS_OUTBOUND 0x2
#define PIPE_ACCESS_DUPLEX 0x3

#define PIPE_CLIENT_END 0x0
#define PIPE_SERVER_END 0x1

#define PIPE_WAIT 0x0
#define PIPE_NOWAIT 0x1
#define PIPE_READMODE_BYTE 0x0
#define PIPE_READMODE_MESSAGE 0x2
#define PIPE_TYPE_BYTE 0x0
#define PIPE_TYPE_MESSAGE 0x4

#define PIPE_UNLIMITED_INSTANCES 255

#define SECURITY_ANONYMOUS (SecurityAnonymous << 16)
#define SECURITY_IDENTIFICATION (SecurityIdentification << 16)
#define SECURITY_IMPERSONATION (SecurityImpersonation << 16)
#define SECURITY_DELEGATION (SecurityDelegation << 16)

#define SECURITY_CONTEXT_TRACKING 0x40000
#define SECURITY_EFFECTIVE_ONLY 0x80000

#define SECURITY_SQOS_PRESENT 0x100000
#define SECURITY_VALID_SQOS_FLAGS 0x1f0000

  typedef struct _OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    union {
      struct {
	DWORD Offset;
	DWORD OffsetHigh;
      };
      PVOID Pointer;
    };
    HANDLE hEvent;
  } OVERLAPPED,*LPOVERLAPPED;

  typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    WINBOOL bInheritHandle;
  } SECURITY_ATTRIBUTES,*PSECURITY_ATTRIBUTES,*LPSECURITY_ATTRIBUTES;

  typedef struct _PROCESS_INFORMATION {
    HANDLE hProcess;
    HANDLE hThread;
    DWORD dwProcessId;
    DWORD dwThreadId;
  } PROCESS_INFORMATION,*PPROCESS_INFORMATION,*LPPROCESS_INFORMATION;

#ifndef _FILETIME_
#define _FILETIME_
  typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
  } FILETIME,*PFILETIME,*LPFILETIME;
#endif

  typedef struct _SYSTEMTIME {
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
  } SYSTEMTIME,*PSYSTEMTIME,*LPSYSTEMTIME;

  typedef DWORD (WINAPI *PTHREAD_START_ROUTINE)(LPVOID lpThreadParameter);
  typedef PTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE;
  typedef VOID (WINAPI *PFIBER_START_ROUTINE)(LPVOID lpFiberParameter);
  typedef PFIBER_START_ROUTINE LPFIBER_START_ROUTINE;

  typedef RTL_CRITICAL_SECTION CRITICAL_SECTION;
  typedef PRTL_CRITICAL_SECTION PCRITICAL_SECTION;
  typedef PRTL_CRITICAL_SECTION LPCRITICAL_SECTION;
  typedef RTL_CRITICAL_SECTION_DEBUG CRITICAL_SECTION_DEBUG;
  typedef PRTL_CRITICAL_SECTION_DEBUG PCRITICAL_SECTION_DEBUG;
  typedef PRTL_CRITICAL_SECTION_DEBUG LPCRITICAL_SECTION_DEBUG;

  WINBASEAPI PVOID WINAPI EncodePointer(PVOID Ptr);
  WINBASEAPI PVOID WINAPI DecodePointer(PVOID Ptr);
  WINBASEAPI PVOID WINAPI EncodeSystemPointer(PVOID Ptr);
  WINBASEAPI PVOID WINAPI DecodeSystemPointer(PVOID Ptr);

#ifdef I_X86_
  typedef PLDT_ENTRY LPLDT_ENTRY;
#else
  typedef LPVOID LPLDT_ENTRY;
#endif

#define MUTEX_MODIFY_STATE MUTANT_QUERY_STATE
#define MUTEX_ALL_ACCESS MUTANT_ALL_ACCESS

#define SP_SERIALCOMM ((DWORD)0x1)

#define PST_UNSPECIFIED ((DWORD)0x0)
#define PST_RS232 ((DWORD)0x1)
#define PST_PARALLELPORT ((DWORD)0x2)
#define PST_RS422 ((DWORD)0x3)
#define PST_RS423 ((DWORD)0x4)
#define PST_RS449 ((DWORD)0x5)
#define PST_MODEM ((DWORD)0x6)
#define PST_FAX ((DWORD)0x21)
#define PST_SCANNER ((DWORD)0x22)
#define PST_NETWORK_BRIDGE ((DWORD)0x100)
#define PST_LAT ((DWORD)0x101)
#define PST_TCPIP_TELNET ((DWORD)0x102)
#define PST_X25 ((DWORD)0x103)

#define PCF_DTRDSR ((DWORD)0x1)
#define PCF_RTSCTS ((DWORD)0x2)
#define PCF_RLSD ((DWORD)0x4)
#define PCF_PARITY_CHECK ((DWORD)0x8)
#define PCF_XONXOFF ((DWORD)0x10)
#define PCF_SETXCHAR ((DWORD)0x20)
#define PCF_TOTALTIMEOUTS ((DWORD)0x40)
#define PCF_INTTIMEOUTS ((DWORD)0x80)
#define PCF_SPECIALCHARS ((DWORD)0x100)
#define PCF_16BITMODE ((DWORD)0x200)

#define SP_PARITY ((DWORD)0x1)
#define SP_BAUD ((DWORD)0x2)
#define SP_DATABITS ((DWORD)0x4)
#define SP_STOPBITS ((DWORD)0x8)
#define SP_HANDSHAKING ((DWORD)0x10)
#define SP_PARITY_CHECK ((DWORD)0x20)
#define SP_RLSD ((DWORD)0x40)

#define BAUD_075 ((DWORD)0x1)
#define BAUD_110 ((DWORD)0x2)
#define BAUD_134_5 ((DWORD)0x4)
#define BAUD_150 ((DWORD)0x8)
#define BAUD_300 ((DWORD)0x10)
#define BAUD_600 ((DWORD)0x20)
#define BAUD_1200 ((DWORD)0x40)
#define BAUD_1800 ((DWORD)0x80)
#define BAUD_2400 ((DWORD)0x100)
#define BAUD_4800 ((DWORD)0x200)
#define BAUD_7200 ((DWORD)0x400)
#define BAUD_9600 ((DWORD)0x800)
#define BAUD_14400 ((DWORD)0x1000)
#define BAUD_19200 ((DWORD)0x2000)
#define BAUD_38400 ((DWORD)0x4000)
#define BAUD_56K ((DWORD)0x8000)
#define BAUD_128K ((DWORD)0x10000)
#define BAUD_115200 ((DWORD)0x20000)
#define BAUD_57600 ((DWORD)0x40000)
#define BAUD_USER ((DWORD)0x10000000)

#define DATABITS_5 ((WORD)0x1)
#define DATABITS_6 ((WORD)0x2)
#define DATABITS_7 ((WORD)0x4)
#define DATABITS_8 ((WORD)0x8)
#define DATABITS_16 ((WORD)0x10)
#define DATABITS_16X ((WORD)0x20)

#define STOPBITS_10 ((WORD)0x1)
#define STOPBITS_15 ((WORD)0x2)
#define STOPBITS_20 ((WORD)0x4)
#define PARITY_NONE ((WORD)0x100)
#define PARITY_ODD ((WORD)0x200)
#define PARITY_EVEN ((WORD)0x400)
#define PARITY_MARK ((WORD)0x800)
#define PARITY_SPACE ((WORD)0x1000)

  typedef struct _COMMPROP {
    WORD wPacketLength;
    WORD wPacketVersion;
    DWORD dwServiceMask;
    DWORD dwReserved1;
    DWORD dwMaxTxQueue;
    DWORD dwMaxRxQueue;
    DWORD dwMaxBaud;
    DWORD dwProvSubType;
    DWORD dwProvCapabilities;
    DWORD dwSettableParams;
    DWORD dwSettableBaud;
    WORD wSettableData;
    WORD wSettableStopParity;
    DWORD dwCurrentTxQueue;
    DWORD dwCurrentRxQueue;
    DWORD dwProvSpec1;
    DWORD dwProvSpec2;
    WCHAR wcProvChar[1];
  } COMMPROP,*LPCOMMPROP;

#define COMMPROP_INITIALIZED ((DWORD)0xE73CF52E)

  typedef struct _COMSTAT {
    DWORD fCtsHold : 1;
    DWORD fDsrHold : 1;
    DWORD fRlsdHold : 1;
    DWORD fXoffHold : 1;
    DWORD fXoffSent : 1;
    DWORD fEof : 1;
    DWORD fTxim : 1;
    DWORD fReserved : 25;
    DWORD cbInQue;
    DWORD cbOutQue;
  } COMSTAT,*LPCOMSTAT;

#define DTR_CONTROL_DISABLE 0x0
#define DTR_CONTROL_ENABLE 0x1
#define DTR_CONTROL_HANDSHAKE 0x2

#define RTS_CONTROL_DISABLE 0x0
#define RTS_CONTROL_ENABLE 0x1
#define RTS_CONTROL_HANDSHAKE 0x2
#define RTS_CONTROL_TOGGLE 0x3

  typedef struct _DCB {
    DWORD DCBlength;
    DWORD BaudRate;
    DWORD fBinary: 1;
    DWORD fParity: 1;
    DWORD fOutxCtsFlow:1;
    DWORD fOutxDsrFlow:1;
    DWORD fDtrControl:2;
    DWORD fDsrSensitivity:1;
    DWORD fTXContinueOnXoff: 1;
    DWORD fOutX: 1;
    DWORD fInX: 1;
    DWORD fErrorChar: 1;
    DWORD fNull: 1;
    DWORD fRtsControl:2;
    DWORD fAbortOnError:1;
    DWORD fDummy2:17;
    WORD wReserved;
    WORD XonLim;
    WORD XoffLim;
    BYTE ByteSize;
    BYTE Parity;
    BYTE StopBits;
    char XonChar;
    char XoffChar;
    char ErrorChar;
    char EofChar;
    char EvtChar;
    WORD wReserved1;
  } DCB,*LPDCB;

  typedef struct _COMMTIMEOUTS {
    DWORD ReadIntervalTimeout;
    DWORD ReadTotalTimeoutMultiplier;
    DWORD ReadTotalTimeoutConstant;
    DWORD WriteTotalTimeoutMultiplier;
    DWORD WriteTotalTimeoutConstant;
  } COMMTIMEOUTS,*LPCOMMTIMEOUTS;

  typedef struct _COMMCONFIG {
    DWORD dwSize;
    WORD wVersion;
    WORD wReserved;
    DCB dcb;
    DWORD dwProviderSubType;
    DWORD dwProviderOffset;
    DWORD dwProviderSize;
    WCHAR wcProviderData[1];
  } COMMCONFIG,*LPCOMMCONFIG;

  typedef struct _SYSTEM_INFO {
    union {
      DWORD dwOemId;
      struct {
	WORD wProcessorArchitecture;
	WORD wReserved;
      };
    };
    DWORD dwPageSize;
    LPVOID lpMinimumApplicationAddress;
    LPVOID lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD dwNumberOfProcessors;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD wProcessorLevel;
    WORD wProcessorRevision;
  } SYSTEM_INFO,*LPSYSTEM_INFO;

#define FreeModule(hLibModule) FreeLibrary((hLibModule))
#define MakeProcInstance(lpProc,hInstance) (lpProc)
#define FreeProcInstance(lpProc) (lpProc)

#define GMEM_FIXED 0x0
#define GMEM_MOVEABLE 0x2
#define GMEM_NOCOMPACT 0x10
#define GMEM_NODISCARD 0x20
#define GMEM_ZEROINIT 0x40
#define GMEM_MODIFY 0x80
#define GMEM_DISCARDABLE 0x100
#define GMEM_NOT_BANKED 0x1000
#define GMEM_SHARE 0x2000
#define GMEM_DDESHARE 0x2000
#define GMEM_NOTIFY 0x4000
#define GMEM_LOWER GMEM_NOT_BANKED
#define GMEM_VALID_FLAGS 0x7F72
#define GMEM_INVALID_HANDLE 0x8000

#define GHND (GMEM_MOVEABLE | GMEM_ZEROINIT)
#define GPTR (GMEM_FIXED | GMEM_ZEROINIT)

#define GlobalLRUNewest(h) ((HANDLE)(h))
#define GlobalLRUOldest(h) ((HANDLE)(h))
#define GlobalDiscard(h) GlobalReAlloc((h),0,GMEM_MOVEABLE)

#define GMEM_DISCARDED 0x4000
#define GMEM_LOCKCOUNT 0xff

  typedef struct _MEMORYSTATUS {
    DWORD dwLength;
    DWORD dwMemoryLoad;
    SIZE_T dwTotalPhys;
    SIZE_T dwAvailPhys;
    SIZE_T dwTotalPageFile;
    SIZE_T dwAvailPageFile;
    SIZE_T dwTotalVirtual;
    SIZE_T dwAvailVirtual;
  } MEMORYSTATUS,*LPMEMORYSTATUS;

#define LMEM_FIXED 0x0
#define LMEM_MOVEABLE 0x2
#define LMEM_NOCOMPACT 0x10
#define LMEM_NODISCARD 0x20
#define LMEM_ZEROINIT 0x40
#define LMEM_MODIFY 0x80
#define LMEM_DISCARDABLE 0xf00
#define LMEM_VALID_FLAGS 0xf72
#define LMEM_INVALID_HANDLE 0x8000

#define LHND (LMEM_MOVEABLE | LMEM_ZEROINIT)
#define LPTR (LMEM_FIXED | LMEM_ZEROINIT)

#define NONZEROLHND (LMEM_MOVEABLE)
#define NONZEROLPTR (LMEM_FIXED)

#define LocalDiscard(h) LocalReAlloc((h),0,LMEM_MOVEABLE)

#define LMEM_DISCARDED 0x4000
#define LMEM_LOCKCOUNT 0xff

#define DEBUG_PROCESS 0x1
#define DEBUG_ONLY_THIS_PROCESS 0x2
#define CREATE_SUSPENDED 0x4
#define DETACHED_PROCESS 0x8
#define CREATE_NEW_CONSOLE 0x10
#define NORMAL_PRIORITY_CLASS 0x20
#define IDLE_PRIORITY_CLASS 0x40
#define HIGH_PRIORITY_CLASS 0x80
#define REALTIME_PRIORITY_CLASS 0x100
#define CREATE_NEW_PROCESS_GROUP 0x200
#define CREATE_UNICODE_ENVIRONMENT 0x400
#define CREATE_SEPARATE_WOW_VDM 0x800
#define CREATE_SHARED_WOW_VDM 0x1000
#define CREATE_FORCEDOS 0x2000
#define BELOW_NORMAL_PRIORITY_CLASS 0x4000
#define ABOVE_NORMAL_PRIORITY_CLASS 0x8000
#define STACK_SIZE_PARAM_IS_A_RESERVATION 0x10000

#define CREATE_BREAKAWAY_FROM_JOB 0x1000000
#define CREATE_PRESERVE_CODE_AUTHZ_LEVEL 0x2000000

#define CREATE_DEFAULT_ERROR_MODE 0x4000000
#define CREATE_NO_WINDOW 0x8000000

#define PROFILE_USER 0x10000000
#define PROFILE_KERNEL 0x20000000
#define PROFILE_SERVER 0x40000000

#define CREATE_IGNORE_SYSTEM_DEFAULT 0x80000000

#define THREAD_PRIORITY_LOWEST THREAD_BASE_PRIORITY_MIN
#define THREAD_PRIORITY_BELOW_NORMAL (THREAD_PRIORITY_LOWEST+1)
#define THREAD_PRIORITY_NORMAL 0
#define THREAD_PRIORITY_HIGHEST THREAD_BASE_PRIORITY_MAX
#define THREAD_PRIORITY_ABOVE_NORMAL (THREAD_PRIORITY_HIGHEST-1)
#define THREAD_PRIORITY_ERROR_RETURN (MAXLONG)

#define THREAD_PRIORITY_TIME_CRITICAL THREAD_BASE_PRIORITY_LOWRT
#define THREAD_PRIORITY_IDLE THREAD_BASE_PRIORITY_IDLE

#define EXCEPTION_DEBUG_EVENT 1
#define CREATE_THREAD_DEBUG_EVENT 2
#define CREATE_PROCESS_DEBUG_EVENT 3
#define EXIT_THREAD_DEBUG_EVENT 4
#define EXIT_PROCESS_DEBUG_EVENT 5
#define LOAD_DLL_DEBUG_EVENT 6
#define UNLOAD_DLL_DEBUG_EVENT 7
#define OUTPUT_DEBUG_STRING_EVENT 8
#define RIP_EVENT 9

  typedef struct _EXCEPTION_DEBUG_INFO {
    EXCEPTION_RECORD ExceptionRecord;
    DWORD dwFirstChance;
  } EXCEPTION_DEBUG_INFO,*LPEXCEPTION_DEBUG_INFO;

  typedef struct _CREATE_THREAD_DEBUG_INFO {
    HANDLE hThread;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
  } CREATE_THREAD_DEBUG_INFO,*LPCREATE_THREAD_DEBUG_INFO;

  typedef struct _CREATE_PROCESS_DEBUG_INFO {
    HANDLE hFile;
    HANDLE hProcess;
    HANDLE hThread;
    LPVOID lpBaseOfImage;
    DWORD dwDebugInfoFileOffset;
    DWORD nDebugInfoSize;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
    LPVOID lpImageName;
    WORD fUnicode;
  } CREATE_PROCESS_DEBUG_INFO,*LPCREATE_PROCESS_DEBUG_INFO;

  typedef struct _EXIT_THREAD_DEBUG_INFO {
    DWORD dwExitCode;
  } EXIT_THREAD_DEBUG_INFO,*LPEXIT_THREAD_DEBUG_INFO;

  typedef struct _EXIT_PROCESS_DEBUG_INFO {
    DWORD dwExitCode;
  } EXIT_PROCESS_DEBUG_INFO,*LPEXIT_PROCESS_DEBUG_INFO;

  typedef struct _LOAD_DLL_DEBUG_INFO {
    HANDLE hFile;
    LPVOID lpBaseOfDll;
    DWORD dwDebugInfoFileOffset;
    DWORD nDebugInfoSize;
    LPVOID lpImageName;
    WORD fUnicode;
  } LOAD_DLL_DEBUG_INFO,*LPLOAD_DLL_DEBUG_INFO;

  typedef struct _UNLOAD_DLL_DEBUG_INFO {
    LPVOID lpBaseOfDll;
  } UNLOAD_DLL_DEBUG_INFO,*LPUNLOAD_DLL_DEBUG_INFO;

  typedef struct _OUTPUT_DEBUG_STRING_INFO {
    LPSTR lpDebugStringData;
    WORD fUnicode;
    WORD nDebugStringLength;
  } OUTPUT_DEBUG_STRING_INFO,*LPOUTPUT_DEBUG_STRING_INFO;

  typedef struct _RIP_INFO {
    DWORD dwError;
    DWORD dwType;
  } RIP_INFO,*LPRIP_INFO;

  typedef struct _DEBUG_EVENT {
    DWORD dwDebugEventCode;
    DWORD dwProcessId;
    DWORD dwThreadId;
    union {
      EXCEPTION_DEBUG_INFO Exception;
      CREATE_THREAD_DEBUG_INFO CreateThread;
      CREATE_PROCESS_DEBUG_INFO CreateProcessInfo;
      EXIT_THREAD_DEBUG_INFO ExitThread;
      EXIT_PROCESS_DEBUG_INFO ExitProcess;
      LOAD_DLL_DEBUG_INFO LoadDll;
      UNLOAD_DLL_DEBUG_INFO UnloadDll;
      OUTPUT_DEBUG_STRING_INFO DebugString;
      RIP_INFO RipInfo;
    } u;
  } DEBUG_EVENT,*LPDEBUG_EVENT;

  typedef PCONTEXT LPCONTEXT;
  typedef PEXCEPTION_RECORD LPEXCEPTION_RECORD;
  typedef PEXCEPTION_POINTERS LPEXCEPTION_POINTERS;

#define DRIVE_UNKNOWN 0
#define DRIVE_NO_ROOT_DIR 1
#define DRIVE_REMOVABLE 2
#define DRIVE_FIXED 3
#define DRIVE_REMOTE 4
#define DRIVE_CDROM 5
#define DRIVE_RAMDISK 6

#define GetFreeSpace(w) (0x100000L)
#define FILE_TYPE_UNKNOWN 0x0
#define FILE_TYPE_DISK 0x1
#define FILE_TYPE_CHAR 0x2
#define FILE_TYPE_PIPE 0x3
#define FILE_TYPE_REMOTE 0x8000

#define STD_INPUT_HANDLE ((DWORD)-10)
#define STD_OUTPUT_HANDLE ((DWORD)-11)
#define STD_ERROR_HANDLE ((DWORD)-12)

#define NOPARITY 0
#define ODDPARITY 1
#define EVENPARITY 2
#define MARKPARITY 3
#define SPACEPARITY 4

#define ONESTOPBIT 0
#define ONE5STOPBITS 1
#define TWOSTOPBITS 2

#define IGNORE 0
#define INFINITE 0xffffffff

#define CBR_110 110
#define CBR_300 300
#define CBR_600 600
#define CBR_1200 1200
#define CBR_2400 2400
#define CBR_4800 4800
#define CBR_9600 9600
#define CBR_14400 14400
#define CBR_19200 19200
#define CBR_38400 38400
#define CBR_56000 56000
#define CBR_57600 57600
#define CBR_115200 115200
#define CBR_128000 128000
#define CBR_256000 256000

#define CE_RXOVER 0x1
#define CE_OVERRUN 0x2
#define CE_RXPARITY 0x4
#define CE_FRAME 0x8
#define CE_BREAK 0x10
#define CE_TXFULL 0x100
#define CE_PTO 0x200
#define CE_IOE 0x400
#define CE_DNS 0x800
#define CE_OOP 0x1000
#define CE_MODE 0x8000

#define IE_BADID (-1)
#define IE_OPEN (-2)
#define IE_NOPEN (-3)
#define IE_MEMORY (-4)
#define IE_DEFAULT (-5)
#define IE_HARDWARE (-10)
#define IE_BYTESIZE (-11)
#define IE_BAUDRATE (-12)

#define EV_RXCHAR 0x1
#define EV_RXFLAG 0x2
#define EV_TXEMPTY 0x4
#define EV_CTS 0x8
#define EV_DSR 0x10
#define EV_RLSD 0x20
#define EV_BREAK 0x40
#define EV_ERR 0x80
#define EV_RING 0x100
#define EV_PERR 0x200
#define EV_RX80FULL 0x400
#define EV_EVENT1 0x800
#define EV_EVENT2 0x1000

#define SETXOFF 1
#define SETXON 2
#define SETRTS 3
#define CLRRTS 4
#define SETDTR 5
#define CLRDTR 6
#define RESETDEV 7
#define SETBREAK 8
#define CLRBREAK 9

#define PURGE_TXABORT 0x1
#define PURGE_RXABORT 0x2
#define PURGE_TXCLEAR 0x4
#define PURGE_RXCLEAR 0x8

#define LPTx 0x80

#define MS_CTS_ON ((DWORD)0x10)
#define MS_DSR_ON ((DWORD)0x20)
#define MS_RING_ON ((DWORD)0x40)
#define MS_RLSD_ON ((DWORD)0x80)

#define S_QUEUEEMPTY 0
#define S_THRESHOLD 1
#define S_ALLTHRESHOLD 2

#define S_NORMAL 0
#define S_LEGATO 1
#define S_STACCATO 2

#define S_PERIOD512 0
#define S_PERIOD1024 1
#define S_PERIOD2048 2
#define S_PERIODVOICE 3
#define S_WHITE512 4
#define S_WHITE1024 5
#define S_WHITE2048 6
#define S_WHITEVOICE 7

#define S_SERDVNA (-1)
#define S_SEROFM (-2)
#define S_SERMACT (-3)
#define S_SERQFUL (-4)
#define S_SERBDNT (-5)
#define S_SERDLN (-6)
#define S_SERDCC (-7)
#define S_SERDTP (-8)
#define S_SERDVL (-9)
#define S_SERDMD (-10)
#define S_SERDSH (-11)
#define S_SERDPT (-12)
#define S_SERDFQ (-13)
#define S_SERDDR (-14)
#define S_SERDSR (-15)
#define S_SERDST (-16)

#define NMPWAIT_WAIT_FOREVER 0xffffffff
#define NMPWAIT_NOWAIT 0x1
#define NMPWAIT_USE_DEFAULT_WAIT 0x0

#define FS_CASE_IS_PRESERVED FILE_CASE_PRESERVED_NAMES
#define FS_CASE_SENSITIVE FILE_CASE_SENSITIVE_SEARCH
#define FS_UNICODE_STORED_ON_DISK FILE_UNICODE_ON_DISK
#define FS_PERSISTENT_ACLS FILE_PERSISTENT_ACLS
#define FS_VOL_IS_COMPRESSED FILE_VOLUME_IS_COMPRESSED
#define FS_FILE_COMPRESSION FILE_FILE_COMPRESSION
#define FS_FILE_ENCRYPTION FILE_SUPPORTS_ENCRYPTION

#define FILE_MAP_COPY SECTION_QUERY
#define FILE_MAP_WRITE SECTION_MAP_WRITE
#define FILE_MAP_READ SECTION_MAP_READ
#define FILE_MAP_ALL_ACCESS SECTION_ALL_ACCESS
#define FILE_MAP_EXECUTE SECTION_MAP_EXECUTE_EXPLICIT

#define OF_READ 0x0
#define OF_WRITE 0x1
#define OF_READWRITE 0x2
#define OF_SHARE_COMPAT 0x0
#define OF_SHARE_EXCLUSIVE 0x10
#define OF_SHARE_DENY_WRITE 0x20
#define OF_SHARE_DENY_READ 0x30
#define OF_SHARE_DENY_NONE 0x40
#define OF_PARSE 0x100
#define OF_DELETE 0x200
#define OF_VERIFY 0x400
#define OF_CANCEL 0x800
#define OF_CREATE 0x1000
#define OF_PROMPT 0x2000
#define OF_EXIST 0x4000
#define OF_REOPEN 0x8000

#define OFS_MAXPATHNAME 128
  typedef struct _OFSTRUCT {
    BYTE cBytes;
    BYTE fFixedDisk;
    WORD nErrCode;
    WORD Reserved1;
    WORD Reserved2;
    CHAR szPathName[OFS_MAXPATHNAME];
  } OFSTRUCT,*LPOFSTRUCT,*POFSTRUCT;

#ifndef NOWINBASEINTERLOCK

#ifndef _NTOS_

#if defined(__ia64__) && !defined(RC_INVOKED)

#define InterlockedIncrement _InterlockedIncrement
#define InterlockedIncrementAcquire _InterlockedIncrement_acq
#define InterlockedIncrementRelease _InterlockedIncrement_rel
#define InterlockedDecrement _InterlockedDecrement
#define InterlockedDecrementAcquire _InterlockedDecrement_acq
#define InterlockedDecrementRelease _InterlockedDecrement_rel
#define InterlockedExchange _InterlockedExchange
#define InterlockedExchangeAdd _InterlockedExchangeAdd
#define InterlockedCompareExchange _InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire _InterlockedCompareExchange_acq
#define InterlockedCompareExchangeRelease _InterlockedCompareExchange_rel
#define InterlockedExchangePointer _InterlockedExchangePointer
#define InterlockedCompareExchangePointer _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerRelease _InterlockedCompareExchangePointer_rel
#define InterlockedCompareExchangePointerAcquire _InterlockedCompareExchangePointer_acq

#define InterlockedIncrement64 _InterlockedIncrement64
#define InterlockedDecrement64 _InterlockedDecrement64
#define InterlockedExchange64 _InterlockedExchange64
#define InterlockedExchangeAcquire64 _InterlockedExchange64_acq
#define InterlockedExchangeAdd64 _InterlockedExchangeAdd64
#define InterlockedCompareExchange64 _InterlockedCompareExchange64
#define InterlockedCompareExchangeAcquire64 _InterlockedCompareExchange64_acq
#define InterlockedCompareExchangeRelease64 _InterlockedCompareExchange64_rel

  LONGLONG __cdecl InterlockedIncrement64(LONGLONG volatile *Addend);
  LONGLONG __cdecl InterlockedDecrement64(LONGLONG volatile *Addend);
  LONG __cdecl InterlockedIncrementAcquire(LONG volatile *Addend);
  LONG __cdecl InterlockedDecrementAcquire(LONG volatile *Addend);
  LONG __cdecl InterlockedIncrementRelease(LONG volatile *Addend);
  LONG __cdecl InterlockedDecrementRelease(LONG volatile *Addend);
  LONGLONG __cdecl InterlockedExchange64 (LONGLONG volatile *Target,LONGLONG Value);
  LONGLONG __cdecl InterlockedExchangeAcquire64 (LONGLONG volatile *Target,LONGLONG Value);
  LONGLONG __cdecl InterlockedExchangeAdd64 (LONGLONG volatile *Addend,LONGLONG Value);
  LONGLONG __cdecl InterlockedCompareExchange64 (LONGLONG volatile *Destination,LONGLONG ExChange,LONGLONG Comperand);
  LONGLONG __cdecl InterlockedCompareExchangeAcquire64 (LONGLONG volatile *Destination,LONGLONG ExChange,LONGLONG Comperand);
  LONGLONG __cdecl InterlockedCompareExchangeRelease64 (LONGLONG volatile *Destination,LONGLONG ExChange,LONGLONG Comperand);
  LONG __cdecl InterlockedIncrement(LONG volatile *lpAddend);
  LONG __cdecl InterlockedDecrement(LONG volatile *lpAddend);
  LONG __cdecl InterlockedExchange(LONG volatile *Target,LONG Value);
  LONG __cdecl InterlockedExchangeAdd(LONG volatile *Addend,LONG Value);
  LONG __cdecl InterlockedCompareExchange(LONG volatile *Destination,LONG ExChange,LONG Comperand);
  LONG __cdecl InterlockedCompareExchangeRelease(LONG volatile *Destination,LONG ExChange,LONG Comperand);
  LONG __cdecl InterlockedCompareExchangeAcquire(LONG volatile *Destination,LONG ExChange,LONG Comperand);
  PVOID __cdecl InterlockedExchangePointer(PVOID volatile *Target,PVOID Value);
  PVOID __cdecl InterlockedCompareExchangePointer(PVOID volatile *Destination,PVOID ExChange,PVOID Comperand);
  PVOID __cdecl InterlockedCompareExchangePointerAcquire(PVOID volatile *Destination,PVOID Exchange,PVOID Comperand);
  PVOID __cdecl InterlockedCompareExchangePointerRelease(PVOID volatile *Destination,PVOID Exchange,PVOID Comperand);

#ifndef InterlockedAnd
#define InterlockedAnd InterlockedAnd_Inline
  __CRT_INLINE LONG InterlockedAnd_Inline(LONG volatile *Target,LONG Set) {
    LONG i;
    LONG j;
    j = *Target;
    do {
      i = j;
      j = InterlockedCompareExchange(Target,i & Set,i);
    } while(i!=j);
    return j;
  }
#endif

#ifndef InterlockedOr
#define InterlockedOr InterlockedOr_Inline

  __CRT_INLINE LONG InterlockedOr_Inline(LONG volatile *Target,LONG Set) {
    LONG i;
    LONG j;
    j = *Target;
    do {
      i = j;
      j = InterlockedCompareExchange(Target,i | Set,i);
    } while(i!=j);
    return j;
  }
#endif

#ifndef InterlockedXor
#define InterlockedXor InterlockedXor_Inline

  __CRT_INLINE LONG InterlockedXor_Inline(LONG volatile *Target,LONG Set) {
    LONG i;
    LONG j;
    j = *Target;
    do {
      i = j;
      j = InterlockedCompareExchange(Target,i ^ Set,i);
    } while(i!=j);
    return j;
  }
#endif

#ifndef !defined (InterlockedAnd64)
#define InterlockedAnd64 InterlockedAnd64_Inline

  __CRT_INLINE LONGLONG InterlockedAnd64_Inline (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old & Value,Old)!=Old);
    return Old;
  }
#endif

#ifndef InterlockedOr64
#define InterlockedOr64 InterlockedOr64_Inline

  __CRT_INLINE LONGLONG InterlockedOr64_Inline (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old | Value,Old)!=Old);
    return Old;
  }
#endif

#ifndef InterlockedXor64
#define InterlockedXor64 InterlockedXor64_Inline

  __CRT_INLINE LONGLONG InterlockedXor64_Inline (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old ^ Value,Old)!=Old);
    return Old;
  }
#endif

#ifndef InterlockedBitTestAndSet
#define InterlockedBitTestAndSet InterlockedBitTestAndSet_Inline

  __CRT_INLINE BOOLEAN InterlockedBitTestAndSet_Inline(LONG *Base,LONG Bit) {
    LONG tBit;
    tBit = 1<<(Bit & (sizeof (*Base)*8-1));
    return (BOOLEAN)((InterlockedOr(&Base[Bit/(sizeof(*Base)*8)],tBit)&tBit)!=0);
  }
#endif

#ifndef InterlockedBitTestAndReset
#define InterlockedBitTestAndReset InterlockedBitTestAndReset_Inline

  __CRT_INLINE BOOLEAN InterlockedBitTestAndReset_Inline(LONG *Base,LONG Bit) {
    LONG tBit;
    tBit = 1<<(Bit & (sizeof (*Base)*8-1));
    return (BOOLEAN)((InterlockedAnd(&Base[Bit/(sizeof(*Base)*8)],~tBit)&tBit)!=0);
  }
#endif

#ifndef InterlockedBitTestAndComplement
#define InterlockedBitTestAndComplement InterlockedBitTestAndComplement_Inline

  __CRT_INLINE BOOLEAN InterlockedBitTestAndComplement_Inline(LONG *Base,LONG Bit) {
    LONG tBit;
    tBit = 1<<(Bit & (sizeof (*Base)*8-1));
    return (BOOLEAN)((InterlockedXor(&Base[Bit/(sizeof(*Base)*8)],tBit)&tBit)!=0);
  }
#endif
#elif defined(__x86_64) && !defined(RC_INVOKED)

#define InterlockedIncrement _InterlockedIncrement
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedDecrement _InterlockedDecrement
#define InterlockedDecrementAcquire InterlockedDecrement
#define InterlockedDecrementRelease InterlockedDecrement
#define InterlockedExchange _InterlockedExchange
#define InterlockedExchangeAdd _InterlockedExchangeAdd
#define InterlockedCompareExchange _InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire InterlockedCompareExchange
#define InterlockedCompareExchangeRelease InterlockedCompareExchange
#define InterlockedExchangePointer _InterlockedExchangePointer
#define InterlockedCompareExchangePointer _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerAcquire _InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerRelease _InterlockedCompareExchangePointer
#define InterlockedAnd64 _InterlockedAnd64
#define InterlockedOr64 _InterlockedOr64
#define InterlockedXor64 _InterlockedXor64
#define InterlockedIncrement64 _InterlockedIncrement64
#define InterlockedDecrement64 _InterlockedDecrement64
#define InterlockedExchange64 _InterlockedExchange64
#define InterlockedExchangeAdd64 _InterlockedExchangeAdd64
#define InterlockedCompareExchange64 _InterlockedCompareExchange64
#define InterlockedCompareExchangeAcquire64 InterlockedCompareExchange64
#define InterlockedCompareExchangeRelease64 InterlockedCompareExchange64

  LONG InterlockedIncrement(LONG volatile *Addend);
  LONG InterlockedDecrement(LONG volatile *Addend);
  LONG InterlockedExchange(LONG volatile *Target,LONG Value);
  LONG InterlockedExchangeAdd(LONG volatile *Addend,LONG Value);
  LONG InterlockedCompareExchange(LONG volatile *Destination,LONG ExChange,LONG Comperand);
  PVOID InterlockedCompareExchangePointer(PVOID volatile *Destination,PVOID Exchange,PVOID Comperand);
  PVOID InterlockedExchangePointer(PVOID volatile *Target,PVOID Value);
  LONG64 InterlockedAnd64(LONG64 volatile *Destination,LONG64 Value);
  LONG64 InterlockedOr64(LONG64 volatile *Destination,LONG64 Value);
  LONG64 InterlockedXor64(LONG64 volatile *Destination,LONG64 Value);
  LONG64 InterlockedIncrement64(LONG64 volatile *Addend);
  LONG64 InterlockedDecrement64(LONG64 volatile *Addend);
  LONG64 InterlockedExchange64(LONG64 volatile *Target,LONG64 Value);
  LONG64 InterlockedExchangeAdd64(LONG64 volatile *Addend,LONG64 Value);
  LONG64 InterlockedCompareExchange64(LONG64 volatile *Destination,LONG64 ExChange,LONG64 Comperand);
#else
  LONG WINAPI InterlockedIncrement(LONG volatile *lpAddend);
  LONG WINAPI InterlockedDecrement(LONG volatile *lpAddend);
  LONG WINAPI InterlockedExchange(LONG volatile *Target,LONG Value);

#define InterlockedExchangePointer(Target,Value) (PVOID)InterlockedExchange((PLONG)(Target),(LONG)(Value))

  LONG WINAPI InterlockedExchangeAdd(LONG volatile *Addend,LONG Value);
  LONG WINAPI InterlockedCompareExchange(LONG volatile *Destination,LONG Exchange,LONG Comperand);
  LONGLONG WINAPI InterlockedCompareExchange64(LONGLONG volatile *Destination,LONGLONG Exchange,LONGLONG Comperand);

  __CRT_INLINE LONGLONG InterlockedAnd64 (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old & Value,Old)!=Old);
    return Old;
  }

  __CRT_INLINE LONGLONG InterlockedOr64 (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old | Value,Old)!=Old);
    return Old;
  }

  __CRT_INLINE LONGLONG InterlockedXor64 (LONGLONG volatile *Destination,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Destination;
    } while(InterlockedCompareExchange64(Destination,Old ^ Value,Old)!=Old);

    return Old;
  }

  __CRT_INLINE LONGLONG InterlockedIncrement64(LONGLONG volatile *Addend) {
    LONGLONG Old;
    do {
      Old = *Addend;
    } while(InterlockedCompareExchange64(Addend,Old + 1,Old)!=Old);
    return Old + 1;
  }

  __CRT_INLINE LONGLONG InterlockedDecrement64(LONGLONG volatile *Addend) {
    LONGLONG Old;
    do {
      Old = *Addend;
    } while(InterlockedCompareExchange64(Addend,Old - 1,Old)!=Old);
    return Old - 1;
  }

  __CRT_INLINE LONGLONG InterlockedExchange64(LONGLONG volatile *Target,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Target;
    } while(InterlockedCompareExchange64(Target,Value,Old)!=Old);
    return Old;
  }

  __CRT_INLINE LONGLONG InterlockedExchangeAdd64(LONGLONG volatile *Addend,LONGLONG Value) {
    LONGLONG Old;
    do {
      Old = *Addend;
    } while(InterlockedCompareExchange64(Addend,Old + Value,Old)!=Old);
    return Old;
  }

#ifdef __cplusplus
  __CRT_INLINE PVOID __cdecl __InlineInterlockedCompareExchangePointer(PVOID volatile *Destination,PVOID ExChange,PVOID Comperand) {
    return((PVOID)(LONG_PTR)InterlockedCompareExchange((LONG volatile *)Destination,(LONG)(LONG_PTR)ExChange,(LONG)(LONG_PTR)Comperand));
  }
#define InterlockedCompareExchangePointer __InlineInterlockedCompareExchangePointer
#else
#define InterlockedCompareExchangePointer(Destination,ExChange,Comperand)(PVOID)(LONG_PTR)InterlockedCompareExchange((LONG volatile *)(Destination),(LONG)(LONG_PTR)(ExChange),(LONG)(LONG_PTR)(Comperand))
#endif

#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedDecrementAcquire InterlockedDecrement
#define InterlockedDecrementRelease InterlockedDecrement
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedCompareExchangeAcquire InterlockedCompareExchange
#define InterlockedCompareExchangeRelease InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire64 InterlockedCompareExchange64
#define InterlockedCompareExchangeRelease64 InterlockedCompareExchange64
#define InterlockedCompareExchangePointerAcquire InterlockedCompareExchangePointer
#define InterlockedCompareExchangePointerRelease InterlockedCompareExchangePointer
#endif

#if defined(_SLIST_HEADER_) && !defined(_NTOSP_)
  WINBASEAPI VOID WINAPI InitializeSListHead(PSLIST_HEADER ListHead);
  WINBASEAPI PSLIST_ENTRY WINAPI InterlockedPopEntrySList(PSLIST_HEADER ListHead);
  WINBASEAPI PSLIST_ENTRY WINAPI InterlockedPushEntrySList(PSLIST_HEADER ListHead,PSLIST_ENTRY ListEntry);
  WINBASEAPI PSLIST_ENTRY WINAPI InterlockedFlushSList(PSLIST_HEADER ListHead);
  WINBASEAPI USHORT WINAPI QueryDepthSList(PSLIST_HEADER ListHead);
#endif
#endif
#endif

  WINBASEAPI WINBOOL WINAPI FreeResource(HGLOBAL hResData);
  WINBASEAPI LPVOID WINAPI LockResource(HGLOBAL hResData);

#define UnlockResource(hResData) ((hResData),0)
#define MAXINTATOM 0xC000
#define MAKEINTATOM(i) (LPTSTR)((ULONG_PTR)((WORD)(i)))
#define INVALID_ATOM ((ATOM)0)

  int WINAPI WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,LPSTR lpCmdLine,int nShowCmd);
  WINBASEAPI WINBOOL WINAPI FreeLibrary(HMODULE hLibModule);
  WINBASEAPI DECLSPEC_NORETURN VOID WINAPI FreeLibraryAndExitThread(HMODULE hLibModule,DWORD dwExitCode);
  WINBASEAPI WINBOOL WINAPI DisableThreadLibraryCalls(HMODULE hLibModule);
  WINBASEAPI FARPROC WINAPI GetProcAddress(HMODULE hModule,LPCSTR lpProcName);
  WINBASEAPI DWORD WINAPI GetVersion(VOID);
  WINBASEAPI HGLOBAL WINAPI GlobalAlloc(UINT uFlags,SIZE_T dwBytes);
  WINBASEAPI HGLOBAL WINAPI GlobalReAlloc(HGLOBAL hMem,SIZE_T dwBytes,UINT uFlags);
  WINBASEAPI SIZE_T WINAPI GlobalSize(HGLOBAL hMem);
  WINBASEAPI UINT WINAPI GlobalFlags(HGLOBAL hMem);
  WINBASEAPI LPVOID WINAPI GlobalLock(HGLOBAL hMem);
  WINBASEAPI HGLOBAL WINAPI GlobalHandle(LPCVOID pMem);
  WINBASEAPI WINBOOL WINAPI GlobalUnlock(HGLOBAL hMem);
  WINBASEAPI HGLOBAL WINAPI GlobalFree(HGLOBAL hMem);
  WINBASEAPI SIZE_T WINAPI GlobalCompact(DWORD dwMinFree);
  WINBASEAPI VOID WINAPI GlobalFix(HGLOBAL hMem);
  WINBASEAPI VOID WINAPI GlobalUnfix(HGLOBAL hMem);
  WINBASEAPI LPVOID WINAPI GlobalWire(HGLOBAL hMem);
  WINBASEAPI WINBOOL WINAPI GlobalUnWire(HGLOBAL hMem);
  WINBASEAPI VOID WINAPI GlobalMemoryStatus(LPMEMORYSTATUS lpBuffer);

  typedef struct _MEMORYSTATUSEX {
    DWORD dwLength;
    DWORD dwMemoryLoad;
    DWORDLONG ullTotalPhys;
    DWORDLONG ullAvailPhys;
    DWORDLONG ullTotalPageFile;
    DWORDLONG ullAvailPageFile;
    DWORDLONG ullTotalVirtual;
    DWORDLONG ullAvailVirtual;
    DWORDLONG ullAvailExtendedVirtual;
  } MEMORYSTATUSEX,*LPMEMORYSTATUSEX;

  WINBASEAPI WINBOOL WINAPI GlobalMemoryStatusEx(LPMEMORYSTATUSEX lpBuffer);
  WINBASEAPI HLOCAL WINAPI LocalAlloc(UINT uFlags,SIZE_T uBytes);
  WINBASEAPI HLOCAL WINAPI LocalReAlloc(HLOCAL hMem,SIZE_T uBytes,UINT uFlags);
  WINBASEAPI LPVOID WINAPI LocalLock(HLOCAL hMem);
  WINBASEAPI HLOCAL WINAPI LocalHandle(LPCVOID pMem);
  WINBASEAPI WINBOOL WINAPI LocalUnlock(HLOCAL hMem);
  WINBASEAPI SIZE_T WINAPI LocalSize(HLOCAL hMem);
  WINBASEAPI UINT WINAPI LocalFlags(HLOCAL hMem);
  WINBASEAPI HLOCAL WINAPI LocalFree(HLOCAL hMem);
  WINBASEAPI SIZE_T WINAPI LocalShrink(HLOCAL hMem,UINT cbNewSize);
  WINBASEAPI SIZE_T WINAPI LocalCompact(UINT uMinFree);
  WINBASEAPI WINBOOL WINAPI FlushInstructionCache(HANDLE hProcess,LPCVOID lpBaseAddress,SIZE_T dwSize);
  WINBASEAPI LPVOID WINAPI VirtualAlloc(LPVOID lpAddress,SIZE_T dwSize,DWORD flAllocationType,DWORD flProtect);
  WINBASEAPI WINBOOL WINAPI VirtualFree(LPVOID lpAddress,SIZE_T dwSize,DWORD dwFreeType);
  WINBASEAPI WINBOOL WINAPI VirtualProtect(LPVOID lpAddress,SIZE_T dwSize,DWORD flNewProtect,PDWORD lpflOldProtect);
  WINBASEAPI SIZE_T WINAPI VirtualQuery(LPCVOID lpAddress,PMEMORY_BASIC_INFORMATION lpBuffer,SIZE_T dwLength);
  WINBASEAPI LPVOID WINAPI VirtualAllocEx(HANDLE hProcess,LPVOID lpAddress,SIZE_T dwSize,DWORD flAllocationType,DWORD flProtect);
  WINBASEAPI UINT WINAPI GetWriteWatch(DWORD dwFlags,PVOID lpBaseAddress,SIZE_T dwRegionSize,PVOID *lpAddresses,ULONG_PTR *lpdwCount,PULONG lpdwGranularity);
  WINBASEAPI UINT WINAPI ResetWriteWatch(LPVOID lpBaseAddress,SIZE_T dwRegionSize);
  WINBASEAPI SIZE_T WINAPI GetLargePageMinimum(VOID);
  WINBASEAPI UINT WINAPI EnumSystemFirmwareTables(DWORD FirmwareTableProviderSignature,PVOID pFirmwareTableEnumBuffer,DWORD BufferSize);
  WINBASEAPI UINT WINAPI GetSystemFirmwareTable(DWORD FirmwareTableProviderSignature,DWORD FirmwareTableID,PVOID pFirmwareTableBuffer,DWORD BufferSize);
  WINBASEAPI WINBOOL WINAPI VirtualFreeEx(HANDLE hProcess,LPVOID lpAddress,SIZE_T dwSize,DWORD dwFreeType);
  WINBASEAPI WINBOOL WINAPI VirtualProtectEx(HANDLE hProcess,LPVOID lpAddress,SIZE_T dwSize,DWORD flNewProtect,PDWORD lpflOldProtect);
  WINBASEAPI SIZE_T WINAPI VirtualQueryEx(HANDLE hProcess,LPCVOID lpAddress,PMEMORY_BASIC_INFORMATION lpBuffer,SIZE_T dwLength);
  WINBASEAPI HANDLE WINAPI HeapCreate(DWORD flOptions,SIZE_T dwInitialSize,SIZE_T dwMaximumSize);
  WINBASEAPI WINBOOL WINAPI HeapDestroy(HANDLE hHeap);
  WINBASEAPI LPVOID WINAPI HeapAlloc(HANDLE hHeap,DWORD dwFlags,SIZE_T dwBytes);
  WINBASEAPI LPVOID WINAPI HeapReAlloc(HANDLE hHeap,DWORD dwFlags,LPVOID lpMem,SIZE_T dwBytes);
  WINBASEAPI WINBOOL WINAPI HeapFree(HANDLE hHeap,DWORD dwFlags,LPVOID lpMem);
  WINBASEAPI SIZE_T WINAPI HeapSize(HANDLE hHeap,DWORD dwFlags,LPCVOID lpMem);
  WINBASEAPI WINBOOL WINAPI HeapValidate(HANDLE hHeap,DWORD dwFlags,LPCVOID lpMem);
  WINBASEAPI SIZE_T WINAPI HeapCompact(HANDLE hHeap,DWORD dwFlags);
  WINBASEAPI HANDLE WINAPI GetProcessHeap(VOID);
  WINBASEAPI DWORD WINAPI GetProcessHeaps(DWORD NumberOfHeaps,PHANDLE ProcessHeaps);

  typedef struct _PROCESS_HEAP_ENTRY {
    PVOID lpData;
    DWORD cbData;
    BYTE cbOverhead;
    BYTE iRegionIndex;
    WORD wFlags;
    union {
      struct {
	HANDLE hMem;
	DWORD dwReserved[3];
      } Block;
      struct {
	DWORD dwCommittedSize;
	DWORD dwUnCommittedSize;
	LPVOID lpFirstBlock;
	LPVOID lpLastBlock;
      } Region;
    };
  } PROCESS_HEAP_ENTRY,*LPPROCESS_HEAP_ENTRY,*PPROCESS_HEAP_ENTRY;

#define PROCESS_HEAP_REGION 0x1
#define PROCESS_HEAP_UNCOMMITTED_RANGE 0x2
#define PROCESS_HEAP_ENTRY_BUSY 0x4
#define PROCESS_HEAP_ENTRY_MOVEABLE 0x10
#define PROCESS_HEAP_ENTRY_DDESHARE 0x20

  WINBASEAPI WINBOOL WINAPI HeapLock(HANDLE hHeap);
  WINBASEAPI WINBOOL WINAPI HeapUnlock(HANDLE hHeap);
  WINBASEAPI WINBOOL WINAPI HeapWalk(HANDLE hHeap,LPPROCESS_HEAP_ENTRY lpEntry);
  WINBASEAPI WINBOOL WINAPI HeapSetInformation(HANDLE HeapHandle,HEAP_INFORMATION_CLASS HeapInformationClass,PVOID HeapInformation,SIZE_T HeapInformationLength);
  WINBASEAPI WINBOOL WINAPI HeapQueryInformation(HANDLE HeapHandle,HEAP_INFORMATION_CLASS HeapInformationClass,PVOID HeapInformation,SIZE_T HeapInformationLength,PSIZE_T ReturnLength);

#define SCS_32BIT_BINARY 0
#define SCS_DOS_BINARY 1
#define SCS_WOW_BINARY 2
#define SCS_PIF_BINARY 3
#define SCS_POSIX_BINARY 4
#define SCS_OS216_BINARY 5
#define SCS_64BIT_BINARY 6

#ifdef UNICODE
#define GetBinaryType GetBinaryTypeW
#define GetShortPathName GetShortPathNameW
#define GetLongPathName GetLongPathNameW
#define GetEnvironmentStrings GetEnvironmentStringsW
#define SetEnvironmentStrings SetEnvironmentStringsW
#define FreeEnvironmentStrings FreeEnvironmentStringsW
#else
#define GetBinaryType GetBinaryTypeA
#define GetShortPathName GetShortPathNameA
#define GetLongPathName GetLongPathNameA
#define GetEnvironmentStringsA GetEnvironmentStrings
#define SetEnvironmentStrings SetEnvironmentStringsA
#define FreeEnvironmentStrings FreeEnvironmentStringsA
#endif

#ifdef _WIN64
#define SCS_THIS_PLATFORM_BINARY SCS_64BIT_BINARY
#else
#define SCS_THIS_PLATFORM_BINARY SCS_32BIT_BINARY
#endif

  WINBASEAPI WINBOOL WINAPI GetBinaryTypeA(LPCSTR lpApplicationName,LPDWORD lpBinaryType);
  WINBASEAPI WINBOOL WINAPI GetBinaryTypeW(LPCWSTR lpApplicationName,LPDWORD lpBinaryType);
  WINBASEAPI DWORD WINAPI GetShortPathNameA(LPCSTR lpszLongPath,LPSTR lpszShortPath,DWORD cchBuffer);
  WINBASEAPI DWORD WINAPI GetShortPathNameW(LPCWSTR lpszLongPath,LPWSTR lpszShortPath,DWORD cchBuffer);
  WINBASEAPI DWORD WINAPI GetLongPathNameA(LPCSTR lpszShortPath,LPSTR lpszLongPath,DWORD cchBuffer);
  WINBASEAPI DWORD WINAPI GetLongPathNameW(LPCWSTR lpszShortPath,LPWSTR lpszLongPath,DWORD cchBuffer);
  WINBASEAPI WINBOOL WINAPI GetProcessAffinityMask(HANDLE hProcess,PDWORD_PTR lpProcessAffinityMask,PDWORD_PTR lpSystemAffinityMask);
  WINBASEAPI WINBOOL WINAPI SetProcessAffinityMask(HANDLE hProcess,DWORD_PTR dwProcessAffinityMask);
  WINBASEAPI WINBOOL WINAPI GetProcessHandleCount(HANDLE hProcess,PDWORD pdwHandleCount);
  WINBASEAPI WINBOOL WINAPI GetProcessTimes(HANDLE hProcess,LPFILETIME lpCreationTime,LPFILETIME lpExitTime,LPFILETIME lpKernelTime,LPFILETIME lpUserTime);
  WINBASEAPI WINBOOL WINAPI GetProcessIoCounters(HANDLE hProcess,PIO_COUNTERS lpIoCounters);
  WINBASEAPI WINBOOL WINAPI GetProcessWorkingSetSize(HANDLE hProcess,PSIZE_T lpMinimumWorkingSetSize,PSIZE_T lpMaximumWorkingSetSize);
  WINBASEAPI WINBOOL WINAPI GetProcessWorkingSetSizeEx(HANDLE hProcess,PSIZE_T lpMinimumWorkingSetSize,PSIZE_T lpMaximumWorkingSetSize,PDWORD Flags);
  WINBASEAPI WINBOOL WINAPI SetProcessWorkingSetSize(HANDLE hProcess,SIZE_T dwMinimumWorkingSetSize,SIZE_T dwMaximumWorkingSetSize);
  WINBASEAPI WINBOOL WINAPI SetProcessWorkingSetSizeEx(HANDLE hProcess,SIZE_T dwMinimumWorkingSetSize,SIZE_T dwMaximumWorkingSetSize,DWORD Flags);
  WINBASEAPI HANDLE WINAPI OpenProcess(DWORD dwDesiredAccess,WINBOOL bInheritHandle,DWORD dwProcessId);
  WINBASEAPI HANDLE WINAPI GetCurrentProcess(VOID);
  WINBASEAPI DWORD WINAPI GetCurrentProcessId(VOID);
  WINBASEAPI DECLSPEC_NORETURN VOID WINAPI ExitProcess(UINT uExitCode);
  WINBASEAPI WINBOOL WINAPI TerminateProcess(HANDLE hProcess,UINT uExitCode);
  WINBASEAPI WINBOOL WINAPI GetExitCodeProcess(HANDLE hProcess,LPDWORD lpExitCode);
  WINBASEAPI VOID WINAPI FatalExit(int ExitCode);
  /*	WINBASEAPI LPCH WINAPI GetEnvironmentStrings(VOID); */
  WINBASEAPI LPWCH WINAPI GetEnvironmentStringsW(VOID);
  WINBASEAPI WINBOOL WINAPI SetEnvironmentStringsA(LPCH NewEnvironment);
  WINBASEAPI WINBOOL WINAPI SetEnvironmentStringsW(LPWCH NewEnvironment);
  WINBASEAPI WINBOOL WINAPI FreeEnvironmentStringsA(LPCH);
  WINBASEAPI WINBOOL WINAPI FreeEnvironmentStringsW(LPWCH);
  WINBASEAPI VOID WINAPI RaiseException(DWORD dwExceptionCode,DWORD dwExceptionFlags,DWORD nNumberOfArguments,CONST ULONG_PTR *lpArguments);
  WINBASEAPI LONG WINAPI UnhandledExceptionFilter(struct _EXCEPTION_POINTERS *ExceptionInfo);

  typedef LONG (WINAPI *PTOP_LEVEL_EXCEPTION_FILTER)(struct _EXCEPTION_POINTERS *ExceptionInfo);
  typedef PTOP_LEVEL_EXCEPTION_FILTER LPTOP_LEVEL_EXCEPTION_FILTER;

  WINBASEAPI LPTOP_LEVEL_EXCEPTION_FILTER WINAPI SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER lpTopLevelExceptionFilter);

#define FIBER_FLAG_FLOAT_SWITCH 0x1

  WINBASEAPI LPVOID WINAPI CreateFiber(SIZE_T dwStackSize,LPFIBER_START_ROUTINE lpStartAddress,LPVOID lpParameter);
  WINBASEAPI LPVOID WINAPI CreateFiberEx(SIZE_T dwStackCommitSize,SIZE_T dwStackReserveSize,DWORD dwFlags,LPFIBER_START_ROUTINE lpStartAddress,LPVOID lpParameter);
  WINBASEAPI VOID WINAPI DeleteFiber(LPVOID lpFiber);
  WINBASEAPI LPVOID WINAPI ConvertThreadToFiber(LPVOID lpParameter);
  WINBASEAPI LPVOID WINAPI ConvertThreadToFiberEx(LPVOID lpParameter,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI ConvertFiberToThread(VOID);
  WINBASEAPI VOID WINAPI SwitchToFiber(LPVOID lpFiber);
  WINBASEAPI WINBOOL WINAPI SwitchToThread(VOID);
  WINBASEAPI HANDLE WINAPI CreateThread(LPSECURITY_ATTRIBUTES lpThreadAttributes,SIZE_T dwStackSize,LPTHREAD_START_ROUTINE lpStartAddress,LPVOID lpParameter,DWORD dwCreationFlags,LPDWORD lpThreadId);
  WINBASEAPI HANDLE WINAPI CreateRemoteThread(HANDLE hProcess,LPSECURITY_ATTRIBUTES lpThreadAttributes,SIZE_T dwStackSize,LPTHREAD_START_ROUTINE lpStartAddress,LPVOID lpParameter,DWORD dwCreationFlags,LPDWORD lpThreadId);
  WINBASEAPI HANDLE WINAPI GetCurrentThread(VOID);
  WINBASEAPI DWORD WINAPI GetCurrentThreadId(VOID);
  WINBASEAPI WINBOOL WINAPI SetThreadStackGuarantee (PULONG StackSizeInBytes);
  WINBASEAPI DWORD WINAPI GetProcessIdOfThread(HANDLE Thread);
  WINBASEAPI DWORD WINAPI GetThreadId(HANDLE Thread);
  WINBASEAPI DWORD WINAPI GetProcessId(HANDLE Process);
  WINBASEAPI DWORD WINAPI GetCurrentProcessorNumber(VOID);
  WINBASEAPI DWORD_PTR WINAPI SetThreadAffinityMask(HANDLE hThread,DWORD_PTR dwThreadAffinityMask);
  WINBASEAPI DWORD WINAPI SetThreadIdealProcessor(HANDLE hThread,DWORD dwIdealProcessor);
  WINBASEAPI WINBOOL WINAPI SetProcessPriorityBoost(HANDLE hProcess,WINBOOL bDisablePriorityBoost);
  WINBASEAPI WINBOOL WINAPI GetProcessPriorityBoost(HANDLE hProcess,PBOOL pDisablePriorityBoost);
  WINBASEAPI WINBOOL WINAPI RequestWakeupLatency(LATENCY_TIME latency);
  WINBASEAPI WINBOOL WINAPI IsSystemResumeAutomatic(VOID);
  WINBASEAPI HANDLE WINAPI OpenThread(DWORD dwDesiredAccess,WINBOOL bInheritHandle,DWORD dwThreadId);
  WINBASEAPI WINBOOL WINAPI SetThreadPriority(HANDLE hThread,int nPriority);
  WINBASEAPI WINBOOL WINAPI SetThreadPriorityBoost(HANDLE hThread,WINBOOL bDisablePriorityBoost);
  WINBASEAPI WINBOOL WINAPI GetThreadPriorityBoost(HANDLE hThread,PBOOL pDisablePriorityBoost);
  WINBASEAPI int WINAPI GetThreadPriority(HANDLE hThread);
  WINBASEAPI WINBOOL WINAPI GetThreadTimes(HANDLE hThread,LPFILETIME lpCreationTime,LPFILETIME lpExitTime,LPFILETIME lpKernelTime,LPFILETIME lpUserTime);
  WINBASEAPI WINBOOL WINAPI GetThreadIOPendingFlag(HANDLE hThread,PBOOL lpIOIsPending);
  WINBASEAPI DECLSPEC_NORETURN VOID WINAPI ExitThread(DWORD dwExitCode);
  WINBASEAPI WINBOOL WINAPI TerminateThread(HANDLE hThread,DWORD dwExitCode);
  WINBASEAPI WINBOOL WINAPI GetExitCodeThread(HANDLE hThread,LPDWORD lpExitCode);
  WINBASEAPI WINBOOL WINAPI GetThreadSelectorEntry(HANDLE hThread,DWORD dwSelector,LPLDT_ENTRY lpSelectorEntry);
  WINBASEAPI EXECUTION_STATE WINAPI SetThreadExecutionState(EXECUTION_STATE esFlags);
  WINBASEAPI DWORD WINAPI GetLastError(VOID);
  WINBASEAPI VOID WINAPI SetLastError(DWORD dwErrCode);

#ifndef RC_INVOKED
#ifdef WINBASE_DECLARE_RESTORE_LAST_ERROR
  WINBASEAPI VOID WINAPI RestoreLastError(DWORD dwErrCode);

  typedef VOID (WINAPI *PRESTORE_LAST_ERROR)(DWORD);

#define RESTORE_LAST_ERROR_NAME_A "RestoreLastError"
#define RESTORE_LAST_ERROR_NAME_W L"RestoreLastError"
#define RESTORE_LAST_ERROR_NAME TEXT("RestoreLastError")
#endif
#endif

#define HasOverlappedIoCompleted(lpOverlapped) (((DWORD)(lpOverlapped)->Internal)!=STATUS_PENDING)

  WINBASEAPI WINBOOL WINAPI GetOverlappedResult(HANDLE hFile,LPOVERLAPPED lpOverlapped,LPDWORD lpNumberOfBytesTransferred,WINBOOL bWait);
  WINBASEAPI HANDLE WINAPI CreateIoCompletionPort(HANDLE FileHandle,HANDLE ExistingCompletionPort,ULONG_PTR CompletionKey,DWORD NumberOfConcurrentThreads);
  WINBASEAPI WINBOOL WINAPI GetQueuedCompletionStatus(HANDLE CompletionPort,LPDWORD lpNumberOfBytesTransferred,PULONG_PTR lpCompletionKey,LPOVERLAPPED *lpOverlapped,DWORD dwMilliseconds);
  WINBASEAPI WINBOOL WINAPI PostQueuedCompletionStatus(HANDLE CompletionPort,DWORD dwNumberOfBytesTransferred,ULONG_PTR dwCompletionKey,LPOVERLAPPED lpOverlapped);

#define SEM_FAILCRITICALERRORS 0x1
#define SEM_NOGPFAULTERRORBOX 0x2
#define SEM_NOALIGNMENTFAULTEXCEPT 0x4
#define SEM_NOOPENFILEERRORBOX 0x8000

  WINBASEAPI UINT WINAPI SetErrorMode(UINT uMode);
  WINBASEAPI WINBOOL WINAPI ReadProcessMemory(HANDLE hProcess,LPCVOID lpBaseAddress,LPVOID lpBuffer,SIZE_T nSize,SIZE_T *lpNumberOfBytesRead);
  WINBASEAPI WINBOOL WINAPI WriteProcessMemory(HANDLE hProcess,LPVOID lpBaseAddress,LPCVOID lpBuffer,SIZE_T nSize,SIZE_T *lpNumberOfBytesWritten);
  WINBASEAPI WINBOOL WINAPI GetThreadContext(HANDLE hThread,LPCONTEXT lpContext);
  WINBASEAPI WINBOOL WINAPI SetThreadContext(HANDLE hThread,CONST CONTEXT *lpContext);
  WINBASEAPI DWORD WINAPI SuspendThread(HANDLE hThread);
  WINBASEAPI DWORD WINAPI ResumeThread(HANDLE hThread);

  typedef VOID (WINAPI *PAPCFUNC)(ULONG_PTR dwParam);

  WINBASEAPI DWORD WINAPI QueueUserAPC(PAPCFUNC pfnAPC,HANDLE hThread,ULONG_PTR dwData);
  WINBASEAPI WINBOOL WINAPI IsDebuggerPresent(VOID);
  WINBASEAPI WINBOOL WINAPI CheckRemoteDebuggerPresent(HANDLE hProcess,PBOOL pbDebuggerPresent);
  WINBASEAPI VOID WINAPI DebugBreak(VOID);
  WINBASEAPI WINBOOL WINAPI WaitForDebugEvent(LPDEBUG_EVENT lpDebugEvent,DWORD dwMilliseconds);
  WINBASEAPI WINBOOL WINAPI ContinueDebugEvent(DWORD dwProcessId,DWORD dwThreadId,DWORD dwContinueStatus);
  WINBASEAPI WINBOOL WINAPI DebugActiveProcess(DWORD dwProcessId);
  WINBASEAPI WINBOOL WINAPI DebugActiveProcessStop(DWORD dwProcessId);
  WINBASEAPI WINBOOL WINAPI DebugSetProcessKillOnExit(WINBOOL KillOnExit);
  WINBASEAPI WINBOOL WINAPI DebugBreakProcess(HANDLE Process);
  WINBASEAPI VOID WINAPI InitializeCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
  WINBASEAPI VOID WINAPI EnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
  WINBASEAPI VOID WINAPI LeaveCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
  WINBASEAPI WINBOOL WINAPI InitializeCriticalSectionAndSpinCount(LPCRITICAL_SECTION lpCriticalSection,DWORD dwSpinCount);
  WINBASEAPI DWORD WINAPI SetCriticalSectionSpinCount(LPCRITICAL_SECTION lpCriticalSection,DWORD dwSpinCount);
  WINBASEAPI WINBOOL WINAPI TryEnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
  WINBASEAPI VOID WINAPI DeleteCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
  WINBASEAPI WINBOOL WINAPI SetEvent(HANDLE hEvent);
  WINBASEAPI WINBOOL WINAPI ResetEvent(HANDLE hEvent);
  WINBASEAPI WINBOOL WINAPI PulseEvent(HANDLE hEvent);
  WINBASEAPI WINBOOL WINAPI ReleaseSemaphore(HANDLE hSemaphore,LONG lReleaseCount,LPLONG lpPreviousCount);
  WINBASEAPI WINBOOL WINAPI ReleaseMutex(HANDLE hMutex);
  WINBASEAPI DWORD WINAPI WaitForSingleObject(HANDLE hHandle,DWORD dwMilliseconds);
  WINBASEAPI DWORD WINAPI WaitForMultipleObjects(DWORD nCount,CONST HANDLE *lpHandles,WINBOOL bWaitAll,DWORD dwMilliseconds);
  WINBASEAPI VOID WINAPI Sleep(DWORD dwMilliseconds);
  WINBASEAPI HGLOBAL WINAPI LoadResource(HMODULE hModule,HRSRC hResInfo);
  WINBASEAPI DWORD WINAPI SizeofResource(HMODULE hModule,HRSRC hResInfo);
  WINBASEAPI ATOM WINAPI GlobalDeleteAtom(ATOM nAtom);
  WINBASEAPI WINBOOL WINAPI InitAtomTable(DWORD nSize);
  WINBASEAPI ATOM WINAPI DeleteAtom(ATOM nAtom);
  WINBASEAPI UINT WINAPI SetHandleCount(UINT uNumber);
  WINBASEAPI DWORD WINAPI GetLogicalDrives(VOID);
  WINBASEAPI WINBOOL WINAPI LockFile(HANDLE hFile,DWORD dwFileOffsetLow,DWORD dwFileOffsetHigh,DWORD nNumberOfBytesToLockLow,DWORD nNumberOfBytesToLockHigh);
  WINBASEAPI WINBOOL WINAPI UnlockFile(HANDLE hFile,DWORD dwFileOffsetLow,DWORD dwFileOffsetHigh,DWORD nNumberOfBytesToUnlockLow,DWORD nNumberOfBytesToUnlockHigh);
  WINBASEAPI WINBOOL WINAPI LockFileEx(HANDLE hFile,DWORD dwFlags,DWORD dwReserved,DWORD nNumberOfBytesToLockLow,DWORD nNumberOfBytesToLockHigh,LPOVERLAPPED lpOverlapped);

#define LOCKFILE_FAIL_IMMEDIATELY 0x1
#define LOCKFILE_EXCLUSIVE_LOCK 0x2

  WINBASEAPI WINBOOL WINAPI UnlockFileEx(HANDLE hFile,DWORD dwReserved,DWORD nNumberOfBytesToUnlockLow,DWORD nNumberOfBytesToUnlockHigh,LPOVERLAPPED lpOverlapped);

  typedef struct _BY_HANDLE_FILE_INFORMATION {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD dwVolumeSerialNumber;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD nNumberOfLinks;
    DWORD nFileIndexHigh;
    DWORD nFileIndexLow;
  } BY_HANDLE_FILE_INFORMATION,*PBY_HANDLE_FILE_INFORMATION,*LPBY_HANDLE_FILE_INFORMATION;

#ifdef UNICODE
#define SetFileShortName SetFileShortNameW
#else
#define SetFileShortName SetFileShortNameA
#endif

  WINBASEAPI WINBOOL WINAPI GetFileInformationByHandle(HANDLE hFile,LPBY_HANDLE_FILE_INFORMATION lpFileInformation);
  WINBASEAPI DWORD WINAPI GetFileType(HANDLE hFile);
  WINBASEAPI DWORD WINAPI GetFileSize(HANDLE hFile,LPDWORD lpFileSizeHigh);
  WINBASEAPI WINBOOL WINAPI GetFileSizeEx(HANDLE hFile,PLARGE_INTEGER lpFileSize);
  WINBASEAPI HANDLE WINAPI GetStdHandle(DWORD nStdHandle);
  WINBASEAPI WINBOOL WINAPI SetStdHandle(DWORD nStdHandle,HANDLE hHandle);
  WINBASEAPI WINBOOL WINAPI WriteFile(HANDLE hFile,LPCVOID lpBuffer,DWORD nNumberOfBytesToWrite,LPDWORD lpNumberOfBytesWritten,LPOVERLAPPED lpOverlapped);
  WINBASEAPI WINBOOL WINAPI ReadFile(HANDLE hFile,LPVOID lpBuffer,DWORD nNumberOfBytesToRead,LPDWORD lpNumberOfBytesRead,LPOVERLAPPED lpOverlapped);
  WINBASEAPI WINBOOL WINAPI FlushFileBuffers(HANDLE hFile);
  WINBASEAPI WINBOOL WINAPI DeviceIoControl(HANDLE hDevice,DWORD dwIoControlCode,LPVOID lpInBuffer,DWORD nInBufferSize,LPVOID lpOutBuffer,DWORD nOutBufferSize,LPDWORD lpBytesReturned,LPOVERLAPPED lpOverlapped);
  WINBASEAPI WINBOOL WINAPI RequestDeviceWakeup(HANDLE hDevice);
  WINBASEAPI WINBOOL WINAPI CancelDeviceWakeupRequest(HANDLE hDevice);
  WINBASEAPI WINBOOL WINAPI GetDevicePowerState(HANDLE hDevice,WINBOOL *pfOn);
  WINBASEAPI WINBOOL WINAPI SetMessageWaitingIndicator(HANDLE hMsgIndicator,ULONG ulMsgCount);
  WINBASEAPI WINBOOL WINAPI SetEndOfFile(HANDLE hFile);
  WINBASEAPI DWORD WINAPI SetFilePointer(HANDLE hFile,LONG lDistanceToMove,PLONG lpDistanceToMoveHigh,DWORD dwMoveMethod);
  WINBASEAPI WINBOOL WINAPI SetFilePointerEx(HANDLE hFile,LARGE_INTEGER liDistanceToMove,PLARGE_INTEGER lpNewFilePointer,DWORD dwMoveMethod);
  WINBASEAPI WINBOOL WINAPI FindClose(HANDLE hFindFile);
  WINBASEAPI WINBOOL WINAPI GetFileTime(HANDLE hFile,LPFILETIME lpCreationTime,LPFILETIME lpLastAccessTime,LPFILETIME lpLastWriteTime);
  WINBASEAPI WINBOOL WINAPI SetFileTime(HANDLE hFile,CONST FILETIME *lpCreationTime,CONST FILETIME *lpLastAccessTime,CONST FILETIME *lpLastWriteTime);
  WINBASEAPI WINBOOL WINAPI SetFileValidData(HANDLE hFile,LONGLONG ValidDataLength);
  WINBASEAPI WINBOOL WINAPI SetFileShortNameA(HANDLE hFile,LPCSTR lpShortName);
  WINBASEAPI WINBOOL WINAPI SetFileShortNameW(HANDLE hFile,LPCWSTR lpShortName);
  WINBASEAPI WINBOOL WINAPI CloseHandle(HANDLE hObject);
  WINBASEAPI WINBOOL WINAPI DuplicateHandle(HANDLE hSourceProcessHandle,HANDLE hSourceHandle,HANDLE hTargetProcessHandle,LPHANDLE lpTargetHandle,DWORD dwDesiredAccess,WINBOOL bInheritHandle,DWORD dwOptions);
  WINBASEAPI WINBOOL WINAPI GetHandleInformation(HANDLE hObject,LPDWORD lpdwFlags);
  WINBASEAPI WINBOOL WINAPI SetHandleInformation(HANDLE hObject,DWORD dwMask,DWORD dwFlags);

#define HANDLE_FLAG_INHERIT 0x1
#define HANDLE_FLAG_PROTECT_FROM_CLOSE 0x2

#define HINSTANCE_ERROR 32

  WINBASEAPI DWORD WINAPI LoadModule(LPCSTR lpModuleName,LPVOID lpParameterBlock);
  WINBASEAPI UINT WINAPI WinExec(LPCSTR lpCmdLine,UINT uCmdShow);
  WINBASEAPI WINBOOL WINAPI ClearCommBreak(HANDLE hFile);
  WINBASEAPI WINBOOL WINAPI ClearCommError(HANDLE hFile,LPDWORD lpErrors,LPCOMSTAT lpStat);
  WINBASEAPI WINBOOL WINAPI SetupComm(HANDLE hFile,DWORD dwInQueue,DWORD dwOutQueue);
  WINBASEAPI WINBOOL WINAPI EscapeCommFunction(HANDLE hFile,DWORD dwFunc);
  WINBASEAPI WINBOOL WINAPI GetCommConfig(HANDLE hCommDev,LPCOMMCONFIG lpCC,LPDWORD lpdwSize);
  WINBASEAPI WINBOOL WINAPI GetCommMask(HANDLE hFile,LPDWORD lpEvtMask);
  WINBASEAPI WINBOOL WINAPI GetCommProperties(HANDLE hFile,LPCOMMPROP lpCommProp);
  WINBASEAPI WINBOOL WINAPI GetCommModemStatus(HANDLE hFile,LPDWORD lpModemStat);
  WINBASEAPI WINBOOL WINAPI GetCommState(HANDLE hFile,LPDCB lpDCB);
  WINBASEAPI WINBOOL WINAPI GetCommTimeouts(HANDLE hFile,LPCOMMTIMEOUTS lpCommTimeouts);
  WINBASEAPI WINBOOL WINAPI PurgeComm(HANDLE hFile,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI SetCommBreak(HANDLE hFile);
  WINBASEAPI WINBOOL WINAPI SetCommConfig(HANDLE hCommDev,LPCOMMCONFIG lpCC,DWORD dwSize);
  WINBASEAPI WINBOOL WINAPI SetCommMask(HANDLE hFile,DWORD dwEvtMask);
  WINBASEAPI WINBOOL WINAPI SetCommState(HANDLE hFile,LPDCB lpDCB);
  WINBASEAPI WINBOOL WINAPI SetCommTimeouts(HANDLE hFile,LPCOMMTIMEOUTS lpCommTimeouts);
  WINBASEAPI WINBOOL WINAPI TransmitCommChar(HANDLE hFile,char cChar);
  WINBASEAPI WINBOOL WINAPI WaitCommEvent(HANDLE hFile,LPDWORD lpEvtMask,LPOVERLAPPED lpOverlapped);
  WINBASEAPI DWORD WINAPI SetTapePosition(HANDLE hDevice,DWORD dwPositionMethod,DWORD dwPartition,DWORD dwOffsetLow,DWORD dwOffsetHigh,WINBOOL bImmediate);
  WINBASEAPI DWORD WINAPI GetTapePosition(HANDLE hDevice,DWORD dwPositionType,LPDWORD lpdwPartition,LPDWORD lpdwOffsetLow,LPDWORD lpdwOffsetHigh);
  WINBASEAPI DWORD WINAPI PrepareTape(HANDLE hDevice,DWORD dwOperation,WINBOOL bImmediate);
  WINBASEAPI DWORD WINAPI EraseTape(HANDLE hDevice,DWORD dwEraseType,WINBOOL bImmediate);
  WINBASEAPI DWORD WINAPI CreateTapePartition(HANDLE hDevice,DWORD dwPartitionMethod,DWORD dwCount,DWORD dwSize);
  WINBASEAPI DWORD WINAPI WriteTapemark(HANDLE hDevice,DWORD dwTapemarkType,DWORD dwTapemarkCount,WINBOOL bImmediate);
  WINBASEAPI DWORD WINAPI GetTapeStatus(HANDLE hDevice);
  WINBASEAPI DWORD WINAPI GetTapeParameters(HANDLE hDevice,DWORD dwOperation,LPDWORD lpdwSize,LPVOID lpTapeInformation);

#define GET_TAPE_MEDIA_INFORMATION 0
#define GET_TAPE_DRIVE_INFORMATION 1

  WINBASEAPI DWORD WINAPI SetTapeParameters(HANDLE hDevice,DWORD dwOperation,LPVOID lpTapeInformation);

#define SET_TAPE_MEDIA_INFORMATION 0
#define SET_TAPE_DRIVE_INFORMATION 1

  WINBASEAPI WINBOOL WINAPI Beep(DWORD dwFreq,DWORD dwDuration);
  WINBASEAPI int WINAPI MulDiv(int nNumber,int nNumerator,int nDenominator);
  WINBASEAPI VOID WINAPI GetSystemTime(LPSYSTEMTIME lpSystemTime);
  WINBASEAPI VOID WINAPI GetSystemTimeAsFileTime(LPFILETIME lpSystemTimeAsFileTime);
  WINBASEAPI WINBOOL WINAPI SetSystemTime(CONST SYSTEMTIME *lpSystemTime);
  WINBASEAPI VOID WINAPI GetLocalTime(LPSYSTEMTIME lpSystemTime);
  WINBASEAPI WINBOOL WINAPI SetLocalTime(CONST SYSTEMTIME *lpSystemTime);
  WINBASEAPI VOID WINAPI GetSystemInfo(LPSYSTEM_INFO lpSystemInfo);
  WINBASEAPI WINBOOL WINAPI SetSystemFileCacheSize(SIZE_T MinimumFileCacheSize,SIZE_T MaximumFileCacheSize,DWORD Flags);
  WINBASEAPI WINBOOL WINAPI GetSystemFileCacheSize(PSIZE_T lpMinimumFileCacheSize,PSIZE_T lpMaximumFileCacheSize,PDWORD lpFlags);
  WINBASEAPI WINBOOL WINAPI GetSystemRegistryQuota(PDWORD pdwQuotaAllowed,PDWORD pdwQuotaUsed);
  WINBOOL WINAPI GetSystemTimes(LPFILETIME lpIdleTime,LPFILETIME lpKernelTime,LPFILETIME lpUserTime);
  WINBASEAPI VOID WINAPI GetNativeSystemInfo(LPSYSTEM_INFO lpSystemInfo);
  WINBASEAPI WINBOOL WINAPI IsProcessorFeaturePresent(DWORD ProcessorFeature);

  typedef struct _TIME_ZONE_INFORMATION {
    LONG Bias;
    WCHAR StandardName[32];
    SYSTEMTIME StandardDate;
    LONG StandardBias;
    WCHAR DaylightName[32];
    SYSTEMTIME DaylightDate;
    LONG DaylightBias;
  } TIME_ZONE_INFORMATION,*PTIME_ZONE_INFORMATION,*LPTIME_ZONE_INFORMATION;

#ifdef UNICODE
#define FormatMessage FormatMessageW
#else
#define FormatMessage FormatMessageA
#endif

  WINBASEAPI WINBOOL WINAPI SystemTimeToTzSpecificLocalTime(LPTIME_ZONE_INFORMATION lpTimeZoneInformation,LPSYSTEMTIME lpUniversalTime,LPSYSTEMTIME lpLocalTime);
  WINBASEAPI WINBOOL WINAPI TzSpecificLocalTimeToSystemTime(LPTIME_ZONE_INFORMATION lpTimeZoneInformation,LPSYSTEMTIME lpLocalTime,LPSYSTEMTIME lpUniversalTime);
  WINBASEAPI DWORD WINAPI GetTimeZoneInformation(LPTIME_ZONE_INFORMATION lpTimeZoneInformation);
  WINBASEAPI WINBOOL WINAPI SetTimeZoneInformation(CONST TIME_ZONE_INFORMATION *lpTimeZoneInformation);
  WINBASEAPI WINBOOL WINAPI SystemTimeToFileTime(CONST SYSTEMTIME *lpSystemTime,LPFILETIME lpFileTime);
  WINBASEAPI WINBOOL WINAPI FileTimeToLocalFileTime(CONST FILETIME *lpFileTime,LPFILETIME lpLocalFileTime);
  WINBASEAPI WINBOOL WINAPI LocalFileTimeToFileTime(CONST FILETIME *lpLocalFileTime,LPFILETIME lpFileTime);
  WINBASEAPI WINBOOL WINAPI FileTimeToSystemTime(CONST FILETIME *lpFileTime,LPSYSTEMTIME lpSystemTime);
  WINBASEAPI LONG WINAPI CompareFileTime(CONST FILETIME *lpFileTime1,CONST FILETIME *lpFileTime2);
  WINBASEAPI WINBOOL WINAPI FileTimeToDosDateTime(CONST FILETIME *lpFileTime,LPWORD lpFatDate,LPWORD lpFatTime);
  WINBASEAPI WINBOOL WINAPI DosDateTimeToFileTime(WORD wFatDate,WORD wFatTime,LPFILETIME lpFileTime);
  WINBASEAPI DWORD WINAPI GetTickCount(VOID);
  WINBASEAPI WINBOOL WINAPI SetSystemTimeAdjustment(DWORD dwTimeAdjustment,WINBOOL bTimeAdjustmentDisabled);
  WINBASEAPI WINBOOL WINAPI GetSystemTimeAdjustment(PDWORD lpTimeAdjustment,PDWORD lpTimeIncrement,PBOOL lpTimeAdjustmentDisabled);
  WINBASEAPI DWORD WINAPI FormatMessageA(DWORD dwFlags,LPCVOID lpSource,DWORD dwMessageId,DWORD dwLanguageId,LPSTR lpBuffer,DWORD nSize,va_list *Arguments);
  WINBASEAPI DWORD WINAPI FormatMessageW(DWORD dwFlags,LPCVOID lpSource,DWORD dwMessageId,DWORD dwLanguageId,LPWSTR lpBuffer,DWORD nSize,va_list *Arguments);

#define FORMAT_MESSAGE_ALLOCATE_BUFFER 0x100
#define FORMAT_MESSAGE_IGNORE_INSERTS 0x200
#define FORMAT_MESSAGE_FROM_STRING 0x400
#define FORMAT_MESSAGE_FROM_HMODULE 0x800
#define FORMAT_MESSAGE_FROM_SYSTEM 0x1000
#define FORMAT_MESSAGE_ARGUMENT_ARRAY 0x2000
#define FORMAT_MESSAGE_MAX_WIDTH_MASK 0xff

#ifdef UNICODE
#define CreateMailslot CreateMailslotW
#define EncryptFile EncryptFileW
#define DecryptFile DecryptFileW
#define FileEncryptionStatus FileEncryptionStatusW
#else
#define CreateMailslot CreateMailslotA
#define EncryptFile EncryptFileA
#define DecryptFile DecryptFileA
#define FileEncryptionStatus FileEncryptionStatusA
#endif

  WINBASEAPI WINBOOL WINAPI CreatePipe(PHANDLE hReadPipe,PHANDLE hWritePipe,LPSECURITY_ATTRIBUTES lpPipeAttributes,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI ConnectNamedPipe(HANDLE hNamedPipe,LPOVERLAPPED lpOverlapped);
  WINBASEAPI WINBOOL WINAPI DisconnectNamedPipe(HANDLE hNamedPipe);
  WINBASEAPI WINBOOL WINAPI SetNamedPipeHandleState(HANDLE hNamedPipe,LPDWORD lpMode,LPDWORD lpMaxCollectionCount,LPDWORD lpCollectDataTimeout);
  WINBASEAPI WINBOOL WINAPI GetNamedPipeInfo(HANDLE hNamedPipe,LPDWORD lpFlags,LPDWORD lpOutBufferSize,LPDWORD lpInBufferSize,LPDWORD lpMaxInstances);
  WINBASEAPI WINBOOL WINAPI PeekNamedPipe(HANDLE hNamedPipe,LPVOID lpBuffer,DWORD nBufferSize,LPDWORD lpBytesRead,LPDWORD lpTotalBytesAvail,LPDWORD lpBytesLeftThisMessage);
  WINBASEAPI WINBOOL WINAPI TransactNamedPipe(HANDLE hNamedPipe,LPVOID lpInBuffer,DWORD nInBufferSize,LPVOID lpOutBuffer,DWORD nOutBufferSize,LPDWORD lpBytesRead,LPOVERLAPPED lpOverlapped);
  WINBASEAPI HANDLE WINAPI CreateMailslotA(LPCSTR lpName,DWORD nMaxMessageSize,DWORD lReadTimeout,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI HANDLE WINAPI CreateMailslotW(LPCWSTR lpName,DWORD nMaxMessageSize,DWORD lReadTimeout,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI GetMailslotInfo(HANDLE hMailslot,LPDWORD lpMaxMessageSize,LPDWORD lpNextSize,LPDWORD lpMessageCount,LPDWORD lpReadTimeout);
  WINBASEAPI WINBOOL WINAPI SetMailslotInfo(HANDLE hMailslot,DWORD lReadTimeout);
  WINBASEAPI LPVOID WINAPI MapViewOfFile(HANDLE hFileMappingObject,DWORD dwDesiredAccess,DWORD dwFileOffsetHigh,DWORD dwFileOffsetLow,SIZE_T dwNumberOfBytesToMap);
  WINBASEAPI WINBOOL WINAPI FlushViewOfFile(LPCVOID lpBaseAddress,SIZE_T dwNumberOfBytesToFlush);
  WINBASEAPI WINBOOL WINAPI UnmapViewOfFile(LPCVOID lpBaseAddress);
  WINADVAPI WINBOOL WINAPI EncryptFileA(LPCSTR lpFileName);
  WINADVAPI WINBOOL WINAPI EncryptFileW(LPCWSTR lpFileName);
  WINADVAPI WINBOOL WINAPI DecryptFileA(LPCSTR lpFileName,DWORD dwReserved);
  WINADVAPI WINBOOL WINAPI DecryptFileW(LPCWSTR lpFileName,DWORD dwReserved);

#define FILE_ENCRYPTABLE 0
#define FILE_IS_ENCRYPTED 1
#define FILE_SYSTEM_ATTR 2
#define FILE_ROOT_DIR 3
#define FILE_SYSTEM_DIR 4
#define FILE_UNKNOWN 5
#define FILE_SYSTEM_NOT_SUPPORT 6
#define FILE_USER_DISALLOWED 7
#define FILE_READ_ONLY 8
#define FILE_DIR_DISALLOWED 9

  WINADVAPI WINBOOL WINAPI FileEncryptionStatusA(LPCSTR lpFileName,LPDWORD lpStatus);
  WINADVAPI WINBOOL WINAPI FileEncryptionStatusW(LPCWSTR lpFileName,LPDWORD lpStatus);

#define EFS_USE_RECOVERY_KEYS (0x1)

  typedef DWORD (WINAPI *PFE_EXPORT_FUNC)(PBYTE pbData,PVOID pvCallbackContext,ULONG ulLength);
  typedef DWORD (WINAPI *PFE_IMPORT_FUNC)(PBYTE pbData,PVOID pvCallbackContext,PULONG ulLength);

#define CREATE_FOR_IMPORT (1)
#define CREATE_FOR_DIR (2)
#define OVERWRITE_HIDDEN (4)

#ifdef UNICODE
#define OpenEncryptedFileRaw OpenEncryptedFileRawW
#define lstrcmp lstrcmpW
#define lstrcmpi lstrcmpiW
#define lstrcpyn lstrcpynW
#define lstrcpy lstrcpyW
#define lstrcat lstrcatW
#define lstrlen lstrlenW
#else
#define OpenEncryptedFileRaw OpenEncryptedFileRawA
#define lstrcmp lstrcmpA
#define lstrcmpi lstrcmpiA
#define lstrcpyn lstrcpynA
#define lstrcpy lstrcpyA
#define lstrcat lstrcatA
#define lstrlen lstrlenA
#endif

  WINADVAPI DWORD WINAPI OpenEncryptedFileRawA(LPCSTR lpFileName,ULONG ulFlags,PVOID *pvContext);
  WINADVAPI DWORD WINAPI OpenEncryptedFileRawW(LPCWSTR lpFileName,ULONG ulFlags,PVOID *pvContext);
  WINADVAPI DWORD WINAPI ReadEncryptedFileRaw(PFE_EXPORT_FUNC pfExportCallback,PVOID pvCallbackContext,PVOID pvContext);
  WINADVAPI DWORD WINAPI WriteEncryptedFileRaw(PFE_IMPORT_FUNC pfImportCallback,PVOID pvCallbackContext,PVOID pvContext);
  WINADVAPI VOID WINAPI CloseEncryptedFileRaw(PVOID pvContext);
  WINBASEAPI int WINAPI lstrcmpA(LPCSTR lpString1,LPCSTR lpString2);
  WINBASEAPI int WINAPI lstrcmpW(LPCWSTR lpString1,LPCWSTR lpString2);
  WINBASEAPI int WINAPI lstrcmpiA(LPCSTR lpString1,LPCSTR lpString2);
  WINBASEAPI int WINAPI lstrcmpiW(LPCWSTR lpString1,LPCWSTR lpString2);
  WINBASEAPI LPSTR WINAPI lstrcpynA(LPSTR lpString1,LPCSTR lpString2,int iMaxLength);
  WINBASEAPI LPWSTR WINAPI lstrcpynW(LPWSTR lpString1,LPCWSTR lpString2,int iMaxLength);
  WINBASEAPI LPSTR WINAPI lstrcpyA(LPSTR lpString1,LPCSTR lpString2);
  WINBASEAPI LPWSTR WINAPI lstrcpyW(LPWSTR lpString1,LPCWSTR lpString2);
  WINBASEAPI LPSTR WINAPI lstrcatA(LPSTR lpString1,LPCSTR lpString2);
  WINBASEAPI LPWSTR WINAPI lstrcatW(LPWSTR lpString1,LPCWSTR lpString2);
  WINBASEAPI int WINAPI lstrlenA(LPCSTR lpString);
  WINBASEAPI int WINAPI lstrlenW(LPCWSTR lpString);
  WINBASEAPI HFILE WINAPI OpenFile(LPCSTR lpFileName,LPOFSTRUCT lpReOpenBuff,UINT uStyle);
  WINBASEAPI HFILE WINAPI _lopen(LPCSTR lpPathName,int iReadWrite);
  WINBASEAPI HFILE WINAPI _lcreat(LPCSTR lpPathName,int iAttribute);
  WINBASEAPI UINT WINAPI _lread(HFILE hFile,LPVOID lpBuffer,UINT uBytes);
  WINBASEAPI UINT WINAPI _lwrite(HFILE hFile,LPCCH lpBuffer,UINT uBytes);
  WINBASEAPI long WINAPI _hread(HFILE hFile,LPVOID lpBuffer,long lBytes);
  WINBASEAPI long WINAPI _hwrite(HFILE hFile,LPCCH lpBuffer,long lBytes);
  WINBASEAPI HFILE WINAPI _lclose(HFILE hFile);
  WINBASEAPI LONG WINAPI _llseek(HFILE hFile,LONG lOffset,int iOrigin);
  WINADVAPI WINBOOL WINAPI IsTextUnicode(CONST VOID *lpv,int iSize,LPINT lpiResult);

#define FLS_OUT_OF_INDEXES ((DWORD)0xffffffff)

  WINBASEAPI DWORD WINAPI FlsAlloc(PFLS_CALLBACK_FUNCTION lpCallback);
  WINBASEAPI PVOID WINAPI FlsGetValue(DWORD dwFlsIndex);
  WINBASEAPI WINBOOL WINAPI FlsSetValue(DWORD dwFlsIndex,PVOID lpFlsData);
  WINBASEAPI WINBOOL WINAPI FlsFree(DWORD dwFlsIndex);

#define TLS_OUT_OF_INDEXES ((DWORD)0xffffffff)

  WINBASEAPI DWORD WINAPI TlsAlloc(VOID);
  WINBASEAPI LPVOID WINAPI TlsGetValue(DWORD dwTlsIndex);
  WINBASEAPI WINBOOL WINAPI TlsSetValue(DWORD dwTlsIndex,LPVOID lpTlsValue);
  WINBASEAPI WINBOOL WINAPI TlsFree(DWORD dwTlsIndex);

  typedef VOID (WINAPI *LPOVERLAPPED_COMPLETION_ROUTINE)(DWORD dwErrorCode,DWORD dwNumberOfBytesTransfered,LPOVERLAPPED lpOverlapped);

  WINBASEAPI DWORD WINAPI SleepEx(DWORD dwMilliseconds,WINBOOL bAlertable);
  WINBASEAPI DWORD WINAPI WaitForSingleObjectEx(HANDLE hHandle,DWORD dwMilliseconds,WINBOOL bAlertable);
  WINBASEAPI DWORD WINAPI WaitForMultipleObjectsEx(DWORD nCount,CONST HANDLE *lpHandles,WINBOOL bWaitAll,DWORD dwMilliseconds,WINBOOL bAlertable);
  WINBASEAPI DWORD WINAPI SignalObjectAndWait(HANDLE hObjectToSignal,HANDLE hObjectToWaitOn,DWORD dwMilliseconds,WINBOOL bAlertable);
  WINBASEAPI WINBOOL WINAPI ReadFileEx(HANDLE hFile,LPVOID lpBuffer,DWORD nNumberOfBytesToRead,LPOVERLAPPED lpOverlapped,LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
  WINBASEAPI WINBOOL WINAPI WriteFileEx(HANDLE hFile,LPCVOID lpBuffer,DWORD nNumberOfBytesToWrite,LPOVERLAPPED lpOverlapped,LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
  WINBASEAPI WINBOOL WINAPI BackupRead(HANDLE hFile,LPBYTE lpBuffer,DWORD nNumberOfBytesToRead,LPDWORD lpNumberOfBytesRead,WINBOOL bAbort,WINBOOL bProcessSecurity,LPVOID *lpContext);
  WINBASEAPI WINBOOL WINAPI BackupSeek(HANDLE hFile,DWORD dwLowBytesToSeek,DWORD dwHighBytesToSeek,LPDWORD lpdwLowByteSeeked,LPDWORD lpdwHighByteSeeked,LPVOID *lpContext);
  WINBASEAPI WINBOOL WINAPI BackupWrite(HANDLE hFile,LPBYTE lpBuffer,DWORD nNumberOfBytesToWrite,LPDWORD lpNumberOfBytesWritten,WINBOOL bAbort,WINBOOL bProcessSecurity,LPVOID *lpContext);

  typedef struct _WIN32_STREAM_ID {
    DWORD dwStreamId;
    DWORD dwStreamAttributes;
    LARGE_INTEGER Size;
    DWORD dwStreamNameSize;
    WCHAR cStreamName[ANYSIZE_ARRAY];
  } WIN32_STREAM_ID,*LPWIN32_STREAM_ID;

#define BACKUP_INVALID 0x0
#define BACKUP_DATA 0x1
#define BACKUP_EA_DATA 0x2
#define BACKUP_SECURITY_DATA 0x3
#define BACKUP_ALTERNATE_DATA 0x4
#define BACKUP_LINK 0x5
#define BACKUP_PROPERTY_DATA 0x6
#define BACKUP_OBJECT_ID 0x7
#define BACKUP_REPARSE_DATA 0x8
#define BACKUP_SPARSE_BLOCK 0x9

#define STREAM_NORMAL_ATTRIBUTE 0x0
#define STREAM_MODIFIED_WHEN_READ 0x1
#define STREAM_CONTAINS_SECURITY 0x2
#define STREAM_CONTAINS_PROPERTIES 0x4
#define STREAM_SPARSE_ATTRIBUTE 0x8

  WINBASEAPI WINBOOL WINAPI ReadFileScatter(HANDLE hFile,FILE_SEGMENT_ELEMENT aSegmentArray[],DWORD nNumberOfBytesToRead,LPDWORD lpReserved,LPOVERLAPPED lpOverlapped);
  WINBASEAPI WINBOOL WINAPI WriteFileGather(HANDLE hFile,FILE_SEGMENT_ELEMENT aSegmentArray[],DWORD nNumberOfBytesToWrite,LPDWORD lpReserved,LPOVERLAPPED lpOverlapped);

#define STARTF_USESHOWWINDOW 0x1
#define STARTF_USESIZE 0x2
#define STARTF_USEPOSITION 0x4
#define STARTF_USECOUNTCHARS 0x8
#define STARTF_USEFILLATTRIBUTE 0x10
#define STARTF_RUNFULLSCREEN 0x20
#define STARTF_FORCEONFEEDBACK 0x40
#define STARTF_FORCEOFFFEEDBACK 0x80
#define STARTF_USESTDHANDLES 0x100

#define STARTF_USEHOTKEY 0x200

  typedef struct _STARTUPINFOA {
    DWORD cb;
    LPSTR lpReserved;
    LPSTR lpDesktop;
    LPSTR lpTitle;
    DWORD dwX;
    DWORD dwY;
    DWORD dwXSize;
    DWORD dwYSize;
    DWORD dwXCountChars;
    DWORD dwYCountChars;
    DWORD dwFillAttribute;
    DWORD dwFlags;
    WORD wShowWindow;
    WORD cbReserved2;
    LPBYTE lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
  } STARTUPINFOA,*LPSTARTUPINFOA;

  typedef struct _STARTUPINFOW {
    DWORD cb;
    LPWSTR lpReserved;
    LPWSTR lpDesktop;
    LPWSTR lpTitle;
    DWORD dwX;
    DWORD dwY;
    DWORD dwXSize;
    DWORD dwYSize;
    DWORD dwXCountChars;
    DWORD dwYCountChars;
    DWORD dwFillAttribute;
    DWORD dwFlags;
    WORD wShowWindow;
    WORD cbReserved2;
    LPBYTE lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
  } STARTUPINFOW,*LPSTARTUPINFOW;

#ifdef UNICODE
  typedef STARTUPINFOW STARTUPINFO;
  typedef LPSTARTUPINFOW LPSTARTUPINFO;
#else
  typedef STARTUPINFOA STARTUPINFO;
  typedef LPSTARTUPINFOA LPSTARTUPINFO;
#endif

#define SHUTDOWN_NORETRY 0x1

  typedef struct _WIN32_FIND_DATAA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR cFileName[MAX_PATH];
    CHAR cAlternateFileName[14];
  } WIN32_FIND_DATAA,*PWIN32_FIND_DATAA,*LPWIN32_FIND_DATAA;

  typedef struct _WIN32_FIND_DATAW {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    WCHAR cFileName[MAX_PATH];
    WCHAR cAlternateFileName[14];
  } WIN32_FIND_DATAW,*PWIN32_FIND_DATAW,*LPWIN32_FIND_DATAW;

#ifdef UNICODE
  typedef WIN32_FIND_DATAW WIN32_FIND_DATA;
  typedef PWIN32_FIND_DATAW PWIN32_FIND_DATA;
  typedef LPWIN32_FIND_DATAW LPWIN32_FIND_DATA;
#else
  typedef WIN32_FIND_DATAA WIN32_FIND_DATA;
  typedef PWIN32_FIND_DATAA PWIN32_FIND_DATA;
  typedef LPWIN32_FIND_DATAA LPWIN32_FIND_DATA;
#endif

  typedef struct _WIN32_FILE_ATTRIBUTE_DATA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
  } WIN32_FILE_ATTRIBUTE_DATA,*LPWIN32_FILE_ATTRIBUTE_DATA;

#ifdef UNICODE
#define CreateMutex CreateMutexW
#define OpenMutex OpenMutexW
#define CreateEvent CreateEventW
#define OpenEvent OpenEventW
#define CreateSemaphore CreateSemaphoreW
#define OpenSemaphore OpenSemaphoreW
#else
#define CreateMutex CreateMutexA
#define OpenMutex OpenMutexA
#define CreateEvent CreateEventA
#define OpenEvent OpenEventA
#define CreateSemaphore CreateSemaphoreA
#define OpenSemaphore OpenSemaphoreA
#endif

  WINBASEAPI HANDLE WINAPI CreateMutexA(LPSECURITY_ATTRIBUTES lpMutexAttributes,WINBOOL bInitialOwner,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateMutexW(LPSECURITY_ATTRIBUTES lpMutexAttributes,WINBOOL bInitialOwner,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenMutexA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenMutexW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateEventA(LPSECURITY_ATTRIBUTES lpEventAttributes,WINBOOL bManualReset,WINBOOL bInitialState,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateEventW(LPSECURITY_ATTRIBUTES lpEventAttributes,WINBOOL bManualReset,WINBOOL bInitialState,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenEventA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenEventW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateSemaphoreA(LPSECURITY_ATTRIBUTES lpSemaphoreAttributes,LONG lInitialCount,LONG lMaximumCount,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateSemaphoreW(LPSECURITY_ATTRIBUTES lpSemaphoreAttributes,LONG lInitialCount,LONG lMaximumCount,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenSemaphoreA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenSemaphoreW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpName);

  typedef VOID (WINAPI *PTIMERAPCROUTINE)(LPVOID lpArgToCompletionRoutine,DWORD dwTimerLowValue,DWORD dwTimerHighValue);

#ifdef UNICODE
#define CreateWaitableTimer CreateWaitableTimerW
#define OpenWaitableTimer OpenWaitableTimerW
#define CreateFileMapping CreateFileMappingW
#define OpenFileMapping OpenFileMappingW
#define GetLogicalDriveStrings GetLogicalDriveStringsW
#define LoadLibrary LoadLibraryW
#define LoadLibraryEx LoadLibraryExW
#define GetModuleFileName GetModuleFileNameW
#define GetModuleHandle GetModuleHandleW
#else
#define CreateWaitableTimer CreateWaitableTimerA
#define OpenWaitableTimer OpenWaitableTimerA
#define CreateFileMapping CreateFileMappingA
#define OpenFileMapping OpenFileMappingA
#define GetLogicalDriveStrings GetLogicalDriveStringsA
#define LoadLibrary LoadLibraryA
#define LoadLibraryEx LoadLibraryExA
#define GetModuleFileName GetModuleFileNameA
#define GetModuleHandle GetModuleHandleA
#endif

  WINBASEAPI HANDLE WINAPI CreateWaitableTimerA(LPSECURITY_ATTRIBUTES lpTimerAttributes,WINBOOL bManualReset,LPCSTR lpTimerName);
  WINBASEAPI HANDLE WINAPI CreateWaitableTimerW(LPSECURITY_ATTRIBUTES lpTimerAttributes,WINBOOL bManualReset,LPCWSTR lpTimerName);
  WINBASEAPI HANDLE WINAPI OpenWaitableTimerA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpTimerName);
  WINBASEAPI HANDLE WINAPI OpenWaitableTimerW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpTimerName);
  WINBASEAPI WINBOOL WINAPI SetWaitableTimer(HANDLE hTimer,const LARGE_INTEGER *lpDueTime,LONG lPeriod,PTIMERAPCROUTINE pfnCompletionRoutine,LPVOID lpArgToCompletionRoutine,WINBOOL fResume);
  WINBASEAPI WINBOOL WINAPI CancelWaitableTimer(HANDLE hTimer);
  WINBASEAPI HANDLE WINAPI CreateFileMappingA(HANDLE hFile,LPSECURITY_ATTRIBUTES lpFileMappingAttributes,DWORD flProtect,DWORD dwMaximumSizeHigh,DWORD dwMaximumSizeLow,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateFileMappingW(HANDLE hFile,LPSECURITY_ATTRIBUTES lpFileMappingAttributes,DWORD flProtect,DWORD dwMaximumSizeHigh,DWORD dwMaximumSizeLow,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenFileMappingA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenFileMappingW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpName);
  WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA(DWORD nBufferLength,LPSTR lpBuffer);
  WINBASEAPI DWORD WINAPI GetLogicalDriveStringsW(DWORD nBufferLength,LPWSTR lpBuffer);

  typedef enum _MEMORY_RESOURCE_NOTIFICATION_TYPE {
    LowMemoryResourceNotification,HighMemoryResourceNotification
  } MEMORY_RESOURCE_NOTIFICATION_TYPE;

  WINBASEAPI HANDLE WINAPI CreateMemoryResourceNotification(MEMORY_RESOURCE_NOTIFICATION_TYPE NotificationType);
  WINBASEAPI WINBOOL WINAPI QueryMemoryResourceNotification(HANDLE ResourceNotificationHandle,PBOOL ResourceState);
  WINBASEAPI HMODULE WINAPI LoadLibraryA(LPCSTR lpLibFileName);
  WINBASEAPI HMODULE WINAPI LoadLibraryW(LPCWSTR lpLibFileName);
  WINBASEAPI HMODULE WINAPI LoadLibraryExA(LPCSTR lpLibFileName,HANDLE hFile,DWORD dwFlags);
  WINBASEAPI HMODULE WINAPI LoadLibraryExW(LPCWSTR lpLibFileName,HANDLE hFile,DWORD dwFlags);

#define DONT_RESOLVE_DLL_REFERENCES 0x1
#define LOAD_LIBRARY_AS_DATAFILE 0x2
#define LOAD_WITH_ALTERED_SEARCH_PATH 0x8
#define LOAD_IGNORE_CODE_AUTHZ_LEVEL 0x10
#define LOAD_LINRARY_AS_IMAGE_RESOURCE 0x20
#define LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE 0x40
#define LOAD_LIBRARY_REQUIRE_SIGNED_TARGET 0x80
#define LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR 0x100
#define LOAD_LIBRARY_SEARCH_APPLICATION_DIR 0x200
#define LOAD_LIBRARY_SEARCH_USER_DIRS 0x400
#define LOAD_LIBRARY_SEARCH_SYSTEM32 0x800
#define LOAD_LIBRARY_SEARCH_DEFAULT_DIRS 0x1000
#define LOAD_LIBRARY_SAFE_CURRENT_DIRS 0x2000

  WINBASEAPI DWORD WINAPI GetModuleFileNameA(HMODULE hModule,LPCH lpFilename,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetModuleFileNameW(HMODULE hModule,LPWCH lpFilename,DWORD nSize);
  WINBASEAPI HMODULE WINAPI GetModuleHandleA(LPCSTR lpModuleName);
  WINBASEAPI HMODULE WINAPI GetModuleHandleW(LPCWSTR lpModuleName);

#ifndef RC_INVOKED
#define GET_MODULE_HANDLE_EX_FLAG_PIN (0x1)
#define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT (0x2)
#define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS (0x4)

  typedef WINBOOL (WINAPI *PGET_MODULE_HANDLE_EXA)(DWORD dwFlags,LPCSTR lpModuleName,HMODULE *phModule);
  typedef WINBOOL (WINAPI *PGET_MODULE_HANDLE_EXW)(DWORD dwFlags,LPCWSTR lpModuleName,HMODULE *phModule);

#ifdef UNICODE
#define PGET_MODULE_HANDLE_EX PGET_MODULE_HANDLE_EXW
#define GetModuleHandleEx GetModuleHandleExW
#else
#define PGET_MODULE_HANDLE_EX PGET_MODULE_HANDLE_EXA
#define GetModuleHandleEx GetModuleHandleExA
#endif

  WINBASEAPI WINBOOL WINAPI GetModuleHandleExA(DWORD dwFlags,LPCSTR lpModuleName,HMODULE *phModule);
  WINBASEAPI WINBOOL WINAPI GetModuleHandleExW(DWORD dwFlags,LPCWSTR lpModuleName,HMODULE *phModule);
#endif

#ifdef UNICODE
#define NeedCurrentDirectoryForExePath NeedCurrentDirectoryForExePathW
#define CreateProcess CreateProcessW
#define FatalAppExit FatalAppExitW
#define GetStartupInfo GetStartupInfoW
#define GetCommandLine GetCommandLineW
#define GetEnvironmentVariable GetEnvironmentVariableW
#define SetEnvironmentVariable SetEnvironmentVariableW
#define ExpandEnvironmentStrings ExpandEnvironmentStringsW
#define GetFirmwareEnvironmentVariable GetFirmwareEnvironmentVariableW
#define SetFirmwareEnvironmentVariable SetFirmwareEnvironmentVariableW
#define OutputDebugString OutputDebugStringW
#define FindResource FindResourceW
#define FindResourceEx FindResourceExW
#else
#define NeedCurrentDirectoryForExePath NeedCurrentDirectoryForExePathA
#define CreateProcess CreateProcessA
#define FatalAppExit FatalAppExitA
#define GetStartupInfo GetStartupInfoA
#define GetCommandLine GetCommandLineA
#define GetEnvironmentVariable GetEnvironmentVariableA
#define SetEnvironmentVariable SetEnvironmentVariableA
#define ExpandEnvironmentStrings ExpandEnvironmentStringsA
#define GetFirmwareEnvironmentVariable GetFirmwareEnvironmentVariableA
#define SetFirmwareEnvironmentVariable SetFirmwareEnvironmentVariableA
#define OutputDebugString OutputDebugStringA
#define FindResource FindResourceA
#define FindResourceEx FindResourceExA
#endif

  WINBASEAPI WINBOOL WINAPI NeedCurrentDirectoryForExePathA(LPCSTR ExeName);
  WINBASEAPI WINBOOL WINAPI NeedCurrentDirectoryForExePathW(LPCWSTR ExeName);
  WINBASEAPI WINBOOL WINAPI CreateProcessA(LPCSTR lpApplicationName,LPSTR lpCommandLine,LPSECURITY_ATTRIBUTES lpProcessAttributes,LPSECURITY_ATTRIBUTES lpThreadAttributes,WINBOOL bInheritHandles,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCSTR lpCurrentDirectory,LPSTARTUPINFOA lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);
  WINBASEAPI WINBOOL WINAPI CreateProcessW(LPCWSTR lpApplicationName,LPWSTR lpCommandLine,LPSECURITY_ATTRIBUTES lpProcessAttributes,LPSECURITY_ATTRIBUTES lpThreadAttributes,WINBOOL bInheritHandles,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCWSTR lpCurrentDirectory,LPSTARTUPINFOW lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);
  WINBASEAPI DWORD WINAPI AddLocalAlternateComputerNameA(LPCSTR lpDnsFQHostname,ULONG ulFlags);
  WINBASEAPI DWORD WINAPI AddLocalAlternateComputerNameW(LPCWSTR lpDnsFQHostname,ULONG ulFlags);
  WINBASEAPI WINBOOL WINAPI SetProcessShutdownParameters(DWORD dwLevel,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI GetProcessShutdownParameters(LPDWORD lpdwLevel,LPDWORD lpdwFlags);
  WINBASEAPI DWORD WINAPI GetProcessVersion(DWORD ProcessId);
  WINBASEAPI VOID WINAPI FatalAppExitA(UINT uAction,LPCSTR lpMessageText);
  WINBASEAPI VOID WINAPI FatalAppExitW(UINT uAction,LPCWSTR lpMessageText);
  WINBASEAPI VOID WINAPI GetStartupInfoA(LPSTARTUPINFOA lpStartupInfo);
  WINBASEAPI VOID WINAPI GetStartupInfoW(LPSTARTUPINFOW lpStartupInfo);
  WINBASEAPI LPSTR WINAPI GetCommandLineA(VOID);
  WINBASEAPI LPWSTR WINAPI GetCommandLineW(VOID);
  WINBASEAPI DWORD WINAPI GetEnvironmentVariableA(LPCSTR lpName,LPSTR lpBuffer,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetEnvironmentVariableW(LPCWSTR lpName,LPWSTR lpBuffer,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI SetEnvironmentVariableA(LPCSTR lpName,LPCSTR lpValue);
  WINBASEAPI WINBOOL WINAPI SetEnvironmentVariableW(LPCWSTR lpName,LPCWSTR lpValue);
  WINBASEAPI DWORD WINAPI ExpandEnvironmentStringsA(LPCSTR lpSrc,LPSTR lpDst,DWORD nSize);
  WINBASEAPI DWORD WINAPI ExpandEnvironmentStringsW(LPCWSTR lpSrc,LPWSTR lpDst,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableA(LPCSTR lpName,LPCSTR lpGuid,PVOID pBuffer,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableW(LPCWSTR lpName,LPCWSTR lpGuid,PVOID pBuffer,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableA(LPCSTR lpName,LPCSTR lpGuid,PVOID pValue,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableW(LPCWSTR lpName,LPCWSTR lpGuid,PVOID pValue,DWORD nSize);
  WINBASEAPI VOID WINAPI OutputDebugStringA(LPCSTR lpOutputString);
  WINBASEAPI VOID WINAPI OutputDebugStringW(LPCWSTR lpOutputString);
  WINBASEAPI HRSRC WINAPI FindResourceA(HMODULE hModule,LPCSTR lpName,LPCSTR lpType);
  WINBASEAPI HRSRC WINAPI FindResourceW(HMODULE hModule,LPCWSTR lpName,LPCWSTR lpType);
  WINBASEAPI HRSRC WINAPI FindResourceExA(HMODULE hModule,LPCSTR lpType,LPCSTR lpName,WORD wLanguage);
  WINBASEAPI HRSRC WINAPI FindResourceExW(HMODULE hModule,LPCWSTR lpType,LPCWSTR lpName,WORD wLanguage);

#ifdef UNICODE
#define ENUMRESTYPEPROC ENUMRESTYPEPROCW
#define ENUMRESNAMEPROC ENUMRESNAMEPROCW
#define ENUMRESLANGPROC ENUMRESLANGPROCW
#define EnumResourceTypes EnumResourceTypesW
#define EnumResourceNames EnumResourceNamesW
#define EnumResourceLanguages EnumResourceLanguagesW
#define BeginUpdateResource BeginUpdateResourceW
#define UpdateResource UpdateResourceW
#define EndUpdateResource EndUpdateResourceW
#define GlobalAddAtom GlobalAddAtomW
#define GlobalFindAtom GlobalFindAtomW
#define GlobalGetAtomName GlobalGetAtomNameW
#define AddAtom AddAtomW
#define FindAtom FindAtomW
#define GetAtomName GetAtomNameW
#define GetProfileInt GetProfileIntW
#define GetProfileString GetProfileStringW
#define WriteProfileString WriteProfileStringW
#define GetProfileSection GetProfileSectionW
#define WriteProfileSection WriteProfileSectionW
#define GetPrivateProfileInt GetPrivateProfileIntW
#define GetPrivateProfileString GetPrivateProfileStringW
#define WritePrivateProfileString WritePrivateProfileStringW
#define GetPrivateProfileSection GetPrivateProfileSectionW
#define WritePrivateProfileSection WritePrivateProfileSectionW
#define GetPrivateProfileSectionNames GetPrivateProfileSectionNamesW
#define GetPrivateProfileStruct GetPrivateProfileStructW
#define WritePrivateProfileStruct WritePrivateProfileStructW
#define GetDriveType GetDriveTypeW
#define GetSystemDirectory GetSystemDirectoryW
#define GetTempPath GetTempPathW
#define GetTempFileName GetTempFileNameW
#define GetWindowsDirectory GetWindowsDirectoryW
#define GetSystemWindowsDirectory GetSystemWindowsDirectoryW
#define AddLocalAlternateComputerName AddLocalAlternateComputerNameW
#else
#define ENUMRESTYPEPROC ENUMRESTYPEPROCA
#define ENUMRESNAMEPROC ENUMRESNAMEPROCA
#define ENUMRESLANGPROC ENUMRESLANGPROCA
#define EnumResourceTypes EnumResourceTypesA
#define EnumResourceNames EnumResourceNamesA
#define EnumResourceLanguages EnumResourceLanguagesA
#define BeginUpdateResource BeginUpdateResourceA
#define UpdateResource UpdateResourceA
#define EndUpdateResource EndUpdateResourceA
#define GlobalAddAtom GlobalAddAtomA
#define GlobalFindAtom GlobalFindAtomA
#define GlobalGetAtomName GlobalGetAtomNameA
#define AddAtom AddAtomA
#define FindAtom FindAtomA
#define GetAtomName GetAtomNameA
#define GetProfileInt GetProfileIntA
#define GetProfileString GetProfileStringA
#define WriteProfileString WriteProfileStringA
#define GetProfileSection GetProfileSectionA
#define WriteProfileSection WriteProfileSectionA
#define GetPrivateProfileInt GetPrivateProfileIntA
#define GetPrivateProfileString GetPrivateProfileStringA
#define WritePrivateProfileString WritePrivateProfileStringA
#define GetPrivateProfileSection GetPrivateProfileSectionA
#define WritePrivateProfileSection WritePrivateProfileSectionA
#define GetPrivateProfileSectionNames GetPrivateProfileSectionNamesA
#define GetPrivateProfileStruct GetPrivateProfileStructA
#define WritePrivateProfileStruct WritePrivateProfileStructA
#define GetDriveType GetDriveTypeA
#define GetSystemDirectory GetSystemDirectoryA
#define GetTempPath GetTempPathA
#define GetTempFileName GetTempFileNameA
#define GetWindowsDirectory GetWindowsDirectoryA
#define GetSystemWindowsDirectory GetSystemWindowsDirectoryA
#define AddLocalAlternateComputerName AddLocalAlternateComputerNameA
#endif

  typedef WINBOOL (CALLBACK *ENUMRESTYPEPROCA)(HMODULE hModule,LPSTR lpType,LONG_PTR lParam);
  typedef WINBOOL (CALLBACK *ENUMRESTYPEPROCW)(HMODULE hModule,LPWSTR lpType,LONG_PTR lParam);
  typedef WINBOOL (CALLBACK *ENUMRESNAMEPROCA)(HMODULE hModule,LPCSTR lpType,LPSTR lpName,LONG_PTR lParam);
  typedef WINBOOL (CALLBACK *ENUMRESNAMEPROCW)(HMODULE hModule,LPCWSTR lpType,LPWSTR lpName,LONG_PTR lParam);
  typedef WINBOOL (CALLBACK *ENUMRESLANGPROCA)(HMODULE hModule,LPCSTR lpType,LPCSTR lpName,WORD wLanguage,LONG_PTR lParam);
  typedef WINBOOL (CALLBACK *ENUMRESLANGPROCW)(HMODULE hModule,LPCWSTR lpType,LPCWSTR lpName,WORD wLanguage,LONG_PTR lParam);

  WINBASEAPI WINBOOL WINAPI EnumResourceTypesA(HMODULE hModule,ENUMRESTYPEPROCA lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI WINBOOL WINAPI EnumResourceTypesW(HMODULE hModule,ENUMRESTYPEPROCW lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI WINBOOL WINAPI EnumResourceNamesA(HMODULE hModule,LPCSTR lpType,ENUMRESNAMEPROCA lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI WINBOOL WINAPI EnumResourceNamesW(HMODULE hModule,LPCWSTR lpType,ENUMRESNAMEPROCW lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesA(HMODULE hModule,LPCSTR lpType,LPCSTR lpName,ENUMRESLANGPROCA lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesW(HMODULE hModule,LPCWSTR lpType,LPCWSTR lpName,ENUMRESLANGPROCW lpEnumFunc,LONG_PTR lParam);
  WINBASEAPI HANDLE WINAPI BeginUpdateResourceA(LPCSTR pFileName,WINBOOL bDeleteExistingResources);
  WINBASEAPI HANDLE WINAPI BeginUpdateResourceW(LPCWSTR pFileName,WINBOOL bDeleteExistingResources);
  WINBASEAPI WINBOOL WINAPI UpdateResourceA(HANDLE hUpdate,LPCSTR lpType,LPCSTR lpName,WORD wLanguage,LPVOID lpData,DWORD cb);
  WINBASEAPI WINBOOL WINAPI UpdateResourceW(HANDLE hUpdate,LPCWSTR lpType,LPCWSTR lpName,WORD wLanguage,LPVOID lpData,DWORD cb);
  WINBASEAPI WINBOOL WINAPI EndUpdateResourceA(HANDLE hUpdate,WINBOOL fDiscard);
  WINBASEAPI WINBOOL WINAPI EndUpdateResourceW(HANDLE hUpdate,WINBOOL fDiscard);
  WINBASEAPI ATOM WINAPI GlobalAddAtomA(LPCSTR lpString);
  WINBASEAPI ATOM WINAPI GlobalAddAtomW(LPCWSTR lpString);
  WINBASEAPI ATOM WINAPI GlobalFindAtomA(LPCSTR lpString);
  WINBASEAPI ATOM WINAPI GlobalFindAtomW(LPCWSTR lpString);
  WINBASEAPI UINT WINAPI GlobalGetAtomNameA(ATOM nAtom,LPSTR lpBuffer,int nSize);
  WINBASEAPI UINT WINAPI GlobalGetAtomNameW(ATOM nAtom,LPWSTR lpBuffer,int nSize);
  WINBASEAPI ATOM WINAPI AddAtomA(LPCSTR lpString);
  WINBASEAPI ATOM WINAPI AddAtomW(LPCWSTR lpString);
  WINBASEAPI ATOM WINAPI FindAtomA(LPCSTR lpString);
  WINBASEAPI ATOM WINAPI FindAtomW(LPCWSTR lpString);
  WINBASEAPI UINT WINAPI GetAtomNameA(ATOM nAtom,LPSTR lpBuffer,int nSize);
  WINBASEAPI UINT WINAPI GetAtomNameW(ATOM nAtom,LPWSTR lpBuffer,int nSize);
  WINBASEAPI UINT WINAPI GetProfileIntA(LPCSTR lpAppName,LPCSTR lpKeyName,INT nDefault);
  WINBASEAPI UINT WINAPI GetProfileIntW(LPCWSTR lpAppName,LPCWSTR lpKeyName,INT nDefault);
  WINBASEAPI DWORD WINAPI GetProfileStringA(LPCSTR lpAppName,LPCSTR lpKeyName,LPCSTR lpDefault,LPSTR lpReturnedString,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetProfileStringW(LPCWSTR lpAppName,LPCWSTR lpKeyName,LPCWSTR lpDefault,LPWSTR lpReturnedString,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI WriteProfileStringA(LPCSTR lpAppName,LPCSTR lpKeyName,LPCSTR lpString);
  WINBASEAPI WINBOOL WINAPI WriteProfileStringW(LPCWSTR lpAppName,LPCWSTR lpKeyName,LPCWSTR lpString);
  WINBASEAPI DWORD WINAPI GetProfileSectionA(LPCSTR lpAppName,LPSTR lpReturnedString,DWORD nSize);
  WINBASEAPI DWORD WINAPI GetProfileSectionW(LPCWSTR lpAppName,LPWSTR lpReturnedString,DWORD nSize);
  WINBASEAPI WINBOOL WINAPI WriteProfileSectionA(LPCSTR lpAppName,LPCSTR lpString);
  WINBASEAPI WINBOOL WINAPI WriteProfileSectionW(LPCWSTR lpAppName,LPCWSTR lpString);
  WINBASEAPI UINT WINAPI GetPrivateProfileIntA(LPCSTR lpAppName,LPCSTR lpKeyName,INT nDefault,LPCSTR lpFileName);
  WINBASEAPI UINT WINAPI GetPrivateProfileIntW(LPCWSTR lpAppName,LPCWSTR lpKeyName,INT nDefault,LPCWSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileStringA(LPCSTR lpAppName,LPCSTR lpKeyName,LPCSTR lpDefault,LPSTR lpReturnedString,DWORD nSize,LPCSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileStringW(LPCWSTR lpAppName,LPCWSTR lpKeyName,LPCWSTR lpDefault,LPWSTR lpReturnedString,DWORD nSize,LPCWSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringA(LPCSTR lpAppName,LPCSTR lpKeyName,LPCSTR lpString,LPCSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringW(LPCWSTR lpAppName,LPCWSTR lpKeyName,LPCWSTR lpString,LPCWSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileSectionA(LPCSTR lpAppName,LPSTR lpReturnedString,DWORD nSize,LPCSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileSectionW(LPCWSTR lpAppName,LPWSTR lpReturnedString,DWORD nSize,LPCWSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionA(LPCSTR lpAppName,LPCSTR lpString,LPCSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionW(LPCWSTR lpAppName,LPCWSTR lpString,LPCWSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesA(LPSTR lpszReturnBuffer,DWORD nSize,LPCSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesW(LPWSTR lpszReturnBuffer,DWORD nSize,LPCWSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructA(LPCSTR lpszSection,LPCSTR lpszKey,LPVOID lpStruct,UINT uSizeStruct,LPCSTR szFile);
  WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructW(LPCWSTR lpszSection,LPCWSTR lpszKey,LPVOID lpStruct,UINT uSizeStruct,LPCWSTR szFile);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructA(LPCSTR lpszSection,LPCSTR lpszKey,LPVOID lpStruct,UINT uSizeStruct,LPCSTR szFile);
  WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructW(LPCWSTR lpszSection,LPCWSTR lpszKey,LPVOID lpStruct,UINT uSizeStruct,LPCWSTR szFile);
  WINBASEAPI UINT WINAPI GetDriveTypeA(LPCSTR lpRootPathName);
  WINBASEAPI UINT WINAPI GetDriveTypeW(LPCWSTR lpRootPathName);
  WINBASEAPI UINT WINAPI GetSystemDirectoryA(LPSTR lpBuffer,UINT uSize);
  WINBASEAPI UINT WINAPI GetSystemDirectoryW(LPWSTR lpBuffer,UINT uSize);
  WINBASEAPI DWORD WINAPI GetTempPathA(DWORD nBufferLength,LPSTR lpBuffer);
  WINBASEAPI DWORD WINAPI GetTempPathW(DWORD nBufferLength,LPWSTR lpBuffer);
  WINBASEAPI UINT WINAPI GetTempFileNameA(LPCSTR lpPathName,LPCSTR lpPrefixString,UINT uUnique,LPSTR lpTempFileName);
  WINBASEAPI UINT WINAPI GetTempFileNameW(LPCWSTR lpPathName,LPCWSTR lpPrefixString,UINT uUnique,LPWSTR lpTempFileName);
  WINBASEAPI UINT WINAPI GetWindowsDirectoryA(LPSTR lpBuffer,UINT uSize);
  WINBASEAPI UINT WINAPI GetWindowsDirectoryW(LPWSTR lpBuffer,UINT uSize);
  WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryA(LPSTR lpBuffer,UINT uSize);
  WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryW(LPWSTR lpBuffer,UINT uSize);

#ifndef RC_INVOKED
#ifdef UNICODE
#define GetSystemWow64Directory GetSystemWow64DirectoryW
#else
#define GetSystemWow64Directory GetSystemWow64DirectoryA
#endif

  WINBASEAPI UINT WINAPI GetSystemWow64DirectoryA(LPSTR lpBuffer,UINT uSize);
  WINBASEAPI UINT WINAPI GetSystemWow64DirectoryW(LPWSTR lpBuffer,UINT uSize);
  WINBASEAPI BOOLEAN WINAPI Wow64EnableWow64FsRedirection(BOOLEAN Wow64FsEnableRedirection);
  WINBASEAPI WINBOOL WINAPI Wow64DisableWow64FsRedirection(PVOID *OldValue);
  WINBASEAPI WINBOOL WINAPI Wow64RevertWow64FsRedirection(PVOID OlValue);

  typedef UINT (WINAPI *PGET_SYSTEM_WOW64_DIRECTORY_A)(LPSTR lpBuffer,UINT uSize);
  typedef UINT (WINAPI *PGET_SYSTEM_WOW64_DIRECTORY_W)(LPWSTR lpBuffer,UINT uSize);

#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A "GetSystemWow64DirectoryA"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W L"GetSystemWow64DirectoryA"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T TEXT("GetSystemWow64DirectoryA")
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A "GetSystemWow64DirectoryW"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W L"GetSystemWow64DirectoryW"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T TEXT("GetSystemWow64DirectoryW")

#ifdef UNICODE
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T
#else
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T
#endif
#endif

#ifdef UNICODE
#define SetCurrentDirectory SetCurrentDirectoryW
#define GetCurrentDirectory GetCurrentDirectoryW
#define SetDllDirectory SetDllDirectoryW
#define GetDllDirectory GetDllDirectoryW
#define GetDiskFreeSpace GetDiskFreeSpaceW
#define GetDiskFreeSpaceEx GetDiskFreeSpaceExW
#define CreateDirectory CreateDirectoryW
#define CreateDirectoryEx CreateDirectoryExW
#define RemoveDirectory RemoveDirectoryW
#define GetFullPathName GetFullPathNameW
#define DefineDosDevice DefineDosDeviceW
#define QueryDosDevice QueryDosDeviceW
#define CreateFile CreateFileW
#define SetFileAttributes SetFileAttributesW
#define GetFileAttributes GetFileAttributesW
#else
#define SetCurrentDirectory SetCurrentDirectoryA
#define GetCurrentDirectory GetCurrentDirectoryA
#define SetDllDirectory SetDllDirectoryA
#define GetDllDirectory GetDllDirectoryA
#define GetDiskFreeSpace GetDiskFreeSpaceA
#define GetDiskFreeSpaceEx GetDiskFreeSpaceExA
#define CreateDirectory CreateDirectoryA
#define CreateDirectoryEx CreateDirectoryExA
#define RemoveDirectory RemoveDirectoryA
#define GetFullPathName GetFullPathNameA
#define DefineDosDevice DefineDosDeviceA
#define QueryDosDevice QueryDosDeviceA
#define CreateFile CreateFileA
#define SetFileAttributes SetFileAttributesA
#define GetFileAttributes GetFileAttributesA
#endif

  WINBASEAPI WINBOOL WINAPI SetCurrentDirectoryA(LPCSTR lpPathName);
  WINBASEAPI WINBOOL WINAPI SetCurrentDirectoryW(LPCWSTR lpPathName);
  WINBASEAPI DWORD WINAPI GetCurrentDirectoryA(DWORD nBufferLength,LPSTR lpBuffer);
  WINBASEAPI DWORD WINAPI GetCurrentDirectoryW(DWORD nBufferLength,LPWSTR lpBuffer);
  WINBASEAPI WINBOOL WINAPI SetDllDirectoryA(LPCSTR lpPathName);
  WINBASEAPI WINBOOL WINAPI SetDllDirectoryW(LPCWSTR lpPathName);
  WINBASEAPI DWORD WINAPI GetDllDirectoryA(DWORD nBufferLength,LPSTR lpBuffer);
  WINBASEAPI DWORD WINAPI GetDllDirectoryW(DWORD nBufferLength,LPWSTR lpBuffer);
  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceA(LPCSTR lpRootPathName,LPDWORD lpSectorsPerCluster,LPDWORD lpBytesPerSector,LPDWORD lpNumberOfFreeClusters,LPDWORD lpTotalNumberOfClusters);
  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceW(LPCWSTR lpRootPathName,LPDWORD lpSectorsPerCluster,LPDWORD lpBytesPerSector,LPDWORD lpNumberOfFreeClusters,LPDWORD lpTotalNumberOfClusters);
  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceExA(LPCSTR lpDirectoryName,PULARGE_INTEGER lpFreeBytesAvailableToCaller,PULARGE_INTEGER lpTotalNumberOfBytes,PULARGE_INTEGER lpTotalNumberOfFreeBytes);
  WINBASEAPI WINBOOL WINAPI GetDiskFreeSpaceExW(LPCWSTR lpDirectoryName,PULARGE_INTEGER lpFreeBytesAvailableToCaller,PULARGE_INTEGER lpTotalNumberOfBytes,PULARGE_INTEGER lpTotalNumberOfFreeBytes);
  WINBASEAPI WINBOOL WINAPI CreateDirectoryA(LPCSTR lpPathName,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI CreateDirectoryW(LPCWSTR lpPathName,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI CreateDirectoryExA(LPCSTR lpTemplateDirectory,LPCSTR lpNewDirectory,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI CreateDirectoryExW(LPCWSTR lpTemplateDirectory,LPCWSTR lpNewDirectory,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI RemoveDirectoryA(LPCSTR lpPathName);
  WINBASEAPI WINBOOL WINAPI RemoveDirectoryW(LPCWSTR lpPathName);
  WINBASEAPI DWORD WINAPI GetFullPathNameA(LPCSTR lpFileName,DWORD nBufferLength,LPSTR lpBuffer,LPSTR *lpFilePart);
  WINBASEAPI DWORD WINAPI GetFullPathNameW(LPCWSTR lpFileName,DWORD nBufferLength,LPWSTR lpBuffer,LPWSTR *lpFilePart);

#define DDD_RAW_TARGET_PATH 0x1
#define DDD_REMOVE_DEFINITION 0x2
#define DDD_EXACT_MATCH_ON_REMOVE 0x4
#define DDD_NO_BROADCAST_SYSTEM 0x8
#define DDD_LUID_BROADCAST_DRIVE 0x10

  WINBASEAPI WINBOOL WINAPI DefineDosDeviceA(DWORD dwFlags,LPCSTR lpDeviceName,LPCSTR lpTargetPath);
  WINBASEAPI WINBOOL WINAPI DefineDosDeviceW(DWORD dwFlags,LPCWSTR lpDeviceName,LPCWSTR lpTargetPath);
  WINBASEAPI DWORD WINAPI QueryDosDeviceA(LPCSTR lpDeviceName,LPSTR lpTargetPath,DWORD ucchMax);
  WINBASEAPI DWORD WINAPI QueryDosDeviceW(LPCWSTR lpDeviceName,LPWSTR lpTargetPath,DWORD ucchMax);

#define EXPAND_LOCAL_DRIVES

  WINBASEAPI HANDLE WINAPI CreateFileA(LPCSTR lpFileName,DWORD dwDesiredAccess,DWORD dwShareMode,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD dwCreationDisposition,DWORD dwFlagsAndAttributes,HANDLE hTemplateFile);
  WINBASEAPI HANDLE WINAPI CreateFileW(LPCWSTR lpFileName,DWORD dwDesiredAccess,DWORD dwShareMode,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD dwCreationDisposition,DWORD dwFlagsAndAttributes,HANDLE hTemplateFile);
  WINBASEAPI HANDLE WINAPI ReOpenFile(HANDLE hOriginalFile,DWORD dwDesiredAccess,DWORD dwShareMode,DWORD dwFlagsAndAttributes);
  WINBASEAPI WINBOOL WINAPI SetFileAttributesA(LPCSTR lpFileName,DWORD dwFileAttributes);
  WINBASEAPI WINBOOL WINAPI SetFileAttributesW(LPCWSTR lpFileName,DWORD dwFileAttributes);
  WINBASEAPI DWORD WINAPI GetFileAttributesA(LPCSTR lpFileName);
  WINBASEAPI DWORD WINAPI GetFileAttributesW(LPCWSTR lpFileName);

  typedef enum _GET_FILEEX_INFO_LEVELS {
    GetFileExInfoStandard,GetFileExMaxInfoLevel
  } GET_FILEEX_INFO_LEVELS;

#ifdef UNICODE
#define GetFileAttributesEx GetFileAttributesExW
#define GetCompressedFileSize GetCompressedFileSizeW
#define DeleteFile DeleteFileW
#define CheckNameLegalDOS8Dot3 CheckNameLegalDOS8Dot3W
#else
#define GetFileAttributesEx GetFileAttributesExA
#define GetCompressedFileSize GetCompressedFileSizeA
#define DeleteFile DeleteFileA
#define CheckNameLegalDOS8Dot3 CheckNameLegalDOS8Dot3A
#endif

  WINBASEAPI WINBOOL WINAPI GetFileAttributesExA(LPCSTR lpFileName,GET_FILEEX_INFO_LEVELS fInfoLevelId,LPVOID lpFileInformation);
  WINBASEAPI WINBOOL WINAPI GetFileAttributesExW(LPCWSTR lpFileName,GET_FILEEX_INFO_LEVELS fInfoLevelId,LPVOID lpFileInformation);
  WINBASEAPI DWORD WINAPI GetCompressedFileSizeA(LPCSTR lpFileName,LPDWORD lpFileSizeHigh);
  WINBASEAPI DWORD WINAPI GetCompressedFileSizeW(LPCWSTR lpFileName,LPDWORD lpFileSizeHigh);
  WINBASEAPI WINBOOL WINAPI DeleteFileA(LPCSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI DeleteFileW(LPCWSTR lpFileName);
  WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3A(LPCSTR lpName,LPSTR lpOemName,DWORD OemNameSize,PBOOL pbNameContainsSpaces,PBOOL pbNameLegal);
  WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3W(LPCWSTR lpName,LPSTR lpOemName,DWORD OemNameSize,PBOOL pbNameContainsSpaces,PBOOL pbNameLegal);

  typedef enum _FINDEX_INFO_LEVELS {
    FindExInfoStandard,FindExInfoMaxInfoLevel
  } FINDEX_INFO_LEVELS;

  typedef enum _FINDEX_SEARCH_OPS {
    FindExSearchNameMatch,FindExSearchLimitToDirectories,FindExSearchLimitToDevices,FindExSearchMaxSearchOp
  } FINDEX_SEARCH_OPS;

#define FIND_FIRST_EX_CASE_SENSITIVE 0x1

#ifdef UNICODE
#define FindFirstFileEx FindFirstFileExW
#define FindFirstFile FindFirstFileW
#define FindNextFile FindNextFileW
#define SearchPath SearchPathW
#define CopyFile CopyFileW
#define CopyFileEx CopyFileExW
#define MoveFile MoveFileW
#define MoveFileEx MoveFileExW
#define MoveFileWithProgress MoveFileWithProgressW
#define ReplaceFile ReplaceFileW
#define CreateHardLink CreateHardLinkW
#define CreateNamedPipe CreateNamedPipeW
#define GetNamedPipeHandleState GetNamedPipeHandleStateW
#define CallNamedPipe CallNamedPipeW
#define WaitNamedPipe WaitNamedPipeW
#define SetVolumeLabel SetVolumeLabelW
#define GetVolumeInformation GetVolumeInformationW
#define ClearEventLog ClearEventLogW
#define BackupEventLog BackupEventLogW
#define OpenEventLog OpenEventLogW
#define RegisterEventSource RegisterEventSourceW
#define OpenBackupEventLog OpenBackupEventLogW
#define ReadEventLog ReadEventLogW
#define ReportEvent ReportEventW
#define AccessCheckAndAuditAlarm AccessCheckAndAuditAlarmW
#define AccessCheckByTypeAndAuditAlarm AccessCheckByTypeAndAuditAlarmW
#define AccessCheckByTypeResultListAndAuditAlarm AccessCheckByTypeResultListAndAuditAlarmW
#define AccessCheckByTypeResultListAndAuditAlarmByHandle AccessCheckByTypeResultListAndAuditAlarmByHandleW
#define ObjectOpenAuditAlarm ObjectOpenAuditAlarmW
#define ObjectPrivilegeAuditAlarm ObjectPrivilegeAuditAlarmW
#define ObjectCloseAuditAlarm ObjectCloseAuditAlarmW
#define ObjectDeleteAuditAlarm ObjectDeleteAuditAlarmW
#define PrivilegedServiceAuditAlarm PrivilegedServiceAuditAlarmW
#define SetFileSecurity SetFileSecurityW
#define GetFileSecurity GetFileSecurityW
#define FindFirstChangeNotification FindFirstChangeNotificationW
#define IsBadStringPtr IsBadStringPtrW
#define LookupAccountSid LookupAccountSidW
#define LookupAccountName LookupAccountNameW
#define LookupPrivilegeValue LookupPrivilegeValueW
#define LookupPrivilegeName LookupPrivilegeNameW
#define LookupPrivilegeDisplayName LookupPrivilegeDisplayNameW
#define BuildCommDCB BuildCommDCBW
#define BuildCommDCBAndTimeouts BuildCommDCBAndTimeoutsW
#define CommConfigDialog CommConfigDialogW
#define GetDefaultCommConfig GetDefaultCommConfigW
#define SetDefaultCommConfig SetDefaultCommConfigW
#define GetComputerName GetComputerNameW
#define SetComputerName SetComputerNameW
#define GetComputerNameEx GetComputerNameExW
#define SetComputerNameEx SetComputerNameExW
#define DnsHostnameToComputerName DnsHostnameToComputerNameW
#define GetUserName GetUserNameW
#else
#define FindFirstFileEx FindFirstFileExA
#define FindFirstFile FindFirstFileA
#define FindNextFile FindNextFileA
#define SearchPath SearchPathA
#define CopyFile CopyFileA
#define CopyFileEx CopyFileExA
#define MoveFile MoveFileA
#define MoveFileEx MoveFileExA
#define MoveFileWithProgress MoveFileWithProgressA
#define ReplaceFile ReplaceFileA
#define CreateHardLink CreateHardLinkA
#define CreateNamedPipe CreateNamedPipeA
#define GetNamedPipeHandleState GetNamedPipeHandleStateA
#define CallNamedPipe CallNamedPipeA
#define WaitNamedPipe WaitNamedPipeA
#define SetVolumeLabel SetVolumeLabelA
#define GetVolumeInformation GetVolumeInformationA
#define ClearEventLog ClearEventLogA
#define BackupEventLog BackupEventLogA
#define OpenEventLog OpenEventLogA
#define RegisterEventSource RegisterEventSourceA
#define OpenBackupEventLog OpenBackupEventLogA
#define ReadEventLog ReadEventLogA
#define ReportEvent ReportEventA
#define AccessCheckAndAuditAlarm AccessCheckAndAuditAlarmA
#define AccessCheckByTypeAndAuditAlarm AccessCheckByTypeAndAuditAlarmA
#define AccessCheckByTypeResultListAndAuditAlarm AccessCheckByTypeResultListAndAuditAlarmA
#define AccessCheckByTypeResultListAndAuditAlarmByHandle AccessCheckByTypeResultListAndAuditAlarmByHandleA
#define ObjectOpenAuditAlarm ObjectOpenAuditAlarmA
#define ObjectPrivilegeAuditAlarm ObjectPrivilegeAuditAlarmA
#define ObjectCloseAuditAlarm ObjectCloseAuditAlarmA
#define ObjectDeleteAuditAlarm ObjectDeleteAuditAlarmA
#define PrivilegedServiceAuditAlarm PrivilegedServiceAuditAlarmA
#define SetFileSecurity SetFileSecurityA
#define GetFileSecurity GetFileSecurityA
#define FindFirstChangeNotification FindFirstChangeNotificationA
#define IsBadStringPtr IsBadStringPtrA
#define LookupAccountSid LookupAccountSidA
#define LookupAccountName LookupAccountNameA
#define LookupPrivilegeValue LookupPrivilegeValueA
#define LookupPrivilegeName LookupPrivilegeNameA
#define LookupPrivilegeDisplayName LookupPrivilegeDisplayNameA
#define BuildCommDCB BuildCommDCBA
#define BuildCommDCBAndTimeouts BuildCommDCBAndTimeoutsA
#define CommConfigDialog CommConfigDialogA
#define GetDefaultCommConfig GetDefaultCommConfigA
#define SetDefaultCommConfig SetDefaultCommConfigA
#define GetComputerName GetComputerNameA
#define SetComputerName SetComputerNameA
#define GetComputerNameEx GetComputerNameExA
#define SetComputerNameEx SetComputerNameExA
#define DnsHostnameToComputerName DnsHostnameToComputerNameA
#define GetUserName GetUserNameA
#endif

  WINBASEAPI HANDLE WINAPI FindFirstFileExA(LPCSTR lpFileName,FINDEX_INFO_LEVELS fInfoLevelId,LPVOID lpFindFileData,FINDEX_SEARCH_OPS fSearchOp,LPVOID lpSearchFilter,DWORD dwAdditionalFlags);
  WINBASEAPI HANDLE WINAPI FindFirstFileExW(LPCWSTR lpFileName,FINDEX_INFO_LEVELS fInfoLevelId,LPVOID lpFindFileData,FINDEX_SEARCH_OPS fSearchOp,LPVOID lpSearchFilter,DWORD dwAdditionalFlags);
  WINBASEAPI HANDLE WINAPI FindFirstFileA(LPCSTR lpFileName,LPWIN32_FIND_DATAA lpFindFileData);
  WINBASEAPI HANDLE WINAPI FindFirstFileW(LPCWSTR lpFileName,LPWIN32_FIND_DATAW lpFindFileData);
  WINBASEAPI WINBOOL WINAPI FindNextFileA(HANDLE hFindFile,LPWIN32_FIND_DATAA lpFindFileData);
  WINBASEAPI WINBOOL WINAPI FindNextFileW(HANDLE hFindFile,LPWIN32_FIND_DATAW lpFindFileData);
  WINBASEAPI DWORD WINAPI SearchPathA(LPCSTR lpPath,LPCSTR lpFileName,LPCSTR lpExtension,DWORD nBufferLength,LPSTR lpBuffer,LPSTR *lpFilePart);
  WINBASEAPI DWORD WINAPI SearchPathW(LPCWSTR lpPath,LPCWSTR lpFileName,LPCWSTR lpExtension,DWORD nBufferLength,LPWSTR lpBuffer,LPWSTR *lpFilePart);
  WINBASEAPI WINBOOL WINAPI CopyFileA(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,WINBOOL bFailIfExists);
  WINBASEAPI WINBOOL WINAPI CopyFileW(LPCWSTR lpExistingFileName,LPCWSTR lpNewFileName,WINBOOL bFailIfExists);

  typedef DWORD (WINAPI *LPPROGRESS_ROUTINE)(LARGE_INTEGER TotalFileSize,LARGE_INTEGER TotalBytesTransferred,LARGE_INTEGER StreamSize,LARGE_INTEGER StreamBytesTransferred,DWORD dwStreamNumber,DWORD dwCallbackReason,HANDLE hSourceFile,HANDLE hDestinationFile,LPVOID lpData);

  WINBASEAPI WINBOOL WINAPI CopyFileExA(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,LPPROGRESS_ROUTINE lpProgressRoutine,LPVOID lpData,LPBOOL pbCancel,DWORD dwCopyFlags);
  WINBASEAPI WINBOOL WINAPI CopyFileExW(LPCWSTR lpExistingFileName,LPCWSTR lpNewFileName,LPPROGRESS_ROUTINE lpProgressRoutine,LPVOID lpData,LPBOOL pbCancel,DWORD dwCopyFlags);
  WINBASEAPI WINBOOL WINAPI MoveFileA(LPCSTR lpExistingFileName,LPCSTR lpNewFileName);
  WINBASEAPI WINBOOL WINAPI MoveFileW(LPCWSTR lpExistingFileName,LPCWSTR lpNewFileName);
  WINBASEAPI WINBOOL WINAPI MoveFileExA(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI MoveFileExW(LPCWSTR lpExistingFileName,LPCWSTR lpNewFileName,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI MoveFileWithProgressA(LPCSTR lpExistingFileName,LPCSTR lpNewFileName,LPPROGRESS_ROUTINE lpProgressRoutine,LPVOID lpData,DWORD dwFlags);
  WINBASEAPI WINBOOL WINAPI MoveFileWithProgressW(LPCWSTR lpExistingFileName,LPCWSTR lpNewFileName,LPPROGRESS_ROUTINE lpProgressRoutine,LPVOID lpData,DWORD dwFlags);

#define MOVEFILE_REPLACE_EXISTING 0x1
#define MOVEFILE_COPY_ALLOWED 0x2
#define MOVEFILE_DELAY_UNTIL_REBOOT 0x4
#define MOVEFILE_WRITE_THROUGH 0x8
#define MOVEFILE_CREATE_HARDLINK 0x10
#define MOVEFILE_FAIL_IF_NOT_TRACKABLE 0x20

  WINBASEAPI WINBOOL WINAPI ReplaceFileA(LPCSTR lpReplacedFileName,LPCSTR lpReplacementFileName,LPCSTR lpBackupFileName,DWORD dwReplaceFlags,LPVOID lpExclude,LPVOID lpReserved);
  WINBASEAPI WINBOOL WINAPI ReplaceFileW(LPCWSTR lpReplacedFileName,LPCWSTR lpReplacementFileName,LPCWSTR lpBackupFileName,DWORD dwReplaceFlags,LPVOID lpExclude,LPVOID lpReserved);
  WINBASEAPI WINBOOL WINAPI CreateHardLinkA(LPCSTR lpFileName,LPCSTR lpExistingFileName,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI CreateHardLinkW(LPCWSTR lpFileName,LPCWSTR lpExistingFileName,LPSECURITY_ATTRIBUTES lpSecurityAttributes);

  typedef enum _STREAM_INFO_LEVELS {
    FindStreamInfoStandard,FindStreamInfoMaxInfoLevel
  } STREAM_INFO_LEVELS;

  typedef struct _WIN32_FIND_STREAM_DATA {
    LARGE_INTEGER StreamSize;
    WCHAR cStreamName[MAX_PATH + 36];
  } WIN32_FIND_STREAM_DATA,*PWIN32_FIND_STREAM_DATA;

  HANDLE WINAPI FindFirstStreamW(LPCWSTR lpFileName,STREAM_INFO_LEVELS InfoLevel,LPVOID lpFindStreamData,DWORD dwFlags);
  WINBOOL WINAPI FindNextStreamW(HANDLE hFindStream,LPVOID lpFindStreamData);
  WINBASEAPI HANDLE WINAPI CreateNamedPipeA(LPCSTR lpName,DWORD dwOpenMode,DWORD dwPipeMode,DWORD nMaxInstances,DWORD nOutBufferSize,DWORD nInBufferSize,DWORD nDefaultTimeOut,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI HANDLE WINAPI CreateNamedPipeW(LPCWSTR lpName,DWORD dwOpenMode,DWORD dwPipeMode,DWORD nMaxInstances,DWORD nOutBufferSize,DWORD nInBufferSize,DWORD nDefaultTimeOut,LPSECURITY_ATTRIBUTES lpSecurityAttributes);
  WINBASEAPI WINBOOL WINAPI GetNamedPipeHandleStateA(HANDLE hNamedPipe,LPDWORD lpState,LPDWORD lpCurInstances,LPDWORD lpMaxCollectionCount,LPDWORD lpCollectDataTimeout,LPSTR lpUserName,DWORD nMaxUserNameSize);
  WINBASEAPI WINBOOL WINAPI GetNamedPipeHandleStateW(HANDLE hNamedPipe,LPDWORD lpState,LPDWORD lpCurInstances,LPDWORD lpMaxCollectionCount,LPDWORD lpCollectDataTimeout,LPWSTR lpUserName,DWORD nMaxUserNameSize);
  WINBASEAPI WINBOOL WINAPI CallNamedPipeA(LPCSTR lpNamedPipeName,LPVOID lpInBuffer,DWORD nInBufferSize,LPVOID lpOutBuffer,DWORD nOutBufferSize,LPDWORD lpBytesRead,DWORD nTimeOut);
  WINBASEAPI WINBOOL WINAPI CallNamedPipeW(LPCWSTR lpNamedPipeName,LPVOID lpInBuffer,DWORD nInBufferSize,LPVOID lpOutBuffer,DWORD nOutBufferSize,LPDWORD lpBytesRead,DWORD nTimeOut);
  WINBASEAPI WINBOOL WINAPI WaitNamedPipeA(LPCSTR lpNamedPipeName,DWORD nTimeOut);
  WINBASEAPI WINBOOL WINAPI WaitNamedPipeW(LPCWSTR lpNamedPipeName,DWORD nTimeOut);
  WINBASEAPI WINBOOL WINAPI SetVolumeLabelA(LPCSTR lpRootPathName,LPCSTR lpVolumeName);
  WINBASEAPI WINBOOL WINAPI SetVolumeLabelW(LPCWSTR lpRootPathName,LPCWSTR lpVolumeName);
  WINBASEAPI VOID WINAPI SetFileApisToOEM(VOID);
  WINBASEAPI VOID WINAPI SetFileApisToANSI(VOID);
  WINBASEAPI WINBOOL WINAPI AreFileApisANSI(VOID);
  WINBASEAPI WINBOOL WINAPI GetVolumeInformationA(LPCSTR lpRootPathName,LPSTR lpVolumeNameBuffer,DWORD nVolumeNameSize,LPDWORD lpVolumeSerialNumber,LPDWORD lpMaximumComponentLength,LPDWORD lpFileSystemFlags,LPSTR lpFileSystemNameBuffer,DWORD nFileSystemNameSize);
  WINBASEAPI WINBOOL WINAPI GetVolumeInformationW(LPCWSTR lpRootPathName,LPWSTR lpVolumeNameBuffer,DWORD nVolumeNameSize,LPDWORD lpVolumeSerialNumber,LPDWORD lpMaximumComponentLength,LPDWORD lpFileSystemFlags,LPWSTR lpFileSystemNameBuffer,DWORD nFileSystemNameSize);
  WINBASEAPI WINBOOL WINAPI CancelIo(HANDLE hFile);
  WINADVAPI WINBOOL WINAPI ClearEventLogA(HANDLE hEventLog,LPCSTR lpBackupFileName);
  WINADVAPI WINBOOL WINAPI ClearEventLogW(HANDLE hEventLog,LPCWSTR lpBackupFileName);
  WINADVAPI WINBOOL WINAPI BackupEventLogA(HANDLE hEventLog,LPCSTR lpBackupFileName);
  WINADVAPI WINBOOL WINAPI BackupEventLogW(HANDLE hEventLog,LPCWSTR lpBackupFileName);
  WINADVAPI WINBOOL WINAPI CloseEventLog(HANDLE hEventLog);
  WINADVAPI WINBOOL WINAPI DeregisterEventSource(HANDLE hEventLog);
  WINADVAPI WINBOOL WINAPI NotifyChangeEventLog(HANDLE hEventLog,HANDLE hEvent);
  WINADVAPI WINBOOL WINAPI GetNumberOfEventLogRecords(HANDLE hEventLog,PDWORD NumberOfRecords);
  WINADVAPI WINBOOL WINAPI GetOldestEventLogRecord(HANDLE hEventLog,PDWORD OldestRecord);
  WINADVAPI HANDLE WINAPI OpenEventLogA(LPCSTR lpUNCServerName,LPCSTR lpSourceName);
  WINADVAPI HANDLE WINAPI OpenEventLogW(LPCWSTR lpUNCServerName,LPCWSTR lpSourceName);
  WINADVAPI HANDLE WINAPI RegisterEventSourceA(LPCSTR lpUNCServerName,LPCSTR lpSourceName);
  WINADVAPI HANDLE WINAPI RegisterEventSourceW(LPCWSTR lpUNCServerName,LPCWSTR lpSourceName);
  WINADVAPI HANDLE WINAPI OpenBackupEventLogA(LPCSTR lpUNCServerName,LPCSTR lpFileName);
  WINADVAPI HANDLE WINAPI OpenBackupEventLogW(LPCWSTR lpUNCServerName,LPCWSTR lpFileName);
  WINADVAPI WINBOOL WINAPI ReadEventLogA(HANDLE hEventLog,DWORD dwReadFlags,DWORD dwRecordOffset,LPVOID lpBuffer,DWORD nNumberOfBytesToRead,DWORD *pnBytesRead,DWORD *pnMinNumberOfBytesNeeded);
  WINADVAPI WINBOOL WINAPI ReadEventLogW(HANDLE hEventLog,DWORD dwReadFlags,DWORD dwRecordOffset,LPVOID lpBuffer,DWORD nNumberOfBytesToRead,DWORD *pnBytesRead,DWORD *pnMinNumberOfBytesNeeded);
  WINADVAPI WINBOOL WINAPI ReportEventA(HANDLE hEventLog,WORD wType,WORD wCategory,DWORD dwEventID,PSID lpUserSid,WORD wNumStrings,DWORD dwDataSize,LPCSTR *lpStrings,LPVOID lpRawData);
  WINADVAPI WINBOOL WINAPI ReportEventW(HANDLE hEventLog,WORD wType,WORD wCategory,DWORD dwEventID,PSID lpUserSid,WORD wNumStrings,DWORD dwDataSize,LPCWSTR *lpStrings,LPVOID lpRawData);

#define EVENTLOG_FULL_INFO 0

  typedef struct _EVENTLOG_FULL_INFORMATION {
    DWORD dwFull;
  } EVENTLOG_FULL_INFORMATION,*LPEVENTLOG_FULL_INFORMATION;

  WINADVAPI WINBOOL WINAPI GetEventLogInformation(HANDLE hEventLog,DWORD dwInfoLevel,LPVOID lpBuffer,DWORD cbBufSize,LPDWORD pcbBytesNeeded);
  WINADVAPI WINBOOL WINAPI DuplicateToken(HANDLE ExistingTokenHandle,SECURITY_IMPERSONATION_LEVEL ImpersonationLevel,PHANDLE DuplicateTokenHandle);
  WINADVAPI WINBOOL WINAPI GetKernelObjectSecurity(HANDLE Handle,SECURITY_INFORMATION RequestedInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,DWORD nLength,LPDWORD lpnLengthNeeded);
  WINADVAPI WINBOOL WINAPI ImpersonateNamedPipeClient(HANDLE hNamedPipe);
  WINADVAPI WINBOOL WINAPI ImpersonateSelf(SECURITY_IMPERSONATION_LEVEL ImpersonationLevel);
  WINADVAPI WINBOOL WINAPI RevertToSelf(VOID);
  WINADVAPI WINBOOL WINAPI SetThreadToken (PHANDLE Thread,HANDLE Token);
  WINADVAPI WINBOOL WINAPI AccessCheck(PSECURITY_DESCRIPTOR pSecurityDescriptor,HANDLE ClientToken,DWORD DesiredAccess,PGENERIC_MAPPING GenericMapping,PPRIVILEGE_SET PrivilegeSet,LPDWORD PrivilegeSetLength,LPDWORD GrantedAccess,LPBOOL AccessStatus);
  WINADVAPI WINBOOL WINAPI AccessCheckByType(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID PrincipalSelfSid,HANDLE ClientToken,DWORD DesiredAccess,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,PPRIVILEGE_SET PrivilegeSet,LPDWORD PrivilegeSetLength,LPDWORD GrantedAccess,LPBOOL AccessStatus);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultList(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID PrincipalSelfSid,HANDLE ClientToken,DWORD DesiredAccess,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,PPRIVILEGE_SET PrivilegeSet,LPDWORD PrivilegeSetLength,LPDWORD GrantedAccessList,LPDWORD AccessStatusList);
  WINADVAPI WINBOOL WINAPI OpenProcessToken(HANDLE ProcessHandle,DWORD DesiredAccess,PHANDLE TokenHandle);
  WINADVAPI WINBOOL WINAPI OpenThreadToken(HANDLE ThreadHandle,DWORD DesiredAccess,WINBOOL OpenAsSelf,PHANDLE TokenHandle);
  WINADVAPI WINBOOL WINAPI GetTokenInformation(HANDLE TokenHandle,TOKEN_INFORMATION_CLASS TokenInformationClass,LPVOID TokenInformation,DWORD TokenInformationLength,PDWORD ReturnLength);
  WINADVAPI WINBOOL WINAPI SetTokenInformation(HANDLE TokenHandle,TOKEN_INFORMATION_CLASS TokenInformationClass,LPVOID TokenInformation,DWORD TokenInformationLength);
  WINADVAPI WINBOOL WINAPI AdjustTokenPrivileges(HANDLE TokenHandle,WINBOOL DisableAllPrivileges,PTOKEN_PRIVILEGES NewState,DWORD BufferLength,PTOKEN_PRIVILEGES PreviousState,PDWORD ReturnLength);
  WINADVAPI WINBOOL WINAPI AdjustTokenGroups(HANDLE TokenHandle,WINBOOL ResetToDefault,PTOKEN_GROUPS NewState,DWORD BufferLength,PTOKEN_GROUPS PreviousState,PDWORD ReturnLength);
  WINADVAPI WINBOOL WINAPI PrivilegeCheck(HANDLE ClientToken,PPRIVILEGE_SET RequiredPrivileges,LPBOOL pfResult);
  WINADVAPI WINBOOL WINAPI AccessCheckAndAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,LPSTR ObjectTypeName,LPSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,DWORD DesiredAccess,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPBOOL AccessStatus,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckAndAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,LPWSTR ObjectTypeName,LPWSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,DWORD DesiredAccess,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPBOOL AccessStatus,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeAndAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,LPCSTR ObjectTypeName,LPCSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPBOOL AccessStatus,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeAndAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,LPCWSTR ObjectTypeName,LPCWSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPBOOL AccessStatus,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,LPCSTR ObjectTypeName,LPCSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPDWORD AccessStatusList,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,LPCWSTR ObjectTypeName,LPCWSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPDWORD AccessStatusList,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmByHandleA(LPCSTR SubsystemName,LPVOID HandleId,HANDLE ClientToken,LPCSTR ObjectTypeName,LPCSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPDWORD AccessStatusList,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmByHandleW(LPCWSTR SubsystemName,LPVOID HandleId,HANDLE ClientToken,LPCWSTR ObjectTypeName,LPCWSTR ObjectName,PSECURITY_DESCRIPTOR SecurityDescriptor,PSID PrincipalSelfSid,DWORD DesiredAccess,AUDIT_EVENT_TYPE AuditType,DWORD Flags,POBJECT_TYPE_LIST ObjectTypeList,DWORD ObjectTypeListLength,PGENERIC_MAPPING GenericMapping,WINBOOL ObjectCreation,LPDWORD GrantedAccess,LPDWORD AccessStatusList,LPBOOL pfGenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectOpenAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,LPSTR ObjectTypeName,LPSTR ObjectName,PSECURITY_DESCRIPTOR pSecurityDescriptor,HANDLE ClientToken,DWORD DesiredAccess,DWORD GrantedAccess,PPRIVILEGE_SET Privileges,WINBOOL ObjectCreation,WINBOOL AccessGranted,LPBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectOpenAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,LPWSTR ObjectTypeName,LPWSTR ObjectName,PSECURITY_DESCRIPTOR pSecurityDescriptor,HANDLE ClientToken,DWORD DesiredAccess,DWORD GrantedAccess,PPRIVILEGE_SET Privileges,WINBOOL ObjectCreation,WINBOOL AccessGranted,LPBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectPrivilegeAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,HANDLE ClientToken,DWORD DesiredAccess,PPRIVILEGE_SET Privileges,WINBOOL AccessGranted);
  WINADVAPI WINBOOL WINAPI ObjectPrivilegeAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,HANDLE ClientToken,DWORD DesiredAccess,PPRIVILEGE_SET Privileges,WINBOOL AccessGranted);
  WINADVAPI WINBOOL WINAPI ObjectCloseAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,WINBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectCloseAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,WINBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectDeleteAuditAlarmA(LPCSTR SubsystemName,LPVOID HandleId,WINBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI ObjectDeleteAuditAlarmW(LPCWSTR SubsystemName,LPVOID HandleId,WINBOOL GenerateOnClose);
  WINADVAPI WINBOOL WINAPI PrivilegedServiceAuditAlarmA(LPCSTR SubsystemName,LPCSTR ServiceName,HANDLE ClientToken,PPRIVILEGE_SET Privileges,WINBOOL AccessGranted);
  WINADVAPI WINBOOL WINAPI PrivilegedServiceAuditAlarmW(LPCWSTR SubsystemName,LPCWSTR ServiceName,HANDLE ClientToken,PPRIVILEGE_SET Privileges,WINBOOL AccessGranted);
  WINADVAPI WINBOOL WINAPI IsWellKnownSid(PSID pSid,WELL_KNOWN_SID_TYPE WellKnownSidType);
  WINADVAPI WINBOOL WINAPI CreateWellKnownSid(WELL_KNOWN_SID_TYPE WellKnownSidType,PSID DomainSid,PSID pSid,DWORD *cbSid);
  WINADVAPI WINBOOL WINAPI EqualDomainSid(PSID pSid1,PSID pSid2,WINBOOL *pfEqual);
  WINADVAPI WINBOOL WINAPI GetWindowsAccountDomainSid(PSID pSid,PSID pDomainSid,DWORD *cbDomainSid);
  WINADVAPI WINBOOL WINAPI IsValidSid(PSID pSid);
  WINADVAPI WINBOOL WINAPI EqualSid(PSID pSid1,PSID pSid2);
  WINADVAPI WINBOOL WINAPI EqualPrefixSid(PSID pSid1,PSID pSid2);
  WINADVAPI DWORD WINAPI GetSidLengthRequired (UCHAR nSubAuthorityCount);
  WINADVAPI WINBOOL WINAPI AllocateAndInitializeSid(PSID_IDENTIFIER_AUTHORITY pIdentifierAuthority,BYTE nSubAuthorityCount,DWORD nSubAuthority0,DWORD nSubAuthority1,DWORD nSubAuthority2,DWORD nSubAuthority3,DWORD nSubAuthority4,DWORD nSubAuthority5,DWORD nSubAuthority6,DWORD nSubAuthority7,PSID *pSid);
  WINADVAPI PVOID WINAPI FreeSid(PSID pSid);
  WINADVAPI WINBOOL WINAPI InitializeSid(PSID Sid,PSID_IDENTIFIER_AUTHORITY pIdentifierAuthority,BYTE nSubAuthorityCount);
  WINADVAPI PSID_IDENTIFIER_AUTHORITY WINAPI GetSidIdentifierAuthority(PSID pSid);
  WINADVAPI PDWORD WINAPI GetSidSubAuthority(PSID pSid,DWORD nSubAuthority);
  WINADVAPI PUCHAR WINAPI GetSidSubAuthorityCount(PSID pSid);
  WINADVAPI DWORD WINAPI GetLengthSid(PSID pSid);
  WINADVAPI WINBOOL WINAPI CopySid(DWORD nDestinationSidLength,PSID pDestinationSid,PSID pSourceSid);
  WINADVAPI WINBOOL WINAPI AreAllAccessesGranted(DWORD GrantedAccess,DWORD DesiredAccess);
  WINADVAPI WINBOOL WINAPI AreAnyAccessesGranted(DWORD GrantedAccess,DWORD DesiredAccess);
  WINADVAPI VOID WINAPI MapGenericMask(PDWORD AccessMask,PGENERIC_MAPPING GenericMapping);
  WINADVAPI WINBOOL WINAPI IsValidAcl(PACL pAcl);
  WINADVAPI WINBOOL WINAPI InitializeAcl(PACL pAcl,DWORD nAclLength,DWORD dwAclRevision);
  WINADVAPI WINBOOL WINAPI GetAclInformation(PACL pAcl,LPVOID pAclInformation,DWORD nAclInformationLength,ACL_INFORMATION_CLASS dwAclInformationClass);
  WINADVAPI WINBOOL WINAPI SetAclInformation(PACL pAcl,LPVOID pAclInformation,DWORD nAclInformationLength,ACL_INFORMATION_CLASS dwAclInformationClass);
  WINADVAPI WINBOOL WINAPI AddAce(PACL pAcl,DWORD dwAceRevision,DWORD dwStartingAceIndex,LPVOID pAceList,DWORD nAceListLength);
  WINADVAPI WINBOOL WINAPI DeleteAce(PACL pAcl,DWORD dwAceIndex);
  WINADVAPI WINBOOL WINAPI GetAce(PACL pAcl,DWORD dwAceIndex,LPVOID *pAce);
  WINADVAPI WINBOOL WINAPI AddAccessAllowedAce(PACL pAcl,DWORD dwAceRevision,DWORD AccessMask,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAccessAllowedAceEx(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD AccessMask,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAccessDeniedAce(PACL pAcl,DWORD dwAceRevision,DWORD AccessMask,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAccessDeniedAceEx(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD AccessMask,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAuditAccessAce(PACL pAcl,DWORD dwAceRevision,DWORD dwAccessMask,PSID pSid,WINBOOL bAuditSuccess,WINBOOL bAuditFailure);
  WINADVAPI WINBOOL WINAPI AddAuditAccessAceEx(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD dwAccessMask,PSID pSid,WINBOOL bAuditSuccess,WINBOOL bAuditFailure);
  WINADVAPI WINBOOL WINAPI AddAccessAllowedObjectAce(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD AccessMask,GUID *ObjectTypeGuid,GUID *InheritedObjectTypeGuid,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAccessDeniedObjectAce(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD AccessMask,GUID *ObjectTypeGuid,GUID *InheritedObjectTypeGuid,PSID pSid);
  WINADVAPI WINBOOL WINAPI AddAuditAccessObjectAce(PACL pAcl,DWORD dwAceRevision,DWORD AceFlags,DWORD AccessMask,GUID *ObjectTypeGuid,GUID *InheritedObjectTypeGuid,PSID pSid,WINBOOL bAuditSuccess,WINBOOL bAuditFailure);
  WINADVAPI WINBOOL WINAPI FindFirstFreeAce(PACL pAcl,LPVOID *pAce);
  WINADVAPI WINBOOL WINAPI InitializeSecurityDescriptor(PSECURITY_DESCRIPTOR pSecurityDescriptor,DWORD dwRevision);
  WINADVAPI WINBOOL WINAPI IsValidSecurityDescriptor(PSECURITY_DESCRIPTOR pSecurityDescriptor);
  WINADVAPI DWORD WINAPI GetSecurityDescriptorLength(PSECURITY_DESCRIPTOR pSecurityDescriptor);
  WINADVAPI WINBOOL WINAPI GetSecurityDescriptorControl(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSECURITY_DESCRIPTOR_CONTROL pControl,LPDWORD lpdwRevision);
  WINADVAPI WINBOOL WINAPI SetSecurityDescriptorControl(PSECURITY_DESCRIPTOR pSecurityDescriptor,SECURITY_DESCRIPTOR_CONTROL ControlBitsOfInterest,SECURITY_DESCRIPTOR_CONTROL ControlBitsToSet);
  WINADVAPI WINBOOL WINAPI SetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR pSecurityDescriptor,WINBOOL bDaclPresent,PACL pDacl,WINBOOL bDaclDefaulted);
  WINADVAPI WINBOOL WINAPI GetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR pSecurityDescriptor,LPBOOL lpbDaclPresent,PACL *pDacl,LPBOOL lpbDaclDefaulted);
  WINADVAPI WINBOOL WINAPI SetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR pSecurityDescriptor,WINBOOL bSaclPresent,PACL pSacl,WINBOOL bSaclDefaulted);
  WINADVAPI WINBOOL WINAPI GetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR pSecurityDescriptor,LPBOOL lpbSaclPresent,PACL *pSacl,LPBOOL lpbSaclDefaulted);
  WINADVAPI WINBOOL WINAPI SetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID pOwner,WINBOOL bOwnerDefaulted);
  WINADVAPI WINBOOL WINAPI GetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID *pOwner,LPBOOL lpbOwnerDefaulted);
  WINADVAPI WINBOOL WINAPI SetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID pGroup,WINBOOL bGroupDefaulted);
  WINADVAPI WINBOOL WINAPI GetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR pSecurityDescriptor,PSID *pGroup,LPBOOL lpbGroupDefaulted);
  WINADVAPI DWORD WINAPI SetSecurityDescriptorRMControl(PSECURITY_DESCRIPTOR SecurityDescriptor,PUCHAR RMControl);
  WINADVAPI DWORD WINAPI GetSecurityDescriptorRMControl(PSECURITY_DESCRIPTOR SecurityDescriptor,PUCHAR RMControl);
  WINADVAPI WINBOOL WINAPI CreatePrivateObjectSecurity(PSECURITY_DESCRIPTOR ParentDescriptor,PSECURITY_DESCRIPTOR CreatorDescriptor,PSECURITY_DESCRIPTOR *NewDescriptor,WINBOOL IsDirectoryObject,HANDLE Token,PGENERIC_MAPPING GenericMapping);
  WINADVAPI WINBOOL WINAPI ConvertToAutoInheritPrivateObjectSecurity(PSECURITY_DESCRIPTOR ParentDescriptor,PSECURITY_DESCRIPTOR CurrentSecurityDescriptor,PSECURITY_DESCRIPTOR *NewSecurityDescriptor,GUID *ObjectType,BOOLEAN IsDirectoryObject,PGENERIC_MAPPING GenericMapping);
  WINADVAPI WINBOOL WINAPI CreatePrivateObjectSecurityEx(PSECURITY_DESCRIPTOR ParentDescriptor,PSECURITY_DESCRIPTOR CreatorDescriptor,PSECURITY_DESCRIPTOR *NewDescriptor,GUID *ObjectType,WINBOOL IsContainerObject,ULONG AutoInheritFlags,HANDLE Token,PGENERIC_MAPPING GenericMapping);
  WINADVAPI WINBOOL WINAPI CreatePrivateObjectSecurityWithMultipleInheritance(PSECURITY_DESCRIPTOR ParentDescriptor,PSECURITY_DESCRIPTOR CreatorDescriptor,PSECURITY_DESCRIPTOR *NewDescriptor,GUID **ObjectTypes,ULONG GuidCount,WINBOOL IsContainerObject,ULONG AutoInheritFlags,HANDLE Token,PGENERIC_MAPPING GenericMapping);
  WINADVAPI WINBOOL WINAPI SetPrivateObjectSecurity (SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR ModificationDescriptor,PSECURITY_DESCRIPTOR *ObjectsSecurityDescriptor,PGENERIC_MAPPING GenericMapping,HANDLE Token);
  WINADVAPI WINBOOL WINAPI SetPrivateObjectSecurityEx (SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR ModificationDescriptor,PSECURITY_DESCRIPTOR *ObjectsSecurityDescriptor,ULONG AutoInheritFlags,PGENERIC_MAPPING GenericMapping,HANDLE Token);
  WINADVAPI WINBOOL WINAPI GetPrivateObjectSecurity(PSECURITY_DESCRIPTOR ObjectDescriptor,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR ResultantDescriptor,DWORD DescriptorLength,PDWORD ReturnLength);
  WINADVAPI WINBOOL WINAPI DestroyPrivateObjectSecurity(PSECURITY_DESCRIPTOR *ObjectDescriptor);
  WINADVAPI WINBOOL WINAPI MakeSelfRelativeSD(PSECURITY_DESCRIPTOR pAbsoluteSecurityDescriptor,PSECURITY_DESCRIPTOR pSelfRelativeSecurityDescriptor,LPDWORD lpdwBufferLength);
  WINADVAPI WINBOOL WINAPI MakeAbsoluteSD(PSECURITY_DESCRIPTOR pSelfRelativeSecurityDescriptor,PSECURITY_DESCRIPTOR pAbsoluteSecurityDescriptor,LPDWORD lpdwAbsoluteSecurityDescriptorSize,PACL pDacl,LPDWORD lpdwDaclSize,PACL pSacl,LPDWORD lpdwSaclSize,PSID pOwner,LPDWORD lpdwOwnerSize,PSID pPrimaryGroup,LPDWORD lpdwPrimaryGroupSize);
  WINADVAPI WINBOOL WINAPI MakeAbsoluteSD2(PSECURITY_DESCRIPTOR pSelfRelativeSecurityDescriptor,LPDWORD lpdwBufferSize);
  WINADVAPI WINBOOL WINAPI SetFileSecurityA(LPCSTR lpFileName,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor);
  WINADVAPI WINBOOL WINAPI SetFileSecurityW(LPCWSTR lpFileName,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor);
  WINADVAPI WINBOOL WINAPI GetFileSecurityA(LPCSTR lpFileName,SECURITY_INFORMATION RequestedInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,DWORD nLength,LPDWORD lpnLengthNeeded);
  WINADVAPI WINBOOL WINAPI GetFileSecurityW(LPCWSTR lpFileName,SECURITY_INFORMATION RequestedInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,DWORD nLength,LPDWORD lpnLengthNeeded);
  WINADVAPI WINBOOL WINAPI SetKernelObjectSecurity(HANDLE Handle,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR SecurityDescriptor);
  WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationA(LPCSTR lpPathName,WINBOOL bWatchSubtree,DWORD dwNotifyFilter);
  WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationW(LPCWSTR lpPathName,WINBOOL bWatchSubtree,DWORD dwNotifyFilter);
  WINBASEAPI WINBOOL WINAPI FindNextChangeNotification(HANDLE hChangeHandle);
  WINBASEAPI WINBOOL WINAPI FindCloseChangeNotification(HANDLE hChangeHandle);
  WINBASEAPI WINBOOL WINAPI ReadDirectoryChangesW(HANDLE hDirectory,LPVOID lpBuffer,DWORD nBufferLength,WINBOOL bWatchSubtree,DWORD dwNotifyFilter,LPDWORD lpBytesReturned,LPOVERLAPPED lpOverlapped,LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine);
  WINBASEAPI WINBOOL WINAPI VirtualLock(LPVOID lpAddress,SIZE_T dwSize);
  WINBASEAPI WINBOOL WINAPI VirtualUnlock(LPVOID lpAddress,SIZE_T dwSize);
  WINBASEAPI LPVOID WINAPI MapViewOfFileEx(HANDLE hFileMappingObject,DWORD dwDesiredAccess,DWORD dwFileOffsetHigh,DWORD dwFileOffsetLow,SIZE_T dwNumberOfBytesToMap,LPVOID lpBaseAddress);
  WINBASEAPI WINBOOL WINAPI SetPriorityClass(HANDLE hProcess,DWORD dwPriorityClass);
  WINBASEAPI DWORD WINAPI GetPriorityClass(HANDLE hProcess);
  WINBASEAPI WINBOOL WINAPI IsBadReadPtr(CONST VOID *lp,UINT_PTR ucb);
  WINBASEAPI WINBOOL WINAPI IsBadWritePtr(LPVOID lp,UINT_PTR ucb);
  WINBASEAPI WINBOOL WINAPI IsBadHugeReadPtr(CONST VOID *lp,UINT_PTR ucb);
  WINBASEAPI WINBOOL WINAPI IsBadHugeWritePtr(LPVOID lp,UINT_PTR ucb);
  WINBASEAPI WINBOOL WINAPI IsBadCodePtr(FARPROC lpfn);
  WINBASEAPI WINBOOL WINAPI IsBadStringPtrA(LPCSTR lpsz,UINT_PTR ucchMax);
  WINBASEAPI WINBOOL WINAPI IsBadStringPtrW(LPCWSTR lpsz,UINT_PTR ucchMax);
  WINADVAPI WINBOOL WINAPI LookupAccountSidA(LPCSTR lpSystemName,PSID Sid,LPSTR Name,LPDWORD cchName,LPSTR ReferencedDomainName,LPDWORD cchReferencedDomainName,PSID_NAME_USE peUse);
  WINADVAPI WINBOOL WINAPI LookupAccountSidW(LPCWSTR lpSystemName,PSID Sid,LPWSTR Name,LPDWORD cchName,LPWSTR ReferencedDomainName,LPDWORD cchReferencedDomainName,PSID_NAME_USE peUse);
  WINADVAPI WINBOOL WINAPI LookupAccountNameA(LPCSTR lpSystemName,LPCSTR lpAccountName,PSID Sid,LPDWORD cbSid,LPSTR ReferencedDomainName,LPDWORD cchReferencedDomainName,PSID_NAME_USE peUse);
  WINADVAPI WINBOOL WINAPI LookupAccountNameW(LPCWSTR lpSystemName,LPCWSTR lpAccountName,PSID Sid,LPDWORD cbSid,LPWSTR ReferencedDomainName,LPDWORD cchReferencedDomainName,PSID_NAME_USE peUse);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeValueA(LPCSTR lpSystemName,LPCSTR lpName,PLUID lpLuid);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeValueW(LPCWSTR lpSystemName,LPCWSTR lpName,PLUID lpLuid);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeNameA(LPCSTR lpSystemName,PLUID lpLuid,LPSTR lpName,LPDWORD cchName);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeNameW(LPCWSTR lpSystemName,PLUID lpLuid,LPWSTR lpName,LPDWORD cchName);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeDisplayNameA(LPCSTR lpSystemName,LPCSTR lpName,LPSTR lpDisplayName,LPDWORD cchDisplayName,LPDWORD lpLanguageId);
  WINADVAPI WINBOOL WINAPI LookupPrivilegeDisplayNameW(LPCWSTR lpSystemName,LPCWSTR lpName,LPWSTR lpDisplayName,LPDWORD cchDisplayName,LPDWORD lpLanguageId);
  WINADVAPI WINBOOL WINAPI AllocateLocallyUniqueId(PLUID Luid);
  WINBASEAPI WINBOOL WINAPI BuildCommDCBA(LPCSTR lpDef,LPDCB lpDCB);
  WINBASEAPI WINBOOL WINAPI BuildCommDCBW(LPCWSTR lpDef,LPDCB lpDCB);
  WINBASEAPI WINBOOL WINAPI BuildCommDCBAndTimeoutsA(LPCSTR lpDef,LPDCB lpDCB,LPCOMMTIMEOUTS lpCommTimeouts);
  WINBASEAPI WINBOOL WINAPI BuildCommDCBAndTimeoutsW(LPCWSTR lpDef,LPDCB lpDCB,LPCOMMTIMEOUTS lpCommTimeouts);
  WINBASEAPI WINBOOL WINAPI CommConfigDialogA(LPCSTR lpszName,HWND hWnd,LPCOMMCONFIG lpCC);
  WINBASEAPI WINBOOL WINAPI CommConfigDialogW(LPCWSTR lpszName,HWND hWnd,LPCOMMCONFIG lpCC);
  WINBASEAPI WINBOOL WINAPI GetDefaultCommConfigA(LPCSTR lpszName,LPCOMMCONFIG lpCC,LPDWORD lpdwSize);
  WINBASEAPI WINBOOL WINAPI GetDefaultCommConfigW(LPCWSTR lpszName,LPCOMMCONFIG lpCC,LPDWORD lpdwSize);
  WINBASEAPI WINBOOL WINAPI SetDefaultCommConfigA(LPCSTR lpszName,LPCOMMCONFIG lpCC,DWORD dwSize);
  WINBASEAPI WINBOOL WINAPI SetDefaultCommConfigW(LPCWSTR lpszName,LPCOMMCONFIG lpCC,DWORD dwSize);

#define MAX_COMPUTERNAME_LENGTH 15

  WINBASEAPI WINBOOL WINAPI GetComputerNameA(LPSTR lpBuffer,LPDWORD nSize);
  WINBASEAPI WINBOOL WINAPI GetComputerNameW(LPWSTR lpBuffer,LPDWORD nSize);
  WINBASEAPI WINBOOL WINAPI SetComputerNameA(LPCSTR lpComputerName);
  WINBASEAPI WINBOOL WINAPI SetComputerNameW(LPCWSTR lpComputerName);

  typedef enum _COMPUTER_NAME_FORMAT {
    ComputerNameNetBIOS,ComputerNameDnsHostname,ComputerNameDnsDomain,ComputerNameDnsFullyQualified,ComputerNamePhysicalNetBIOS,ComputerNamePhysicalDnsHostname,ComputerNamePhysicalDnsDomain,ComputerNamePhysicalDnsFullyQualified,ComputerNameMax
  } COMPUTER_NAME_FORMAT;

  WINBASEAPI WINBOOL WINAPI GetComputerNameExA(COMPUTER_NAME_FORMAT NameType,LPSTR lpBuffer,LPDWORD nSize);
  WINBASEAPI WINBOOL WINAPI GetComputerNameExW(COMPUTER_NAME_FORMAT NameType,LPWSTR lpBuffer,LPDWORD nSize);
  WINBASEAPI WINBOOL WINAPI SetComputerNameExA(COMPUTER_NAME_FORMAT NameType,LPCSTR lpBuffer);
  WINBASEAPI WINBOOL WINAPI SetComputerNameExW(COMPUTER_NAME_FORMAT NameType,LPCWSTR lpBuffer);
  WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameA(LPCSTR Hostname,LPSTR ComputerName,LPDWORD nSize);
  WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameW(LPCWSTR Hostname,LPWSTR ComputerName,LPDWORD nSize);
  WINADVAPI WINBOOL WINAPI GetUserNameA(LPSTR lpBuffer,LPDWORD pcbBuffer);
  WINADVAPI WINBOOL WINAPI GetUserNameW(LPWSTR lpBuffer,LPDWORD pcbBuffer);

#define LOGON32_LOGON_INTERACTIVE 2
#define LOGON32_LOGON_NETWORK 3
#define LOGON32_LOGON_BATCH 4
#define LOGON32_LOGON_SERVICE 5
#define LOGON32_LOGON_UNLOCK 7
#define LOGON32_LOGON_NETWORK_CLEARTEXT 8
#define LOGON32_LOGON_NEW_CREDENTIALS 9

#define LOGON32_PROVIDER_DEFAULT 0
#define LOGON32_PROVIDER_WINNT35 1
#define LOGON32_PROVIDER_WINNT40 2
#define LOGON32_PROVIDER_WINNT50 3

#ifdef UNICODE
#define LogonUser LogonUserW
#define LogonUserEx LogonUserExW
#define CreateProcessAsUser CreateProcessAsUserW
#else
#define LogonUser LogonUserA
#define LogonUserEx LogonUserExA
#define CreateProcessAsUser CreateProcessAsUserA
#endif

  WINADVAPI WINBOOL WINAPI LogonUserA(LPCSTR lpszUsername,LPCSTR lpszDomain,LPCSTR lpszPassword,DWORD dwLogonType,DWORD dwLogonProvider,PHANDLE phToken);
  WINADVAPI WINBOOL WINAPI LogonUserW(LPCWSTR lpszUsername,LPCWSTR lpszDomain,LPCWSTR lpszPassword,DWORD dwLogonType,DWORD dwLogonProvider,PHANDLE phToken);
  WINADVAPI WINBOOL WINAPI LogonUserExA(LPCSTR lpszUsername,LPCSTR lpszDomain,LPCSTR lpszPassword,DWORD dwLogonType,DWORD dwLogonProvider,PHANDLE phToken,PSID *ppLogonSid,PVOID *ppProfileBuffer,LPDWORD pdwProfileLength,PQUOTA_LIMITS pQuotaLimits);
  WINADVAPI WINBOOL WINAPI LogonUserExW(LPCWSTR lpszUsername,LPCWSTR lpszDomain,LPCWSTR lpszPassword,DWORD dwLogonType,DWORD dwLogonProvider,PHANDLE phToken,PSID *ppLogonSid,PVOID *ppProfileBuffer,LPDWORD pdwProfileLength,PQUOTA_LIMITS pQuotaLimits);
  WINADVAPI WINBOOL WINAPI ImpersonateLoggedOnUser(HANDLE hToken);
  WINADVAPI WINBOOL WINAPI CreateProcessAsUserA(HANDLE hToken,LPCSTR lpApplicationName,LPSTR lpCommandLine,LPSECURITY_ATTRIBUTES lpProcessAttributes,LPSECURITY_ATTRIBUTES lpThreadAttributes,WINBOOL bInheritHandles,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCSTR lpCurrentDirectory,LPSTARTUPINFOA lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);
  WINADVAPI WINBOOL WINAPI CreateProcessAsUserW(HANDLE hToken,LPCWSTR lpApplicationName,LPWSTR lpCommandLine,LPSECURITY_ATTRIBUTES lpProcessAttributes,LPSECURITY_ATTRIBUTES lpThreadAttributes,WINBOOL bInheritHandles,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCWSTR lpCurrentDirectory,LPSTARTUPINFOW lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);

#define LOGON_WITH_PROFILE 0x1
#define LOGON_NETCREDENTIALS_ONLY 0x2
#define LOGON_ZERO_PASSWORD_BUFFER 0x80000000

  WINADVAPI WINBOOL WINAPI CreateProcessWithLogonW(LPCWSTR lpUsername,LPCWSTR lpDomain,LPCWSTR lpPassword,DWORD dwLogonFlags,LPCWSTR lpApplicationName,LPWSTR lpCommandLine,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCWSTR lpCurrentDirectory,LPSTARTUPINFOW lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);
  WINADVAPI WINBOOL WINAPI CreateProcessWithTokenW(HANDLE hToken,DWORD dwLogonFlags,LPCWSTR lpApplicationName,LPWSTR lpCommandLine,DWORD dwCreationFlags,LPVOID lpEnvironment,LPCWSTR lpCurrentDirectory,LPSTARTUPINFOW lpStartupInfo,LPPROCESS_INFORMATION lpProcessInformation);
  WINADVAPI WINBOOL WINAPI ImpersonateAnonymousToken(HANDLE ThreadHandle);
  WINADVAPI WINBOOL WINAPI DuplicateTokenEx(HANDLE hExistingToken,DWORD dwDesiredAccess,LPSECURITY_ATTRIBUTES lpTokenAttributes,SECURITY_IMPERSONATION_LEVEL ImpersonationLevel,TOKEN_TYPE TokenType,PHANDLE phNewToken);
  WINADVAPI WINBOOL WINAPI CreateRestrictedToken(HANDLE ExistingTokenHandle,DWORD Flags,DWORD DisableSidCount,PSID_AND_ATTRIBUTES SidsToDisable,DWORD DeletePrivilegeCount,PLUID_AND_ATTRIBUTES PrivilegesToDelete,DWORD RestrictedSidCount,PSID_AND_ATTRIBUTES SidsToRestrict,PHANDLE NewTokenHandle);
  WINADVAPI WINBOOL WINAPI IsTokenRestricted(HANDLE TokenHandle);
  WINADVAPI WINBOOL WINAPI IsTokenUntrusted(HANDLE TokenHandle);
  WINADVAPI WINBOOL WINAPI CheckTokenMembership(HANDLE TokenHandle,PSID SidToCheck,PBOOL IsMember);

  typedef WAITORTIMERCALLBACKFUNC WAITORTIMERCALLBACK;

  WINBASEAPI WINBOOL WINAPI RegisterWaitForSingleObject(PHANDLE phNewWaitObject,HANDLE hObject,WAITORTIMERCALLBACK Callback,PVOID Context,ULONG dwMilliseconds,ULONG dwFlags);
  WINBASEAPI HANDLE WINAPI RegisterWaitForSingleObjectEx(HANDLE hObject,WAITORTIMERCALLBACK Callback,PVOID Context,ULONG dwMilliseconds,ULONG dwFlags);
  WINBASEAPI WINBOOL WINAPI UnregisterWait(HANDLE WaitHandle);
  WINBASEAPI WINBOOL WINAPI UnregisterWaitEx(HANDLE WaitHandle,HANDLE CompletionEvent);
  WINBASEAPI WINBOOL WINAPI QueueUserWorkItem(LPTHREAD_START_ROUTINE Function,PVOID Context,ULONG Flags);
  WINBASEAPI WINBOOL WINAPI BindIoCompletionCallback(HANDLE FileHandle,LPOVERLAPPED_COMPLETION_ROUTINE Function,ULONG Flags);
  WINBASEAPI HANDLE WINAPI CreateTimerQueue(VOID);
  WINBASEAPI WINBOOL WINAPI CreateTimerQueueTimer(PHANDLE phNewTimer,HANDLE TimerQueue,WAITORTIMERCALLBACK Callback,PVOID Parameter,DWORD DueTime,DWORD Period,ULONG Flags);
  WINBASEAPI WINBOOL WINAPI ChangeTimerQueueTimer(HANDLE TimerQueue,HANDLE Timer,ULONG DueTime,ULONG Period);
  WINBASEAPI WINBOOL WINAPI DeleteTimerQueueTimer(HANDLE TimerQueue,HANDLE Timer,HANDLE CompletionEvent);
  WINBASEAPI WINBOOL WINAPI DeleteTimerQueueEx(HANDLE TimerQueue,HANDLE CompletionEvent);
  WINBASEAPI HANDLE WINAPI SetTimerQueueTimer(HANDLE TimerQueue,WAITORTIMERCALLBACK Callback,PVOID Parameter,DWORD DueTime,DWORD Period,WINBOOL PreferIo);
  WINBASEAPI WINBOOL WINAPI CancelTimerQueueTimer(HANDLE TimerQueue,HANDLE Timer);
  WINBASEAPI WINBOOL WINAPI DeleteTimerQueue(HANDLE TimerQueue);

#define HW_PROFILE_GUIDLEN 39
#define MAX_PROFILE_LEN 80

#define DOCKINFO_UNDOCKED (0x1)
#define DOCKINFO_DOCKED (0x2)
#define DOCKINFO_USER_SUPPLIED (0x4)
#define DOCKINFO_USER_UNDOCKED (DOCKINFO_USER_SUPPLIED | DOCKINFO_UNDOCKED)
#define DOCKINFO_USER_DOCKED (DOCKINFO_USER_SUPPLIED | DOCKINFO_DOCKED)

  typedef struct tagHW_PROFILE_INFOA {
    DWORD dwDockInfo;
    CHAR szHwProfileGuid[HW_PROFILE_GUIDLEN];
    CHAR szHwProfileName[MAX_PROFILE_LEN];
  } HW_PROFILE_INFOA,*LPHW_PROFILE_INFOA;

  typedef struct tagHW_PROFILE_INFOW {
    DWORD dwDockInfo;
    WCHAR szHwProfileGuid[HW_PROFILE_GUIDLEN];
    WCHAR szHwProfileName[MAX_PROFILE_LEN];
  } HW_PROFILE_INFOW,*LPHW_PROFILE_INFOW;

#ifdef UNICODE
  typedef HW_PROFILE_INFOW HW_PROFILE_INFO;
  typedef LPHW_PROFILE_INFOW LPHW_PROFILE_INFO;
#else
  typedef HW_PROFILE_INFOA HW_PROFILE_INFO;
  typedef LPHW_PROFILE_INFOA LPHW_PROFILE_INFO;
#endif

#ifdef UNICODE
#define GetCurrentHwProfile GetCurrentHwProfileW
#define GetVersionEx GetVersionExW
#define VerifyVersionInfo VerifyVersionInfoW
#else
#define GetCurrentHwProfile GetCurrentHwProfileA
#define GetVersionEx GetVersionExA
#define VerifyVersionInfo VerifyVersionInfoA
#endif

  WINADVAPI WINBOOL WINAPI GetCurrentHwProfileA (LPHW_PROFILE_INFOA lpHwProfileInfo);
  WINADVAPI WINBOOL WINAPI GetCurrentHwProfileW (LPHW_PROFILE_INFOW lpHwProfileInfo);
  WINBASEAPI WINBOOL WINAPI QueryPerformanceCounter(LARGE_INTEGER *lpPerformanceCount);
  WINBASEAPI WINBOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER *lpFrequency);
  WINBASEAPI WINBOOL WINAPI GetVersionExA(LPOSVERSIONINFOA lpVersionInformation);
  WINBASEAPI WINBOOL WINAPI GetVersionExW(LPOSVERSIONINFOW lpVersionInformation);
  WINBASEAPI WINBOOL WINAPI VerifyVersionInfoA(LPOSVERSIONINFOEXA lpVersionInformation,DWORD dwTypeMask,DWORDLONG dwlConditionMask);
  WINBASEAPI WINBOOL WINAPI VerifyVersionInfoW(LPOSVERSIONINFOEXW lpVersionInformation,DWORD dwTypeMask,DWORDLONG dwlConditionMask);

#include <winerror.h>

#define TC_NORMAL 0
#define TC_HARDERR 1
#define TC_GP_TRAP 2
#define TC_SIGNAL 3

#define AC_LINE_OFFLINE 0x0
#define AC_LINE_ONLINE 0x1
#define AC_LINE_BACKUP_POWER 0x2
#define AC_LINE_UNKNOWN 0xff

#define BATTERY_FLAG_HIGH 0x1
#define BATTERY_FLAG_LOW 0x2
#define BATTERY_FLAG_CRITICAL 0x4
#define BATTERY_FLAG_CHARGING 0x8
#define BATTERY_FLAG_NO_BATTERY 0x80
#define BATTERY_FLAG_UNKNOWN 0xff

#define BATTERY_PERCENTAGE_UNKNOWN 0xff

#define BATTERY_LIFE_UNKNOWN 0xffffffff

  typedef struct _SYSTEM_POWER_STATUS {
    BYTE ACLineStatus;
    BYTE BatteryFlag;
    BYTE BatteryLifePercent;
    BYTE Reserved1;
    DWORD BatteryLifeTime;
    DWORD BatteryFullLifeTime;
  } SYSTEM_POWER_STATUS,*LPSYSTEM_POWER_STATUS;

#ifdef UNICODE
#define CreateJobObject CreateJobObjectW
#define OpenJobObject OpenJobObjectW
#define FindFirstVolume FindFirstVolumeW
#define FindNextVolume FindNextVolumeW
#define FindFirstVolumeMountPoint FindFirstVolumeMountPointW
#define FindNextVolumeMountPoint FindNextVolumeMountPointW
#define SetVolumeMountPoint SetVolumeMountPointW
#define DeleteVolumeMountPoint DeleteVolumeMountPointW
#define GetVolumeNameForVolumeMountPoint GetVolumeNameForVolumeMountPointW
#define GetVolumePathName GetVolumePathNameW
#define GetVolumePathNamesForVolumeName GetVolumePathNamesForVolumeNameW
#else
#define CreateJobObject CreateJobObjectA
#define OpenJobObject OpenJobObjectA
#define FindFirstVolume FindFirstVolumeA
#define FindNextVolume FindNextVolumeA
#define FindFirstVolumeMountPoint FindFirstVolumeMountPointA
#define FindNextVolumeMountPoint FindNextVolumeMountPointA
#define SetVolumeMountPoint SetVolumeMountPointA
#define DeleteVolumeMountPoint DeleteVolumeMountPointA
#define GetVolumeNameForVolumeMountPoint GetVolumeNameForVolumeMountPointA
#define GetVolumePathName GetVolumePathNameA
#define GetVolumePathNamesForVolumeName GetVolumePathNamesForVolumeNameA
#endif

  WINBOOL WINAPI GetSystemPowerStatus(LPSYSTEM_POWER_STATUS lpSystemPowerStatus);
  WINBOOL WINAPI SetSystemPowerState(WINBOOL fSuspend,WINBOOL fForce);
  WINBASEAPI WINBOOL WINAPI AllocateUserPhysicalPages(HANDLE hProcess,PULONG_PTR NumberOfPages,PULONG_PTR PageArray);
  WINBASEAPI WINBOOL WINAPI FreeUserPhysicalPages(HANDLE hProcess,PULONG_PTR NumberOfPages,PULONG_PTR PageArray);
  WINBASEAPI WINBOOL WINAPI MapUserPhysicalPages(PVOID VirtualAddress,ULONG_PTR NumberOfPages,PULONG_PTR PageArray);
  WINBASEAPI WINBOOL WINAPI MapUserPhysicalPagesScatter(PVOID *VirtualAddresses,ULONG_PTR NumberOfPages,PULONG_PTR PageArray);
  WINBASEAPI HANDLE WINAPI CreateJobObjectA(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI CreateJobObjectW(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCWSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenJobObjectA(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCSTR lpName);
  WINBASEAPI HANDLE WINAPI OpenJobObjectW(DWORD dwDesiredAccess,WINBOOL bInheritHandle,LPCWSTR lpName);
  WINBASEAPI WINBOOL WINAPI AssignProcessToJobObject(HANDLE hJob,HANDLE hProcess);
  WINBASEAPI WINBOOL WINAPI TerminateJobObject(HANDLE hJob,UINT uExitCode);
  WINBASEAPI WINBOOL WINAPI QueryInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInformationClass,LPVOID lpJobObjectInformation,DWORD cbJobObjectInformationLength,LPDWORD lpReturnLength);
  WINBASEAPI WINBOOL WINAPI SetInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInformationClass,LPVOID lpJobObjectInformation,DWORD cbJobObjectInformationLength);
  WINBASEAPI WINBOOL WINAPI IsProcessInJob(HANDLE ProcessHandle,HANDLE JobHandle,PBOOL Result);
  WINBASEAPI WINBOOL WINAPI CreateJobSet(ULONG NumJob,PJOB_SET_ARRAY UserJobSet,ULONG Flags);
  WINBASEAPI PVOID WINAPI AddVectoredExceptionHandler (ULONG First,PVECTORED_EXCEPTION_HANDLER Handler);
  WINBASEAPI ULONG WINAPI RemoveVectoredExceptionHandler(PVOID Handle);
  WINBASEAPI PVOID WINAPI AddVectoredContinueHandler (ULONG First,PVECTORED_EXCEPTION_HANDLER Handler);
  WINBASEAPI ULONG WINAPI RemoveVectoredContinueHandler(PVOID Handle);
  WINBASEAPI HANDLE WINAPI FindFirstVolumeA(LPSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI HANDLE WINAPI FindFirstVolumeW(LPWSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindNextVolumeA(HANDLE hFindVolume,LPSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindNextVolumeW(HANDLE hFindVolume,LPWSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindVolumeClose(HANDLE hFindVolume);
  WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointA(LPCSTR lpszRootPathName,LPSTR lpszVolumeMountPoint,DWORD cchBufferLength);
  WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointW(LPCWSTR lpszRootPathName,LPWSTR lpszVolumeMountPoint,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointA(HANDLE hFindVolumeMountPoint,LPSTR lpszVolumeMountPoint,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointW(HANDLE hFindVolumeMountPoint,LPWSTR lpszVolumeMountPoint,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI FindVolumeMountPointClose(HANDLE hFindVolumeMountPoint);
  WINBASEAPI WINBOOL WINAPI SetVolumeMountPointA(LPCSTR lpszVolumeMountPoint,LPCSTR lpszVolumeName);
  WINBASEAPI WINBOOL WINAPI SetVolumeMountPointW(LPCWSTR lpszVolumeMountPoint,LPCWSTR lpszVolumeName);
  WINBASEAPI WINBOOL WINAPI DeleteVolumeMountPointA(LPCSTR lpszVolumeMountPoint);
  WINBASEAPI WINBOOL WINAPI DeleteVolumeMountPointW(LPCWSTR lpszVolumeMountPoint);
  WINBASEAPI WINBOOL WINAPI GetVolumeNameForVolumeMountPointA(LPCSTR lpszVolumeMountPoint,LPSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI GetVolumeNameForVolumeMountPointW(LPCWSTR lpszVolumeMountPoint,LPWSTR lpszVolumeName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI GetVolumePathNameA(LPCSTR lpszFileName,LPSTR lpszVolumePathName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI GetVolumePathNameW(LPCWSTR lpszFileName,LPWSTR lpszVolumePathName,DWORD cchBufferLength);
  WINBASEAPI WINBOOL WINAPI GetVolumePathNamesForVolumeNameA(LPCSTR lpszVolumeName,LPCH lpszVolumePathNames,DWORD cchBufferLength,PDWORD lpcchReturnLength);
  WINBASEAPI WINBOOL WINAPI GetVolumePathNamesForVolumeNameW(LPCWSTR lpszVolumeName,LPWCH lpszVolumePathNames,DWORD cchBufferLength,PDWORD lpcchReturnLength);

#define ACTCTX_FLAG_PROCESSOR_ARCHITECTURE_VALID 0x1
#define ACTCTX_FLAG_LANGID_VALID 0x2
#define ACTCTX_FLAG_ASSEMBLY_DIRECTORY_VALID 0x4
#define ACTCTX_FLAG_RESOURCE_NAME_VALID 0x8
#define ACTCTX_FLAG_SET_PROCESS_DEFAULT 0x10
#define ACTCTX_FLAG_APPLICATION_NAME_VALID 0x20
#define ACTCTX_FLAG_SOURCE_IS_ASSEMBLYREF 0x40
#define ACTCTX_FLAG_HMODULE_VALID 0x80

  typedef struct tagACTCTXA {
    ULONG cbSize;
    DWORD dwFlags;
    LPCSTR lpSource;
    USHORT wProcessorArchitecture;
    LANGID wLangId;
    LPCSTR lpAssemblyDirectory;
    LPCSTR lpResourceName;
    LPCSTR lpApplicationName;
    HMODULE hModule;
  } ACTCTXA,*PACTCTXA;

  typedef struct tagACTCTXW {
    ULONG cbSize;
    DWORD dwFlags;
    LPCWSTR lpSource;
    USHORT wProcessorArchitecture;
    LANGID wLangId;
    LPCWSTR lpAssemblyDirectory;
    LPCWSTR lpResourceName;
    LPCWSTR lpApplicationName;
    HMODULE hModule;
  } ACTCTXW,*PACTCTXW;

  typedef const ACTCTXA *PCACTCTXA;
  typedef const ACTCTXW *PCACTCTXW;

#ifdef UNICODE
  typedef ACTCTXW ACTCTX;
  typedef PACTCTXW PACTCTX;
  typedef PCACTCTXW PCACTCTX;
#else
  typedef ACTCTXA ACTCTX;
  typedef PACTCTXA PACTCTX;
  typedef PCACTCTXA PCACTCTX;
#endif

#ifdef UNICODE
#define CreateActCtx CreateActCtxW
#else
#define CreateActCtx CreateActCtxA
#endif

  WINBASEAPI HANDLE WINAPI CreateActCtxA(PCACTCTXA pActCtx);
  WINBASEAPI HANDLE WINAPI CreateActCtxW(PCACTCTXW pActCtx);
  WINBASEAPI VOID WINAPI AddRefActCtx(HANDLE hActCtx);
  WINBASEAPI VOID WINAPI ReleaseActCtx(HANDLE hActCtx);
  WINBASEAPI WINBOOL WINAPI ZombifyActCtx(HANDLE hActCtx);
  WINBASEAPI WINBOOL WINAPI ActivateActCtx(HANDLE hActCtx,ULONG_PTR *lpCookie);

#define DEACTIVATE_ACTCTX_FLAG_FORCE_EARLY_DEACTIVATION (0x1)

  WINBASEAPI WINBOOL WINAPI DeactivateActCtx(DWORD dwFlags,ULONG_PTR ulCookie);
  WINBASEAPI WINBOOL WINAPI GetCurrentActCtx(HANDLE *lphActCtx);

  typedef struct tagACTCTX_SECTION_KEYED_DATA_2600 {
    ULONG cbSize;
    ULONG ulDataFormatVersion;
    PVOID lpData;
    ULONG ulLength;
    PVOID lpSectionGlobalData;
    ULONG ulSectionGlobalDataLength;
    PVOID lpSectionBase;
    ULONG ulSectionTotalLength;
    HANDLE hActCtx;
    ULONG ulAssemblyRosterIndex;
  } ACTCTX_SECTION_KEYED_DATA_2600,*PACTCTX_SECTION_KEYED_DATA_2600;

  typedef const ACTCTX_SECTION_KEYED_DATA_2600 *PCACTCTX_SECTION_KEYED_DATA_2600;

  typedef struct tagACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA {
    PVOID lpInformation;
    PVOID lpSectionBase;
    ULONG ulSectionLength;
    PVOID lpSectionGlobalDataBase;
    ULONG ulSectionGlobalDataLength;
  } ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA,*PACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;

  typedef const ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA *PCACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;

  typedef struct tagACTCTX_SECTION_KEYED_DATA {
    ULONG cbSize;
    ULONG ulDataFormatVersion;
    PVOID lpData;
    ULONG ulLength;
    PVOID lpSectionGlobalData;
    ULONG ulSectionGlobalDataLength;
    PVOID lpSectionBase;
    ULONG ulSectionTotalLength;
    HANDLE hActCtx;
    ULONG ulAssemblyRosterIndex;

    ULONG ulFlags;
    ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA AssemblyMetadata;
  } ACTCTX_SECTION_KEYED_DATA,*PACTCTX_SECTION_KEYED_DATA;

  typedef const ACTCTX_SECTION_KEYED_DATA *PCACTCTX_SECTION_KEYED_DATA;

#define FIND_ACTCTX_SECTION_KEY_RETURN_HACTCTX 0x1
#define FIND_ACTCTX_SECTION_KEY_RETURN_FLAGS 0x2
#define FIND_ACTCTX_SECTION_KEY_RETURN_ASSEMBLY_METADATA 0x4

#ifdef UNICODE
#define FindActCtxSectionString FindActCtxSectionStringW
#else
#define FindActCtxSectionString FindActCtxSectionStringA
#endif

  WINBASEAPI WINBOOL WINAPI FindActCtxSectionStringA(DWORD dwFlags,const GUID *lpExtensionGuid,ULONG ulSectionId,LPCSTR lpStringToFind,PACTCTX_SECTION_KEYED_DATA ReturnedData);
  WINBASEAPI WINBOOL WINAPI FindActCtxSectionStringW(DWORD dwFlags,const GUID *lpExtensionGuid,ULONG ulSectionId,LPCWSTR lpStringToFind,PACTCTX_SECTION_KEYED_DATA ReturnedData);
  WINBASEAPI WINBOOL WINAPI FindActCtxSectionGuid(DWORD dwFlags,const GUID *lpExtensionGuid,ULONG ulSectionId,const GUID *lpGuidToFind,PACTCTX_SECTION_KEYED_DATA ReturnedData);

#ifndef RC_INVOKED
#ifndef ACTIVATION_CONTEXT_BASIC_INFORMATION_DEFINED

  typedef struct _ACTIVATION_CONTEXT_BASIC_INFORMATION {
    HANDLE hActCtx;
    DWORD dwFlags;
  } ACTIVATION_CONTEXT_BASIC_INFORMATION,*PACTIVATION_CONTEXT_BASIC_INFORMATION;

  typedef const struct _ACTIVATION_CONTEXT_BASIC_INFORMATION *PCACTIVATION_CONTEXT_BASIC_INFORMATION;

#define ACTIVATION_CONTEXT_BASIC_INFORMATION_DEFINED 1
#endif
#endif

#define QUERY_ACTCTX_FLAG_USE_ACTIVE_ACTCTX 0x4
#define QUERY_ACTCTX_FLAG_ACTCTX_IS_HMODULE 0x8
#define QUERY_ACTCTX_FLAG_ACTCTX_IS_ADDRESS 0x10
#define QUERY_ACTCTX_FLAG_NO_ADDREF 0x80000000

  WINBASEAPI WINBOOL WINAPI QueryActCtxW(DWORD dwFlags,HANDLE hActCtx,PVOID pvSubInstance,ULONG ulInfoClass,PVOID pvBuffer,SIZE_T cbBuffer,SIZE_T *pcbWrittenOrRequired);

  typedef WINBOOL (WINAPI *PQUERYACTCTXW_FUNC)(DWORD dwFlags,HANDLE hActCtx,PVOID pvSubInstance,ULONG ulInfoClass,PVOID pvBuffer,SIZE_T cbBuffer,SIZE_T *pcbWrittenOrRequired);

  WINBASEAPI WINBOOL WINAPI ProcessIdToSessionId(DWORD dwProcessId,DWORD *pSessionId);
  WINBASEAPI DWORD WINAPI WTSGetActiveConsoleSessionId();
  WINBASEAPI WINBOOL WINAPI IsWow64Process(HANDLE hProcess,PBOOL Wow64Process);
  WINBASEAPI WINBOOL WINAPI GetLogicalProcessorInformation(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION Buffer,PDWORD ReturnedLength);
  WINBASEAPI WINBOOL WINAPI GetNumaHighestNodeNumber(PULONG HighestNodeNumber);
  WINBASEAPI WINBOOL WINAPI GetNumaProcessorNode(UCHAR Processor,PUCHAR NodeNumber);
  WINBASEAPI WINBOOL WINAPI GetNumaNodeProcessorMask(UCHAR Node,PULONGLONG ProcessorMask);
  WINBASEAPI WINBOOL WINAPI GetNumaAvailableMemoryNode(UCHAR Node,PULONGLONG AvailableBytes);

#ifdef __cplusplus
}
#endif
#endif
