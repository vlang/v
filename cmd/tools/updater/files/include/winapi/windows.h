/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _WINDOWS_
#define _WINDOWS_

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN 1
#endif

#ifndef WINVER
#define WINVER 0x0502
#endif

#include <_mingw.h>

#ifndef _INC_WINDOWS
#define _INC_WINDOWS

#if defined(RC_INVOKED) && !defined(NOWINRES)

#include <winresrc.h>
#else

#ifdef RC_INVOKED
#define NOATOM
#define NOGDI
#define NOGDICAPMASKS
#define NOMETAFILE
#define NOMINMAX
#define NOMSG
#define NOOPENFILE
#define NORASTEROPS
#define NOSCROLL
#define NOSOUND
#define NOSYSMETRICS
#define NOTEXTMETRIC
#define NOWH
#define NOCOMM
#define NOKANJI
#define NOCRYPT
#define NOMCX
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

#ifndef RC_INVOKED
#include <excpt.h>
#include <stdarg.h>
#endif

#include <windef.h>
#include <winbase.h>
#include <wingdi.h>
#include <winuser.h>
#include <winnls.h>
#include <wincon.h>
#include <winver.h>
#include <winreg.h>
//gr #include <winnetwk.h>

#ifndef WIN32_LEAN_AND_MEAN
#include <cderr.h>
#include <dde.h>
#include <ddeml.h>
#include <dlgs.h>
#include <lzexpand.h>
#include <mmsystem.h>
#include <nb30.h>
#include <rpc.h>
#include <shellapi.h>
#include <winperf.h>
#include <winsock.h>
#ifndef NOCRYPT
#include <wincrypt.h>
#include <winefs.h>
#include <winscard.h>
#endif

#ifndef NOUSER
#ifndef NOGDI
#include <winspool.h>
#ifdef INC_OLE1
#include <ole.h>
#else
#include <ole2.h>
#endif
#include <commdlg.h>
#endif
#endif
#endif

//gr #include <stralign.h>

#ifdef INC_OLE2
#include <ole2.h>
#endif

#ifndef NOSERVICE
#include <winsvc.h>
#endif

#ifndef NOMCX
#include <mcx.h>
#endif

#ifndef NOIME
#include <imm.h>
#endif

#endif
#endif
#endif
