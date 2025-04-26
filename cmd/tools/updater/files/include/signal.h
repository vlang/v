/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef _INC_SIGNAL
#define _INC_SIGNAL

#include <_mingw.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _SIG_ATOMIC_T_DEFINED
#define _SIG_ATOMIC_T_DEFINED
  typedef int sig_atomic_t;
#endif

#define NSIG 23

#define	SIGHUP	1	/* hangup */
#define SIGINT 2
#define	SIGQUIT	3	/* quit */
#define SIGILL 4
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGIOT	6	/* IOT instruction */
#define	SIGABRT 6	/* used by abort, replace SIGIOT in the future */
#define	SIGEMT	7	/* EMT instruction */
#define SIGFPE 8
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define SIGSEGV 11
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#ifdef __USE_MINGW_ALARM
#define	SIGALRM	14	/* alarm clock */
#endif
#define SIGTERM 15
#define SIGBREAK 21
#define SIGABRT2 22

#define SIGABRT_COMPAT 6

  typedef	void (*__p_sig_fn_t)(int);

#define SIG_DFL (__p_sig_fn_t)0
#define SIG_IGN (__p_sig_fn_t)1
#define SIG_GET (__p_sig_fn_t)2
#define SIG_SGE (__p_sig_fn_t)3
#define SIG_ACK (__p_sig_fn_t)4
#define SIG_ERR (__p_sig_fn_t)-1

  extern void **__cdecl __pxcptinfoptrs(void);
#define _pxcptinfoptrs (*__pxcptinfoptrs())

  __p_sig_fn_t __cdecl signal(int _SigNum,__p_sig_fn_t _Func);
  int __cdecl raise(int _SigNum);

#ifdef __cplusplus
}
#endif
#endif
