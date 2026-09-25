#ifndef V_CMDEXEC_POLL_H
#define V_CMDEXEC_POLL_H

// glibc's <poll.h> only includes <sys/poll.h>. The linuxroot sysroot used for
// `-os linux` cross compilation from macOS ships just the latter, while musl
// warns about <sys/poll.h>. With `-os cross`, the libc is only known where the
// generated C is compiled, not where it was generated, so the C preprocessor
// makes the choice here instead of a `$if musl ?` check.
#if defined(__linux__) && !defined(__ANDROID__)
#include <features.h>
#endif
#if defined(__linux__) && defined(__GLIBC__)
#include <sys/poll.h>
#else
#include <poll.h>
#endif

#endif
