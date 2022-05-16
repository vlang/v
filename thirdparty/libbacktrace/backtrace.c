/* Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Google.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

    (3) The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  */

#define HAVE_ATOMIC_FUNCTIONS 1
#define HAVE_CLOCK_GETTIME 1
#define HAVE_DECL_GETPAGESIZE 0
#define HAVE_DECL_STRNLEN 1
#define HAVE_DL_ITERATE_PHDR 1
#define HAVE_GETIPINFO 1
#define HAVE_LSTAT 1
#define HAVE_READLINK 1
#define HAVE_SYNC_FUNCTIONS 1

#define HAVE_DLFCN_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_LINK_H 1
#define HAVE_MEMORY_H 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_UNISTD_H 1
#define STDC_HEADERS 1
#include <stdint.h>
#if UINTPTR_MAX == 0xFFFFFFFF
  #define BACKTRACE_ELF_SIZE 32
  #define BACKTRACE_XCOFF_SIZE 32
#elif UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFu
  #define BACKTRACE_ELF_SIZE 64
  #define BACKTRACE_XCOFF_SIZE 64
#endif
#ifdef __TINYC__
  #undef HAVE_ATOMIC_FUNCTIONS
  #undef HAVE_SYNC_FUNCTIONS
#endif
#ifndef _WIN32
#define HAVE_FCNTL 1
#endif
#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
  #define HAVE_KERN_PROC 1
  #define HAVE_KERN_PROC_ARGS 1
#endif
#ifdef __APPLE__
  #define HAVE_MACH_O_DYLD_H 1
#endif
#ifndef _ALL_SOURCE
  #define _ALL_SOURCE 1
#endif
#ifndef _GNU_SOURCE
  #define _GNU_SOURCE 1
  #undef HAVE_DL_ITERATE_PHDR
#endif
#ifndef _POSIX_PTHREAD_SEMANTICS
  #define _POSIX_PTHREAD_SEMANTICS 1
#endif
#ifndef _TANDEM_SOURCE
  #define _TANDEM_SOURCE 1
#endif
#ifndef __EXTENSIONS__
  #define __EXTENSIONS__ 1
#endif
#ifndef _DARWIN_USE_64_BIT_INODE
  #define _DARWIN_USE_64_BIT_INODE 1
#endif

#define BACKTRACE_SUPPORTED 1
#define BACKTRACE_USES_MALLOC 1
#define BACKTRACE_SUPPORTS_THREADS 1
#define BACKTRACE_SUPPORTS_DATA 0

#if __TINYC__
  #undef BACKTRACE_SUPPORTED
#endif

#include "base.c"

#if defined(__linux__) || defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
  #include "linux.c"
#elif defined(__APPLE__)
  #include "darwin.c"
#elif defined(_WIN32)
  #include "windows.c"
#endif
