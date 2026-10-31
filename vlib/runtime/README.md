## Description

`runtime` provides access to functions describing the current platform:
- whether it is 32bit or 64bit
- how many CPUs/cores are available
- total/free physical memory
- whether the platform is little endian or big endian
- etc.

On FreeBSD, `used_memory()` reads current resident memory through `kern.proc.rlimit_usage`.
It does not require `libprocstat`. It reports peak resident memory through `getrusage` on
kernels without that interface and when compiled with TinyCC.
