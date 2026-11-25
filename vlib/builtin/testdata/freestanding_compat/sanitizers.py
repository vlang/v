"""Instrument the C adapters under distinct symbols, preserving ASan's host libc.

The ordinary V regression separately links the unchanged adapters freestanding.
This harness adds native memory/error-path coverage without loading ASan into a
no-libc executable or interposing its host FILE objects and allocator.
"""
import os
import pathlib
import subprocess
import sys

fixture = pathlib.Path(__file__).resolve().parent
runtime = fixture.parent.parent / 'linux_bare'
work = pathlib.Path(sys.argv[1]).resolve()
compiler = sys.argv[2] if len(sys.argv) > 2 else 'clang'
work.mkdir(parents=True, exist_ok=True)
base = [compiler, '-std=gnu11', '-D_GNU_SOURCE=1', '-D__NO_INLINE__=1', '-O1', '-g',
        '-fno-builtin', '-fno-omit-frame-pointer', '-fsanitize=address,undefined']
sources = ['libc_descriptors.c', 'libc_socket.c', 'libc_resolver.c', 'libc_stdio.c',
           'libc_format.c', 'libc_services.c', 'libc_mutex.c', 'libc_system.c',
           'libc_strings.c', 'libc_sort.c', 'libc_environment.c', 'libc_locale.c', 'libc_scan.c',
           'libc_scanf.c', 'libc_intscan.c', 'libc_floatscan.c', 'libc_misc.c',
           'libc_realpath.c', 'libc_strftime.c', 'libc_random.c', 'libc_math.c',
           'libc_jump.c', 'libc_exit.c']
objects, symbols = [], set()


def run(command, **kwargs):
    subprocess.run([str(part) for part in command], check=True, **kwargs)


for source in sources:
    output = work / (source + '.o')
    run(base + ['-c', runtime / source, '-o', output])
    objects.append(output)
    listing = subprocess.check_output(['nm', '-g', '--defined-only', str(output)], text=True)
    for line in listing.splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[1] in ['T', 'D', 'B', 'R'] and not parts[2].startswith(('v_', '__odr_asan')):
            symbols.add(parts[2])
mapping = work / 'symbols.txt'
mapping.write_text(''.join(f'{symbol} v_bare_{symbol}\n' for symbol in sorted(symbols)))
for output in objects:
    run(['objcopy', '--redefine-syms=' + str(mapping), output])

headers = ['stdio.h', 'stdlib.h', 'string.h', 'unistd.h', 'fcntl.h', 'time.h', 'signal.h',
           'pthread.h', 'sched.h', 'sys/stat.h', 'sys/socket.h', 'sys/epoll.h',
           'sys/eventfd.h', 'sys/sendfile.h', 'arpa/inet.h', 'netdb.h', 'assert.h',
           'ctype.h', 'dirent.h', 'inttypes.h', 'langinfo.h', 'locale.h', 'math.h',
           'poll.h', 'pwd.h', 'setjmp.h', 'sys/ioctl.h', 'sys/mman.h',
           'sys/select.h', 'sys/shm.h', 'sys/statfs.h', 'sys/time.h', 'sys/uio.h',
           'sys/utsname.h', 'sys/random.h', 'libintl.h', 'wchar.h']
# These ABI entry points are exported by the runtime even when the native
# headers redirect their ordinary declarations to a different language version.
extra = ['extern int (sigsetjmp)(sigjmp_buf, int) __attribute__((returns_twice));',
         'extern int __cxa_atexit(void (*)(void *), void *, void *);',
         'extern void __cxa_finalize(void *);']
for prefix in ['__isoc99_', '__isoc23_']:
    extra += [f'extern int {prefix}sscanf(const char *, const char *, ...);',
              f'extern int {prefix}vsscanf(const char *, const char *, va_list);',
              f'extern int {prefix}fscanf(FILE *, const char *, ...);',
              f'extern int {prefix}vfscanf(FILE *, const char *, va_list);',
              f'extern int {prefix}scanf(const char *, ...);',
              f'extern int {prefix}vscanf(const char *, va_list);']
for name, result in [('strtol', 'long'), ('strtoul', 'unsigned long'),
                     ('strtoll', 'long long'), ('strtoull', 'unsigned long long'),
                     ('strtoimax', 'intmax_t'), ('strtoumax', 'uintmax_t')]:
    extra.append(f'extern {result} __isoc23_{name}(const char *, char **, int);')
namespace = work / 'namespace.h'
namespace.write_text(''.join(f'#include <{header}>\n' for header in headers)
                     + '\n'.join(extra) + '\n'
                     + ''.join(f'extern __typeof__({symbol}) v_bare_{symbol};\n' for symbol in sorted(symbols))
                     + ''.join(f'#undef {symbol}\n#define {symbol}'
                               + ('(...)' if symbol in ['stat', 'stat64'] else '')
                               + f' v_bare_{symbol}'
                               + ('(__VA_ARGS__)' if symbol in ['stat', 'stat64'] else '') + '\n'
                               for symbol in sorted(symbols)))
test_objects = []
for source in ['probe.c', 'stdio_probe.c', 'mutex_probe.c', 'format_probe.c',
               'resolver_probe.c', 'dns_fuzz.c', 'fontstack_probe.c', 'sanitized_main.c']:
    output = work / (source + '.o')
    run(base + ['-D__real_socket=__real_v_bare_socket', '-D__wrap_socket=__wrap_v_bare_socket',
                '-DV_PROBE_SANITIZED=1', '-include', namespace, '-c', fixture / source, '-o', output])
    test_objects.append(output)
allocator = work / 'allocator.o'
run(base + ['-c', fixture / 'sanitized_allocator.c', '-o', allocator])
program = work / 'native-check'
run(base + objects + test_objects + [allocator, '-pthread', '-lm', '-Wl,--wrap=malloc',
    '-Wl,--wrap=calloc', '-Wl,--wrap=realloc', '-Wl,--wrap=free', '-Wl,--wrap=v_bare_socket', '-o', program])
environment = os.environ.copy()
environment['ASAN_OPTIONS'] = 'detect_leaks=1:abort_on_error=1'
environment['UBSAN_OPTIONS'] = 'halt_on_error=1:print_stacktrace=1'
run([program, work], env=environment, timeout=60)
run([sys.executable, fixture / 'dns_fixture.py', work / 'dns', program], env=environment, timeout=90)
print(f'{compiler}: namespaced ASan/UBSan ABI and resolver checks passed')
