"""Run the instrumented V runtime and native ABI adapters alongside ASan's host libc."""
import filecmp
import os
from pathlib import Path
import subprocess
import sys

from sanitizer_runtime import compile_v_runtime

fixture = Path(__file__).resolve().parent
runtime = fixture.parent.parent / 'linux_bare'
work = Path(sys.argv[1]).resolve()
compiler = sys.argv[2] if len(sys.argv) > 2 else 'clang'
work.mkdir(parents=True, exist_ok=True)
flags = [compiler, '-std=gnu11', '-D_GNU_SOURCE=1', '-D__NO_INLINE__=1', '-O1', '-g',
         '-fno-builtin', '-fno-omit-frame-pointer', '-ffunction-sections', '-fdata-sections',
         '-fsanitize=address,undefined']


def run(command, **kwargs):
    subprocess.run(command, check=True, **kwargs)


def definitions(obj):
    listing = subprocess.check_output(['nm', '-g', '--defined-only', '--format=posix', obj], text=True)
    return {line.split()[0] for line in listing.splitlines()
            if not line.startswith(('v_', 'bare_', '__asan', '__odr_asan', '__ubsan'))}


generated = compile_v_runtime(fixture, work, compiler, [*flags, '-fno-inline'])
objects = [generated]
for flag in (work / 'runtime.flags').read_text().split('\0'):
    source = Path(flag)
    if source.suffix == '.o':
        obj = work / source.name
        run(['objcopy', source, obj])
        objects.append(obj)
        continue
    if source.parent != runtime or source.suffix != '.c':
        continue
    obj = work / (source.name + '.o')
    run([*flags, '-c', source, '-o', obj])
    if source.name == 'initializers.c':
        # The host owns the initial thread pointer and process startup.
        run(['objcopy', '--rename-section',
             '.preinit_array=.data.sanitized_preinit,contents,alloc,load,data', obj])
    objects.append(obj)
if len(objects) == 1:
    raise RuntimeError('No C adapters found in the compiler runtime manifest')

public = {obj: definitions(obj) for obj in objects}
symbols = set().union(*public.values())
# Run workers under ASan's pthread interceptor. The allocator wrapper below
# forwards to ASan, including when the resolver injects a failed allocation.
hosted = {'malloc', 'calloc', 'realloc', 'free', 'pthread_create', 'pthread_join'}
mapping = work / 'symbols.txt'
mapping.write_text(''.join(f'{name} v_bare_{name}\n' for name in sorted(symbols - hosted)))
for obj in objects:
    args = ['objcopy', '--redefine-syms=' + str(mapping)]
    for name in hosted & public[obj]:
        args += ['--redefine-sym', f'{name}=v_bare_{name}']
    if obj == generated:
        args += ['--redefine-sym', 'v_freestanding_aligned_alloc=v_sanitized_v_aligned_alloc']
    run([*args, obj])
    if obj == generated:
        # Generated functions call private V heap entry points directly. Use
        # weak definitions so the hosted heap also owns these allocations.
        allocators = ['__malloc', '__calloc', '__free', '__malloc_align', 'realloc']
        run(['objcopy', *['--weaken-symbol=v_bare_' + name for name in allocators], obj])

# Let the native headers select versioned or redirected ABI names. Rename the
# resulting relocations under the same map as the implementation objects.
for name in ['probe.c', 'stdio_probe.c', 'mutex_probe.c', 'format_probe.c', 'resolver_probe.c',
             'dns_fuzz.c', 'fontstack_probe.c', 'sanitized_main.c']:
    obj = work / (name + '.o')
    run([*flags, '-DV_PROBE_SANITIZED=1', '-D__real_socket=__real_v_bare_socket',
         '-D__wrap_socket=__wrap_v_bare_socket', '-c', fixture / name, '-o', obj])
    run(['objcopy', '--redefine-syms=' + str(mapping), obj])
    objects.append(obj)

program = work / 'native-check'
run([*flags, fixture / 'sanitized_allocator.c', *objects, '-pthread', '-lm', '-Wl,--gc-sections',
     '-Wl,--wrap=malloc', '-Wl,--wrap=calloc', '-Wl,--wrap=realloc', '-Wl,--wrap=free',
     '-Wl,--wrap=v_bare_socket', '-o', program])
environment = dict(os.environ, ASAN_OPTIONS='detect_leaks=1:abort_on_error=1',
                   UBSAN_OPTIONS='halt_on_error=1:print_stacktrace=1')
run([program, work], env=environment, timeout=60)
main = work / 'format_main.c'
main.write_text('int v_freestanding_format_oracle(const char *path);\n'
                'int main(int argc, char **argv) {\n'
                '    return argc != 2 || v_freestanding_format_oracle(argv[1]);\n}\n')
oracle = work / 'host-format'
run([compiler, '-O2', '-ffunction-sections', '-fdata-sections', fixture / 'format_probe.c',
     main, '-Wl,--gc-sections', '-lm', '-o', oracle])
host_format = work / 'host-format.bin'
run([oracle, host_format], timeout=60)
if not filecmp.cmp(host_format, work / 'format.bin', shallow=False):
    raise RuntimeError('The instrumented formatting output differs from the host libc oracle')
run([sys.executable, fixture / 'dns_fixture.py', work / 'dns', program], env=environment, timeout=90)
print(f'{compiler}: namespaced ASan/UBSan ABI and resolver checks passed')
