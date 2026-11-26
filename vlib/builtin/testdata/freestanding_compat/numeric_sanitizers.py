"""Compare namespaced bare numeric/locale routines with the hosted C library."""
from pathlib import Path
import subprocess
import sys

from sanitizer_runtime import compile_v_runtime


def names(obj, mode):
    output = subprocess.check_output(['nm', '-g', mode, '--format=posix', obj], text=True)
    return {line.split()[0] for line in output.splitlines() if line.strip()}


fixture = Path(__file__).resolve().parent
runtime = fixture.parent.parent / 'linux_bare'
work = Path(sys.argv[1]).resolve()
work.mkdir(exist_ok=True, parents=True)
compiler = sys.argv[2] if len(sys.argv) > 2 else 'clang'
base = [compiler, '-O1', '-g', '-D_GNU_SOURCE', '-D__NO_INLINE__=1',
        '-ffunction-sections', '-fdata-sections', '-fno-builtin', '-fsanitize=address,undefined',
        '-fno-sanitize-recover=undefined']
objects, symbols = [], set()
for name in ['scan', 'scanf', 'intscan', 'floatscan', 'locale', 'math', 'random', 'numeric_abi']:
    output = work / (name + '.o')
    subprocess.run(base + ['-c', str(runtime / ('libc_' + name + '.c')),
                           '-o', str(output)], check=True)
    objects.append(output)
    listing = subprocess.check_output(['nm', '-g', '--defined-only', str(output)], text=True)
    for line in listing.splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[1] in ['T', 'B', 'D', 'R']:
            if not parts[2].startswith(('v_', '__odr_asan')):
                symbols.add(parts[2])
# The numeric V providers need no whole-program initialization. Let the host
# start the process and keep the compiler's sanitizer constructors intact.
generated = compile_v_runtime(fixture, work, compiler, base, shim_object=False)
# Keep only generated functions called by these adapters. In particular, their
# ordinary C dependencies (errno, memory allocation and termination) must bind
# to the hosted sanitizer runtime just as the oracle's dependencies do.
roots = set.union(*(names(obj, '--undefined-only') for obj in objects)) & names(generated, '--defined-only')
roots = {name for name in roots if name.startswith('v_')}
providers = work / 'numeric-v.o'
subprocess.run(['ld', '-r', '--gc-sections', *['--undefined=' + name for name in sorted(roots)],
                generated, '-o', providers], check=True)
# Generated V has a few C ABI aliases for its own dependencies. Keep those
# private so they cannot interpose on the host or on the C adapter objects.
private = sorted(name for name in names(providers, '--defined-only') if name not in roots
                 and not name.startswith(('__asan', '__odr_asan')))
if private:
    listing = work / 'private.txt'
    listing.write_text('\n'.join(private) + '\n')
    subprocess.run(['objcopy', '--localize-symbols=' + str(listing), providers], check=True)
# V's bounds and allocation checks retain its emergency exit path. The host
# owns this process's teardown. Treat reaching that path as a failed check.
subprocess.run(['objcopy', '--redefine-sym=v_bare_run_finalizers=abort', providers], check=True)
objects.append(providers)
mapping = work / 'symbols.txt'
mapping.write_text(''.join(f'{name} v_bare_{name}\n' for name in sorted(symbols)))
for output in objects:
    subprocess.run(['objcopy', '--redefine-syms=' + str(mapping), str(output)], check=True)
program = work / 'numeric-check'
subprocess.run(base + [str(fixture / 'numeric_sanitized.c')] + list(map(str, objects))
               + ['-Wl,--gc-sections', '-lm', '-o', str(program)], check=True)
subprocess.run([str(program)], check=True, timeout=60)
