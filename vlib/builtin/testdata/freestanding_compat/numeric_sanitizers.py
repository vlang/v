"""Compare namespaced bare numeric/locale routines with the hosted C library."""
from pathlib import Path
import subprocess
import sys

fixture = Path(__file__).resolve().parent
runtime = fixture.parent.parent / 'linux_bare'
work = Path(sys.argv[1]).resolve()
work.mkdir(exist_ok=True, parents=True)
compiler = sys.argv[2] if len(sys.argv) > 2 else 'clang'
base = [compiler, '-O1', '-g', '-D_GNU_SOURCE', '-D__NO_INLINE__=1',
        '-fno-builtin', '-fsanitize=address,undefined']
objects, symbols = [], set()
for name in ['scan', 'scanf', 'intscan', 'floatscan', 'locale', 'math', 'random']:
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
mapping = work / 'symbols.txt'
mapping.write_text(''.join(f'{name} v_bare_{name}\n' for name in sorted(symbols)))
for output in objects:
    subprocess.run(['objcopy', '--redefine-syms=' + str(mapping), str(output)], check=True)
program = work / 'numeric-check'
subprocess.run(base + [str(fixture / 'numeric_sanitized.c')] + list(map(str, objects))
               + ['-lm', '-o', str(program)], check=True)
subprocess.run([str(program)], check=True, timeout=60)
