"""Use native PIC dependency archives with the host SDK for build-time programs.

Usage: python3 build_pkgconfig.py PREFIX [ordinary pkg-config arguments ...]
The target's package metadata and genuine shared libraries are left unchanged.
Only compiled native library archives and link-search arguments are selected;
C headers/declarations are never inspected.
"""
import os
import hashlib
from pathlib import Path
import shlex
import subprocess
import sys

if len(sys.argv) < 2:
    raise SystemExit('expected native prefix and pkg-config arguments')
prefix = Path(sys.argv[1]).resolve()
arguments = sys.argv[2:]
tool = shlex.split(os.environ.get('V_NATIVE_HOST_PKG_CONFIG', 'pkg-config'))
result = subprocess.run(tool + arguments, stdout=subprocess.PIPE)
if result.returncode:
    sys.stdout.buffer.write(result.stdout)
    raise SystemExit(result.returncode)
if not any(argument.startswith('--libs') for argument in arguments):
    sys.stdout.buffer.write(result.stdout)
    raise SystemExit(0)
flags = shlex.split(result.stdout.decode())
directories = [prefix / 'lib']
for flag in flags:
    if flag.startswith('-L'):
        directory = Path(flag[2:]).resolve()
        if prefix == directory or prefix in directory.parents:
            directories.append(directory)
sdk = {'c', 'm', 'pthread', 'dl', 'rt', 'gcc', 'gcc_s', 'stdc++', 'quadmath', 'resolv', 'util'}
output = []
sdk_directory = prefix / 'private/build-sdk/lib'
for flag in flags:
    if flag.startswith('-L') and Path(flag[2:]).resolve() in directories:
        continue
    if flag.startswith('-l') and flag[2:] not in sdk:
        name = flag[2:]
        archive = next((directory / ('lib' + name + '.a') for directory in directories
                        if (directory / ('lib' + name + '.a')).is_file()), None)
        if archive:
            # Keep this a -l option: configure scripts distinguish libraries
            # from compiler flags by that syntax and place them after objects.
            # Unique exact filenames avoid overriding host SDK libraries such
            # as libm when GCC appends its implicit link arguments.
            sdk_directory.mkdir(parents=True, exist_ok=True)
            alias = sdk_directory / ('libv_build_' + hashlib.sha256(str(archive).encode()).hexdigest()[:24] + '.a')
            if not alias.is_symlink():
                if alias.exists():
                    raise SystemExit('unmanaged build SDK archive: ' + str(alias))
                alias.symlink_to(archive)
            elif alias.resolve() != archive.resolve():
                raise SystemExit('build SDK archive identity changed: ' + str(alias))
            flag = '-l:' + alias.name
        elif any((directory / ('lib' + name + '.so')).exists() for directory in directories):
            raise SystemExit('build-time dependency has no native PIC archive: ' + name)
    output.append(flag)
if any(flag.startswith('-l:libv_build_') for flag in output):
    output.insert(0, '-L' + str(sdk_directory))
print(shlex.join(output))
