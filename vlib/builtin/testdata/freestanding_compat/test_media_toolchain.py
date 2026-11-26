"""Build/run the native C++ dependency fixture in debug and production, sequentially."""
import argparse
import os
from pathlib import Path
import shlex
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('prefix', type=Path)
parser.add_argument('work', type=Path)
parser.add_argument('--v', type=Path, default=Path(__file__).resolve().parents[4] / 'v')
args = parser.parse_args()
fixture = Path(__file__).resolve().parent
prefix = args.prefix.resolve()
work = args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
environment = os.environ.copy()
environment.pop('VEXE', None)
environment.pop('VFLAGS', None)
environment.update(VLIBC_PREFIX=str(prefix), VJOBS='1', V3_PARALLEL_CC_JOBS='1')
cc = shlex.split(environment.get('CC', 'gcc'))
cxx = shlex.split(environment.get('CXX', 'g++'))
target = subprocess.check_output(cc + ['-dumpmachine'], text=True).strip()
includes = [path for path in (prefix / 'include/c++').iterdir()
            if path.is_dir() and ((path / target / 'bits/c++config.h').is_file()
                                 or (path / 'bits/c++config.h').is_file())]
if len(includes) != 1:
    raise RuntimeError('the prefix must contain one matching native C++ header installation')
headers = includes[0]
target_headers = headers / target if (headers / target / 'bits/c++config.h').is_file() else headers


def run(name, command, timeout=240):
    print(name + ': ' + shlex.join(map(str, command)), flush=True)
    with (work / (name + '.log')).open('w') as log:
        subprocess.run(list(map(str, command)), env=environment, stdout=log,
                       stderr=subprocess.STDOUT, check=True, timeout=timeout)


native_object = work / 'native_cpp_probe.o'
run('cpp-object', cxx + ['-std=gnu++20', '-O2', '-fPIC', '-fno-stack-protector',
    '-D_GNU_SOURCE=1', '-D__NO_INLINE__=1', '-U_FORTIFY_SOURCE', '-D_FORTIFY_SOURCE=0',
    '-nostdinc++', '-isystem', headers, '-isystem', target_headers,
    '-isystem', headers / 'backward', '-I' + str(prefix / 'include'),
    '-c', fixture / 'native_cpp_probe.cpp', '-o', native_object])
cleanup_object = work / 'native_cpp_cleanup_probe.o'
run('cpp-cleanup-object', cc + ['-O2', '-fPIC', '-D_GNU_SOURCE=1', '-fno-stack-protector',
    '-c', fixture / 'native_cpp_cleanup_probe.c', '-o', cleanup_object])
crt_begin = subprocess.check_output(cc + ['-print-file-name=crtbeginS.o'], text=True).strip()
crt_end = subprocess.check_output(cc + ['-print-file-name=crtendS.o'], text=True).strip()
runtime = subprocess.check_output(cc + ['-print-libgcc-file-name'], text=True).strip()
run('cpp-shared', cc + ['-shared', '-nostdlib', crt_begin, native_object, cleanup_object,
    '-L' + str(prefix / 'lib'), '-Wl,--no-as-needed', '-lstdc++', '-lquadmath', '-lm',
    '-lgcc_s', runtime, crt_end, '-Wl,--eh-frame-hdr', '-Wl,-soname,libv_cpp_probe.so',
    '-Wl,-rpath,' + str(prefix / 'lib'), '-o', work / 'libv_cpp_probe.so'])
for production in [False, True]:
    name = 'cpp-prod' if production else 'cpp-debug'
    binary = work / name
    command = [args.v.resolve(), '-syscalls', '-vlibc', '-no-system-libc', '-cc', 'gcc', '-no-retry-compilation',
               '-cflags', '-L' + str(work), '-ldflags',
               '-Wl,--export-dynamic -Wl,-rpath,' + str(work), '-o', binary]
    if production:
        command.append('-prod')
    command.append(fixture / 'native_cpp.c.v')
    run(name + '-build', command)
    run(name + '-runtime', ['python3', fixture / 'native_cpp_probe.py', binary], timeout=90)
    print(name + ': passed', flush=True)

run('cpp-profile-symbols', ['python3', fixture / 'audit_native_symbols.py', prefix, binary])
