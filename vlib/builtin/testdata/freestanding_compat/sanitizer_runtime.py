"""Compile the V half of the instrumented libc adapters using the checkout compiler."""
import os
import subprocess


def compile_v_runtime(fixture, work, cc, flags, *, shim_object=True):
    source = work / 'runtime.c'
    entry = work / 'entry.v'
    entry.write_text('module main\n')
    v = fixture.parents[3] / 'v'
    runtime_flags = ['-freestanding', '-gc', 'none', '-d', 'no_main']
    if shim_object:
        runtime_flags += ['-d', 'vlibc_shim_object']
    subprocess.run([v, '-new-compiler', '-no-retry-compilation', '-cc', cc, *runtime_flags,
                    '-dump-c-flags', work / 'runtime.flags', '-o', source, entry], check=True,
                   env={k: value for k, value in os.environ.items() if k not in ('VFLAGS', 'VEXE')})
    obj = work / 'runtime.o'
    subprocess.run([*flags, '-fwrapv',
                    '-Wno-incompatible-pointer-types', '-Wno-int-conversion',
                    '-I' + str(fixture.parents[3] / 'thirdparty/stdatomic/nix'),
                    '-c', source, '-o', obj], check=True)
    return obj
