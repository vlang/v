"""Build/run the real media prerequisite fixture, hosted then native debug/prod."""
import argparse
import os
from pathlib import Path
import shlex
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('prefix', type=Path)
parser.add_argument('work', type=Path)
parser.add_argument('--v', type=Path, default=Path(__file__).resolve().parents[4] / 'v')
parser.add_argument('--modes', nargs='+', choices=('hosted', 'debug', 'prod'),
                    default=('hosted', 'debug', 'prod'))
args = parser.parse_args()
fixture = Path(__file__).resolve().parent
prefix = args.prefix.resolve()
work = args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
environment = os.environ.copy()
for key in ('VEXE', 'VFLAGS', 'VLIBC_PREFIX', 'PKG_CONFIG_PATH', 'PKG_CONFIG_LIBDIR'):
    environment.pop(key, None)
environment.update(VJOBS='1', V3_PARALLEL_CC_JOBS='1')


def run(name, command, env, timeout=300):
    print(name + ': ' + shlex.join(map(str, command)), flush=True)
    with (work / (name + '.log')).open('w') as log:
        subprocess.run(list(map(str, command)), env=env, stdout=log,
                       stderr=subprocess.STDOUT, check=True, timeout=timeout)


native_binary = None
for mode in args.modes:
    selected = environment.copy()
    binary = work / ('codecs-' + mode)
    command = [args.v.resolve(), '-cc', 'gcc', '-no-retry-compilation', '-o', binary]
    if mode != 'hosted':
        selected['VLIBC_PREFIX'] = str(prefix)
        command += ['-syscalls', '-vlibc', '-no-system-libc']
        # The final whole-prefix audit also checks libraries this application
        # does not load. Keep their runtime API providers in the ELF manifest.
        command += ['-ldflags', '-Wl,--export-dynamic']
        # Third-party probe objects use the same no-private-FILE-inlining ABI
        # contract as the native libraries they exercise.
        command += ['-cflags', '-D__NO_INLINE__=1']
    if mode == 'prod':
        command.append('-prod')
    command.append(fixture / 'native_codecs.c.v')
    run(mode + '-build', command, selected)
    probe = ['python3', fixture / 'native_codecs_probe.py', binary]
    if mode == 'hosted':
        probe.append('--hosted')
    run(mode + '-runtime', probe, selected, timeout=90)
    if mode != 'hosted':
        native_binary = binary
    print(mode + ': passed', flush=True)

if native_binary:
    run('codecs-profile-symbols', ['python3', fixture / 'audit_native_symbols.py', prefix, native_binary], selected)
