"""Build native math, binary128 and C++ prerequisites for the Vendetta port.

This extends a complete native X11/package-library prefix. It does not build
the media/graphics stack or publish a default installation. Builds are resumable
and use one compiler worker unless --jobs is explicitly supplied.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tarfile

from build_openlibm import build_openlibm

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('work', type=Path)
parser.add_argument('--base', type=Path)
parser.add_argument('--prefix', type=Path)
parser.add_argument('--sources', type=Path)
parser.add_argument('--jobs', type=int, default=1)
args = parser.parse_args()
if args.jobs < 1:
    parser.error('--jobs must be positive')
fixture = Path(__file__).resolve().parent
manifest = json.loads((fixture / 'media_toolchain_sources.json').read_text())
work = args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
cache = (args.sources or work / 'sources').resolve()
cache.mkdir(parents=True, exist_ok=True)
base = (args.base or Path.home() / '.vmodules/vlibc/linux-amd64-shared').resolve()
prefix = (args.prefix or work / 'prefix').resolve()
marker = 'vlibc-native-v1\nlinux\namd64\n'
if not (base / 'vlibc-target').is_file() or (base / 'vlibc-target').read_text() != marker:
    parser.error('--base must be a complete Linux amd64 native prefix')
if prefix == base or prefix in base.parents:
    parser.error('--prefix must be separate from --base')
environment = os.environ.copy()
cc = shlex.split(environment.get('CC', 'gcc'))
cxx = shlex.split(environment.get('CXX', 'g++'))
target = subprocess.check_output(cc + ['-dumpmachine'], text=True).strip()
if not target.startswith('x86_64-') or 'linux' not in target:
    parser.error('this initial media-toolchain profile requires Linux amd64')
if not prefix.exists():
    shutil.copytree(base, prefix, symlinks=True)
    for path in prefix.rglob('*'):
        if path.is_file() and not path.is_symlink() and path.suffix in ('.pc', '.cmake', '.la', '.m4', '.json'):
            path.write_text(path.read_text().replace(str(base), str(prefix)))
flags = ('-O2 -fPIC -fno-stack-protector -D_GNU_SOURCE=1 -D__NO_INLINE__=1 '
         '-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -ffunction-sections -fdata-sections')
# Configure/build-time programs run on the host. Native ELF dependencies are
# selected explicitly only when packaging the PIC archives below.
environment.update(CC=shlex.join(cc), CXX=shlex.join(cxx), CFLAGS=flags, CXXFLAGS=flags,
                   CPPFLAGS='-I' + str(prefix / 'include'), LDFLAGS='',
                   MAKEINFO='true', CMAKE_BUILD_PARALLEL_LEVEL=str(args.jobs))
sources = {}
for name, record in manifest.items():
    archive = cache / (name + '-vendetta-source.tar')
    if not archive.exists():
        subprocess.run(['curl', '-fLsS', '--retry', '2', '--max-time', '180',
                        record['url'], '-o', str(archive)], check=True)
    for algorithm in ('sha256', 'sha512'):
        if algorithm in record:
            with archive.open('rb') as stream:
                digest = hashlib.file_digest(stream, algorithm).hexdigest()
            if digest != record[algorithm]:
                raise RuntimeError(name + ': source checksum mismatch')
    source = cache / record['folder']
    if not source.exists():
        with tarfile.open(archive) as tar:
            if any(member.name.split('/')[0] != record['folder'] for member in tar):
                raise RuntimeError(name + ': unexpected source archive layout')
            tar.extractall(cache, filter='data')
    sources[name] = source


def run(name, commands, cwd):
    print(name + ': starting', flush=True)
    with (work / (name + '.log')).open('w') as log:
        for command in commands:
            print(name + ': ' + shlex.join(map(str, command)), flush=True)
            subprocess.run(list(map(str, command)), cwd=cwd, env=environment,
                           stdout=log, stderr=subprocess.STDOUT, check=True)


def configure(name, source, extra=(), overrides=()):
    directory = work / (name + '-build')
    directory.mkdir(exist_ok=True)
    command = [source / 'configure', '--prefix=' + str(prefix),
               '--libdir=' + str(prefix / 'lib'), '--disable-shared', '--enable-static',
               '--with-pic', '--disable-multilib', '--disable-nls', *extra]
    identity = hashlib.sha256(json.dumps({'command': list(map(str, command)),
        'compiler': subprocess.check_output(cxx + ['--version'], text=True),
        'flags': flags, 'sources': manifest, 'overrides': overrides}, sort_keys=True).encode()).hexdigest()
    stamp = directory / 'vlibc-configured'
    if not stamp.exists() or stamp.read_text() != identity:
        if (directory / 'Makefile').is_file():
            # Generated configuration changes can alter which definitions are
            # compiled. Do not reuse objects from an incompatible configuration.
            run(name + '-clean', [['make', 'clean']], directory)
        run(name + '-configure', [command], directory)
        if overrides:
            # These explicit target-feature choices affect generated configuration,
            # not C declarations or source/header dependency inference.
            with (directory / 'config.h').open('a') as config:
                config.write('\n/* Native vlibc profile: unsupported GNU locale/printf plugins. */\n')
                for option in overrides:
                    config.write('#undef ' + option + '\n')
        stamp.write_text(identity)
    return directory


def definitions(path, dynamic=False, hidden=False, functions=False):
    result = subprocess.check_output(['readelf', '--wide', '--dyn-syms' if dynamic else '--syms', str(path)], text=True)
    return {fields[7].split('@', 1)[0] for line in result.splitlines()
            if len(fields := line.split()) >= 8 and fields[4] in ('GLOBAL', 'WEAK', 'UNIQUE', 'GNU_UNIQUE')
            and (hidden or fields[5] in ('DEFAULT', 'PROTECTED')) and fields[6] != 'UND'
            and (not functions or fields[3]=='FUNC')}


runtime = Path(subprocess.check_output(cc + ['-print-libgcc-file-name'], text=True).strip())
eh = Path(subprocess.check_output(cc + ['-print-file-name=libgcc_eh.a'], text=True).strip())
crt_begin = Path(subprocess.check_output(cc + ['-print-file-name=crtbeginS.o'], text=True).strip())
crt_end = Path(subprocess.check_output(cc + ['-print-file-name=crtendS.o'], text=True).strip())
if not all(path.is_file() for path in (runtime, eh, crt_begin, crt_end)):
    raise RuntimeError('the selected GCC toolchain must expose its PIC C++ startup/unwinding objects')


def package(name, archive, children=(), extra=(), cxx_startup=False, exclude=()):
    public = definitions(archive)
    for native_object in extra:
        public.update(definitions(native_object))
    public = {symbol for symbol in public if not any(re.fullmatch(pattern, symbol) for pattern in exclude)}
    export_map = work / (name + '-public.map')
    export_map.write_text('{ global:\n' + ''.join('  ' + symbol + ';\n' for symbol in sorted(public)) +
                          'local: *; };\n')
    output = prefix / 'lib' / ('lib' + name + '-vlibc.so')
    command = cc + ['-shared', '-nostdlib']
    if cxx_startup:
        command += [str(crt_begin)]
    command += ['-Wl,--whole-archive', str(archive), '-Wl,--no-whole-archive', *map(str, extra),
                '-Wl,-soname,' + output.name, '-Wl,--version-script,' + str(export_map),
                '-Wl,--eh-frame-hdr', '-L' + str(prefix / 'lib'), '-Wl,--no-as-needed']
    command += ['-l' + child for child in children]
    command += [str(runtime)]
    if cxx_startup:
        command += [str(crt_end)]
    command += ['-Wl,-rpath,$ORIGIN', '-o', str(output)]
    run(name + '-shared', [command], work)
    actual = subprocess.check_output(['readelf', '--wide', '--dyn-syms', str(output)], text=True)
    actual = {fields[7].split('@', 1)[0] for line in actual.splitlines()
              if len(fields := line.split()) >= 8 and fields[6] != 'UND'}
    if not public or public - actual:
        raise RuntimeError(name + ': missing public exports: ' + ', '.join(sorted(public - actual)))
    alias = prefix / 'lib' / ('lib' + name + '.so')
    alias.unlink(missing_ok=True)
    alias.symlink_to(output.name)


build_openlibm(sources['openlibm'], prefix, work, cc, flags, manifest['openlibm'], args.jobs, run,
               base=base)


quad_source = sources['gcc'] / 'libquadmath'
quad = configure('quadmath', quad_source,
                 overrides=('HAVE_PRINTF_HOOKS', 'USE_NL_LANGINFO_WC', 'USE_I18N_NUMBER_H'))
run('quadmath', [['make', '-j' + str(args.jobs)]], quad)
quad_archive = prefix / 'lib/libquadmath.a'
shutil.copyfile(quad / '.libs/libquadmath.a', quad_archive)
shutil.copyfile(quad_source / 'quadmath.h', prefix / 'include/quadmath.h')
bridge = work / 'float128-bridge.o'
run('float128-bridge', [cc + shlex.split(flags) + ['-I' + str(prefix / 'include'), '-c',
    str(fixture / 'float128_bridge.c'), '-o', str(bridge)]], work)
# Export the two native C compatibility entry points along with quadmath's API.
combined_quad = work / 'libquadmath-complete.a'
shutil.copyfile(quad_archive, combined_quad)
run('float128-archive', [['ar', 'r', combined_quad, bridge]], work)
shutil.copyfile(combined_quad, quad_archive)
package('quadmath', quad_archive, ('m',))

# The toolchain's PIC EH archive intentionally hides its implementations. Keep
# that binding and export native ABI jump entries for the real SDK's public
# unwinding functions. No argument/result types or stack frames are invented.
sdk_unwinder = Path(subprocess.check_output(cc + ['-print-file-name=libgcc_s.so.1'], text=True).strip())
if not sdk_unwinder.is_file():
    raise RuntimeError('the selected toolchain must expose its shared unwinder ABI manifest')
eh_definitions = definitions(eh, hidden=True)
eh_public = definitions(eh, hidden=True, functions=True) & definitions(sdk_unwinder, dynamic=True)
if not eh_public or '_Unwind_RaiseException' not in eh_public:
    raise RuntimeError('the toolchain has no complete native unwinding implementation')
renaming = work / 'unwind-namespace.map'
renaming.write_text(''.join(symbol + ' vlibc_unwind_' + symbol + '\n' for symbol in sorted(eh_definitions)))
eh_native = work / 'libgcc-eh-native.a'
run('unwind-namespace', [['objcopy', '--redefine-syms=' + str(renaming), eh, eh_native]], work)
entries = work / 'unwind-abi.S'
entries.write_text('.text\n' + ''.join(
    '.p2align 4\n.globl ' + symbol + '\n.type ' + symbol + ',@function\n' + symbol +
    ':\n.cfi_startproc\nendbr64\njmp vlibc_unwind_' + symbol + '\n.cfi_endproc\n.size ' + symbol +
    ',.-' + symbol + '\n' for symbol in sorted(eh_public)) + '.section .note.GNU-stack,"",@progbits\n')
entries_object = work / 'unwind-abi.o'
run('unwind-abi', [cc + ['-fPIC', '-c', str(entries), '-o', str(entries_object)]], work)
package('gcc_s', eh_native, ('m',), extra=(entries_object,))

# GCC's top-level build normally provides this generated target selection.
# Supply that opaque header for a standalone libstdc++ configure as well,
# allowing its real POSIX-thread feature checks to compile successfully.
compiler_details = subprocess.run(cxx + ['-v'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                  text=True, check=True).stdout
if not re.search(r'^Thread model: posix$', compiler_details, re.MULTILINE):
    raise RuntimeError('the native C++ profile requires a POSIX-thread GCC toolchain')
gthread_directory = work / 'libgcc'
gthread_directory.mkdir(exist_ok=True)
gthread_default = gthread_directory / 'gthr-default.h'
gthread_contents = (sources['gcc'] / 'libgcc/gthr-posix.h').read_bytes()
if not gthread_default.exists() or gthread_default.read_bytes() != gthread_contents:
    gthread_default.write_bytes(gthread_contents)
cpp = configure('libstdcxx', sources['gcc'] / 'libstdc++-v3',
                extra=('--disable-libstdcxx-pch', '--disable-symvers', '--enable-libstdcxx-threads=yes',
                       '--enable-libstdcxx-time=yes'))
run('libstdcxx', [['make', '-j' + str(args.jobs)], ['make', 'install']], cpp)
cpp_archive = prefix / 'lib/libstdc++.a'
package('stdc++', cpp_archive, ('quadmath', 'm', 'gcc_s'), cxx_startup=True)

libraries = sorted((prefix / 'lib').glob('lib*-vlibc.so'))
imports = set()
for library in libraries:
    dynamic = subprocess.check_output(['readelf', '-d', str(library)], text=True)
    for child in re.findall(r'\(NEEDED\).*\[([^]]+)\]', dynamic):
        if not child.endswith('-vlibc.so') or not (prefix / 'lib' / child).is_file():
            raise RuntimeError(str(library) + ': unexpected runtime dependency ' + child)
    for line in subprocess.check_output(['nm', '-D', '--undefined-only', str(library)], text=True).splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[0] == 'U':
            imports.add(fields[1])
(prefix / 'media-toolchain-required-symbols.txt').write_text('\n'.join(sorted(imports)) + '\n')
(prefix / 'media-toolchain-profile.json').write_text(json.dumps({
    'schema': 1, 'base': str(base), 'sources': manifest,
    'features': ['native OpenLibm math', 'native quadmath binary128', 'native C++ standard library',
                 'shared GCC unwinding', 'compiler-provided C++ DSO startup'],
    'environment_owner': 'static V GNU fenv implementation',
    'excluded_optional_extensions': ['GNU printf registration hooks', 'GNU private wide locale items',
                                     'GNU i18n number rewriting'],
    'media_stack_complete': False,
}, indent=2) + '\n')
print('Native C++ prerequisites ready, not published: ' + str(prefix), flush=True)
