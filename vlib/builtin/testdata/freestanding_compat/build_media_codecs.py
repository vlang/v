"""Build native subtitle, ALSA and FFmpeg dependencies for the Vendetta port.

The input is the native C++ prerequisite prefix. This separate, unpublished
profile preserves that input and builds one compiler job by default. Graphics,
libplacebo and libmpv are not provided by this prerequisite builder.
"""
import argparse
import fnmatch
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tarfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('work', type=Path)
parser.add_argument('--base', type=Path, required=True)
parser.add_argument('--prefix', type=Path)
parser.add_argument('--sources', type=Path)
parser.add_argument('--jobs', type=int, default=1)
args = parser.parse_args()
if args.jobs < 1:
    parser.error('--jobs must be positive')
fixture = Path(__file__).resolve().parent
manifest = json.loads((fixture / 'media_codecs_sources.json').read_text())
work = args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
cache = (args.sources or work / 'sources').resolve()
cache.mkdir(parents=True, exist_ok=True)
base = args.base.resolve()
prefix = (args.prefix or work / 'prefix').resolve()
marker = 'vlibc-native-v1\nlinux\namd64\n'
if not (base / 'vlibc-target').is_file() or (base / 'vlibc-target').read_text() != marker:
    parser.error('--base must be a complete Linux amd64 native prefix')
if not (base / 'media-toolchain-profile.json').is_file():
    parser.error('--base must contain the native C++ prerequisite profile')
if prefix == base or prefix in base.parents:
    parser.error('--prefix must be separate from --base')
environment = os.environ.copy()
cc = shlex.split(environment.get('CC', 'gcc'))
cxx = shlex.split(environment.get('CXX', 'g++'))
target = subprocess.check_output(cc + ['-dumpmachine'], text=True).strip()
if not target.startswith('x86_64-') or 'linux' not in target:
    parser.error('this initial media profile requires Linux amd64')
if not prefix.exists():
    shutil.copytree(base, prefix, symlinks=True)
    for path in prefix.rglob('*'):
        if path.is_file() and not path.is_symlink() and path.suffix in ('.pc', '.cmake', '.la', '.m4', '.json'):
            path.write_text(path.read_text().replace(str(base), str(prefix)))
flags = ('-O2 -fPIC -fno-stack-protector -D_GNU_SOURCE=1 -D__NO_INLINE__=1 '
         '-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -ffunction-sections -fdata-sections')
# Build-time executables use the host runtime. Only the final shared-library
# packaging below uses -nostdlib and the explicitly enumerated native closure.
environment.update(CC=shlex.join(cc), CXX=shlex.join(cxx), CFLAGS=flags, CXXFLAGS=flags,
    CPPFLAGS='-I' + str(prefix / 'include'), LDFLAGS='',
    PKG_CONFIG_PATH='', PKG_CONFIG_LIBDIR=os.pathsep.join(str(prefix / part) for part in
        ('lib/pkgconfig', 'share/pkgconfig', 'private/mbedtls/lib/pkgconfig')))
compiler_identity = subprocess.check_output(cc + ['--version'], text=True)
# Older prefixes built from the pinned X11 recipe predate this metadata.
# FreeType declares bzip2 in Requires.private, so all pkg-config consumers
# need it even when linking the shared native FreeType implementation.
bzip_metadata = prefix / 'lib/pkgconfig/bzip2.pc'
if not bzip_metadata.exists():
    if not all((prefix / item).is_file() for item in ('lib/libbz2.a', 'lib/libbz2-vlibc.so', 'include/bzlib.h')):
        parser.error('--base must contain the native bzip2 backend')
    bzip_metadata.write_text('prefix=' + str(prefix) + '\nlibdir=${prefix}/lib\n'
        'includedir=${prefix}/include\n\nName: bzip2\nDescription: bzip2 compression library\n'
        'Version: 1.0.8\nLibs: -L${libdir} -lbz2\nCflags: -I${includedir}\n')
sources = {}
for name, record in manifest.items():
    archive = cache / (name + '-media-source.tar')
    if not archive.exists():
        subprocess.run(['curl', '-fLsS', '--retry', '2', '--max-time', '180',
                        record['url'], '-o', str(archive)], check=True)
    with archive.open('rb') as stream:
        if hashlib.file_digest(stream, 'sha256').hexdigest() != record['sha256']:
            raise RuntimeError(name + ': source checksum mismatch')
    source = cache / record['folder']
    if not source.exists():
        with tarfile.open(archive) as tar:
            if any(member.name.split('/')[0] != record['folder'] for member in tar):
                raise RuntimeError(name + ': unexpected source archive layout')
            tar.extractall(cache, filter='data')
    sources[name] = source


def run(name, commands, cwd, env=None):
    with (work / (name + '.log')).open('w') as log:
        for command in commands:
            print(name + ': ' + shlex.join(map(str, command)), flush=True)
            subprocess.run(list(map(str, command)), cwd=cwd, env=env or environment,
                           stdout=log, stderr=subprocess.STDOUT, check=True)


def configured(name, command, directory, clean=None):
    inputs = {'command': list(map(str, command)),
        'flags': flags, 'compiler': compiler_identity, 'source': manifest[name],
        'environment': {key: environment.get(key) for key in
            ('CC', 'CXX', 'CFLAGS', 'CXXFLAGS', 'CPPFLAGS', 'LDFLAGS', 'PKG_CONFIG_LIBDIR')}}
    if name == 'ffmpeg':
        inputs['pkgconfig'] = environment.get('PKG_CONFIG')
        inputs['pkgconfig_helper'] = hashlib.sha256((fixture / 'build_pkgconfig.py').read_bytes()).hexdigest()
    identity = hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()
    stamp = directory / 'vlibc-configured'
    if not stamp.exists() or stamp.read_text() != identity:
        if clean and (directory / 'Makefile').is_file():
            run(name + '-clean', [clean], directory)
        run(name + '-configure', [command], directory)
        stamp.write_text(identity)


def meson(name, options):
    directory = work / (name + '-build')
    command = ['meson', 'setup']
    if (directory / 'meson-private/coredata.dat').is_file():
        command.append('--reconfigure')
    command += [directory, sources[name], '--prefix=' + str(prefix), '--libdir=lib',
        '--buildtype=release', '--default-library=static', '--wrap-mode=nofallback',
        '-Db_staticpic=true', '-Dauto_features=disabled', '-Dc_args=' + flags,
        '-Dcpp_args=' + flags, *options]
    directory.mkdir(exist_ok=True)
    # Meson/Ninja retain compiler-produced dependency manifests and rebuild
    # affected objects when either generated configuration or headers change.
    run(name + '-configure', [command], work)
    run(name, [['meson', 'compile', '-C', directory, '-j', args.jobs],
               ['meson', 'install', '-C', directory, '--no-rebuild']], work)


def definitions(path, dynamic=False):
    output = subprocess.check_output(['readelf', '--wide',
        '--dyn-syms' if dynamic else '--syms', str(path)], text=True)
    return {fields[7].split('@', 1)[0] for line in output.splitlines()
        if len(fields := line.split()) >= 8 and fields[4] in ('GLOBAL', 'WEAK', 'UNIQUE', 'GNU_UNIQUE')
        and fields[5] in ('DEFAULT', 'PROTECTED') and fields[6] != 'UND'}


runtime = subprocess.check_output(cc + ['-print-libgcc-file-name'], text=True).strip()


def package(name, children=(), version_map=None):
    archive = prefix / 'lib' / ('lib' + name + '.a')
    public = definitions(archive)
    if not public:
        raise RuntimeError(name + ': archive has no public definitions')
    if version_map:
        # The upstream-generated linker manifest owns the ABI boundary.
        # FFmpeg's internal PC-relative tables require this local binding.
        # These are linker patterns, never declarations inferred from headers.
        rules = re.fullmatch(r'\s*\w+\s*\{\s*global:\s*((?:[\w*?]+\s*;\s*)+)local:\s*\*\s*;\s*\}\s*;\s*',
                             version_map.read_text())
        if not rules:
            raise RuntimeError(name + ': unsupported upstream linker manifest syntax')
        patterns = [entry.strip() for entry in rules[1].split(';') if entry.strip()]
        public = {symbol for symbol in public if any(fnmatch.fnmatchcase(symbol, pattern) for pattern in patterns)}
        export_map = version_map
    else:
        export_map = work / (name + '-public.map')
        export_map.write_text('{ global:\n' + ''.join('  ' + symbol + ';\n' for symbol in sorted(public)) +
                              'local: *; };\n')
    if not public:
        raise RuntimeError(name + ': linker manifest selects no compiled public definitions')
    output = prefix / 'lib' / ('lib' + name + '-vlibc.so')
    command = cc + ['-shared', '-nostdlib', '-Wl,--whole-archive', archive,
        '-Wl,--no-whole-archive', '-Wl,-soname,' + output.name,
        '-Wl,--version-script,' + str(export_map), '-Wl,--eh-frame-hdr',
        '-L' + str(prefix / 'lib'), '-Wl,--no-as-needed']
    command += ['-l' + child for child in children]
    command += [runtime, '-Wl,-rpath,$ORIGIN', '-o', output]
    run(name + '-shared', [command], work)
    missing = public - definitions(output, dynamic=True)
    if missing:
        raise RuntimeError(name + ': missing public exports: ' + ', '.join(sorted(missing)))
    alias = prefix / 'lib' / ('lib' + name + '.so')
    alias.unlink(missing_ok=True)
    alias.symlink_to(output.name)


# NASM is a build-time tool, never a target dependency. Bootstrap it into
# this work directory when absent instead of disabling optimized media code
# or changing the user's system packages.
if shutil.which('nasm') is None:
    assembler = work / 'nasm-build'
    assembler.mkdir(exist_ok=True)
    configured('nasm', [sources['nasm'] / 'configure', '--prefix=' + str(work / 'tools')],
               assembler, clean=['make', 'distclean'])
    run('nasm', [['make', '-C', assembler, '-j' + str(args.jobs)],
                 ['make', '-C', assembler, 'install']], work)
    environment['PATH'] = str(work / 'tools/bin') + os.pathsep + environment.get('PATH', '')

meson('fribidi', ['-Ddocs=false', '-Dbin=false', '-Dtests=false'])
package('fribidi')
meson('harfbuzz', ['-Dfreetype=enabled', '-Dzlib=enabled', '-Dpng=enabled',
                  '-Dtests=disabled', '-Dutilities=disabled'])
package('harfbuzz', ('freetype', 'z', 'png16', 'm', 'gcc_s'))
# Preserve every enabled HarfBuzz library/API rather than reducing the profile
# to the single shaping entry point libass happens to call.
for archive in sorted((prefix / 'lib').glob('libharfbuzz-*.a')):
    package(archive.name[3:-2], ('harfbuzz', 'freetype', 'm', 'gcc_s'))
meson('ass', ['-Dfontconfig=enabled', '-Dasm=enabled', '-Dlibunibreak=disabled'])
package('ass', ('harfbuzz', 'fribidi', 'freetype', 'fontconfig', 'm'))

alsa = work / 'asound-build'
alsa.mkdir(exist_ok=True)
configured('asound', [sources['asound'] / 'configure', '--prefix=' + str(prefix),
    '--libdir=' + str(prefix / 'lib'), '--disable-shared', '--enable-static',
    '--disable-python', '--disable-mixer-pymods', '--with-versioned=no',
    '--with-configdir=' + str(prefix / 'share/alsa'),
    '--with-plugindir=' + str(prefix / 'lib/alsa-lib')], alsa, clean=['make', 'distclean'])
# CFLAGS supplies real position-independent code. ALSA's separate --with-pic
# switch also changes its plugin-registration model and rejects static builds;
# retain the upstream static model so all built-in PCM plugins are included.
run('asound', [['make', '-C', alsa, '-j' + str(args.jobs)],
               ['make', '-C', alsa, 'install']], work)
package('asound', ('m',))
if (prefix / 'lib/libatopology.a').is_file():
    package('atopology', ('asound', 'm'))

ffmpeg = work / 'ffmpeg-build'
ffmpeg.mkdir(exist_ok=True)
# Feature-check executables need the host SDK. Use the real native PIC
# dependency archives so their C runtime references originate in the executable
# and the SDK can supply nonshared entry points such as atexit normally.
# Final target DSOs below still link only the enumerated native shared closure.
environment['V_NATIVE_HOST_PKG_CONFIG'] = environment.get('PKG_CONFIG', 'pkg-config')
environment['PKG_CONFIG'] = shlex.join([sys.executable, str(fixture / 'build_pkgconfig.py'), str(prefix)])
command = [sources['ffmpeg'] / 'configure', '--prefix=' + str(prefix),
    '--libdir=' + str(prefix / 'lib'), '--disable-shared', '--enable-static',
    '--enable-pic', '--disable-programs', '--disable-doc', '--disable-autodetect',
    '--enable-gpl', '--enable-version3', '--enable-pthreads', '--enable-alsa', '--enable-zlib', '--enable-bzlib',
    '--enable-lzma', '--enable-libass', '--enable-libfreetype', '--enable-libfontconfig',
    '--enable-libfribidi', '--enable-libharfbuzz', '--enable-mbedtls',
    '--pkg-config=' + environment['PKG_CONFIG'], '--pkg-config-flags=--static',
    '--cc=' + shlex.join(cc), '--cxx=' + shlex.join(cxx),
    '--extra-cflags=' + flags + ' -I' + str(prefix / 'include'), '--extra-ldflags=']
configured('ffmpeg', command, ffmpeg, clean=['make', 'distclean'])
run('ffmpeg', [['make', '-C', ffmpeg, '-j' + str(args.jobs)],
               ['make', '-C', ffmpeg, 'install']], work)

# Upstream's shared-compatible archives duplicate private cross-library
# tables into each consumer. Reuse the same PIC objects, and ask its own
# archive targets to include SHLIBOBJS; do not export private symbols or
# build/install host-libc shared libraries to satisfy those references.
ffmpeg_libraries = ('avutil', 'swresample', 'swscale', 'avcodec', 'avformat', 'avfilter', 'avdevice')
run('ffmpeg-shared-archives', [['make', '-C', ffmpeg, '-j' + str(args.jobs),
    'CONFIG_SHARED=yes', *['lib' + name + '/lib' + name + '.a' for name in ffmpeg_libraries]]], work)
for name in ffmpeg_libraries:
    shutil.copyfile(ffmpeg / ('lib' + name) / ('lib' + name + '.a'), prefix / 'lib' / ('lib' + name + '.a'))

# Consumers compile against the backend's private native headers. Rename only
# symbols from the compiled crypto export manifest, never C header declarations.
crypto = set()
for library in (prefix / 'lib').glob('libvlibc_mbed*-vlibc.so'):
    crypto.update(symbol.removeprefix('vlibc_private_') for symbol in definitions(library, True)
                  if symbol.startswith('vlibc_private_'))
if not crypto:
    raise RuntimeError('the base profile has no namespaced native Mbed TLS exports')
renaming = work / 'mbedtls-namespace.map'
renaming.write_text(''.join(name + ' vlibc_private_' + name + '\n' for name in sorted(crypto)))
dependencies = {
    'avutil': ('m',),
    'swresample': ('avutil', 'm'),
    'swscale': ('avutil', 'm'),
    'avcodec': ('avutil', 'swresample', 'z', 'bz2', 'lzma', 'm'),
    'avformat': ('avcodec', 'avutil', 'vlibc_mbedtls', 'vlibc_mbedx509', 'vlibc_mbedcrypto', 'z', 'bz2', 'lzma', 'm'),
    'avfilter': ('avformat', 'avcodec', 'swscale', 'swresample', 'avutil', 'ass', 'harfbuzz', 'freetype', 'fontconfig', 'fribidi', 'm'),
    'avdevice': ('avfilter', 'avformat', 'avcodec', 'avutil', 'asound', 'm'),
}
for name, children in dependencies.items():
    archive = prefix / 'lib' / ('lib' + name + '.a')
    native_archive = work / archive.name
    run(name + '-namespace', [['objcopy', '--redefine-syms=' + str(renaming), archive, native_archive]], work)
    shutil.copyfile(native_archive, archive)
    version_map = ffmpeg / ('lib' + name) / ('lib' + name + '.ver')
    run(name + '-manifest', [['make', '-C', ffmpeg, '-j' + str(args.jobs),
        str(version_map.relative_to(ffmpeg))]], work)
    package(name, children, version_map=version_map)

# Dynamic consumers need only the selected public library. Preserve the full
# static dependency list separately, using the native crypto archive names.
for package_name, name in [('alsa', 'asound'), ('libass', 'ass'), ('fribidi', 'fribidi'),
        ('harfbuzz', 'harfbuzz'), *[('lib' + name, name) for name in dependencies]]:
    metadata = prefix / 'lib/pkgconfig' / (package_name + '.pc')
    lines = metadata.read_text().splitlines()
    # Static-only upstream installs place implementation dependencies in Libs.
    # Keep that full closure when reducing the public dynamic link line.
    private_options = []
    for line in lines:
        if line.startswith(('Libs:', 'Libs.private:')):
            private_options += shlex.split(line.split(':', 1)[1])
    private_options = ['-l' + (prefix / 'private/build-sdk/lib' / flag[3:]).resolve().name[3:-2]
        if flag.startswith('-l:libv_build_') else flag for flag in private_options]
    private_options = ['-lvlibc_' + flag[2:] if flag in ('-lmbedtls', '-lmbedx509', '-lmbedcrypto') else flag
        for flag in private_options if flag not in ('-lc', '-lpthread', '-pthread', '-ldl', '-lrt', '-lutil', '-l' + name)]
    private_options = [flag for flag in private_options if not flag.startswith('-L' + str(prefix / 'private/'))]
    output = []
    for line in lines:
        if line.startswith('Libs:'):
            line = 'Libs: -L${libdir} -l' + name
        elif line.startswith('Libs.private:'):
            continue
        output.append(line)
    output.append('Libs.private: ' + ' '.join(private_options))
    metadata.write_text('\n'.join(output) + '\n')

imports = set()
for library in (prefix / 'lib').glob('lib*-vlibc.so'):
    dynamic = subprocess.check_output(['readelf', '-d', str(library)], text=True)
    for child in re.findall(r'\(NEEDED\).*\[([^]]+)\]', dynamic):
        if not child.endswith('-vlibc.so') or not (prefix / 'lib' / child).is_file():
            raise RuntimeError(str(library) + ': unexpected runtime dependency ' + child)
    for line in subprocess.check_output(['nm', '-D', '--undefined-only', str(library)], text=True).splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[0] == 'U':
            imports.add(fields[1])
(prefix / 'media-codecs-required-symbols.txt').write_text('\n'.join(sorted(imports)) + '\n')
(prefix / 'media-codecs-profile.json').write_text(json.dumps({
    'schema': 1, 'base': str(base), 'sources': manifest,
    'features': ['FriBidi bidirectional text', 'HarfBuzz shaping and enabled companion APIs',
                 'libass subtitles with fontconfig', 'ALSA PCM and mixer',
                 'FFmpeg built-in codecs, formats, filters and protocols', 'Mbed TLS HTTPS'],
    'excluded_optional_backends': ['host codec/hardware libraries', 'ICU', 'Graphite', 'libunibreak',
                                   'Python ALSA mixer plugins'],
    'graphics_complete': False, 'media_stack_complete': False,
}, indent=2) + '\n')
print('Native subtitle/audio/codec prerequisites ready, not published: ' + str(prefix), flush=True)
