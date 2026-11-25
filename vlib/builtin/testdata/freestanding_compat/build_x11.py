"""Build the optional static X11/font-stack fixture without system installation.

Usage: python3 build_x11.py /tmp/vlibc-x11
Build tools and development headers are prerequisites, not installed by this file.
Every download is pinned; a failed step leaves its log and can be resumed.
Builds use one compiler worker unless --jobs is explicitly supplied.
"""
import argparse
import platform
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import shutil
import shlex
import subprocess
import sys
import tarfile

from build_openlibm import build_openlibm
from prefix_install import PrefixInstall, lock_build_directories

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('work', type=Path)
parser.add_argument('--jobs', type=int, default=1, help='maximum compiler workers (default: 1)')
selection = parser.add_mutually_exclusive_group()
selection.add_argument('--prefix', type=Path, help='install to an explicit dependency prefix')
selection.add_argument('--install', action='store_true', help='register a persistent native vlibc prefix')
parser.add_argument('--shared', action='store_true', help='build libc-free shared libraries instead of static-only archives')
args = parser.parse_args()
if args.jobs < 1:
    parser.error('--jobs must be positive')
fixture = Path(__file__).resolve().parent
root = args.work.resolve()
root.mkdir(parents=True, exist_ok=True)
manifest = json.loads((fixture / 'x11_sources.json').read_text())
manifest['openlibm'] = json.loads((fixture / 'media_toolchain_sources.json').read_text())['openlibm']
flags = ('-O2 -std=gnu11 -fno-stack-protector -D__NO_INLINE__=1 '
         '-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -ffunction-sections -fdata-sections')
if args.shared:
    flags += ' -fPIC'
environment = os.environ.copy()
compiler_command = shlex.split(environment.get('CC', 'cc'))
compiler_identity = subprocess.check_output(compiler_command + ['--version'],
                                          stderr=subprocess.STDOUT)
compiler_target = subprocess.check_output(compiler_command + ['-dumpmachine'], text=True).strip()
architecture = {'x86_64': 'amd64', 'aarch64': 'arm64'}.get(compiler_target.split('-')[0])
if not architecture or 'linux' not in compiler_target:
    parser.error('this dependency builder requires a Linux amd64/arm64 C toolchain')
publication = None
recipe = (Path(__file__).read_bytes() + (fixture / 'build_openlibm.py').read_bytes() + compiler_identity +
          json.dumps(manifest, sort_keys=True).encode() + str(args.shared).encode())
if args.install:
    host = {'x86_64': 'amd64', 'aarch64': 'arm64'}.get(platform.machine())
    if sys.platform != 'linux' or host != architecture:
        parser.error('--install requires a native Linux C toolchain')
    modules = Path(os.environ.get('VMODULES', '').split(os.pathsep)[0] or
                   str(Path.home() / '.vmodules')).expanduser().resolve()
    store = modules / 'vlibc' / '.store'
    lock_build_directories(root, store)
    publication = modules / 'vlibc' / ('linux-' + architecture + ('-shared' if args.shared else '-static'))
    if publication.exists() or publication.is_symlink():
        if not publication.is_symlink() or store not in publication.resolve().parents:
            parser.error('refusing to replace an unmanaged prefix: ' + str(publication))
    release = hashlib.sha256(recipe + str(store).encode()).hexdigest()[:24]
    completion = publication / '.x11-install-recipe'
    if completion.is_file() and completion.read_text().strip() == release and not (publication / 'vpac-profile.json').exists():
        print(f'Already installed: {publication}', flush=True)
        sys.exit(0)
    installation = PrefixInstall(publication, f'linux-{architecture}-{release}')
    prefix = installation.prefix
else:
    lock_build_directories(root)
    prefix = (args.prefix or root / 'prefix').resolve()
environment.update(CFLAGS=flags, CPPFLAGS='-I' + str(prefix / 'include'),
                   LDFLAGS='-L' + str(prefix / 'lib'),
                   PKG_CONFIG_PATH=os.pathsep.join(str(prefix / path) for path in
                                                  ['lib/pkgconfig', 'share/pkgconfig']))
# The immutable installation path is part of the fixture build identity.
identity = hashlib.sha256(recipe + str(prefix).encode()).hexdigest()
sources = {}


def prepare(item):
    name, record = item
    archive = root / (name + '-source.tar')
    if not archive.exists():
        subprocess.run(['curl', '-fLsS', '--retry', '2', '--max-time', '120',
                        record['url'], '-o', str(archive)], check=True)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != record['sha256']:
        raise RuntimeError(f'{name}: source archive does not match its pinned SHA-256')
    with tarfile.open(archive) as tar:
        folder = tar.getnames()[0].split('/')[0]
        source = root / folder
        if not source.exists():
            tar.extractall(root, filter='data')
    return name, source


with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
    sources.update(pool.map(prepare, manifest.items()))


def run(name, commands, cwd):
    with (root / (name + '-stack.log')).open('w') as log:
        for command in commands:
            print(name + ': ' + ' '.join(map(str, command)), flush=True)
            subprocess.run(list(map(str, command)), cwd=cwd, env=environment,
                           stdout=log, stderr=subprocess.STDOUT, check=True)


def autotools(name, extra=()):
    run(name, [['./configure', '--prefix=' + str(prefix), '--libdir=' + str(prefix / 'lib'),
                '--disable-shared', '--enable-static', *extra],
               ['make', '-j' + str(args.jobs)], ['make', 'install']], sources[name])


def cmake(name, extra=()):
    source = sources[name]
    run(name, [['cmake', '-S', source, '-B', source / 'bare-build', '-G', 'Ninja',
                '-DCMAKE_BUILD_TYPE=Release', '-DCMAKE_INSTALL_PREFIX=' + str(prefix),
                '-DCMAKE_INSTALL_LIBDIR=lib', '-DCMAKE_C_FLAGS=' + flags,
                '-DBUILD_SHARED_LIBS=OFF', *extra],
               ['cmake', '--build', source / 'bare-build', '--parallel', str(args.jobs)],
               ['cmake', '--install', source / 'bare-build']], root)


def build(name):
    if name in ['xau', 'xdmcp', 'xrender', 'xft']:
        autotools(name)
    elif name == 'xtrans':
        run(name, [['./configure', '--prefix=' + str(prefix)], ['make', 'install']],
            sources[name])
    elif name == 'xcb':
        autotools(name, ['--disable-devel-docs'])
    elif name == 'x11':
        autotools(name, ['--disable-specs', '--disable-loadable-i18n',
                         '--disable-loadable-xcursor'])
    elif name == 'fontconfig':
        source = sources[name]
        setup = ['meson', 'setup', 'bare-build', '--prefix=' + str(prefix),
                 '--libdir=lib', '--default-library=static', '--buildtype=release',
                 '-Ddoc=disabled', '-Dtools=disabled', '-Dtests=disabled',
                 '-Dcache-build=disabled', '-Dc_args=' + flags]
        if (source / 'bare-build/meson-private/coredata.dat').exists():
            setup.append('--reconfigure')
        run(name, [setup, ['ninja', '-C', 'bare-build', '-j' + str(args.jobs)],
                   ['meson', 'install', '-C', 'bare-build', '--no-rebuild']], source)
    elif name == 'zlib':
        run(name, [['./configure', '--prefix=' + str(prefix), '--static'],
                   ['make', '-j' + str(args.jobs)], ['make', 'install']], sources[name])
    elif name == 'bzip2':
        run(name, [['make', '-j' + str(args.jobs), 'libbz2.a', 'CFLAGS=' + flags]], sources[name])
        (prefix / 'lib').mkdir(exist_ok=True, parents=True)
        (prefix / 'include').mkdir(exist_ok=True, parents=True)
        shutil.copyfile(sources[name] / 'libbz2.a', prefix / 'lib/libbz2.a')
        shutil.copyfile(sources[name] / 'bzlib.h', prefix / 'include/bzlib.h')
        (prefix / 'lib/pkgconfig').mkdir(exist_ok=True)
        (prefix / 'lib/pkgconfig/bzip2.pc').write_text('prefix=' + str(prefix) +
            '\nlibdir=${prefix}/lib\nincludedir=${prefix}/include\n\nName: bzip2\n'
            'Description: bzip2 compression library\nVersion: 1.0.8\n'
            'Libs: -L${libdir} -lbz2\nCflags: -I${includedir}\n')
    elif name == 'brotli':
        cmake(name, ['-DBROTLI_DISABLE_TESTS=ON'])
    elif name == 'png':
        cmake(name, ['-DPNG_SHARED=OFF', '-DPNG_STATIC=ON', '-DPNG_TESTS=OFF',
                     '-DPNG_TOOLS=OFF', '-DZLIB_LIBRARY=' + str(prefix / 'lib/libz.a'),
                     '-DZLIB_INCLUDE_DIR=' + str(prefix / 'include')])
    elif name == 'expat':
        cmake(name, ['-DEXPAT_BUILD_TESTS=OFF', '-DEXPAT_BUILD_EXAMPLES=OFF',
                     '-DEXPAT_BUILD_TOOLS=OFF',
                     '-DCMAKE_C_FLAGS=' + flags +
                     (' -DXML_ENABLE_VISIBILITY=1' if args.shared else '')])
    elif name == 'freetype':
        cmake(name, ['-DFT_DISABLE_HARFBUZZ=ON', '-DCMAKE_PREFIX_PATH=' + str(prefix),
                     '-DZLIB_LIBRARY=' + str(prefix / 'lib/libz.a'),
                     '-DPNG_LIBRARY_RELEASE=' + str(prefix / 'lib/libpng16.a'),
                     '-DBZIP2_LIBRARY_RELEASE=' + str(prefix / 'lib/libbz2.a'),
                     '-DBROTLIDEC_LIBRARIES=' + str(prefix / 'lib/libbrotlidec.a')])
    else:
        raise RuntimeError('unknown dependency: ' + name)
    (root / (name + '.built')).write_text(identity + ':' + manifest[name]['sha256'] + '\n')


dependencies = {
    'xau': [], 'xdmcp': [], 'xtrans': [], 'xcb': ['xau', 'xdmcp'],
    'x11': ['xcb', 'xtrans'], 'xrender': ['x11'],
    'zlib': [], 'bzip2': [], 'brotli': [], 'expat': [], 'png': ['zlib'],
    'freetype': ['png', 'brotli', 'bzip2'], 'fontconfig': ['freetype', 'expat'],
    'xft': ['xrender', 'fontconfig'],
}
complete = {name for name in dependencies if (root / (name + '.built')).exists()
            and (root / (name + '.built')).read_text().strip() == identity + ':' + manifest[name]['sha256']}
remaining = set(dependencies) - complete
while remaining:
    ready = [name for name in sorted(remaining) if set(dependencies[name]) <= complete]
    if not ready:
        raise RuntimeError('dependency cycle')
    # The job limit is global: do not multiply it across independent libraries.
    for name in ready:
        build(name)
    complete.update(ready)
    remaining.difference_update(ready)
# The regular V math module declares -lm. Both native profiles provide it.
build_openlibm(sources['openlibm'], prefix, root, compiler_command, flags, manifest['openlibm'],
               args.jobs, run, shared=args.shared)
libraries = ['Xft', 'fontconfig', 'Xrender', 'X11', 'xcb', 'Xau', 'Xdmcp',
             'freetype', 'expat', 'png16', 'z', 'bz2', 'brotlidec', 'brotlicommon', 'm']
arguments = ['-L' + str(prefix / 'lib'), '-Wl,--start-group']
arguments += [str(prefix / 'lib' / ('lib' + name + '.a')) for name in libraries]
arguments.append('-Wl,--end-group')
response = root / 'link.rsp'
response.write_text('\n'.join(json.dumps(arg) for arg in arguments) + '\n')
for name in libraries:
    if not (prefix / 'lib' / ('lib' + name + '.a')).is_file():
        raise RuntimeError('incomplete dependency installation: ' + name)
if args.shared:
    compiler_runtime = Path(subprocess.check_output(
        compiler_command + ['-print-libgcc-file-name'], text=True).strip())
    if compiler_runtime.suffix != '.a' or not compiler_runtime.is_file():
        raise RuntimeError('the selected C toolchain must expose a compiler runtime archive')
    # Keep the complete public library surface. Linking only the objects reached
    # by one application would hide missing runtime APIs in other entry points.
    shared_dependencies = {
        'Xau': [], 'Xdmcp': [], 'xcb': ['Xau', 'Xdmcp'], 'X11': ['xcb'],
        'Xrender': ['X11'], 'z': [], 'bz2': [], 'brotlicommon': [],
        'brotlidec': ['brotlicommon'], 'png16': ['z', 'm'], 'expat': [],
        'freetype': ['png16', 'z', 'bz2', 'brotlidec', 'brotlicommon'],
        'fontconfig': ['freetype', 'expat'],
        'Xft': ['Xrender', 'X11', 'fontconfig', 'freetype'],
    }
    for name, children in shared_dependencies.items():
        command = compiler_command + ['-shared', '-nostdlib',
            '-Wl,--whole-archive', str(prefix / 'lib' / ('lib' + name + '.a')),
            '-Wl,--no-whole-archive', '-Wl,-soname,lib' + name + '-vlibc.so',
            '-L' + str(prefix / 'lib'), '-Wl,--no-as-needed']
        command += ['-l' + child for child in children]
        # Compiler helpers have hidden ABI visibility; resolve them within each
        # DSO instead of importing them from the executable's libgcc archive.
        command += [str(compiler_runtime), '-Wl,-rpath,$ORIGIN', '-o', str(prefix / 'lib' / ('lib' + name + '-vlibc.so'))]
        run(name + '-shared', [command], root)
        # Distinct SONAMEs prevent the linker/loader from substituting a host
        # libc-dependent DSO with the same public -l name (and vice versa).
        alias = prefix / 'lib' / ('lib' + name + '.so')
        alias.unlink(missing_ok=True)
        alias.symlink_to('lib' + name + '-vlibc.so')
    # These DSOs use the executable's replacement C/math/pthread ABI. Upstream
    # static-build metadata promotes host -lm and private archive closures to
    # public flags, which no longer describe the wrapped shared libraries.
    # Preserve those closures for --static, keeping normal shared queries exact.
    pc_libraries = {'x11': 'X11', 'xft': 'Xft', 'xrender': 'Xrender', 'xau': 'Xau',
        'xdmcp': 'Xdmcp', 'xcb': 'xcb', 'fontconfig': 'fontconfig', 'freetype2': 'freetype',
        'expat': 'expat', 'libpng16': 'png16', 'zlib': 'z', 'bzip2': 'bz2',
        'libbrotlidec': 'brotlidec', 'libbrotlicommon': 'brotlicommon'}
    for package, library in pc_libraries.items():
        metadata = prefix / 'lib/pkgconfig' / (package + '.pc')
        if not metadata.exists():
            continue
        lines = metadata.read_text().splitlines()
        original_libs = next((line[5:].strip() for line in lines if line.startswith('Libs:')), '')
        original_requires = next((line[9:].strip() for line in lines if line.startswith('Requires:')), '')
        rebuilt = []
        for line in lines:
            if line.startswith('Libs:'):
                line = 'Libs: -L${libdir} -l' + library
            elif line.startswith('Libs.private:'):
                line += ' ' + original_libs
            elif line.startswith('Requires:'):
                line = 'Requires:'
            elif line.startswith('Requires.private:'):
                line += ' ' + original_requires
            rebuilt.append(line)
        if not any(line.startswith('Libs.private:') for line in rebuilt):
            rebuilt.append('Libs.private: ' + original_libs)
        if original_requires and not any(line.startswith('Requires.private:') for line in rebuilt):
            rebuilt.append('Requires.private: ' + original_requires)
        metadata.write_text('\n'.join(rebuilt) + '\n')
(prefix / 'vlibc-target').write_text('vlibc-native-v1\nlinux\n' + architecture + '\n')
if publication:
    installation.publish('.x11-install-recipe', release)
    print(f'Installed native vlibc dependencies: {publication}', flush=True)
else:
    print(f'Dependencies built. Use VLIBC_PREFIX={prefix} or -ldflags @{response}.', flush=True)
