"""Build the optional libc-free shared dependency profile for vpac.

This adds curl (Mbed TLS, HTTP/2), libarchive (compression, XML, ACLs),
GPGME and libseccomp to an existing X11/font-stack prefix. It does not install
system packages. Builds use one compiler worker unless --jobs is supplied.
The prefix is published only after its ELF dependency audit passes.
"""
import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tarfile

from build_openlibm import build_openlibm
from prefix_install import PrefixInstall, lock_build_directories

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('work', type=Path)
parser.add_argument('--base', type=Path, help='existing native shared prefix to preserve')
parser.add_argument('--sources', type=Path, help='reuse an existing download/source cache')
parser.add_argument('--jobs', type=int, default=1)
choice = parser.add_mutually_exclusive_group()
choice.add_argument('--prefix', type=Path)
choice.add_argument('--install', action='store_true')
args = parser.parse_args()
if args.jobs < 1:
    parser.error('--jobs must be positive')
fixture = Path(__file__).resolve().parent
manifest = json.loads((fixture / 'vpac_sources.json').read_text())
manifest['openlibm'] = json.loads((fixture / 'media_toolchain_sources.json').read_text())['openlibm']
work = args.work.resolve()
work.mkdir(parents=True, exist_ok=True)
cache = (args.sources or work / 'sources').resolve()
cache.mkdir(parents=True, exist_ok=True)
environment = os.environ.copy()
compiler = shlex.split(environment.get('CC', 'cc'))
target = subprocess.check_output(compiler + ['-dumpmachine'], text=True).strip()
architecture = {'x86_64': 'amd64', 'aarch64': 'arm64'}.get(target.split('-')[0])
if not architecture or 'linux' not in target:
    parser.error('this profile requires a Linux amd64/arm64 C toolchain')
modules = Path(environment.get('VMODULES', '').split(os.pathsep)[0] or
               str(Path.home() / '.vmodules')).expanduser().resolve()
publication = modules / 'vlibc' / ('linux-' + architecture + '-shared')
store = publication.parent / '.store'
build_directories = [work, cache]
if args.install:
    build_directories.append(store)
lock_build_directories(*build_directories)
base = (args.base or publication).resolve()
if not args.base and (base / 'vpac-profile.json').is_file():
    base = Path(json.loads((base / 'vpac-profile.json').read_text())['base']).resolve()
expected_marker = 'vlibc-native-v1\nlinux\n' + architecture + '\n'
if not (base / 'vlibc-target').is_file() or (base / 'vlibc-target').read_text() != expected_marker:
    parser.error('--base must be a matching, complete native vlibc prefix')
compiler_identity = subprocess.check_output(compiler + ['--version'])
recipe = hashlib.sha256(Path(__file__).read_bytes() + (fixture / 'build_openlibm.py').read_bytes() + compiler_identity +
    json.dumps(manifest, sort_keys=True).encode() + str(base).encode()).hexdigest()
if args.install:
    if sys.platform != 'linux' or platform.machine() != target.split('-')[0]:
        parser.error('--install requires a native C toolchain')
    if not publication.is_symlink() or store not in publication.resolve().parents:
        parser.error('refusing to replace an unmanaged prefix')
    completion = publication / '.vpac-install-recipe'
    if (store in base.parents and (base / '.x11-install-recipe').is_file()
            and completion.is_file() and completion.read_text().strip() == recipe):
        print(f'Already installed: {publication}', flush=True)
        sys.exit(0)
    # Build at its permanent path (embedded pkg-config/CMake paths must work).
    # Only the atomic symlink replacement makes this release visible.
    installation = PrefixInstall(publication, f'linux-{architecture}-vpac-{recipe[:24]}')
    prefix = installation.prefix
else:
    prefix = (args.prefix or work / 'prefix').resolve()
if prefix == base or prefix in base.parents or base in prefix.parents:
    parser.error('the output prefix must be separate from --base')
crypto_prefix = prefix / 'private/mbedtls'
state = work / 'build-state'
state.mkdir(exist_ok=True)
if args.install or not prefix.exists():
    shutil.copytree(base, prefix, symlinks=True, dirs_exist_ok=args.install)
    # Existing metadata continues to name the new, complete prefix.
    for directory in ['lib/pkgconfig', 'share/pkgconfig', 'lib/cmake']:
        for path in (prefix / directory).rglob('*'):
            if path.is_file() and not path.is_symlink():
                try:
                    text = path.read_text()
                except UnicodeDecodeError:
                    continue
                path.write_text(text.replace(str(base), str(prefix)))
flags = ('-O2 -std=gnu11 -fPIC -fno-stack-protector -D_GNU_SOURCE=1 '
         '-D__NO_INLINE__=1 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 '
         '-ffunction-sections -fdata-sections')
environment.update(CFLAGS=flags, CPPFLAGS='-I' + str(prefix / 'include'),
    LDFLAGS='-L' + str(prefix / 'lib'),
    PKG_CONFIG_LIBDIR=os.pathsep.join(str(prefix / p) for p in ['lib/pkgconfig', 'share/pkgconfig']),
    PKG_CONFIG_PATH='', CMAKE_BUILD_PARALLEL_LEVEL=str(args.jobs))
sources = {}


def prepare(item):
    name, record = item
    archive = cache / (name + '-vpac-source.tar')
    if not archive.exists():
        subprocess.run(['curl', '-fLsS', '--retry', '2', '--max-time', '120',
                        record['url'], '-o', str(archive)], check=True)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != record['sha256']:
        raise RuntimeError(name + ': source checksum mismatch')
    source = cache / record['folder']
    if not source.exists():
        with tarfile.open(archive) as tar:
            if any(p.split('/')[0] != record['folder'] for p in tar.getnames()):
                raise RuntimeError(name + ': unexpected source archive layout')
            tar.extractall(cache, filter='data')
    return name, source


with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
    sources.update(pool.map(prepare, manifest.items()))


def run(name, commands, cwd, configuration_steps=1):
    command_identity = hashlib.sha256(json.dumps({
        'commands': [[str(value) for value in command] for command in commands],
        'cwd': str(cwd), 'sources': manifest, 'compiler': compiler_identity.decode(),
        'environment': {key: environment.get(key) for key in
            ['CC', 'CFLAGS', 'CPPFLAGS', 'LDFLAGS', 'PKG_CONFIG_LIBDIR']},
    }, sort_keys=True).encode()).hexdigest()
    stamp = state / name
    if not name.endswith(('-shared', '-namespace')) and stamp.exists() and stamp.read_text() == command_identity:
        print(name + ': using completed configuration', flush=True)
        commands = commands[configuration_steps:]
    # Always run the upstream incremental build. Its compiler-produced .d
    # files track source/header changes without inspecting C declarations.
    with (work / (name + '.log')).open('w') as log:
        for command in commands:
            print(name + ': ' + shlex.join(map(str, command)), flush=True)
            subprocess.run(list(map(str, command)), cwd=cwd, env=environment,
                           stdout=log, stderr=subprocess.STDOUT, check=True)
    stamp.write_text(command_identity)


def cmake(name, extra=(), source_subdir='', prepare=(), install_root=None):
    source = sources[name] / source_subdir
    directory = work / (name + '-build')
    run(name, [*prepare, ['cmake', '-S', source, '-B', directory, '-G', 'Ninja',
        '-DCMAKE_BUILD_TYPE=Release', '-DCMAKE_INSTALL_PREFIX=' + str(install_root or prefix),
        '-DCMAKE_INSTALL_LIBDIR=lib', '-DCMAKE_POSITION_INDEPENDENT_CODE=ON',
        '-DCMAKE_C_FLAGS=' + flags, '-DCMAKE_PREFIX_PATH=' + str(prefix),
        '-DBUILD_SHARED_LIBS=OFF', *extra],
        ['cmake', '--build', directory, '--parallel', str(args.jobs)],
        ['cmake', '--install', directory]], work, configuration_steps=len(prepare) + 1)


def autotools(name, extra=(), subdir=None):
    directory = work / (name + '-build')
    directory.mkdir(exist_ok=True)
    commands = [[sources[name] / 'configure', '--prefix=' + str(prefix),
        '--libdir=' + str(prefix / 'lib'), '--disable-shared', '--enable-static',
        '--with-pic', *extra]]
    if subdir:
        commands += [['make', '-C', subdir, '-j' + str(args.jobs)],
                     ['make', '-C', subdir, 'install']]
    else:
        commands += [['make', '-j' + str(args.jobs)], ['make', 'install']]
    run(name, commands, directory)


def build(name):
    if name == 'mbedtls':
        config = sources[name] / 'include/mbedtls/mbedtls_config.h'
        prepare = [[sys.executable, sources[name] / 'scripts/config.py', '-f', config, 'set', option]
                   for option in ['MBEDTLS_THREADING_C', 'MBEDTLS_THREADING_PTHREAD']]
        cmake(name, ['-DENABLE_PROGRAMS=OFF', '-DENABLE_TESTING=OFF',
            '-DUSE_SHARED_MBEDTLS_LIBRARY=OFF', '-DUSE_STATIC_MBEDTLS_LIBRARY=ON',
            '-DMBEDTLS_FATAL_WARNINGS=OFF'], prepare=prepare, install_root=crypto_prefix)
    elif name == 'lz4':
        cmake(name, ['-DLZ4_BUILD_CLI=OFF', '-DLZ4_BUILD_LEGACY_LZ4C=OFF'], 'build/cmake')
    elif name == 'zstd':
        cmake(name, ['-DZSTD_BUILD_PROGRAMS=OFF', '-DZSTD_BUILD_TESTS=OFF',
                    '-DZSTD_BUILD_SHARED=OFF', '-DZSTD_BUILD_STATIC=ON'], 'build/cmake')
    elif name == 'xz':
        cmake(name, ['-DXZ_TOOL_XZ=OFF', '-DXZ_TOOL_XZDEC=OFF', '-DXZ_TOOL_LZMADEC=OFF',
                    '-DXZ_TOOL_LZMAINFO=OFF', '-DXZ_TOOL_SCRIPTS=OFF', '-DBUILD_TESTING=OFF'])
    elif name == 'nghttp2':
        cmake(name, ['-DENABLE_LIB_ONLY=ON', '-DENABLE_STATIC_LIB=ON',
                    '-DENABLE_SHARED_LIB=OFF', '-DBUILD_STATIC_LIBS=ON', '-DBUILD_TESTING=OFF'])
    elif name == 'acl':
        autotools(name, ['--disable-nls'])
    elif name == 'gpg-error':
        autotools(name, ['--disable-nls', '--disable-doc', '--disable-tests'], 'src')
    elif name == 'assuan':
        autotools(name, ['--with-libgpg-error-prefix=' + str(prefix)], 'src')
    elif name == 'gpgme':
        autotools(name, ['--with-libgpg-error-prefix=' + str(prefix),
            '--with-libassuan-prefix=' + str(prefix), '--disable-gpgsm-test',
            '--disable-gpgconf-test', '--disable-gpg-test', '--disable-g13-test'], 'src')
    elif name == 'seccomp':
        autotools(name, ['--disable-tools'], 'src')
        shutil.copyfile(work / 'seccomp-build/include/seccomp.h', prefix / 'include/seccomp.h')
    elif name == 'curl':
        autotools(name, ['--with-mbedtls=' + str(crypto_prefix), '--without-openssl', '--disable-symbol-hiding', '--disable-ldap', '--disable-ldaps',
            '--without-gnutls', '--without-wolfssl', '--without-rustls', '--without-gssapi',
            '--without-libssh2', '--without-libssh', '--without-libpsl', '--without-libidn2',
            '--without-libgsasl', '--without-quiche', '--without-ngtcp2', '--without-nghttp3',
            '--with-nghttp2=' + str(prefix), '--with-zlib=' + str(prefix),
            '--with-brotli=' + str(prefix), '--with-zstd=' + str(prefix),
            '--with-ca-bundle=/etc/ssl/certs/ca-certificates.crt',
            '--with-ca-path=/etc/ssl/certs', '--disable-docs', '--disable-manual'], 'lib')
        shutil.copytree(sources[name] / 'include/curl', prefix / 'include/curl', dirs_exist_ok=True)
        for header in (work / 'curl-build/include/curl').glob('*.h'):
            shutil.copyfile(header, prefix / 'include/curl' / header.name)
    elif name == 'archive':
        cmake(name, ['-DENABLE_TAR=OFF', '-DENABLE_CPIO=OFF', '-DENABLE_CAT=OFF',
            '-DENABLE_UNZIP=OFF', '-DENABLE_TEST=OFF', '-DENABLE_OPENSSL=OFF',
            '-DENABLE_NETTLE=OFF', '-DENABLE_MD=OFF', '-DENABLE_LIBB2=OFF',
            '-DENABLE_MBEDTLS=ON', '-DENABLE_LIBXML2=OFF', '-DENABLE_BSDXML=OFF',
            '-DENABLE_EXPAT=ON', '-DENABLE_PCREPOSIX=OFF', '-DENABLE_PCRE2POSIX=OFF',
            '-DENABLE_ACL=ON', '-DENABLE_XATTR=ON', '-DENABLE_ICONV=ON',
            '-DMBEDTLS_INCLUDE_DIRS=' + str(crypto_prefix / 'include'),
            '-DMBEDTLS_LIBRARY=' + str(crypto_prefix / 'lib/libmbedtls.a'),
            '-DMBEDX509_LIBRARY=' + str(crypto_prefix / 'lib/libmbedx509.a'),
            '-DMBEDCRYPTO_LIBRARY=' + str(crypto_prefix / 'lib/libmbedcrypto.a'),
            '-DACL_LIBRARY=' + str(prefix / 'lib/libacl.a')])


# Sequential libraries bound total compiler use to --jobs, including configure
# probes and generated build-time tools. Each completed step can be resumed.
for name in ['mbedtls', 'lz4', 'zstd', 'xz', 'nghttp2', 'acl', 'gpg-error',
             'assuan', 'gpgme', 'seccomp', 'curl', 'archive']:
    build(name)

# Configure probes above execute on the host. Install the replacement math ABI
# alongside the target DSOs after the host build steps.
build_openlibm(sources['openlibm'], prefix, work, compiler, flags, manifest['openlibm'],
               args.jobs, lambda name, commands, cwd: run(name, commands, cwd, configuration_steps=0),
               base=base)

# Keep backend headers out of the generic include search path. Native TLS and
# V's bundled TLS may have different versions/configurations in one process.
# Namespace opaque compiled symbols, never infer an API from C headers.
crypto_names = ['mbedcrypto', 'mbedx509', 'mbedtls']
crypto_archives = [crypto_prefix / 'lib' / ('lib' + name + '.a') for name in crypto_names]
crypto_symbols = set()
for archive in crypto_archives:
    for line in subprocess.check_output(['nm', '-g', '--defined-only', archive], text=True).splitlines():
        fields = line.split()
        if len(fields) == 3:
            crypto_symbols.add(fields[-1])
namespace_map = work / 'mbedtls-namespace.map'
namespace_map.write_text(''.join(name + ' vlibc_private_' + name + '\n' for name in sorted(crypto_symbols)))
archives = {
    'vlibc_' + name: crypto_prefix / 'lib' / ('lib' + name + '.a') for name in crypto_names
}
archives['curl'] = work / 'curl-build/lib/.libs/libcurl.a'
archives['archive'] = work / 'archive-build/libarchive/libarchive.a'
for name, source in archives.items():
    output = prefix / 'lib' / ('lib' + name + '.a')
    run(name + '-namespace', [['objcopy', '--redefine-syms=' + str(namespace_map), source, output]],
        work, configuration_steps=0)
libraries = {
    'vlibc_mbedcrypto': [], 'vlibc_mbedx509': ['vlibc_mbedcrypto'],
    'vlibc_mbedtls': ['vlibc_mbedx509', 'vlibc_mbedcrypto'],
    'lz4': [], 'zstd': [], 'lzma': [], 'nghttp2': [], 'acl': [], 'gpg-error': [],
    'assuan': ['gpg-error'], 'gpgme': ['assuan', 'gpg-error'], 'seccomp': [],
    'curl': ['vlibc_mbedtls', 'vlibc_mbedx509', 'vlibc_mbedcrypto', 'nghttp2',
             'z', 'brotlidec', 'brotlicommon', 'zstd'],
    'archive': ['vlibc_mbedcrypto', 'lzma', 'zstd', 'lz4', 'bz2', 'z', 'expat', 'acl'],
}

compiler_runtime = Path(subprocess.check_output(compiler + ['-print-libgcc-file-name'], text=True).strip())
if compiler_runtime.suffix != '.a' or not compiler_runtime.is_file():
    raise RuntimeError('the selected C toolchain must expose its compiler runtime archive')
for name, children in libraries.items():
    archive = prefix / 'lib' / ('lib' + name + '.a')
    if not archive.is_file():
        raise RuntimeError('missing complete library archive: ' + str(archive))
    output = prefix / 'lib' / ('lib' + name + '-vlibc.so')
    command = compiler + ['-shared', '-nostdlib', '-Wl,--whole-archive', str(archive),
        '-Wl,--no-whole-archive', '-Wl,-soname,' + output.name,
        '-L' + str(prefix / 'lib'), '-Wl,--no-as-needed']
    command += ['-l' + child for child in children]
    if name == 'curl':
        exports = work / 'curl-public.map'
        exports.write_text('{ global: curl_*; local: *; };\n')
        command += ['-Wl,--version-script,' + str(exports)]
    command += [str(compiler_runtime), '-Wl,-rpath,$ORIGIN', '-o', str(output)]
    run(name + '-shared', [command], work)
    alias = prefix / 'lib' / ('lib' + name + '.so')
    alias.unlink(missing_ok=True)
    alias.symlink_to(output.name)

# Verify the entire compiled public API, not only the functions one smoke
# application happens to call. Static-build visibility must not hide it.
public_prefixes = {'curl': 'curl_', 'archive': 'archive_', 'gpgme': 'gpgme_',
                   'seccomp': 'seccomp_', 'nghttp2': 'nghttp2_'}
for library, public in public_prefixes.items():
    def definitions(path, dynamic=False):
        command = ['readelf', '--wide', '--dyn-syms' if dynamic else '--syms', str(path)]
        result = subprocess.check_output(command, text=True)
        return {fields[7].split('@', 1)[0] for line in result.splitlines()
                if len(fields := line.split()) >= 8 and fields[4] in ('GLOBAL', 'WEAK')
                and fields[5] in ('DEFAULT', 'PROTECTED') and fields[6] != 'UND'
                and fields[7].startswith(public)}
    expected = definitions(prefix / 'lib' / ('lib' + library + '.a'))
    actual = definitions(prefix / 'lib' / ('lib' + library + '-vlibc.so'), True)
    if not expected or expected - actual:
        raise RuntimeError(library + ': missing public exports: ' + ', '.join(sorted(expected - actual)))

# Install generated package metadata skipped by the library-only make targets.
for package, path in [('libcurl', work / 'curl-build/libcurl.pc'),
                      ('libseccomp', work / 'seccomp-build/libseccomp.pc')]:
    shutil.copyfile(path, prefix / 'lib/pkgconfig' / (package + '.pc'))
packages = {'mbedcrypto': 'vlibc_mbedcrypto', 'mbedx509': 'vlibc_mbedx509',
    'mbedtls': 'vlibc_mbedtls', 'liblz4': 'lz4', 'libzstd': 'zstd', 'liblzma': 'lzma',
    'libnghttp2': 'nghttp2', 'libacl': 'acl', 'gpg-error': 'gpg-error',
    'libassuan': 'assuan', 'gpgme': 'gpgme', 'libseccomp': 'seccomp',
    'libcurl': 'curl', 'libarchive': 'archive'}
for package, library in packages.items():
    metadata = prefix / 'lib/pkgconfig' / (package + '.pc')
    if not metadata.exists():
        continue
    lines = metadata.read_text().splitlines()
    public = next((line[5:].strip() for line in lines if line.startswith('Libs:')), '')
    required = next((line[9:].strip() for line in lines if line.startswith('Requires:')), '')
    private = next((line[13:].strip() for line in lines if line.startswith('Libs.private:')), '')
    private_required = next((line[17:].strip() for line in lines if line.startswith('Requires.private:')), '')
    private_flags = []
    for flag in shlex.split(private + ' ' + public):
        if flag in ['-lc', '-lpthread', '-pthread', '-ldl', '-lrt', '-lresolv', '-lutil']:
            continue
        if flag in ['-lmbedcrypto', '-lmbedx509', '-lmbedtls']:
            flag = '-lvlibc_' + flag[2:]
        if flag not in private_flags:
            private_flags.append(flag)
    rewritten = []
    for line in lines:
        if line.startswith('Libs:'):
            line = 'Libs: -L${libdir} -l' + library
        elif line.startswith('Libs.private:'):
            continue
        elif line.startswith('Requires:'):
            line = 'Requires:'
        elif line.startswith('Requires.private:'):
            continue
        elif line.startswith('Cflags:'):
            line = line.replace(' -DCURL_STATICLIB', '').replace(' -DNGHTTP2_STATICLIB', '')
        rewritten.append(line)
    rewritten.append('Libs.private: ' + ' '.join(private_flags))
    rewritten.append('Requires.private: ' + (private_required + ' ' + required).strip())
    metadata.write_text('\n'.join(rewritten) + '\n')
# Installed CMake static consumers must use the same namespaced crypto objects.
for metadata in (prefix / 'lib/cmake').rglob('*.cmake'):
    text = metadata.read_text()
    for name in crypto_names:
        text = text.replace(str(crypto_prefix / 'lib' / ('lib' + name + '.a')),
                            str(prefix / 'lib' / ('libvlibc_' + name + '.a')))
    metadata.write_text(text)

# The explicit profile is recorded, rather than inheriting the host's optional
# C++/OpenSSL/ICU/GSSAPI/plugin dependency closure by accident.
profile = {'schema': 1, 'base': str(base), 'sources': manifest,
           'features': ['HTTP/HTTPS', 'HTTP/2', 'Mbed TLS', 'zlib', 'bzip2', 'LZ4', 'XZ',
                        'Zstandard', 'Brotli decoding', 'Expat', 'POSIX ACLs', 'xattrs',
                        'GPGME', 'libseccomp', 'OpenLibm'],
           'crypto_namespace': 'vlibc_private_', 'crypto_headers': str(crypto_prefix / 'include'),
           'excluded_optional_backends': ['OpenSSL', 'ICU', 'GSSAPI', 'SSH', 'HTTP/3', 'libpsl', 'IDN', 'LDAP']}
(prefix / 'vpac-profile.json').write_text(json.dumps(profile, indent=2) + '\n')
imports = set()
for library in (prefix / 'lib').glob('lib*-vlibc.so'):
    dynamic = subprocess.check_output(['readelf', '-d', library], text=True)
    for needed in re.findall(r'\(NEEDED\).*\[([^]]+)\]', dynamic):
        if not needed.endswith('-vlibc.so') or not (prefix / 'lib' / needed).is_file():
            raise RuntimeError(str(library) + ': unexpected runtime dependency ' + needed)
    symbols = subprocess.check_output(['nm', '-D', '--undefined-only', library], text=True)
    for line in symbols.splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[0] == 'U':
            imports.add(fields[1])
(prefix / 'vpac-required-symbols.txt').write_text('\n'.join(sorted(imports)) + '\n')
(prefix / 'vlibc-target').write_text(expected_marker)
if args.install:
    installation.publish('.vpac-install-recipe', recipe)
    print('Installed combined native vlibc dependencies: ' + str(publication), flush=True)
else:
    print('Dependencies ready. Use VLIBC_PREFIX=' + str(prefix), flush=True)
