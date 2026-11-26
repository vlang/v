"""Build the native libm shared by the package and media dependency profiles."""
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess


def build_openlibm(source, prefix, work, compiler, flags, record, jobs, run, *, shared=True,
                  base=None):
    libdir = prefix / 'lib'
    installed = libdir / 'libm.a'
    dso = libdir / 'libm-vlibc.so'
    alias = libdir / 'libm.so'
    receipt = 'openlibm-profile.json'
    target = subprocess.check_output(compiler + ['-dumpmachine'], text=True).strip()
    provenance = {'source_sha256': record['sha256'], 'target': target,
                  'builder_sha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest()}
    if shared and base and (base / receipt).is_file() and installed.is_file() and dso.is_file():
        # The parent profile already validated its math ABI. Reuse its installed
        # objects only when our copied prefix still contains those exact files.
        expected = json.loads((base / receipt).read_text())
        if (expected.get('provenance') == provenance and alias.is_symlink()
                and alias.readlink() == Path(dso.name)
                and all(hashlib.sha256(path.read_bytes()).hexdigest() == expected.get(path.name)
                        for path in (installed, dso))):
            (prefix / receipt).write_text(json.dumps(expected, sort_keys=True) + '\n')
            return
    make_args = ['USEGCC=1', 'CC=' + shlex.join(compiler), 'CFLAGS=' + flags + ' -MD -MP',
                 'SFLAGS=-MD -MP', 'CPPFLAGS=-I' + str(prefix / 'include')]
    identity = hashlib.sha256(json.dumps({
        'compiler': subprocess.check_output(compiler + ['--version'], text=True),
        'target': target,
        'args': make_args, 'record': record,
    }, sort_keys=True).encode()).hexdigest()
    stamp = source / '.vlibc-build-flags'
    if not stamp.exists() or stamp.read_text() != identity:
        stamp.unlink(missing_ok=True)
        run('openlibm-clean', [['make', 'clean']], source)
    # Read the compiler-generated dependency files for whichever architecture
    # the selected compiler targets, including its long-double implementation.
    deps = work / 'openlibm-dependencies.mk'
    deps.write_text('-include $(wildcard */*.d)\n')
    run('openlibm', [['make', '-f', 'Makefile', '-f', deps, '-j' + str(jobs),
        'libopenlibm.a', *make_args]], source)
    stamp.write_text(identity)
    archive = source / 'libopenlibm.a'
    symbols = subprocess.check_output(['readelf', '--wide', '--syms', archive], text=True)
    internal = {fields[7] for line in symbols.splitlines()
                if len(fields := line.split()) >= 8 and fields[4] in ('GLOBAL', 'WEAK')
                and fields[6] != 'UND' and re.fullmatch(r'fe.*|__fe_.*', fields[7])}
    # Keep OpenLibm's BSD fenv representation private to its algorithms.
    renames = work / 'openlibm-fenv-namespace.map'
    renames.write_text(''.join(f'{name} vlibc_openlibm_{name}\n' for name in sorted(internal)))
    run('openlibm-namespace', [['objcopy', '--redefine-syms=' + str(renames), archive, installed]], work)
    if not shared:
        return
    public = {fields[7] for line in symbols.splitlines()
              if len(fields := line.split()) >= 8 and fields[4] in ('GLOBAL', 'WEAK')
              and fields[5] in ('DEFAULT', 'PROTECTED') and fields[6] != 'UND'} - internal
    exports = work / 'openlibm-public.map'
    exports.write_text('{ global:\n' + ''.join('  ' + name + ';\n' for name in sorted(public))
                       + 'local: *; };\n')
    runtime = subprocess.check_output(compiler + ['-print-libgcc-file-name'], text=True).strip()
    run('openlibm-shared', [compiler + ['-shared', '-nostdlib', '-Wl,--whole-archive', installed,
        '-Wl,--no-whole-archive', runtime, '-Wl,--version-script,' + str(exports),
        '-Wl,--eh-frame-hdr', '-Wl,-rpath,$ORIGIN', '-Wl,-soname,' + dso.name, '-o', dso]], work)
    defined = subprocess.check_output(['nm', '-D', '--defined-only', dso], text=True)
    actual = {line.split()[-1] for line in defined.splitlines() if line.split()}
    if not public or public - actual:
        raise RuntimeError('OpenLibm lost public exports: ' + ', '.join(sorted(public - actual)))
    alias.unlink(missing_ok=True)
    alias.symlink_to(dso.name)
    hashes = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in (installed, dso)}
    (prefix / receipt).write_text(json.dumps({'provenance': provenance, **hashes}, sort_keys=True) + '\n')
