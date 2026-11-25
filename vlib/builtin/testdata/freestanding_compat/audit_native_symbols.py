"""Check every strong native-profile import against compiled runtime/DSO exports.

This inspects ELF manifests only. It does not infer declarations from headers,
load the libraries into the auditing process, or substitute for runtime tests.
"""
import argparse
from collections import defaultdict, deque
from functools import cache
from pathlib import Path
import re
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('prefix', type=Path)
parser.add_argument('runtime', type=Path, help='native V executable built with -Wl,--export-dynamic')
args = parser.parse_args()
prefix = args.prefix.resolve()
runtime = args.runtime.resolve()
libraries = sorted((prefix / 'lib').glob('lib*-vlibc.so'))
if not libraries:
    parser.error('the prefix contains no native shared libraries')


@cache
def symbols(path):
    output = subprocess.check_output(['readelf', '--wide', '--dyn-syms', str(path)], text=True)
    defined, imported = set(), set()
    for line in output.splitlines():
        fields = line.split()
        if len(fields) < 8:
            continue
        binding, visibility, section, name = fields[4:8]
        if section == 'UND':
            if binding == 'GLOBAL':
                imported.add(name)
        elif binding in ('GLOBAL', 'WEAK', 'UNIQUE', 'GNU_UNIQUE') and visibility in ('DEFAULT', 'PROTECTED'):
            # Only default-version exports can satisfy an unversioned reference.
            if '@' not in name or '@@' in name:
                defined.add(name.split('@', 1)[0])
            if '@' in name:
                defined.add(name.replace('@@', '@'))
    return defined, imported


headers = subprocess.check_output(['readelf', '--wide', '-l', str(runtime)], text=True)
match = re.search(r'Requesting program interpreter: ([^\]]+)\]', headers)
interpreter = Path(match[1]).resolve() if match else None
native = {path.resolve() for path in libraries}


@cache
def dynamic(path):
    text = subprocess.check_output(['readelf', '-Wd', path], text=True)
    needed = re.findall(r'\(NEEDED\).*?\[([^]]+)\]', text)
    def paths(tag):
        match = re.search(r'\(' + tag + r'\).*?\[([^]]*)\]', text)
        if not match:
            return []
        return [Path(entry.replace('${ORIGIN}', str(path.parent)).replace('$ORIGIN', str(path.parent)))
                for entry in match[1].split(':')]

    soname = re.search(r'\(SONAME\).*?\[([^]]+)\]', text)
    return needed, paths('RUNPATH'), paths('RPATH'), soname[1] if soname else path.name


def closure(root, initial=()):
    # Reuse already-loaded SONAMEs before searching the requesting object's
    # paths. RUNPATH is local to its direct dependencies; old RPATH is inherited.
    seen = set(initial)
    loaded = {dynamic(path)[3]: path for path in initial}
    if interpreter:
        loaded[interpreter.name] = interpreter
    pending = deque([(root, [])])
    while pending:
        path, inherited = pending.popleft()
        if path in seen:
            continue
        seen.add(path)
        needed, runpath, rpath, soname = dynamic(path)
        loaded.setdefault(soname, path)
        ancestors = inherited if runpath else rpath + inherited
        search = runpath or ancestors
        if interpreter:
            search = [*search, interpreter.parent]
        for name in needed:
            resolved = loaded.get(name)
            if resolved is None:
                resolved = next(((d / name).resolve() for d in search if (d / name).is_file()), None)
            if resolved is None:
                raise RuntimeError(f'{path.name}: cannot resolve DT_NEEDED {name}')
            if path in native and (not name.endswith('-vlibc.so') or resolved not in native):
                raise RuntimeError(f'{path.name}: non-native dependency {name}')
            loaded.setdefault(name, resolved)
            pending.append((resolved, ancestors))
    return seen


startup = closure(runtime)
if interpreter:
    startup.add(interpreter)
missing = defaultdict(set)


def check(scope, consumers):
    provided = set().union(*(symbols(path)[0] for path in scope))
    for obj in consumers:
        for symbol in symbols(obj)[1] - provided:
            missing[symbol].add(obj.name)


check(startup, startup)
for path in native - startup:
    # Validate each independently loadable library in the initial process
    # scope plus its own declared dependencies. Unrelated files cannot help.
    local = closure(path, startup)
    check(local, local - startup)
if missing:
    for symbol, consumers in sorted(missing.items()):
        print(symbol + ': ' + ', '.join(sorted(consumers)))
    raise SystemExit(f'{len(missing)} strong imports lack a provider in their load scope')
print(f'{len(native)} native DSOs: every strong import has a provider in its load scope')
