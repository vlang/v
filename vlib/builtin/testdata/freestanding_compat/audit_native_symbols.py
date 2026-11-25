"""Check every strong native-profile import against compiled runtime/DSO exports.

This inspects ELF manifests only. It does not infer declarations from headers,
load the libraries into the auditing process, or substitute for runtime tests.
"""
import argparse
from collections import defaultdict
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
provided = symbols(runtime)[0]
if match:
    provided.update(symbols(Path(match[1]))[0])
imports = {}
for library in libraries:
    definitions, required = symbols(library)
    provided.update(definitions)
    imports[library.name] = required
    dynamic = subprocess.check_output(['readelf', '-d', str(library)], text=True)
    for child in re.findall(r'\(NEEDED\).*\[([^]]+)\]', dynamic):
        if not child.endswith('-vlibc.so') or not (prefix / 'lib' / child).is_file():
            raise RuntimeError(library.name + ': non-native dependency ' + child)
missing = defaultdict(list)
for library, required in imports.items():
    for symbol in sorted(required - provided):
        missing[symbol].append(library)
if missing:
    for symbol, consumers in sorted(missing.items()):
        print(symbol + ': ' + ', '.join(consumers))
    raise SystemExit(str(len(missing)) + ' strong runtime imports are not provided')
print(str(len(libraries)) + ' native DSOs: every strong import has a compiled provider')
