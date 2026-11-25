"""Identify the ELF libraries actually mapped by a Linux fixture process."""
from functools import lru_cache
import os
from pathlib import Path
import re
import subprocess


@lru_cache(maxsize=512)
def _soname(path, device, inode):
    # The filename may be a release name (libc-2.33.so) or a renamed DSO.
    # The inode also makes the cache safe across replacement of a pathname.
    stat = path.stat()
    if (os.major(stat.st_dev), os.minor(stat.st_dev), stat.st_ino) != (*device, inode):
        raise OSError(f'mapped file changed: {path}')
    result = subprocess.run(['readelf', '-Wd', str(path)], capture_output=True, text=True,
                            env=dict(os.environ, LC_ALL='C'))
    result.check_returncode()
    match = re.search(r'\(SONAME\).*?\[([^]]+)\]', result.stdout)
    return match[1] if match else path.name


def mapped_libraries(pid):
    libraries = set()
    for line in Path(f'/proc/{pid}/maps').read_text().splitlines():
        fields = line.split(None, 5)
        if len(fields) < 6 or 'x' not in fields[1] or not fields[5].startswith('/'):
            continue
        device = tuple(int(part, 16) for part in fields[3].split(':'))
        file = Path(fields[5].removesuffix(' (deleted)'))
        try:
            name = _soname(file, device, int(fields[4]))
        except OSError:
            name = _soname(Path(f'/proc/{pid}/map_files/{fields[0]}'), device, int(fields[4]))
        libraries.add(name)
    return libraries


def system_libc(libraries):
    return any(name in ('libc.so', 'libpthread.so') or
               name.startswith(('libc.so.', 'libpthread.so.', 'ld-musl-', 'libc.musl-'))
               for name in libraries)
