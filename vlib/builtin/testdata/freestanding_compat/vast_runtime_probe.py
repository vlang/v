"""Run Vast's PTY and clipboard integration test on a private Linux X display."""
import argparse
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time

from mapped_libraries import mapped_libraries, system_libc


def child_libraries(pid, binary):
    # x11_smoke launches Vast directly alongside short-lived X11 clients.
    children = Path(f'/proc/{pid}/task/{pid}/children')
    for child in children.read_text().split():
        proc = Path('/proc') / child
        try:
            if not (proc / 'exe').samefile(binary):
                continue
            return mapped_libraries(child)
        except (FileNotFoundError, ProcessLookupError):
            continue
    return set()


def check(binary, project, libc_free):
    suffix = '-vlibc.so' if libc_free else '.so.'
    required = [name + suffix for name in ('libX11', 'libXft', 'libfontconfig')]
    smoke = project / 'tests/x11_smoke.py'
    with tempfile.TemporaryDirectory(prefix='vast-runtime-') as tmp:
        env = dict(os.environ, HOME=tmp, PYTHONDONTWRITEBYTECODE='1')
        env.update({f'XDG_{kind}_HOME': f'{tmp}/{kind.lower()}'
                    for kind in ('CONFIG', 'CACHE', 'DATA', 'STATE')})
        with subprocess.Popen([sys.executable, smoke, binary], env=env,
                              start_new_session=True) as test:
            try:
                seen = False
                deadline = time.monotonic() + 60
                while test.poll() is None and time.monotonic() < deadline:
                    libs = child_libraries(test.pid, binary)
                    if libc_free and system_libc(libs):
                        raise RuntimeError(f'system libc loaded in Vast: {sorted(libs)}')
                    if all(any(lib.startswith(prefix) for lib in libs) for prefix in required):
                        if libc_free or system_libc(libs):
                            seen = True
                    time.sleep(0.05)
                status = test.wait(timeout=max(0, deadline - time.monotonic()))
                if status:
                    raise SystemExit(status)
                if not seen:
                    raise RuntimeError(f'Vast did not load the expected font stack: {required}')
            finally:
                if test.poll() is None:
                    os.killpg(test.pid, signal.SIGKILL)
                    test.wait()
    print('Vast process maps verified with ' + ('vlibc' if libc_free else 'system libc'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('--project', type=Path, required=True)
    parser.add_argument('--libc-free', action='store_true')
    args = parser.parse_args()
    check(args.binary.resolve(strict=True), args.project.resolve(strict=True), args.libc_free)
