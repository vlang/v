"""Run read-only vpac checks with a private configuration and database."""
import argparse
import errno
import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time

from mapped_libraries import mapped_libraries, system_libc


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
parser.add_argument('--libc-free', action='store_true')
args = parser.parse_args()
with tempfile.TemporaryDirectory(prefix='vlibc-vpac-app-') as temporary:
    root = Path(temporary)
    local = root / 'db/local'
    local.mkdir(parents=True)
    (local / 'ALPM_DB_VERSION').write_text('9\n')
    for directory in ('fs', 'keys', 'cache'):
        (root / directory).mkdir(mode=0o700)
    config = root / 'config.lip'
    source = (f'(path #:root "{root}/fs/" #:db "{root}/db/" #:gpg "{root}/keys" '
        f'#:log "{root}/vpac.log" #:cache #("{root}/cache/") #:hook #())\n'
        '(network #:parallel 2)\n')
    config.write_text(source)
    environment = os.environ.copy()
    environment.pop('LD_PRELOAD', None)
    environment['GNUPGHOME'] = str(root / 'keys')
    for name in ('CONFIG', 'CACHE', 'DATA', 'STATE'):
        environment[f'XDG_{name}_HOME'] = str(root / name.lower())
    binary = str(args.binary.resolve())

    def run(options, expected=0):
        result = subprocess.run([binary, '--config', str(config), *options],
            env=environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True, timeout=20)
        assert result.returncode == expected, (options, result.returncode, result.stdout, result.stderr)
        return result.stdout

    # The FIFO holds the real program in configuration input after loader startup.
    # It lets the parent inspect its mappings without modifying application code.
    fifo = root / 'held-config.lip'
    os.mkfifo(fifo, 0o600)
    process = subprocess.Popen([binary, '--config', str(fifo), '--help'],
        env=environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True, start_new_session=True)
    try:
        deadline = time.monotonic() + 20
        while True:
            if process.poll() is not None:
                output, errors = process.communicate()
                raise RuntimeError((process.returncode, output, errors))
            try:
                writer = os.open(fifo, os.O_WRONLY | os.O_NONBLOCK)
            except OSError as error:
                if error.errno != errno.ENXIO:
                    raise
            else:
                # Only vpac opens this FIFO. It cannot finish reading until we
                # write, so /proc now describes the loaded application.
                try:
                    proc = Path(f'/proc/{process.pid}')
                    assert (proc / 'exe').samefile(binary)
                    maps = (proc / 'maps').read_text()
                    if args.libc_free:
                        assert not system_libc(mapped_libraries(process.pid)), maps
                        for name in ('libcurl-vlibc.so', 'libarchive-vlibc.so', 'libgpgme-vlibc.so'):
                            assert name in maps, name
                    os.write(writer, source.encode())
                finally:
                    os.close(writer)
                break
            if time.monotonic() >= deadline:
                raise TimeoutError('vpac configuration input')
            time.sleep(.01)
        output, errors = process.communicate(timeout=20)
        assert process.returncode == 0 and 'usage  vpac' in output, (process.returncode, output, errors)
    finally:
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()

    assert 'usage  vpac' in run(['--help'])
    events = [json.loads(line) for line in run(['--ui', 'ipc', '--help']).splitlines()]
    assert [event['type'] for event in events] == ['hello', 'help', 'exit'], events
    assert events[0]['protocol'] > 0 and events[-1]['status'] == 0, events
    events = [json.loads(line) for line in run(['--ui', 'ipc', '--invalid-smoke-option'], 1).splitlines()]
    assert events[0]['type'] == 'hello' and any(event['type'] == 'error' for event in events), events
    assert events[-1]['type'] == 'exit' and events[-1]['status'] != 0, events
    assert run(['s', '--local']).strip() == ''
    version = '1.2-1'
    for name, description in [('runtime-alpha', 'Vlibc integration package'),
                              ('runtime-beta', 'Unrelated package')]:
        entry = local / f'{name}-{version}'
        entry.mkdir()
        (entry / 'desc').write_text(f'%NAME%\n{name}\n\n%VERSION%\n{version}\n\n'
                                    f'%DESC%\n{description}\n\n')
    events = [json.loads(line) for line in
              run(['--ui', 'ipc', 's', '--local', 'integration']).splitlines()]
    assert [event['type'] for event in events] == ['hello', 'listing', 'exit'], events
    listing = events[1]
    assert [item['name'] for item in listing['entries']] == ['runtime-alpha'], listing
    values = listing['entries'][0]['values']
    assert len(listing['fields']) == len(values), listing
    fields = dict(zip(listing['fields'], values))
    assert fields['source'] == 'local' and fields['version'] == version, fields
    assert fields['description'] == 'Vlibc integration package' and events[-1]['status'] == 0
    print('vpac private configuration, help, IPC framing/errors, package search and process maps passed')
