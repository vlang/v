"""Exercise an already-built Veb runtime fixture without external network access."""
import argparse
import concurrent.futures
import http.client
import json
import os
from pathlib import Path
import signal
import socket
import subprocess
import tempfile
import time

from mapped_libraries import mapped_libraries, system_libc


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
parser.add_argument('--libc-free', action='store_true')
args = parser.parse_args()
with tempfile.TemporaryDirectory(prefix='veb-syscalls-') as temp:
    root = Path(temp)
    asset = root / 'asset.txt'
    asset.write_bytes(b'native static file\n' * 4096)
    with socket.socket() as reservation:
        reservation.bind(('127.0.0.1', 0))
        port = reservation.getsockname()[1]
    environment = os.environ.copy()
    environment.update(XDG_CACHE_HOME=str(root / 'cache'))
    with (root / 'server.log').open('w+') as log:
        process = subprocess.Popen([str(args.binary.resolve()), str(port), str(asset)],
            env=environment, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            deadline = time.monotonic() + 15
            while True:
                if process.poll() is not None:
                    raise RuntimeError('server exited before listening')
                try:
                    with socket.create_connection(('127.0.0.1', port), timeout=.2):
                        break
                except OSError:
                    if time.monotonic() >= deadline:
                        raise TimeoutError('server startup')
                    time.sleep(.05)
            if args.libc_free and system_libc(mapped_libraries(process.pid)):
                raise AssertionError('server loaded libc/pthread')

            def request(method, path, body=None, headers=None):
                connection = http.client.HTTPConnection('127.0.0.1', port, timeout=5)
                try:
                    connection.request(method, path, body=body, headers=headers or {})
                    response = connection.getresponse()
                    return response.status, {key.lower(): value for key, value in response.getheaders()}, response.read()
                finally:
                    connection.close()

            status, headers, body = request('GET', '/')
            assert status == 200 and body == b'veb syscall runtime'
            assert headers.get('server') == 'veb'
            assert headers.get('x-runtime-probe') == 'before-request'
            status, _, body = request('GET', '/hello/world')
            assert status == 200 and json.loads(body) == {'ok': True, 'name': 'world'}
            payload = b'echo-body-' * 4096
            status, _, body = request('POST', '/echo', payload)
            assert status == 200 and body == payload
            status, _, body = request('GET', '/asset.txt')
            assert status == 200 and body == asset.read_bytes()
            status, _, body = request('GET', '/does-not-exist')
            assert status == 404
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
                results = list(pool.map(lambda _: request('GET', '/'), range(20)))
            assert all(code == 200 and body == b'veb syscall runtime' for code, _, body in results)
            status, _, body = request('GET', '/stop')
            assert status == 200 and body == b'stopping'
            assert process.wait(timeout=8) == 0
            log.seek(0)
            output = log.read()
            assert 'veb graceful shutdown passed' in output, output
            print('veb routes, JSON, middleware, POST, static files, concurrent requests and shutdown passed')
        except BaseException:
            log.seek(0)
            print(log.read())
            raise
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
