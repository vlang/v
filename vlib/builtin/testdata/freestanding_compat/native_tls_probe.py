"""Validate a built native-library fixture using private HTTPS and GPG data."""
import argparse
import contextlib
import http.server
import os
from pathlib import Path
import selectors
import signal
import socket
import ssl
import struct
import subprocess
import tempfile
import threading
import time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
args = parser.parse_args()
body = b'native HTTPS body\n'


def frame(kind, flags, stream, payload=b''):
    return len(payload).to_bytes(3, 'big') + bytes([kind, flags]) + struct.pack('!I', stream) + payload


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def exact(self, length):
        data = self.rfile.read(length)
        if len(data) != length:
            raise EOFError()
        return data

    def handle(self):
        if self.connection.selected_alpn_protocol() != 'h2':
            return super().handle()
        try:
            assert self.exact(24) == b'PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n'
            self.wfile.write(frame(4, 0, 0))  # SETTINGS
            responded = set()
            while True:
                header = self.exact(9)
                length = int.from_bytes(header[:3], 'big')
                kind, flags = header[3], header[4]
                stream = int.from_bytes(header[5:], 'big') & 0x7fffffff
                if length > 16384:
                    raise ValueError('oversized client frame')
                payload = self.exact(length)
                if kind == 4 and not flags & 1:
                    self.wfile.write(frame(4, 1, 0))
                elif kind == 6 and not flags & 1:
                    self.wfile.write(frame(6, 1, 0, payload))
                elif kind in (1, 9) and flags & 4 and stream not in responded:
                    responded.add(stream)
                    # HPACK: indexed :status=200, literal content-length=18.
                    headers = b'\x88\x0f\x0d\x02' + b'18'
                    self.wfile.write(frame(1, 4, stream, headers))
                    self.wfile.write(frame(0, 1, stream, body))
                elif kind == 7:
                    return
        except (EOFError, ConnectionError, ssl.SSLError):
            return


class Server(http.server.ThreadingHTTPServer):
    daemon_threads = True

    def handle_error(self, *_):
        # A deliberately rejected certificate closes a TLS handshake.
        pass


def checked(command, environment):
    result = subprocess.run(list(map(str, command)), env=environment,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=30)
    if result.returncode:
        raise RuntimeError(result.stdout)
    return result.stdout


with tempfile.TemporaryDirectory(prefix='vlibc-native-libraries-') as temporary:
    root = Path(temporary)
    environment = os.environ.copy()
    environment.pop('LD_PRELOAD', None)
    for key in ('HTTP_PROXY', 'HTTPS_PROXY', 'ALL_PROXY', 'http_proxy', 'https_proxy', 'all_proxy'):
        environment.pop(key, None)
    home = root / 'gnupg'
    home.mkdir(mode=0o700)
    environment.update(GNUPGHOME=str(home), XDG_CACHE_HOME=str(root / 'cache'))
    certificate, key = root / 'certificate.pem', root / 'key.pem'
    wrong_certificate = root / 'wrong-certificate.pem'
    data, signature = root / 'payload', root / 'payload.sig'
    data.write_bytes(b'local GPG verification payload\n')
    release = root / 'release'
    environment['V_NATIVE_PROBE_GATE'] = str(release)
    server = None
    worker = None
    process = None
    try:
        checked(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes',
            '-keyout', key, '-out', certificate, '-days', '1', '-subj', '/CN=localhost',
            '-addext', 'subjectAltName=IP:127.0.0.1,DNS:localhost'], environment)
        checked(['openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes',
            '-keyout', root / 'wrong-key.pem', '-out', wrong_certificate, '-days', '1',
            '-subj', '/CN=untrusted-root'], environment)
        environment['SSL_CERT_FILE'] = str(certificate)
        checked(['gpg', '--batch', '--pinentry-mode', 'loopback', '--passphrase', '',
            '--quick-generate-key', 'native-runtime-fixture', 'ed25519', 'sign', '0'], environment)
        checked(['gpg', '--batch', '--pinentry-mode', 'loopback', '--passphrase', '',
            '--detach-sign', '--output', signature, data], environment)
        server = Server(('127.0.0.1', 0), Handler)
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(certificate, key)
        context.set_alpn_protocols(['h2', 'http/1.1'])
        server.socket = context.wrap_socket(server.socket, server_side=True)
        worker = threading.Thread(target=server.serve_forever, kwargs={'poll_interval': .05})
        worker.start()
        url = f'https://127.0.0.1:{server.server_port}/'
        process = subprocess.Popen([str(args.binary.resolve()), url, str(certificate),
            str(signature), str(data), str(wrong_certificate)], env=environment,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, start_new_session=True)
        with selectors.DefaultSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ)
            if not selector.select(timeout=15):
                raise TimeoutError('native fixture startup')
            ready = process.stdout.readline()
        if ready.strip() != 'native library fixture ready':
            output, _ = process.communicate(timeout=5)
            raise RuntimeError(ready + output)
        maps = Path(f'/proc/{process.pid}/maps').read_text()
        for line in maps.splitlines():
            if '/libc.so' in line or '/libpthread.so' in line:
                raise AssertionError('native fixture loaded libc/pthread: ' + line)
        assert 'libcurl-vlibc.so' in maps and 'libvlibc_mbedtls-vlibc.so' in maps, maps
        release.touch()
        output, _ = process.communicate(timeout=25)
        if process.returncode:
            raise RuntimeError(output)
        assert 'independent native/V TLS, compression, GPG verification and seccomp APIs passed' in output, output
        sandbox = checked([args.binary.resolve(), 'sandbox'], environment)
        assert 'native seccomp enforcement passed' in sandbox, sandbox
        print('libc-free HTTP/2 and V HTTPS, certificate rejection, compression, valid/tampered GPG signatures and seccomp passed')
    finally:
        if process is not None and process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        if server is not None:
            server.shutdown()
            server.server_close()
        if worker is not None:
            worker.join(timeout=5)
        with contextlib.suppress(Exception):
            checked(['gpgconf', '--kill', 'gpg-agent'], environment)
