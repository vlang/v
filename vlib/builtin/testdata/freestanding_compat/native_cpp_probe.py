"""Check a built native C++ fixture with private files and live process maps."""
import argparse
import os
from pathlib import Path
import selectors
import resource
import signal
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
args = parser.parse_args()
with tempfile.TemporaryDirectory(prefix='vlibc-native-cpp-') as temporary:
    root = Path(temporary)
    gate = root / 'release'
    environment = os.environ.copy()
    environment.pop('LD_PRELOAD', None)
    environment['V_NATIVE_CPP_GATE'] = str(gate)
    process = subprocess.Popen([str(args.binary.resolve()), str(root)], env=environment,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, start_new_session=True,
        preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)))
    try:
        with selectors.DefaultSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ)
            if not selector.select(timeout=15):
                raise TimeoutError('native C++ startup')
            ready = process.stdout.readline()
        if ready.strip() != 'native C++ fixture ready':
            output, _ = process.communicate(timeout=5)
            raise RuntimeError(ready + output)
        maps = Path(f'/proc/{process.pid}/maps').read_text()
        for line in maps.splitlines():
            if '/libc.so' in line or '/libpthread.so' in line or '/libstdc++.so.6' in line:
                raise AssertionError('native C++ fixture loaded a host runtime: ' + line)
        for library in ['libstdc++-vlibc.so', 'libquadmath-vlibc.so', 'libgcc_s-vlibc.so', 'libm-vlibc.so']:
            assert library in maps, (library, maps)
        gate.touch()
        output, _ = process.communicate(timeout=30)
        if process.returncode:
            raise RuntimeError(f'native C++ fixture exit {process.returncode}: ' + output)
        assert 'native C++ streams, locales, filesystem, shared exceptions, threads, cancellation and binary128 passed' in output, output
        assert (root / 'destructor.marker').read_bytes() == b'native C++ destructor\n'
        print('libc-free native C++ runtime, full-precision binary128 and DSO finalization passed')
    finally:
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
