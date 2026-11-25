"""Check Vast's private memory across visible repaints on a 24-bit Xvfb display."""
import ctypes as c
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import time


FRAMES_PER_SAMPLE = 200
SAMPLES = 8
COLORS = (0xf12040, 0x2050e0)


def child(address):
    assert os.isatty(1)
    with socket.socket(socket.AF_UNIX) as conn:
        conn.connect(address)
        for index in range(FRAMES_PER_SAMPLE * SAMPLES):
            if conn.recv(1) != b'+':
                raise RuntimeError('lost the redraw controller')
            color = COLORS[index % 2]
            sgr = f'\x1b[48;2;{color >> 16};{(color >> 8) & 255};{color & 255}m'
            # Overwrite the same cells without scrolling or growing terminal history.
            frame = f'\x1b[?25l\x1b[H{sgr}{" " * 48}\x1b[2;1H{index:04d}{" " * 44}'
            os.write(1, frame.encode('ascii'))
        conn.recv(1)  # Keep the window alive for the last measurement.


def open_x11():
    x = c.CDLL('libX11.so.6')
    x.XOpenDisplay.argtypes = [c.c_char_p]
    x.XOpenDisplay.restype = c.c_void_p
    x.XGetImage.argtypes = [c.c_void_p, c.c_ulong, c.c_int, c.c_int, c.c_uint,
                            c.c_uint, c.c_ulong, c.c_int]
    x.XGetImage.restype = c.c_void_p
    x.XGetPixel.argtypes = [c.c_void_p, c.c_int, c.c_int]
    x.XGetPixel.restype = c.c_ulong
    x.XDestroyImage.argtypes = [c.c_void_p]
    x.XCloseDisplay.argtypes = [c.c_void_p]
    return x


def wait_for_color(x, display, window, color):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        image = x.XGetImage(display, window, 18, 6, 1, 1, c.c_ulong(-1), 2)
        if not image:
            raise RuntimeError('XGetImage failed')
        pixel = x.XGetPixel(image, 0, 0)
        x.XDestroyImage(image)
        if pixel == color:
            return
        time.sleep(0.004)
    raise RuntimeError(f'X11 pixel remained {pixel:#08x} while waiting for {color:#08x}')


def private_dirty(pid):
    for line in Path(f'/proc/{pid}/smaps_rollup').read_text().splitlines():
        if line.startswith('Private_Dirty:'):
            return int(line.split()[1])
    raise RuntimeError('missing Private_Dirty in smaps_rollup')


def check(binary):
    x = open_x11()
    display = x.XOpenDisplay(None)
    if not display:
        raise RuntimeError('run with xvfb-run -a')
    try:
        with tempfile.TemporaryDirectory(prefix='vast-redraw-') as tmp, \
                socket.socket(socket.AF_UNIX) as listener:
            address = str(Path(tmp) / 'control')
            listener.bind(address)
            listener.listen(1)
            listener.settimeout(12)
            name = f'vast-redraw-{os.getpid()}'
            env = dict(os.environ, HOME=tmp)
            env.update({f'XDG_{kind}_HOME': f'{tmp}/{kind.lower()}'
                        for kind in ('CONFIG', 'CACHE', 'STATE', 'DATA')})
            command = [binary, '-t', name, '-g', '60x12', '-f', 'monospace:pixelsize=14',
                       '-e', sys.executable, Path(__file__).resolve(), '--child', address]
            with subprocess.Popen(command, env=env) as proc:
                try:
                    window = int(subprocess.check_output(
                        ['xdotool', 'search', '--sync', '--onlyvisible', '--name', f'^{name}$'],
                        timeout=12).splitlines()[0])
                    conn, _ = listener.accept()
                    samples = []
                    with conn:
                        for index in range(FRAMES_PER_SAMPLE * SAMPLES):
                            conn.sendall(b'+')
                            wait_for_color(x, display, window, COLORS[index % 2])
                            if (index + 1) % FRAMES_PER_SAMPLE == 0:
                                samples.append(private_dirty(proc.pid))
                        conn.sendall(b'.')
                    if proc.wait(timeout=5):
                        raise RuntimeError(f'Vast exited with status {proc.returncode}')
                finally:
                    if proc.poll() is None:
                        proc.kill()
    finally:
        x.XCloseDisplay(display)
    # Allow two samples for font/renderer caches and the initial GC heap.
    growth = max(samples[1:]) - samples[1]
    print(f'{FRAMES_PER_SAMPLE * SAMPLES} verified X11 redraws; Private_Dirty KiB: {samples}; '
          f'growth after warmup: {growth} KiB', flush=True)
    if growth > 8192:
        raise RuntimeError('private dirty memory grew by more than 8 MiB after warmup')


if __name__ == '__main__':
    if sys.argv[1:2] == ['--child']:
        child(sys.argv[2])
    else:
        check(Path(sys.argv[1]).resolve(strict=True))
